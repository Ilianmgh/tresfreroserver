open Utils
open Lexer
open Syntax
open Parser
open TypeSyntax
open Typechecker
open Interpreter

(** Defining extern functions *)

type extern_symbol = {namespaces : module_name list ; name : string ; v : extern_function ; tau : ml_type}

let ml_type_of_sqlite_exec =
  Arr (
    TypeDb,
    Arr (
      Arr (TypeHtml, Arr (TypeHtml, TypeHtml)),
      Arr (
        Arr (TypeHtml, Arr (TypeString, Arr (TypeString, TypeHtml))),
        Arr (TypeString,
          TypeHtml
        )
      )
    )
  )

(** [straightforward_ml_function (Argsi f)] (with [i = 1,2,3,4]) is a value [v] corresponding to the ml function that reflects [f : value -> value]*)
let straigthforward_ml_function (name : string) (f : extern_function) : value =
  Clos (Environment.empty, VExternFunction (name, f))

let ml_fst (v : value) : value = match v with
  | VCouple (v1, v2) -> v1
  | _ -> raise (InterpreterError (Printf.sprintf "%s: expected a pair." (string_of_value v)))
let ml_snd (v : value) : value = match v with
  | VCouple (v1, v2) -> v2
  | _ -> raise (InterpreterError (Printf.sprintf "%s: expected a pair." (string_of_value v)))

(** List of predefined symbols, pre-loaded in the environment at execution. *)
let predefined_symbols = [
      {namespaces = [sqlite_module_name] ; name = "exec"    ; v = Args4 extern_sqlite_exec ; tau = ml_type_of_sqlite_exec}
    ; {namespaces = [sqlite_module_name] ; name = "opendb"  ; v = Args1 extern_sqlite_open_db ; tau = Arr (TypeString, TypeDb)}
    ; {namespaces = [sqlite_module_name] ; name = "closedb" ; v = Args1 extern_sqlite_close_db ; tau = Arr (TypeDb, TypeString)}
    ; {namespaces = [] ; name = "fst" ; v = Args1 ml_fst ; tau = TypeForall ("'fst", TypeForall ("'snd", Arr (Prod (TypeVar "'fst", TypeVar "'snd"), TypeVar "'fst")))}
    ; {namespaces = [] ; name = "snd" ; v = Args1 ml_snd ; tau = TypeForall ("'fst", TypeForall ("'snd", Arr (Prod (TypeVar "'fst", TypeVar "'snd"), TypeVar "'snd")))}
    (* ; {namespaces = [session_module_name] ; name = "let" ; v = Args1 ml_snd ; tau = TypeForall ("'fst", TypeForall ("'snd", Arr (Prod (TypeVar "'fst", TypeVar "'snd"), TypeVar "'snd")))} *)
  ]

let pre_included_environment : environment =
  List.fold_left
    (fun acc entry -> Environment.add_to_sub entry.namespaces entry.name (straigthforward_ml_function entry.name entry.v) acc)
    (Environment.add_sub sqlite_module_name Environment.empty Environment.empty) (* FIXME add this generically, not one by one for each added module *)
    predefined_symbols

let pre_included_typing_env : modular_typing_environment =
  List.fold_left
    (fun acc entry -> Environment.add_to_sub entry.namespaces entry.name entry.tau acc)
    (Environment.add_sub sqlite_module_name Environment.empty Environment.empty) (* FIXME add this generically, not one by one for each added module *)
    predefined_symbols

(** Producing a page *)

let debug = true

let displayed = ["raw"; "lexed"; "parsed"; "typed"; "eval'd"]

(** [str_of_path f] returns the content of the file [f] (passed as a [in_channel]), as a string *)
let str_of_file (f : in_channel) =
  let rec char_list_of_path (acc : char list) (f : in_channel) : char list =
    try
      char_list_of_path ((input_char f) :: acc) f
    with
      | End_of_file -> acc
  in
  let s = string_of_char_list (List.rev (char_list_of_path [] f)) in
  s

(** [produce_page path dest] reads the HTML/ML webpage pointed to by [path], computes the resulting html webpage and writes it in the file [dest] *)
let produce_page (arguments : environment) (path : string) (dest : string) : unit =
  let f_in = open_in path in
  let code : string = str_of_file f_in in
  close_in f_in;
  try
    let lexed = lexer code in
    let parsed = parser lexed in
    let typ_env = Environment.disjoint_union (Environment.map (fun _ -> TypeString) arguments) pre_included_typing_env in
    let _ = type_inferer typ_env parsed in
    let final_env, values = eval arguments parsed in
    let f_out = open_out dest in
    (* The first line contains information we want to send to the server, but that won't be sent to the client. *)
    (* slight optimization: do not re-send session variables that were not modified; but simply mention to the server to keep them *)
    let session_bindings = match Environment.submap_opt session_module_name final_env with
      | None -> ""
      | Some session_env -> Environment.fold
        (fun prefixes x v acc ->
          if List.is_empty prefixes then begin (* for now, only top-level within module Session, could change for further needs *)
            Printf.fprintf stderr "trying to repr: %s" (string_of_value v);
            Printf.sprintf "%s&%s=%s" acc x (repr_of_value v)
          end else
            acc
        )
        session_env ""
    in
    Printf.fprintf f_out "session%s\n" session_bindings;
    List.iter (fun v -> fprintf_value f_out v) values;
    close_out f_out
  with
    | PrelexingError s -> let f_out = open_out dest in Printf.fprintf f_out "\nPrelexingError: %s\n" s; close_out f_out
    | LexingError s -> let f_out = open_out dest in Printf.fprintf f_out "\nLexingError: %s\n" s; close_out f_out
    | ParsingError s -> let f_out = open_out dest in Printf.fprintf f_out "\nParsingError: %s\n" s; close_out f_out
    | TypingError s -> let f_out = open_out dest in Printf.fprintf f_out "\nTypingError: %s\n" s; close_out f_out
    | UnificationError (alpha, beta, Recursive) -> let f_out = open_out dest in Printf.fprintf f_out "\nUnificationError: %s and %s recursive.\n" (string_of_ml_type alpha) (string_of_ml_type beta); close_out f_out
    | UnificationError (alpha, beta, Incompatible) -> let f_out = open_out dest in Printf.fprintf f_out "\nUnificationError: %s and %s incompatible.\n" (string_of_ml_type alpha) (string_of_ml_type beta); close_out f_out
    | InterpreterError s -> let f_out = open_out dest in Printf.fprintf f_out "\nInterpreterError: %s\n" s; close_out f_out
    | UnsupportedError s -> let f_out = open_out dest in Printf.fprintf f_out "\nUnsupportedError: %s\n" s; close_out f_out
  
let test_file (source : string) : unit =
  let f_in = open_in source in
  let code : string = str_of_file f_in in
  close_in f_in;
  Test.test (-1, code)

exception MalformedCommandLine

(** [parse_command_line command_line = (source, dest, env)] parses the command line and retrieves the path to the source file [source], the path to the destination [dest] and the environment passed in argument [env] e.g. Post/Get/Session variables.
  Raises [MalformedCommandLine] if [command_line] is ill-formed. *)
let parse_command_line (command_line : string array) : string * string * environment =
  let n = Array.length command_line in
  if n > 2 then
    let source_path = command_line.(1) in
    let dest_path = command_line.(2) in
    let rec parse_options (command_line : string array) (i : int) (env_acc : environment) : environment =
      if i < n then
        if i + 1 >= n then (* options are, for now, all of the form [-options ARGUMENTS] *)
          (Printf.fprintf stderr "1\n"; raise MalformedCommandLine)
        else begin
          match String.lowercase_ascii command_line.(i) with
            | "-argrepr" -> parse_options command_line (i + 2) (Parse_url_dictionary.parse_url_dictionary value_of_repr command_line.(i + 1) env_acc)
            | "-argstr" -> parse_options command_line (i + 2) (Parse_url_dictionary.parse_url_dictionary (fun s -> VString s) command_line.(i + 1) env_acc)
            | _ -> Printf.fprintf stderr "2\n"; raise MalformedCommandLine
        end
      else
        env_acc
    in
    (source_path, dest_path, parse_options command_line 3 Environment.empty)
  else
    (Printf.fprintf stderr "3\n"; raise MalformedCommandLine)
let () =
  try
    let source_path, dest_path, env_args = parse_command_line Sys.argv in
    produce_page env_args source_path dest_path
  with
    MalformedCommandLine -> Printf.printf "Usage: <program> <source path> <dest path> <arguments>*\n<arguments> are pre-defined variable to give to the program. It has to be of the form METHOD&x1=v1&...&xn=vn" (* TODO refresh that *)

    (* Namespacing works for variable ; implemented correctly for Get/Post request. TODO implement it for Sqlite functions + for Session, maybe add a function or a different global declaration variable in the namespace Session to declare session variables. FIXME problem with GET form in tests *)