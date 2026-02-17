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
    ; {namespaces = ["String"] ; name = "replace" ; v = Args3 ml_string_replace ; tau = Arr (TypeString, Arr (TypeString, Arr (TypeString, TypeString)))} (* TODO properly namespace *)
  ]

let pre_included_environment : environment =
  List.fold_left
    (fun acc entry -> Environment.add_and_path entry.namespaces entry.name (straigthforward_ml_function entry.name entry.v) acc)
    Environment.empty
    predefined_symbols

let pre_included_typing_env : modular_typing_environment =
  List.fold_left
    (fun acc entry -> Environment.add_and_path entry.namespaces entry.name entry.tau acc)
    Environment.empty
    predefined_symbols

(** Producing a page *)

let debug = true

let displayed = ["raw"; "lexed"; "parsed"; "typed"; "eval'd"]

(** [produce_page source dest] reads a HTML/ML webpage from [source], computes the resulting html webpage and writes it in [dest] *)
let produce_page (arguments : environment) (source : in_channel) (dest : out_channel) : unit =
  let f_in = source in
  let code : string = read_whole_file_str f_in in
  close_in f_in;
  try
    let lexed = lexer code in
    let parsed = parser lexed in
    let typ_env = Environment.disjoint_union (Environment.map (fun _ -> TypeString) arguments) pre_included_typing_env in
    let eval_env = Environment.disjoint_union arguments pre_included_environment in
    let _ = type_inferer typ_env parsed in
    let final_env, values = eval eval_env parsed in
    let f_out = dest in
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
    | PrelexingError s -> let f_out = dest in Printf.fprintf f_out "\nPrelexingError: %s\n" s; close_out f_out
    | LexingError s -> let f_out = dest in Printf.fprintf f_out "\nLexingError: %s\n" s; close_out f_out
    | ParsingError s -> let f_out = dest in Printf.fprintf f_out "\nParsingError: %s\n" s; close_out f_out
    | TypingError s -> let f_out = dest in Printf.fprintf f_out "\nTypingError: %s\n" s; close_out f_out
    | UnificationError (alpha, beta, Recursive) -> let f_out = dest in Printf.fprintf f_out "\nUnificationError: %s and %s recursive.\n" (string_of_ml_type alpha) (string_of_ml_type beta); close_out f_out
    | UnificationError (alpha, beta, Incompatible) -> let f_out = dest in Printf.fprintf f_out "\nUnificationError: %s and %s incompatible.\n" (string_of_ml_type alpha) (string_of_ml_type beta); close_out f_out
    | InterpreterError s -> let f_out = dest in Printf.fprintf f_out "\nInterpreterError: %s\n" s; close_out f_out
    | UnsupportedError s -> let f_out = dest in Printf.fprintf f_out "\nUnsupportedError: %s\n" s; close_out f_out
  
let test_file (source : string) : unit =
  let f_in = open_in source in
  let code : string = read_whole_file_str f_in in
  close_in f_in;
  Test.test (-1, code)

exception MalformedCommandLine

(** [parse_command_line command_line = (source, dest, env)] parses the command line and retrieves the path to the source file [source], the path to the destination [dest] and the environment passed in argument [env] e.g. Post/Get/Session variables.
  Raises [MalformedCommandLine] if [command_line] is ill-formed. *)
let parse_command_line (command_line : string array) : in_channel * out_channel * environment =
  let n = Array.length command_line in
  if n > 2 then
    let source_path = command_line.(1) in
    let dest_path = command_line.(2) in
    let source_fd =
      if source_path = "-stdin" then
        stdin
      else
        open_in source_path
    in
    let dest_fd =
      if dest_path = "-stdout" then
        stdout
      else
        open_out dest_path
    in
    let rec parse_options (command_line : string array) (i : int) (env_acc : environment) : environment =
      if i < n then
        if i + 1 >= n then (* options are, for now, all of the form [-options ARGUMENTS] *)
          raise MalformedCommandLine
        else begin
          match String.lowercase_ascii command_line.(i) with
            | "-argrepr" -> parse_options command_line (i + 2) (Parse_url_dictionary.parse_url_dictionary value_of_repr command_line.(i + 1) env_acc)
            | "-argstr" -> parse_options command_line (i + 2) (Parse_url_dictionary.parse_url_dictionary (fun s -> VString s) command_line.(i + 1) env_acc)
            | _ -> raise MalformedCommandLine
        end
      else
        env_acc
    in
    (source_fd, dest_fd, parse_options command_line 3 Environment.empty)
  else
    raise MalformedCommandLine

let () =
  try
    let source, dest, env_args = parse_command_line Sys.argv in
    produce_page env_args source dest;
    close_in source;
    close_out dest
  with
    MalformedCommandLine -> begin
      let help_message_path = "./help_message.txt" in
      let f_help_msg = open_in help_message_path in
      let help_message = read_whole_file_str f_help_msg in
      Printf.printf "%s\n" help_message
    end