open Utils
open Lexer
open Syntax
open Parser
open TypeSyntax
open Typechecker
open Interpreter

(** Defining extern functions *)

type extern_symbol = {namespaces : module_name list ; name : variable ; v : value ; tau : ml_type}

let sqlite_module_name = "Sqlite"

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

(* 
let canonical_environment =
  let sql_module = Environment.add "exec" (Clos (Environment.empty, VExternFunction (Args4 extern_sqlite_exec))) Environment.empty in
  Environment.add_sub "Sql" sql_module Environment.empty *)

(** [straightforward_ml_function1 f] is a value [v] corresponding to the ml function that reflects [f : value -> value]*)
let straigthforward_ml_function1 (f : value -> value) : value = Clos (Environment.empty, VExternFunction (Args1 f))

(** [straightforward_ml_function2 f] is a value [v] corresponding to the ml function that reflects [f : value -> value]*)
let straigthforward_ml_function2 (f : value -> value -> value) : value = Clos (Environment.empty, VExternFunction (Args2 f))

(** [straightforward_ml_function3 f] is a value [v] corresponding to the ml function that reflects [f : value -> value]*)
let straigthforward_ml_function3 (f : value -> value -> value -> value) : value = Clos (Environment.empty, VExternFunction (Args3 f))

(** [straightforward_ml_function4 f] is a value [v] corresponding to the ml function that reflects [f : value -> value]*)
let straigthforward_ml_function4 (f : value -> value -> value -> value -> value) : value = Clos (Environment.empty, VExternFunction (Args4 f))

let predefined_symbols = [
      {namespaces = [sqlite_module_name] ; name = "exec"    ; v = straigthforward_ml_function4 extern_sqlite_exec ; tau = ml_type_of_sqlite_exec}
    ; {namespaces = [sqlite_module_name] ; name = "opendb"  ; v = straigthforward_ml_function1 extern_sqlite_open_db ; tau = Arr (TypeString, TypeDb)}
    ; {namespaces = [sqlite_module_name] ; name = "closedb" ; v = straigthforward_ml_function1 extern_sqlite_close_db ; tau = Arr (TypeDb, TypeString)}
  ]

let pre_included_environment =
  List.fold_left
    (fun acc entry -> Environment.add_to_sub entry.namespaces entry.name entry.v acc)
    (Environment.add_sub sqlite_module_name Environment.empty Environment.empty)
    predefined_symbols

let pre_included_typing_env =
  List.fold_left
    (fun acc entry -> Environment.add_to_sub entry.namespaces entry.name entry.tau acc)
    (Environment.add_sub sqlite_module_name Environment.empty Environment.empty)
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
    let typ_env = Environment.fold Environment.add_to_sub pre_included_typing_env (Environment.map (fun _ -> TypeString) arguments) in (* FIXME for now, adding one by one implement union/merge for next time. *)
    let _ = type_inferer typ_env parsed in
    let (session_vars, final_env), values = eval arguments parsed in
    let f_out = open_out dest in
    (* The first line contains information we want to send to the server, but that won't be sent to the client. *)
    Printf.fprintf f_out "session%s\n" (List.fold_left (fun acc name -> Printf.sprintf "&%s=%s" name (repr_of_value (Environment.find name final_env))) "" session_vars);
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

let () =
  if Array.length Sys.argv > 2 then begin
    let source_path = Sys.argv.(1) in
    let dest_path = Sys.argv.(2) in
    (* TODO allow more lax multiple dictionaries passed this way, not strictly GETorPOST SESSION *)
    let arguments = if Array.length Sys.argv > 3 then (* parsing GET/POST arguments *)
        Parse_url_dictionary.parse_url_dictionary (fun s -> VString s) Sys.argv.(3) pre_included_environment
      else
        pre_included_environment
    in
    let args = if Array.length Sys.argv > 4 then (* parsing SESSION arguments *)
        Parse_url_dictionary.parse_url_dictionary (fun s -> VString s) Sys.argv.(4) arguments
      else
        arguments
    in
    if debug then Printf.fprintf stderr "Pre-env: %s\n" (string_of_env args);
    produce_page args source_path dest_path
    (* test_file source_path *)
  end else
    Printf.printf "Usage: <program> <source path> <dest path> <arguments>*\n<arguments> are pre-defined variable to give to the program. It has to be of the form METHOD&x1=v1&...&xn=vn"

    (* Namespacing works for variable ; implemented correctly for Get/Post request. TODO implement it for Sqlite functions + for Session, maybe add a function or a different global declaration variable in the namespace Session to declare session variables. FIXME problem with GET form in tests *)