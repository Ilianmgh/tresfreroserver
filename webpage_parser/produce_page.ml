open Utils
open Lexer
open Syntax
open Parser
open TypeSyntax
open Typechecker
open Interpreter

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
    let _ = type_inferer (StringMap.map (fun s -> TypeString) arguments) parsed in
    let (session_vars, final_env), values = eval arguments parsed in
    let f_out = open_out dest in
    (* The first line contains information we want to send to the server, but that won't be sent to the client. *)
    Printf.fprintf f_out "session%s\n" (List.fold_left (fun acc name -> Printf.sprintf "&%s=%s" name (repr_of_value (StringMap.find name final_env))) "" session_vars);
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
  let f_in = open_in source in (* TODO add actual namespacing, especially for GET, POST and SESSION (and COOKIES at some point) variables *)
  let code : string = str_of_file f_in in
  close_in f_in;
  Test.test (-1, code)

let () =
  if Array.length Sys.argv > 2 then begin
    let source_path = Sys.argv.(1) in
    let dest_path = Sys.argv.(2) in
    (* TODO allow more lax multiple dictionaries passed this way, not strictly GETorPOST SESSION *)
    let arguments = if Array.length Sys.argv > 3 then
        Parse_url_dictionary.parse_url_dictionary (fun s -> VString s) Sys.argv.(3)
      else
        StringMap.empty
    in
    let session_arguments = if Array.length Sys.argv > 4 then
        Parse_url_dictionary.parse_url_dictionary (fun s -> VString s) Sys.argv.(4)
      else
        StringMap.empty
    in
    let args = StringMap.union (fun key val1 val2 -> None)(* TODO replace by parse_url_dictionary taking a dictionary as argument ... *) arguments session_arguments in
    (* Printf.printf "\nCURRENTENVPREEVALUATION:%s\n%!" (string_of_env args); *)
    produce_page args source_path dest_path
    (* test_file source_path *)
  end else
    Printf.printf "Usage: <program> <source path> <dest path> <arguments>*\n<arguments> are pre-defined variable to give to the program. It has to be of the form METHOD&x1=v1&...&xn=vn"
