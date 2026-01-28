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
let produce_page (path : string) (dest : string) : unit =
  let f_in = open_in path in
  let code : string = str_of_file f_in in
  close_in f_in;
  let lexed = lexer code in
  let parsed = parser lexed in
  let _ = type_inferer StringMap.empty parsed in
  let values = eval StringMap.empty parsed in
  let f_out = open_out dest in
  List.iter (fun v -> match v with
    | VContent h -> Printf.fprintf f_out "%s" h
    | _ -> Printf.fprintf f_out "%s" (web_of_string (string_of_value ~escape_html:true v))
  ) values;
  close_out f_out
  
let test_file (source : string) : unit =
  let f_in = open_in source in
  let code : string = str_of_file f_in in
  close_in f_in;
  Test.test (-1, code)


let () =
  if Array.length Sys.argv > 2 then begin
    let source_path = Sys.argv.(1) in
    let dest_path = Sys.argv.(2) in
    produce_page source_path dest_path
    (* test_file source_path *)
  end else
    Printf.fprintf stderr "Usage: <program> <source path> <dest path>"
