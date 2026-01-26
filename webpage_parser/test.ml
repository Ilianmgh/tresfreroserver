open Utils
open Lexic
open Lexer
open Syntax
open Parser
open TypeSyntax
open Typechecker
open Interpreter

let displayed = ["raw"; "parsed"; "typed"; "eval'd"]

let test (i, code : int * string) : unit =
  begin if i < 0 then
    Printf.printf "\n\tTEST\n"
  else
    Printf.printf "\n\tTEST n°%d\n\n" i
  end;
  try
    if List.mem "raw" displayed then Printf.printf "raw: %s\n%!" code;
    let lexed : token list = lexer code in
    if List.mem "lexed" displayed then Printf.printf "lexed: %s\n%!" (string_of_list string_of_token lexed);
    let _, parsed, _ = parser lexed in
    if List.mem "parsed" displayed then Printf.printf "parsed: %s\n%!" (string_of_expr parsed);
    let gamma, tau = type_inferer StringMap.empty parsed in
    if List.mem "typed" displayed then Printf.printf "typed: %s\nIn env: %s\n%!" (string_of_ml_type tau) (string_of_typing_env gamma);
    let v = eval StringMap.empty parsed in
    if List.mem "eval'd" displayed then Printf.printf "eval'd: %s\n%!" (string_of_value v)
  with
    | LexingError s -> Printf.fprintf stdout "LexingError: %s\n" s
    | ParsingError s -> Printf.fprintf stdout "ParsingError: %s\n" s
    | TypingError s -> Printf.fprintf stdout "TypingError: %s\n" s
    | UnificationError (alpha, beta, Recursive) -> Printf.fprintf stdout "UnificationError: %s and %s recursive.\n" (string_of_ml_type alpha) (string_of_ml_type beta)
    | UnificationError (alpha, beta, Incompatible) -> Printf.fprintf stdout "UnificationError: %s and %s incompatible.\n" (string_of_ml_type alpha) (string_of_ml_type beta)
    | InterpreterError s -> Printf.fprintf stdout "InterpreterError: %s\n" s
    | Failure "TODO" -> Printf.fprintf stdout "TODO\n"

let () =
  let tests = [
      "<{}>"
      ;"something%else<{begin fun x -> y end}>some%more%<{let fun fun ^ \"coucou\"}>%and%finally%"
      ;"<{let x = 5 in x}>"
      ;"<{let x = 5 in % x}>"
      ;"<{f\"coucou\"}>"
      ;"<{let x = 5, 2 in fst x}>"
      ;"<h1>Exampel</h1>%<{% let x = 1 in% if x + 1 = 2 then%}>% 2%<{% else %}>% DEADCODE%<{}>" (* FIXME parsed [if x + (1 = 2) ...] instead of [if (x + 1) = 2 ...]*)
      ;"<{2 + 1 = 3}>" (* FIXME parsed [if x + (1 = 2) ...] instead of [if (x + 1) = 2 ...]*)
      ;"<{1-1}>" (* FIXME is parsed as the function 1 applied to (-1) *)
      ;"<{let y = 3 in let x = 4 in (fun x -> y) 5}>"
      ;"<{f x y}>"
      ;"<{if true then 1 else 2; 3}>" (* FIXME is parsed [if true then 1 else (2; 3)] and not as it should [(if true then 1 else 2); 3] *)
      (* ;"<{if true then () else (); 2}>" FIXME parse unit cf parser.ml *)
      (* ;"<{if true then 1; 2}>" FIXME add this syntax sugar *)
    ]
  in
  if Array.length Sys.argv <= 1 then
    Printf.fprintf stderr "Usage: ./test.x <test>\n<test>\n\ti: the i-th hard-coded test, if negative, run all the tests\n\ta string of a code that will be tested."
  else begin
    let tests = List.map (fun s -> String.map (fun c -> if c = '%' then '\n' else c) s) tests in
    let builtin_test, param = try
        (true, string_of_int (int_of_string Sys.argv.(1))) (* indicates one of the hard-coded example, if [< 0], then we test them all. *)
      with
        _ -> (false, Sys.argv.(1)) in
    let cases = if builtin_test then begin
        let i = int_of_string param in
        if i >= 0 then
          [(i, List.nth tests i)]
        else
          List.mapi (fun i x -> (i, x)) tests
      end else
        [(-1, Sys.argv.(1))]
    in
    List.iter test cases
  end