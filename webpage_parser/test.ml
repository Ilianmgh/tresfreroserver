open Utils
open Lexic
open Prelexer
open Lexer
open Syntax
open Parser
open TypeSyntax
open Typechecker
open Interpreter

let displayed = ["raw"; "prelexed"; "lexed"; "parsed"; "typed"; "eval'd"]

let test (i, code : int * string) : unit =
  begin if i < 0 then
    Printf.printf "\n\tTEST\n"
  else
    Printf.printf "\n\tTEST n°%d\n\n" i
  end;
  try
    if List.mem "raw" displayed then Printf.printf "raw: %s\n%!" code;
    let prelexed : pre_token list = prelexer_all code 0 (String.length code) in
    if List.mem "prelexed" displayed then Printf.printf "prelexed: %s\n%!" (string_of_list string_of_pre_token prelexed);
    let lexed : token list = pre_tokens_lexer prelexed in
    if List.mem "lexed" displayed then Printf.printf "lexed: %s\n%!" (string_of_list string_of_token lexed);
    let parsed = parser lexed in
    if List.mem "parsed" displayed then Printf.printf "parsed: %s\n%!" (string_of_dynpage parsed);
    let types_infered = type_inferer StringMap.empty parsed in
    if List.mem "typed" displayed then List.iter (fun (gamma, tau) -> Printf.printf "typed: %s\nIn env: %s\n%!" (string_of_ml_type tau) (string_of_typing_env gamma)) types_infered;
    let values_evald = eval StringMap.empty parsed in
    if List.mem "eval'd" displayed then List.iter (fun v -> Printf.printf "eval'd: %s\n%!" (string_of_value v)) values_evald
  with
    | LexingError s -> Printf.fprintf stdout "LexingError: %s\n" s
    | ParsingError s -> Printf.fprintf stdout "ParsingError: %s\n" s
    | TypingError s -> Printf.fprintf stdout "TypingError: %s\n" s
    | UnificationError (alpha, beta, Recursive) -> Printf.fprintf stdout "UnificationError: %s and %s recursive.\n" (string_of_ml_type alpha) (string_of_ml_type beta)
    | UnificationError (alpha, beta, Incompatible) -> Printf.fprintf stdout "UnificationError: %s and %s incompatible.\n" (string_of_ml_type alpha) (string_of_ml_type beta)
    | InterpreterError s -> Printf.fprintf stdout "InterpreterError: %s\n" s
    | Failure "TODO" -> Printf.fprintf stdout "TODO\n"

(*

<h1>Example</h1>
<{
  let x = 1 in
  if x + 1 = 2 then <[
    2
  ]> else <[
    DEADCODE
  ]>
}>
*)

let test_input () =
  let tests = [
      "<{}>"
      ;"something%else<{begin fun x -> y end}>some%more%<{let fun fun ^ \"coucou\"}>%and%finally%"
      ;"<{let x = 5 in x}>"
      ;"<{let x = 5 in % x}>"
      ;"<{f\"coucou\"}>"
      ;"<{let x = 5, 2 in fst x}>"
      ;"<h1>Example</h1>%<{% let x = 1 in% if x + 1 = 2 then%<[% 2%]>% else %<[% DEADCODE%]>}>" (* TODO add this delimiter. Now prelexed alright, has to be parsed *)
      ;"<{2 + 1 = 3}>"
      ;"<{1-1}>"
      ;"<{let y = 3 in let x = 4 in (fun x -> y) 5}>"
      ;"<{let f = fun x -> fun y -> x in let x = 1 in let y = 2 in f x y}>"
      ;"<{if true then 1 else 2; 3}>" (* FIXME is parsed [if true then 1 else (2; 3)] and not as it should [(if true then 1 else 2); 3] *)
      ;"<{let f = fun x -> x in (f (1))}>" (* FIXME is parsed [if true then 1 else (2; 3)] and not as it should [(if true then 1 else 2); 3] *)
      ;"<{f\"cou#\"cou\"}>"
      ;"<{\"cou<cou\"}>"
      ;"<{\"co<i>uc</i>ou\"}>"
      (* ;"<{if true then () else (); 2}>" FIXME parse unit cf parser.ml *)
      (* ;"<{if true then 1; 2}>" FIXME add this syntax sugar *)
    ]
  in
  if Array.length Sys.argv <= 1 then
    Printf.fprintf stderr "Usage: ./test.x <test>\n<test>\n\ti: the i-th hard-coded test, if negative, run all the tests\n\ta string of a code that will be tested."
  else begin
    let tests = List.map (fun s -> String.map (fun c -> if c = '%' then '\n' else if c = '#' then '\\' else c) s) tests in
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

let () = Printf.printf "%s" Sys.argv.(0); if Sys.argv.(0) = "./test.x" then test_input ()