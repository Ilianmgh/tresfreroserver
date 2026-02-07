open Utils
open Lexic
open Prelexer
open Lexer
open Syntax
open Parser
open TypeSyntax
open Typechecker
open Interpreter

let displayed = ["raw"; "lexed"; "parsed"; "typed"; "eval'd"]

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
    let types_infered = type_inferer Environment.empty parsed in
    if List.mem "typed" displayed then List.iter (fun (gamma, tau) -> Printf.printf "typed: %s\nIn env: %s\n%!" (string_of_ml_type tau) (string_of_modular_typing_environment gamma)) types_infered;
    let _, values_evald = eval Environment.empty parsed in
    if List.mem "eval'd" displayed then List.iter (fun v -> Printf.printf "eval'd: %s\n%!" (string_of_value v)) values_evald
  with
    | PrelexingError s -> Printf.fprintf stderr "PrelexingError: %s\n%!" s
    | LexingError s -> Printf.fprintf stderr "LexingError: %s\n%!" s
    | ParsingError s -> Printf.fprintf stderr "ParsingError: %s\n%!" s
    | TypingError s -> Printf.fprintf stderr "TypingError: %s\n%!" s
    | UnificationError (alpha, beta, Recursive) -> Printf.fprintf stderr "UnificationError: %s and %s recursive.\n%!" (string_of_ml_type alpha) (string_of_ml_type beta)
    | UnificationError (alpha, beta, Incompatible) -> Printf.fprintf stderr "UnificationError: %s and %s incompatible.\n%!" (string_of_ml_type alpha) (string_of_ml_type beta)
    | InterpreterError s -> Printf.fprintf stderr "InterpreterError: %s\n%!" s
    | UnsupportedError s -> Printf.fprintf stderr "UnsupportedError: %s\n%!" s

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
      "<{}>" (* parsing error *)
      ;"something%else<{begin fun x -> y end}>some%more%<{let fun fun ^ \"coucou\"}>%and%finally%" (* lex but parsing error *)
      ;"<{let x = 5 in x}>" (* ok *)
      ;"<{let x = 5 in % x}>" (* ok *)
      ;"<{f\"coucou\"}>" (* FIXME lexing error + not implemented *)
      ;"<{let x = 5, 2 in fst x}>" (* ok *)
      ;"<h1>Example</h1>%<{% let x = 1 in% if x + 1 = 2 then%<[% 2%]>% else %<[% DEADCODE%]>}>" (* ok *)
      ;"<{2 + 1 = 3}>" (* ok *)
      ;"<{1-1}>" (* ok *)
      ;"<{let y = 3 in let x = 4 in (fun x -> y) 5}>" (* ok *)
      ;"<{ let f = fun a -> fun b -> if (a > b) then 5 else 3 in f 12 }>" (* ok *)
      ;"<{let f = fun x -> fun y -> x in let x = 1 in let y = 2 in f x y}>" (* ok *)
      ;"<{if true then 1 else 2; 3}>" (* FIXME is parsed [if true then 1 else (2; 3)] and not as it should [(if true then 1 else 2); 3] *)
      ;"<{let f = fun x -> x in (f (1))}>" (* ok *)
      ;"<{f\"cou#\"cou\"}>" (* FIXME lexing error *)
      ;"<{\"cou<cou\"}>" (* ok *)
      ;"<{\"co<i>\\\"uc</i>ou\"}>" (* ok *)
      ;
"<{let post_language = \"rust\"
let post_version = \"1\" }>
<{ if post_language = \"rust\" then
  <[
    <{ post_language }>
    <{ post_version }>
    est super!
  ]>
  else
  <[
    couocu
  ]>
}>" (* ok *)
      ; "<{ (fun x -> x) = (fun x -> 1) }>" (* interpreter error *)
      ;
"<{
let db = Sqlite.opendb \"test.db\"
}>
<{
Sqlite.exec db
(fun acc -> fun new_line -> <[ <{acc}> <{new_line}> ]>) (fun acc -> fun hd -> fun content -> <[ <{ acc }> <{hd}>: <{content}> <br/> ]>)
\"SELECT * FROM Test\"
}>
<{
Sqlite.closedb db
}>" (* ko, Sqlite module is not included on its own, only in produce_page, to test use server. *)
      ; "<{let f = fun x -> fun y -> x}> <{f (fun x -> x) 2}>" (* ok *)
      ; "<{ Test.x }>" (* should lex and parse *)
      ;
"
<{
  if Get.langage = \"zig\" then
    Get.langage ++ \" même en version \" ++ Get.version ++ \" n'a toujours pas de borrow-checker\"
  else
    Get.langage ++ \" même en version \" ++ Get.version ++ \" n'a toujours pas d'argument defer\"
}>
<{
  let n = 20
}>" (* should lex and parse *)
(* <{
  (fixfun f n -> if n = 0 then
    \"\"
  else begin
    Get.langage ++ (f (n-1))
  end) n
}>
" *)
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