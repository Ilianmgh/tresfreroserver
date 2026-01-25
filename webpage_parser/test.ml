open Utils
open Lexic
open Lexer
open Syntax
open Parser
open TypeSyntax
open Typechecker
open Interpreter

let () =
  let tests = [
      "<{}>"
      ;"something%else<{begin fun x -> y end}>some%more%<{let fun fun ^ \"coucou\"}>%and%finally%"
      ;"<{let x = 5 in x}>"
      ;"<{let x = 5 in % x}>"
      ;"<{f\"coucou\"}>"
      ;"<{let x = 5, 2 in fst x}>"
      ;"<h1>Exampel</h1>%<{% let x = 1 in% if x = 2 then%}>% 2%<{% else %}>% what?%<{}>"
      ;"<{1-1}>" (* FIXME is parsed as the function 1 applied to (-1) *)
      ;"<{let y = 3 in let x = 4 in (fun x -> y) 5}>"
    ]
  in
  let tests = List.map (fun s -> String.map (fun c -> if c = '%' then '\n' else c) s) tests in
  let s = if Array.length Sys.argv > 1 then Sys.argv.(1)
    else List.nth tests 8
  in
  if debug then Printf.printf "raw: %s\n%!" s;
  let lexed : token list = lexer s in
  if debug then Printf.printf "lexed: %s\n%!" (string_of_list string_of_token lexed);
  let _, parsed, _ = parser lexed in
  if debug then Printf.printf "parsed: %s\n%!" (string_of_expr parsed);
  let gamma, tau = type_inferer StringMap.empty parsed in
  if debug then Printf.printf "typed: %s\nIn env: %s\n%!" (string_of_ml_type tau) (string_of_typing_env gamma);
  let v = eval StringMap.empty parsed in
  if debug then Printf.printf "eval'd: %s\n%!" (string_of_value v);