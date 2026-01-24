open Lexer
open Utils
open Syntax

(** Eating functions. When expecting a certain token, use it to get: the new line number, the important data of the token read, and the remainder of the token list.
  Must be provided with an error message to display in case the next token is not the token expected *)

let eat_variable (l : token list) (error_msg : string) : int * variable * token list = match l with
  | (i_line, Id name) :: l_rem -> (i_line, name, l_rem)
  | _ -> raise (ParsingError error_msg)

let eat_html (l : token list) (error_msg : string) : int * string * token list = match l with
  | (i_line, TokHtml code) :: l_rem -> (i_line, code, l_rem)
  | _ -> raise (ParsingError error_msg)

let eat_token (tok : raw_token) (l : token list) (error_msg : string) : int * raw_token * token list = match l with
  | (i_line, tok') :: l_rem -> if tok = tok' then (i_line, tok, l_rem) else raise (ParsingError error_msg)
  | _ -> raise (ParsingError error_msg)

(** Eats a token among [toks] if it is the next to be read; [None] otherwise *)
let eat_token_opt (toks : raw_token list) (l : token list) : (int * raw_token * token list) option = match l with
  | (i_line, tok') :: l_rem -> if List.mem tok' toks then Some (i_line, tok', l_rem) else None
  | _ -> None

(** Returns [(last_line, remaining, e)]: [last_line] the number of the last line from which we parsed something; [remaining] the list of tokens that remained to be parsed; [e] the parsed expression *)
let rec parse_exp (l : token list) : int * expr * token list = match l with
  | [] -> raise (ParsingError "Unexpected end of document")
  | (i, tok) :: l_rem -> begin match tok with
    (* Constructors for which we know how to parse by reading the first token *)
    | Keyword TokLet -> begin
      let i_var, var, l_rem = eat_variable l_rem (Printf.sprintf "line %d: let-expression: variable expected after 'let'." i) in
      let i_eq, eq, l_rem = eat_token (Keyword TokEq) l_rem (Printf.sprintf "line %d: let-expression: '=' expected after 'let'." i_var) in
      let i_x_expr, x_expr, l_rem = parse_exp l_rem in
      let i_in, in_, l_rem = eat_token (Keyword TokIn) l_rem (Printf.sprintf "line %d: let-expression: 'in' expected after 'let'." i_x_expr) in
      let i, body_expr, l_rem = parse_exp l_rem in
      (i, Let (var, x_expr, body_expr), l_rem)
    end
    | Keyword TokFun -> begin
      let i_var, var, l_rem = eat_variable l_rem (Printf.sprintf "line %d: fun-expression: variable expected after 'fun'." i) in
      let i_arrow, arrow, l_rem = eat_token (Keyword TokArr) l_rem (Printf.sprintf "line %d: fun-expression: '->' expected after 'fun <var>'." i_var) in
      let i_body, body, l_rem = parse_exp l_rem in
      (i_body, Fun (var, body), l_rem)
    end
    | Keyword TokFix -> begin
      let i_f_var, f_var, l_rem = eat_variable l_rem (Printf.sprintf "line %d: fixfun-expression: function variable expected after 'fixfun'." i) in
      let i_x_var, x_var, l_rem = eat_variable l_rem (Printf.sprintf "line %d: fixfun-expression: variable expected after 'fixfun'." i) in
      let i_arrow, arrow, l_rem = eat_token (Keyword TokArr) l_rem (Printf.sprintf "line %d: fun-expression: '->' expected after 'fun <var>'." i_x_var) in
      let i_body, body, l_rem = parse_exp l_rem in
      (i_body, Fix (f_var, x_var, body), l_rem)
    end
    | Keyword TokIf -> begin
      let i_condition, condition, l_rem = parse_exp l_rem in
      let i_then, tok_then, l_rem = eat_token (Keyword TokThen) l_rem (Printf.sprintf "line %d: if-expression: `then` expected after 'if <condition>'." i_condition) in
      let i_then_body, then_body, l_rem = parse_exp l_rem in
      let i_else, tok_else, l_rem = eat_token (Keyword TokElse) l_rem (Printf.sprintf "line %d: if-expression: `else` expected after 'if <condition> then <body>'." i_then_body) in
      let i_else_body, else_body, l_rem = parse_exp l_rem in
      (i_else_body, If (condition, then_body, else_body), l_rem)
    end
    | Keyword TokCloseML -> begin (* TODO not sure it has the expected behaviour *)
      let i_html, html, l_rem = eat_html l_rem (Printf.sprintf "line %d: HTML code expected after ML closing bracket." i) in
      try (* TODO replace by eat_token *)
        let i_open_ml, open_ml, l_rem' = eat_token (Keyword TokOpenML) l_rem "" in (* if there is ml code again afterwards *)
        (i_open_ml, Html html, l_rem')
      with
        ParsingError _ -> (i_html, Html html, l_rem)
    end
    | Lit TokTrue -> (i, Bool true, l_rem)
    | Lit TokFalse -> (i, Bool false, l_rem)
    | Lit (TokInt n) -> (i, Int n, l_rem)
    | Lit (TokStr s) -> (i, String s, l_rem)
    | Lit (TokFstr s) -> (i, Fstring s, l_rem)
    | Id s -> (i, Var s, l_rem)
    | TokHtml s -> raise (ParsingError (Printf.sprintf "line %d: Unexpected html code withing ML delimiters." i))
    | _ -> parse_application l
  end
and parse_application (l : token list) : int * expr * token list =
  (* TODO should be left associative *)
  let i_func, func, l_rem = parse_negation l in
  try (* TODO see if it does the trick *)
    let i_arg, arg, l_rem = parse_application l_rem in
    (i_arg, App (func, arg), l_rem)
  with
    ParsingError _ -> (i_func, func, l_rem)
and parse_negation (l : token list) : int * expr * token list = match l with
  | (i, Keyword TokMinus) :: l_rem ->
    let i_number, number, l_rem = parse_power l_rem in
    (i_number, Neg number, l_rem)
  | _ -> parse_power l 
and parse_power (l : token list) : int * expr * token list =
  let i_number, number, l_rem = parse_multiplication l in
  match eat_token_opt [(Keyword TokExp)] l_rem with
    | Some (i_pow_op, pow_op, l_rem) ->
      let i_power, power, l_rem = parse_power l_rem in (i_power, Pow (number, power), l_rem)
    | None -> (i_number, number, l_rem)
and parse_multiplication (l : token list) : int * expr * token list =
  let i_lhs, lhs, l_rem = parse_sum l in
  match eat_token_opt [(Keyword TokTimes)] l_rem with
    | Some (i_times, times, l_rem) ->
      let i_rhs, rhs, l_rem = parse_multiplication l_rem in (i_rhs, Mult (lhs, rhs), l_rem)
    | None -> (i_lhs, lhs, l_rem)
and parse_sum (l : token list) : int * expr * token list =
  let i_lhs, lhs, l_rem = parse_concatenation l in
  match eat_token_opt [(Keyword TokPlus)] l_rem with
    | Some (i_plus, plus, l_rem) ->
      let i_rhs, rhs, l_rem = parse_sum l_rem in (i_rhs, Plus (lhs, rhs), l_rem)
    | None -> (i_lhs, lhs, l_rem)
and parse_concatenation (l : token list) : int * expr * token list =
  let i_lhs, lhs, l_rem = parse_comparison l in
  match eat_token_opt [(Keyword TokStrConcat)] l_rem with
    | Some (i_concat, concat, l_rem) ->
      let i_rhs, rhs, l_rem = parse_concatenation l_rem in (i_rhs, Concat (lhs, rhs), l_rem)
    | None -> (i_lhs, lhs, l_rem)
and parse_comparison (l : token list) : int * expr * token list =
  (* TODO should be left associative *)
  let i_lhs, lhs, l_rem = parse_conjunction l in
  match eat_token_opt [Keyword TokEq; Keyword TokNeq; Keyword TokGt; Keyword TokLt; Keyword TokGeq; Keyword TokLeq] l_rem with
    | Some (i_op, Keyword TokEq, l_rem) ->
      let i_rhs, rhs, l_rem = parse_comparison l_rem in (i_rhs, Eq (lhs, rhs), l_rem)
    | Some (i_op, Keyword TokNeq, l_rem) ->
      let i_rhs, rhs, l_rem = parse_comparison l_rem in (i_rhs, Neq (lhs, rhs), l_rem)
    | Some (i_op, Keyword TokGt, l_rem) ->
      let i_rhs, rhs, l_rem = parse_comparison l_rem in (i_rhs, Lt (lhs, rhs), l_rem)
    | Some (i_op, Keyword TokLt, l_rem) ->
      let i_rhs, rhs, l_rem = parse_comparison l_rem in (i_rhs, Gt (lhs, rhs), l_rem)
    | Some (i_op, Keyword TokGeq, l_rem) ->
      let i_rhs, rhs, l_rem = parse_comparison l_rem in (i_rhs, Leq (lhs, rhs), l_rem)
    | Some (i_op, Keyword TokLeq, l_rem) ->
      let i_rhs, rhs, l_rem = parse_comparison l_rem in (i_rhs, Geq (lhs, rhs), l_rem)
    | None -> (i_lhs, lhs, l_rem)
    | _ -> assert false
and parse_conjunction (l : token list) : int * expr * token list =
  let i_lhs, lhs, l_rem = parse_disjunction l in
  match eat_token_opt [(Keyword TokAnd)] l_rem with
    | Some (i_and, op_and, l_rem) ->
      let i_rhs, rhs, l_rem = parse_conjunction l_rem in (i_rhs, And (lhs, rhs), l_rem)
    | None -> (i_lhs, lhs, l_rem)
and parse_disjunction (l : token list) : int * expr * token list =
  let i_lhs, lhs, l_rem = parse_tuple l in
  match eat_token_opt [(Keyword TokOr)] l_rem with
    | Some (i_or, op_or, l_rem) ->
      let i_rhs, rhs, l_rem = parse_disjunction l_rem in (i_rhs, Or (lhs, rhs), l_rem)
    | None -> (i_lhs, lhs, l_rem)
and parse_tuple (l : token list) : int * expr * token list = (* for now, only couples are allowed *)
  let i_lhs, lhs, l_rem = parse_sequence l in
  match eat_token_opt [(Keyword TokComma)] l_rem with
    | Some (i_comma, op_comma, l_rem) ->
      let i_rhs, rhs, l_rem = parse_tuple l_rem in (i_rhs, Couple (lhs, rhs), l_rem)
    | None -> (i_lhs, lhs, l_rem)
and parse_sequence (l : token list) : int * expr * token list =
  let i_lhs, lhs, l_rem = parse_atom l in
  match eat_token_opt [(Keyword TokSeq)] l_rem with
    | Some (i_seq, seq, l_rem) ->
      let i_rhs, rhs, l_rem = parse_sequence l_rem in (i_rhs, Seq (lhs, rhs), l_rem)
    | None -> (i_lhs, lhs, l_rem)
and parse_atom (l : token list) : int * expr * token list = failwith "TODO" (*TODO bring back to here literals, functions and stuff*)

(** Parsing ml, considering `}> html code <{` as a literal of type content *)
let parser (lexed : token list) = match lexed with
  | [] -> raise (ParsingError "Unexpected end of document")
  | (i, TokHtml s) :: (j, Keyword TokOpenML) :: lexed' -> parse_exp lexed'
  | _ -> assert false

let () =
  let tests = [
      "<{}>"
      ;"something%else<{begin fun x -> y end}>some%more%<{let fun fun ^ \"coucou\"}>%and%finally%"
      ;"<{let x = 5 in x}>"
      ;"<{let x = 5 in % x}>"
      ;"<{f\"coucou\"}>"
    ]
  in
  let tests = List.map (fun s -> String.map (fun c -> if c = '%' then '\n' else c) s) tests in
  let s = if Array.length Sys.argv > 1 then Sys.argv.(1)
    else List.nth tests 3
  in
  Printf.printf "raw: %s\n" s;
  let lexed : token list = lexer s in
  Printf.printf "lexed: %s\n" (string_of_list string_of_token lexed);
  let _, parsed, _ = parser lexed in
  Printf.printf "parsed: %s\n" (string_of_expr parsed);