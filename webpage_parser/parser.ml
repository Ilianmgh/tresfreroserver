open Lexer
open Utils
open Syntax

let debug = false

(** Eating functions. When expecting a certain token, use it to get: the new line number, the important data of the token read, and the remainder of the token list.
  Must be provided with an error message to display in case the next token is not the token expected *)

let eat_variable (l : token list) (error_msg : string) : int * variable * token list = match l with
  | (i_line, Id name) :: l_rem -> (i_line, name, l_rem)
  | _ -> raise (ParsingError error_msg)

let eat_html (l : token list) (error_msg : string) : int * string * token list = match l with
  | (i_line, TokHtml code) :: l_rem -> (i_line, code, l_rem)
  | _ -> raise (ParsingError error_msg)

let eat_token (tok : raw_token) (l : token list) (error_msg : raw_token option -> string) : int * raw_token * token list = match l with
  | (i_line, tok') :: l_rem -> if tok = tok' then (i_line, tok, l_rem) else raise (ParsingError (error_msg (Some tok')))
  | _ -> raise (ParsingError (error_msg None))

(** Eats a token among [toks] if it is the next to be read; [None] otherwise *)
let eat_token_opt (toks : raw_token list) (l : token list) : (int * raw_token * token list) option = match l with
  | (i_line, tok') :: l_rem -> if List.mem tok' toks then Some (i_line, tok', l_rem) else None
  | _ -> None

(** LEFT ASSOCIATIVITY *)

let merge_additive (op : raw_token) (e1 : expr) (e2 : expr) = match op with
    | Keyword TokPlus -> Plus (e1, e2)
    | Keyword TokMinus -> Minus (e1, e2)
    | _ -> assert false

let merge_multiplicative (op : raw_token) (e1 : expr) (e2 : expr) = match op with
    | Keyword TokTimes -> Mult (e1, e2)
    | Keyword TokDiv -> Div (e1, e2)
    | _ -> assert false

(** [parse_left_assoc parse_operand sep merge_operands last_line acc l] parses from [l] where the last line number from which we read was [last_line] given an accumulator of expressions already parsed [acc].
  The goal is to parse a series of expressions parsed by [parse_operand] separated by separator in the list [sep].
  They are combined in a left-associative manner using the function [merge_operands].
  Example:
  [parse_left_assoc parse_factors [KeyWord TokPlus; KeyWord TokMinus] merge_additive 156 (Int 0) [Keyword TokPlus; Lit TokInt 3; Keyword TokTimes; Lit TokInt 2; Keyword TokMinus; Lit TokInt 4]] parses [((0 + (3 * 2)) - 4)].
*)
let rec parse_left_assoc (parse_operand : token list -> (int * expr * token list)) (sep : raw_token list) (merge_operands : raw_token -> expr -> expr -> expr) (last_line : int) (acc : expr) (l : token list) : int * expr * token list =
  match eat_token_opt sep l with
  | Some (i_line, operator, l_rem) -> let i_line', operand, l_rem' = parse_operand l_rem in
    parse_left_assoc parse_operand sep merge_operands i_line' (merge_operands operator acc operand) l_rem'
  | _ -> (last_line, acc, l)

let rec parse_application_accumulator (parse_argument : token list -> (int * expr * token list)) (last_line : int) (acc : expr) (l : token list) : int * expr * token list =
  try
    let i_line, arg, l_rem = parse_argument l in
    parse_application_accumulator parse_argument i_line (App (acc, arg)) l_rem (* can't raise a ParsingError *)
  with
    | ParsingError _ -> (last_line, acc, l)

(** Returns [(last_line, remaining, e)]: [last_line] the number of the last line from which we parsed something; [remaining] the list of tokens that remained to be parsed; [e] the parsed expression *)
let rec parse_exp (l : token list) : int * expr * token list = if debug then Printf.printf "PARSING: %s\n" (string_of_list string_of_token l); parse_sequence l
and parse_sequence (l : token list) : int * expr * token list =
  (if debug then Printf.fprintf stderr "PARSE SEQ\n%!");
  let i_lhs, lhs, l_rem = parse_tuple l in
  (if debug then Printf.fprintf stderr "PARSE SEQ OK\n%!");
  match eat_token_opt [(Keyword TokSeq)] l_rem with
    | Some (i_seq, seq, l_rem) ->
      let i_rhs, rhs, l_rem = parse_sequence l_rem in (i_rhs, Seq (lhs, rhs), l_rem)
    | None -> (i_lhs, lhs, l_rem)
and parse_tuple (l : token list) : int * expr * token list = (* for now, only couples are allowed *)
(if debug then Printf.fprintf stderr "PARSE TUPLE\n%!");
  let i_lhs, lhs, l_rem = parse_disjunction l in
  match eat_token_opt [(Keyword TokComma)] l_rem with
    | Some (i_comma, op_comma, l_rem) ->
      let i_rhs, rhs, l_rem = parse_tuple l_rem in (i_rhs, Couple (lhs, rhs), l_rem)
    | None -> (i_lhs, lhs, l_rem)
and parse_disjunction (l : token list) : int * expr * token list =
  (if debug then Printf.fprintf stderr "PARSE DISJUNCTION\n%!");
  let i_lhs, lhs, l_rem = parse_conjunction l in
  match eat_token_opt [(Keyword TokOr)] l_rem with
    | Some (i_or, op_or, l_rem) ->
      let i_rhs, rhs, l_rem = parse_disjunction l_rem in (i_rhs, Or (lhs, rhs), l_rem)
    | None -> (i_lhs, lhs, l_rem)
and parse_conjunction (l : token list) : int * expr * token list =
  (if debug then Printf.fprintf stderr "PARSE CONJUNCTION\n%!");
  let i_lhs, lhs, l_rem = parse_comparison l in
  match eat_token_opt [(Keyword TokAnd)] l_rem with
    | Some (i_and, op_and, l_rem) ->
      let i_rhs, rhs, l_rem = parse_conjunction l_rem in (i_rhs, And (lhs, rhs), l_rem)
    | None -> (i_lhs, lhs, l_rem)
and parse_comparison (l : token list) : int * expr * token list =
  (if debug then Printf.fprintf stderr "PARSE COMPARISON\n%!");
  let i_lhs, lhs, l_rem = parse_concatenation l in
  match eat_token_opt [Keyword TokEq; Keyword TokNeq; Keyword TokGt; Keyword TokLt; Keyword TokGeq; Keyword TokLeq] l_rem with
    | Some (i_op, Keyword TokEq, l_rem) ->
      let i_rhs, rhs, l_rem = parse_comparison l_rem in (i_rhs, Eq (lhs, rhs), l_rem)
    | Some (i_op, Keyword TokNeq, l_rem) ->
      let i_rhs, rhs, l_rem = parse_comparison l_rem in (i_rhs, Neq (lhs, rhs), l_rem)
    | Some (i_op, Keyword TokGt, l_rem) ->
      let i_rhs, rhs, l_rem = parse_comparison l_rem in (i_rhs, Gt (lhs, rhs), l_rem)
    | Some (i_op, Keyword TokLt, l_rem) ->
      let i_rhs, rhs, l_rem = parse_comparison l_rem in (i_rhs, Lt (lhs, rhs), l_rem)
    | Some (i_op, Keyword TokGeq, l_rem) ->
      let i_rhs, rhs, l_rem = parse_comparison l_rem in (i_rhs, Geq (lhs, rhs), l_rem)
    | Some (i_op, Keyword TokLeq, l_rem) ->
      let i_rhs, rhs, l_rem = parse_comparison l_rem in (i_rhs, Leq (lhs, rhs), l_rem)
    | None -> (i_lhs, lhs, l_rem)
    | _ -> assert false
and parse_concatenation (l : token list) : int * expr * token list =
  (if debug then Printf.fprintf stderr "PARSE CONCATENATION\n%!");
  let i_lhs, lhs, l_rem = parse_sum l in
  match eat_token_opt [(Keyword TokStrConcat)] l_rem with
    | Some (i_concat, concat, l_rem) ->
      let i_rhs, rhs, l_rem = parse_concatenation l_rem in (i_rhs, Concat (lhs, rhs), l_rem)
    | None -> (i_lhs, lhs, l_rem)
and parse_sum (l : token list) : int * expr * token list =
  let i, first_operand, l_rem = parse_multiplication l in
  parse_left_assoc parse_multiplication [Keyword TokPlus; Keyword TokMinus] merge_additive i first_operand l_rem
and parse_multiplication (l : token list) : int * expr * token list =
  let i, first_operand, l_rem = parse_power l in
  parse_left_assoc parse_power [Keyword TokTimes; Keyword TokDiv] merge_multiplicative i first_operand l_rem
and parse_power (l : token list) : int * expr * token list =
  (if debug then Printf.fprintf stderr "PARSE POWER\n%!");
  let i_number, number, l_rem = parse_negation l in
  match eat_token_opt [(Keyword TokExp)] l_rem with
    | Some (i_pow_op, pow_op, l_rem) ->
      let i_power, power, l_rem = parse_power l_rem in (i_power, Pow (number, power), l_rem)
    | None -> (i_number, number, l_rem)
and parse_negation (l : token list) : int * expr * token list =
  (if debug then Printf.fprintf stderr "PARSE NEGATION\n%!");
match l with
  | (i, Keyword TokMinus) :: l_rem ->
    let i_number, number, l_rem = parse_application l_rem in
    (i_number, Neg number, l_rem)
  | _ -> parse_application l 
and parse_application (l : token list) : int * expr * token list =
  (if debug then Printf.fprintf stderr "PARSE APPLICATION\n%!");
  let i_func, func, l_rem = parse_atom l in
  parse_application_accumulator parse_atom i_func func l_rem
and parse_atom (l : token list) : int * expr * token list =
  (if debug then Printf.fprintf stderr "PARSE ATOM\n%!");
  match l with
  | [] -> raise (ParsingError "Unexpected end of document")
  | (i, tok) :: l_rem -> begin match tok with
    (* Constructors for which we know how to parse by reading the first token *)
    | Keyword TokLet -> begin
      let i_var, var, l_rem = eat_variable l_rem (Printf.sprintf "line %d: let-expression: variable expected after 'let'." i) in
      let i_eq, eq, l_rem = eat_token (Keyword TokEq) l_rem (fun _ -> Printf.sprintf "line %d: let-expression: '=' expected after 'let'." i_var) in
      let i_x_expr, x_expr, l_rem = parse_exp l_rem in
      let i_in, in_, l_rem = eat_token (Keyword TokIn) l_rem (fun _ -> Printf.sprintf "line %d: let-expression: 'in' expected after 'let'." i_x_expr) in
      let i, body_expr, l_rem = parse_exp l_rem in
      (i, Let (var, x_expr, body_expr), l_rem)
    end
    | Keyword TokFun -> begin
      let i_var, var, l_rem = eat_variable l_rem (Printf.sprintf "line %d: fun-expression: variable expected after 'fun'." i) in
      let i_arrow, arrow, l_rem = eat_token (Keyword TokArr) l_rem (fun _ -> Printf.sprintf "line %d: fun-expression: '->' expected after 'fun <var>'." i_var) in
      let i_body, body, l_rem = parse_exp l_rem in
      (i_body, Fun (var, body), l_rem)
    end
    | Keyword TokFix -> begin
      let i_f_var, f_var, l_rem = eat_variable l_rem (Printf.sprintf "line %d: fixfun-expression: function variable expected after 'fixfun'." i) in
      let i_x_var, x_var, l_rem = eat_variable l_rem (Printf.sprintf "line %d: fixfun-expression: variable expected after 'fixfun'." i) in
      let i_arrow, arrow, l_rem = eat_token (Keyword TokArr) l_rem (fun _ -> Printf.sprintf "line %d: fun-expression: '->' expected after 'fun <var>'." i_x_var) in
      let i_body, body, l_rem = parse_exp l_rem in
      (i_body, Fix (f_var, x_var, body), l_rem)
    end
    | Keyword TokIf -> begin
      let i_condition, condition, l_rem = parse_exp l_rem in
      let i_then, tok_then, l_rem = eat_token (Keyword TokThen) l_rem (fun _ -> Printf.sprintf "line %d: if-expression: `then` expected after 'if <condition>'." i_condition) in
      let i_then_body, then_body, l_rem = parse_exp l_rem in
      let i_else, tok_else, l_rem = eat_token (Keyword TokElse) l_rem (fun _ -> Printf.sprintf "line %d: if-expression: `else` expected after 'if <condition> then <body>'." i_then_body) in
      let i_else_body, else_body, l_rem = parse_exp l_rem in
      (i_else_body, If (condition, then_body, else_body), l_rem)
    end
    | Keyword TokCloseML -> raise (ParsingError (Printf.sprintf "line %d: Unexpected end of ml code." i))
    | Lit TokTrue -> (i, Bool true, l_rem)
    | Lit TokFalse -> (i, Bool false, l_rem)
    | Lit (TokInt n) -> (i, Int n, l_rem)
    | Lit (TokStr s) -> (i, String s, l_rem)
    | Lit (TokFstr s) -> (i, Fstring s, l_rem)
    | Keyword TokFst -> (i, Fst, l_rem)
    | Keyword TokSnd -> (i, Snd, l_rem)
    | Id s -> (i, Var s, l_rem)
    | Keyword TokLpar ->
      let i_expr, expr, l_rem = parse_exp l_rem in
      let i_rpar, rpar, l_rem = eat_token (Keyword TokRpar) l_rem (fun x -> Printf.sprintf "line %d: %s closing parenthesis expected." i_expr (match x with | Some tok -> string_of_raw_token tok | None -> "")) in
      (i_rpar, expr, l_rem)
    | Keyword TokOpenHTML ->
      let i_html, html, l_rem = eat_html l_rem (Printf.sprintf "line %d: No HTML code between HTML brackets??" i) in
      let i_close_html, close_html, l_rem = eat_token (Keyword TokCloseHTML) l_rem (fun _ -> Printf.sprintf "line %d: HTML-closing bracket expected." i_html) in
      (i_close_html, Html html, l_rem)
    | TokHtml s -> raise (ParsingError (Printf.sprintf "line %d: Unexpected html code within ML delimiters." i)) (* assert false ? *)
    | _ -> raise (ParsingError (Printf.sprintf "line %d: Malformed expression." i))
  end

(** Parsing globals *)

type parsed_let_expression =
  | LetToBe of int * global_declaration * token list
  | LetInToBe of int * (variable * expr) * token list
  | Nothing

(** [parse_global l] tries to parse a global from the token list [l], otherwise, raises [NotAGlobal].
  If the beginning of [l] contains a let-in expression, the exception contains the variable name and the expression.
*)
let parse_global (l : token list) : parsed_let_expression =
  match eat_token_opt [(Keyword TokLet)] l with
    | Some (i_let, let_, l_rem) -> begin
      let i_var, var, l_rem = eat_variable l_rem (Printf.sprintf "line %d: let-expression: variable expected after 'let'." i_let) in
      let i_eq, eq, l_rem = eat_token (Keyword TokEq) l_rem (fun _ -> Printf.sprintf "line %d: let-expression: '=' expected after 'let'." i_var) in
      let i_x_expr, x_expr, l_rem = parse_exp l_rem in
      begin match eat_token_opt [(Keyword TokIn)] l_rem with
        | Some (_, tok_in, l_rem') -> LetInToBe (i_x_expr, (var, x_expr), l_rem')
        | None -> LetToBe (i_x_expr, ExprDecl (var, x_expr), l_rem)
      end 
    end
    | None -> Nothing

type ml_code = Global of global_declaration list | Expr of expr

let rec parse_globals (l : token list) (acc : global_declaration list) : int * global_declaration list * token list = match l with
  | [] -> raise (ParsingError "Unexpected end of document")
  | (i, Keyword TokCloseML) :: l_rem -> (i, acc, (i, Keyword TokCloseML) :: l_rem) (* TODO not exactly (for the line number) should be the line number of previous token *)
  | (i, h_tok) :: l_rem -> begin match parse_global ((i, h_tok) :: l_rem) with
    | LetToBe (i, g, l_rem) -> parse_globals l_rem (g :: acc)
    | _ -> raise (ParsingError (Printf.sprintf "line %d: Unexpected expression after global declaration" i))
  end

let parse_ml_code (l : token list) : int * ml_code * token list =
  match parse_global l with
  | LetToBe (i, g, l_rem) -> let i, parsed_globals, l_rem = parse_globals l_rem [g] in (i, Global (List.rev parsed_globals), l_rem)
  | Nothing -> let i, e, l_rem' = parse_exp l in (i, Expr e, l_rem')
  | LetInToBe (i_line, (x, e), l_rem) -> let i, body, l_rem = parse_exp l in (i, Expr (Let (x, e, body)), l_rem)

(** Parsing ml *)
let rec parser (lexed : token list) : dynml_webpage  = match lexed with
  | [] -> []
  | [(i, TokHtml s)] -> [Pure s]
  | (i, TokHtml s) :: (j, Keyword TokOpenML) :: lexed' -> begin match parse_ml_code lexed' with
    | i_line, Expr parsed, (_, Keyword TokCloseML) :: l_rem' -> (Pure s) :: (Script parsed) :: (parser l_rem')
    | i_line, Global globals, (_, Keyword TokCloseML) :: l_rem' -> (Pure s) :: (List.map (fun x -> Decl x) globals) @ (parser l_rem')
    | i_line, _, tok :: _ -> raise (ParsingError (Printf.sprintf "line %d: %s, HTML-closing bracket %s expected" i_line (string_of_token tok) (string_of_raw_token (Keyword TokCloseML))))
    | i_line, _, [] -> raise (ParsingError (Printf.sprintf "line %d: ML-closing bracket %s expected" i_line (string_of_raw_token (Keyword TokCloseML))))
  end
  | _ -> Printf.fprintf stderr "%s\n" (string_of_list string_of_token lexed); raise (ParsingError "I don't know what happened")

(*
  TODO:
  [] check if precedence of if is not under-evaluated : test if true then 1; 2 === (if true then 1); 2 or if true then (1; 2)
  [] put everything together in [parser]
  [] manage left-associativity
  [] parse () as a unit
*)

(**

  let x = 5 in x
  LEXER 
  [let,x,=,5,in,x]
  Let (x, 5, x)
*)