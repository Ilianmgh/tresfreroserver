exception ParsingError of string

type html_code = string

type variable = string

type expr =
    Empty (* translation of <{}> *)
  | Let of variable * expr * expr
  | Fun of variable * expr
  | Fix of variable * variable * expr
  | App of expr * expr
  | If of expr * expr * expr
  | Seq of expr * expr
  | Html of html_code
  | Var of variable
  (* tuples *)
  | Couple of expr * expr
  (* arithmetic connectors *)
  | Plus of expr * expr
  | Minus of expr * expr
  | Neg of expr
  | Mult of expr * expr
  | Div of expr * expr
  | Pow of expr * expr
  | Int of int
  (* boolean connectors *)
  | Gt of expr * expr
  | Lt of expr * expr
  | Geq of expr * expr
  | Leq of expr * expr
  | Eq of expr * expr
  | Neq of expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Bool of bool
  (* strings connectors *)
  | Concat of expr * expr
  | String of string
  | Fstring of string

(** Pretty-printing *)

let rec string_of_expr : expr -> string = function
  | Empty -> "<{}>"
  | Let (x, e, e') -> Printf.sprintf "let %s = %s in %s" x (string_of_expr e) (string_of_expr e')
  | Fun (x, e) -> Printf.sprintf "fun %s -> %s" x (string_of_expr e)
  | Fix (f, x, e) -> Printf.sprintf "fixfun %s %s -> %s" f x (string_of_expr e)
  | App (e, e') -> Printf.sprintf "(%s) %s" (string_of_expr e) (string_of_expr e)
  | If (c, t, e) -> Printf.sprintf "if %s then %s else %s" (string_of_expr c) (string_of_expr t) (string_of_expr e)
  | Seq (e, e') -> Printf.sprintf "%s;%s" (string_of_expr e) (string_of_expr e')
  | Html h -> Printf.sprintf "Html(%s)" h
  | Var x -> x
  | Couple (e, e') -> Printf.sprintf "(%s, %s)" (string_of_expr e) (string_of_expr e')
  | Plus (e, e') -> Printf.sprintf "%s + %s" (string_of_expr e) (string_of_expr e')
  | Minus (e, e') -> Printf.sprintf "%s - %s" (string_of_expr e) (string_of_expr e')
  | Neg e -> Printf.sprintf "-%s" (string_of_expr e)
  | Mult (e, e') -> Printf.sprintf "%s * %s" (string_of_expr e) (string_of_expr e')
  | Div (e, e') -> Printf.sprintf "%s / %s" (string_of_expr e) (string_of_expr e')
  | Pow (e, e') -> Printf.sprintf "%s ^ %s" (string_of_expr e) (string_of_expr e')
  | Int n -> Printf.sprintf "%d" n
  | Gt (e, e') -> Printf.sprintf "%s > %s" (string_of_expr e) (string_of_expr e')
  | Lt (e, e') -> Printf.sprintf "%s < %s" (string_of_expr e) (string_of_expr e')
  | Geq (e, e') -> Printf.sprintf "%s >= %s" (string_of_expr e) (string_of_expr e')
  | Leq (e, e') -> Printf.sprintf "%s <= %s" (string_of_expr e) (string_of_expr e')
  | Eq (e, e') -> Printf.sprintf "%s = %s" (string_of_expr e) (string_of_expr e')
  | Neq (e, e') -> Printf.sprintf "%s <> %s" (string_of_expr e) (string_of_expr e')
  | And (e, e') -> Printf.sprintf "%s && %s" (string_of_expr e) (string_of_expr e')
  | Or (e, e') -> Printf.sprintf "%s || %s" (string_of_expr e) (string_of_expr e')
  | Not e -> Printf.sprintf "not %s" (string_of_expr e)
  | Bool b -> if b then "true" else "false"
  | Concat (e, e') -> Printf.sprintf "%s ++ %s" (string_of_expr e) (string_of_expr e')
  | String s -> s
  | Fstring s -> s