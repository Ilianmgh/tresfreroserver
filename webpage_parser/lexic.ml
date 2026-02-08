include Utils

(** Lexic definition *)

let open_ml_bracket = "<{"
let close_ml_bracket = "}>"

let open_html_bracket = "<["
let close_html_bracket = "]>"

let len_open_ml_bracket = String.length open_ml_bracket

let len_open_html_bracket = String.length open_html_bracket
let len_close_html_bracket = String.length close_html_bracket

type keyword =
  | TokLet | TokFun | TokArr | TokFix | TokIn (* declarations & functions *)
  | TokIf | TokThen | TokElse (* conditions *)
  | TokAnd | TokOr | TokNot (* boolean operators *)
  | TokGt | TokLt | TokGeq | TokLeq | TokNeq | TokEq (* comparison operators, if we allow user-defined infixed notation, should be defined just as Sqlite.opendb/... *)
  | TokPlus | TokMinus | TokTimes | TokDiv | TokExp (* arithmetic operators, if we allow user-defined infixed notation, should be defined just as Sqlite.opendb/... *)
  | TokStrConcat (* strings, if we allow user-defined infixed notation, should be defined just as Sqlite.opendb/... *)
  | TokSeq (* sequence *)
  | TokComma (* pairs & tuples *)
  | TokLpar | TokRpar (* parenthesis *)
  | TokOpenML | TokCloseML (* ML-opening/closing brackets *)
  | TokOpenHTML | TokCloseHTML (* HTML-opening/closing brackets *)
  | TokDot (* Dots (for namespacing) *)

type literal = TokTrue | TokFalse | TokInt of int | TokStr of string | TokFstr of string

(** tokens without line numbering *)
type raw_token = MId of string (* ids of modules (starting with a capital)*)| Id of string | Lit of literal | Keyword of keyword | TokHtml of string

(** tokens labelled with a line number *)
type token = int * raw_token

let symbols_tokens : (string * raw_token) list = [
  ("=", Keyword TokEq);
  ("->", Keyword TokArr);
  ("&&", Keyword TokAnd);
  ("||", Keyword TokOr);
  ("=", Keyword TokEq);
  (">", Keyword TokGt);
  ("<", Keyword TokLt);
  (">=", Keyword TokGeq);
  ("<=", Keyword TokLeq);
  ("<>", Keyword TokNeq);
  ("+", Keyword TokPlus);
  ("-", Keyword TokMinus);
  ("*", Keyword TokTimes);
  ("/", Keyword TokDiv);
  ("^", Keyword TokExp);
  (open_ml_bracket, Keyword TokOpenML);
  (close_ml_bracket, Keyword TokCloseML);
  (open_html_bracket, Keyword TokOpenHTML);
  (close_html_bracket, Keyword TokCloseHTML);
  ("(", Keyword TokLpar);
  (")", Keyword TokRpar);
  (";", Keyword TokSeq);
  (",", Keyword TokComma);
  (".", Keyword TokDot);
  ("++", Keyword TokStrConcat)]

let keywords_tokens : (string * raw_token) list = [
  ("let", Keyword TokLet);
  ("fun", Keyword TokFun);
  ("fixfun", Keyword TokFix);
  ("in", Keyword TokIn);
  ("if", Keyword TokIf);
  ("then", Keyword TokThen);
  ("else", Keyword TokElse);
  ("begin", Keyword TokLpar);
  ("end", Keyword TokRpar);
  ("not", Keyword TokNot);
  ("true", Lit TokTrue);
  ("false", Lit TokFalse)]

let symbols : string list = List.map fst symbols_tokens

let keywords_map : raw_token StringMap.t =
  let symbols_only_map = List.fold_left (fun acc_keywords_map (s, tok) -> StringMap.add s tok acc_keywords_map) StringMap.empty symbols_tokens in (* FIXME replace by of_list ? *)
  List.fold_left (fun acc_keywords_map (s, tok) -> StringMap.add s tok acc_keywords_map) symbols_only_map keywords_tokens


(** Pretty-printing *)

let string_of_keyword (k : keyword) : string = match k with
  | TokLet -> "let"
  | TokEq -> "=" 
  | TokArr -> "->" 
  | TokFun -> "fun"
  | TokFix -> "fixfun"
  | TokIn -> "in"
  | TokIf -> "if"
  | TokThen -> "then"
  | TokElse -> "else"
  | TokAnd -> "&&"
  | TokOr -> "||"
  | TokNot -> "not"
  | TokGt -> ">"
  | TokLt -> "<"
  | TokGeq -> ">="
  | TokLeq -> "<="
  | TokNeq -> "<>"
  | TokPlus -> "+"
  | TokMinus -> "-"
  | TokTimes -> "*"
  | TokDiv -> "/"
  | TokExp -> "^"
  | TokStrConcat -> "++"
  | TokSeq -> ";"
  | TokComma -> ","
  | TokDot -> "."
  | TokLpar -> "("
  | TokRpar -> ")"
  | TokOpenML -> "<{"
  | TokCloseML -> "}>"
  | TokOpenHTML -> "<["
  | TokCloseHTML -> "]>"

let string_of_raw_token (tok : raw_token) : string = match tok with
  | MId s -> Printf.sprintf "MId:%s" s
  | Id s -> Printf.sprintf "Id:%s" s
  | Keyword k -> Printf.sprintf "Kw:%s" (string_of_keyword k)
  | Lit TokFalse -> "Lit:false"
  | Lit TokTrue -> "Lit:true"
  | Lit (TokInt n) -> Printf.sprintf "Lit:%d" n
  | Lit (TokStr s) -> Printf.sprintf "Lit:%s" s
  | Lit (TokFstr s) -> Printf.sprintf "Lit(f):%s" s
  | TokHtml s -> Printf.sprintf "Html:%s" s

let string_of_token ((i, tok) : token) : string = Printf.sprintf "l. %d %s" i (string_of_raw_token tok)
