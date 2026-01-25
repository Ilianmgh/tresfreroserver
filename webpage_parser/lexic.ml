(** Lexic definition *)

let open_ml_bracket = "<{"

let close_ml_bracket = "}>"

let len_open_ml_bracket = String.length open_ml_bracket

type keyword =
  | TokLet | TokFun | TokArr | TokFix | TokIn
  | TokIf | TokThen | TokElse
  | TokAnd | TokOr | TokNot
  | TokGt | TokLt | TokGeq | TokLeq | TokNeq | TokEq
  | TokPlus | TokMinus | TokTimes | TokDiv | TokExp
  | TokStrConcat
  | TokSeq
  | TokComma | TokFst | TokSnd
  | TokLpar | TokRpar
  | TokOpenML | TokCloseML

type literal = TokTrue | TokFalse | TokInt of int | TokStr of string | TokFstr of string

(** tokens without line numbering *)
type raw_token = Id of string | Lit of literal | Keyword of keyword | TokHtml of string

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
  ("(", Keyword TokLpar);
  (")", Keyword TokRpar);
  (";", Keyword TokSeq);
  (",", Keyword TokComma);
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
  ("fst", Keyword TokFst); (* TODO change to literals *)
  ("snd", Keyword TokSnd); (* TODO change to literals *)
  ("true", Lit TokTrue);
  ("false", Lit TokFalse)]

let symbols : string list = List.map fst symbols_tokens

module StringMap = Map.Make(String)

let keywords_map : raw_token StringMap.t =
  let symbols_only_map = List.fold_left (fun acc_keywords_map (s, tok) -> StringMap.add s tok acc_keywords_map) StringMap.empty symbols_tokens in
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
  | TokFst -> "fst"
  | TokSnd -> "snd"
  | TokLpar -> "("
  | TokRpar -> ")"
  | TokOpenML -> "<{"
  | TokCloseML -> "}>"

let string_of_raw_token (tok : raw_token) : string = match tok with
  | Id s -> Printf.sprintf "Id:%s" s
  | Keyword k -> Printf.sprintf "Kw:%s" (string_of_keyword k)
  | Lit TokFalse -> "Lit:false"
  | Lit TokTrue -> "Lit:true"
  | Lit (TokInt n) -> Printf.sprintf "Lit:%d" n
  | Lit (TokStr s) -> Printf.sprintf "Lit:%s" s
  | Lit (TokFstr s) -> Printf.sprintf "Lit(f):%s" s
  | TokHtml s -> Printf.sprintf "Html:%s" s

let string_of_token ((i, tok) : token) : string = Printf.sprintf "%d|%s" i (string_of_raw_token tok)
