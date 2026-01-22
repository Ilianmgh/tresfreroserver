open Utils
open Trie

exception LexingError of string

(* Globals defining the lexic *)
let whitespaces : char list = ['\r'; '\n'; ' '; '\t']

let keywords : string list = ["let"; "="; "fun"; "->"; "fixfun"; "if"; "then"; "else"; "&&"; "||"; "="; ">"; "<"; ">="; "<="; "<>"; "+"; "-"; "*"; "/"; "^"; "}>"; "("; ")"; "begin"; "end"]
let keywords_trie = List.fold_left (fun tr keyword -> Trie.add_word tr keyword) Trie.empty keywords

let first_id_characters : char list =
  '_' :: (* an underscore, or... *)
  (List.init 26 (fun i -> char_of_int (int_of_char 'a' + i))) (* ...a lowercase letter *)

let id_characters : char list =
  '\'' :: '_' :: (* an underscore, or *)
  (List.init 10 (fun i -> char_of_int (int_of_char '0' + i))) @ (* a digit, or *)
  (List.init 26 (fun i -> char_of_int (int_of_char 'a' + i))) @ (* a lowercase letter, or *)
  (List.init 26 (fun i -> char_of_int (int_of_char 'A' + i)))   (* an uppercase letter *)

(* While we are lexing a string, either a suffix can become a token [CouldBe s'] or it is [Is s'] *)
type 'a lexing_state = CouldBe of 'a * string | Is of 'a * string

(* lexing identifiers *)
let eat_letter_lex_identifier (st : unit lexing_state) (c : char) : unit lexing_state = match st, c with
  | CouldBe ((), ""), c -> if List.mem c first_id_characters then Is ((), string_of_char c) else CouldBe ((), "")
  | Is ((), s'), c -> if List.mem c id_characters then Is ((), s' ^ (string_of_char c)) else CouldBe ((), "")
  | _, _ -> CouldBe ((), "")

(* lexing literals *)
type literals_kind = String | Bool | Int

let is_digit_char (c : char) : bool = int_of_char '0' <= int_of_char c && int_of_char c < int_of_char '0' + 10

let rec eat_letter_lex_literal (st : literals_kind lexing_state) (c : char) : literals_kind lexing_state = match st, c with
  | CouldBe (_, ""), c -> if c == 't' then CouldBe (Bool, "t")
    else if c == 'f' then
      CouldBe (Bool, "f")
    else if c == '"' then
      CouldBe (String, "\"")
    else if is_digit_char c then
      Is (Int, string_of_char c)
    else
      CouldBe (String(*unimportant*), "")
  | Is (Int, s), c -> if is_digit_char c || c = '_' then Is (Int, s ^ string_of_char c) else CouldBe (String, "")
  | CouldBe (String, s), c -> if c = '"' then Is (String, s ^ string_of_char '"') else CouldBe (String, s ^ string_of_char c)
  | CouldBe (Bool, "t"),   'r' -> CouldBe (Bool, "tr")
  | CouldBe (Bool, "tr"),  'u' -> CouldBe (Bool, "tru")
  | CouldBe (Bool, "tru"), 'e' ->      Is (Bool, "true")
  | CouldBe (Bool, "f"),    'r' -> CouldBe (Bool, "fa")
  | CouldBe (Bool, "fa"),   'u' -> CouldBe (Bool, "fal")
  | CouldBe (Bool, "fal"),  'e' ->      Is (Bool, "fals")
  | CouldBe (Bool, "fals"), 'e' ->      Is (Bool, "false")
  | Is _, c -> eat_letter_lex_literal (CouldBe (String, "")) c
  | _, _ -> CouldBe (String, "")

(* lexing keywords *)
let rec eat_letter_lex_keyword (st : (trie * trie) lexing_state) (c : char) : (trie * trie) lexing_state = match st, c with
  | CouldBe ((orig_tr, tr), ""), c -> begin match eat_letter_opt tr c with
    | Some tr' -> if is_final tr' then Is ((orig_tr, tr'), string_of_char c) else CouldBe ((orig_tr, tr'), string_of_char c)
    | None -> CouldBe ((orig_tr, orig_tr), "")
  end
  | CouldBe ((orig_tr, tr), s), c | Is ((orig_tr, tr), s), c -> begin match eat_letter_opt tr c with
    | Some tr' -> if is_final tr' then Is ((orig_tr, tr'), s ^ string_of_char c) else CouldBe ((orig_tr, tr'), s ^ string_of_char c)
    | None -> eat_letter_lex_keyword (CouldBe ((orig_tr, orig_tr), "")) c
  end

(** Lexer *)

type keyword = TokLet | TokEq | TokFun | TokArr | TokFix | TokIf | TokThen | TokElse | TokAnd | TokOr | TokGt | TokLt | TokGeq | TokLeq | TokNeq | TokPlus | TokMinus | TokTimes | TokDiv | TokExp | TokLpar | TokRpar | TokCloseML

type literal = TokTrue | TokFalse | TokInt of int | TokStr of string

type token = Id of string | Lit of literal | Keyword of keyword | Html of string

type tokenization = {state_id : unit lexing_state ; state_lit : literals_kind lexing_state ; state_keyword : (trie * trie) lexing_state ; tokenized : token list}

(* Pretty-printing *)

let string_of_keyword (k : keyword) : string = match k with
  | TokLet -> "let"
  | TokEq -> "=" 
  | TokArr -> "->" 
  | TokFun -> "fun"
  | TokFix -> "fixfun"
  | TokIf -> "if"
  | TokThen -> "then"
  | TokElse -> "else"
  | TokAnd -> "&&"
  | TokOr -> "||"
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
  | TokLpar -> "("
  | TokRpar -> ")"
  | TokCloseML -> "}>"

let string_of_token (tok : token) : string = match tok with
  | Id s -> Printf.sprintf "Id: %s" s
  | Keyword k -> Printf.sprintf "Kw: %s" (string_of_keyword k)
  | Lit TokFalse -> "Lit: false"
  | Lit TokTrue -> "Lit: true"
  | Lit (TokInt n) -> Printf.sprintf "Lit: %d" n
  | Lit (TokStr s) -> Printf.sprintf "Lit: %s" s
  | Html s -> Printf.sprintf "Html: %s" s

let string_of_state (s : 'a lexing_state) : string = match s with
  | CouldBe (_, s) -> Printf.sprintf "CouldBe \"%s\"" s
  | Is (_, s) -> Printf.sprintf "Is \"%s\"" s

let string_of_tokenization ({state_id = st_id ; state_lit = st_lit ; state_keyword = st_keyword ; tokenized} : tokenization) : string =
  Printf.sprintf "{%s ; %s ; %s ; %s}" (string_of_state st_id) (string_of_state st_lit) (string_of_state st_keyword) (string_of_list string_of_token tokenized)

(* TODO put types & pp at the beginning of the file, or on some other file *)

let keyword_of_string (s : string) : keyword = match s with
  | "let" -> TokLet
  | "=" -> TokEq
  | "->" -> TokArr
  | "fun" -> TokFun
  | "fixfun" -> TokFix
  | "if" -> TokIf
  | "then" -> TokThen
  | "else" -> TokElse
  | "&&" -> TokAnd
  | "||" -> TokOr
  | ">" -> TokGt
  | "<" -> TokLt
  | ">=" -> TokGeq
  | "<=" -> TokLeq
  | "<>" -> TokNeq
  | "+" -> TokPlus
  | "-" -> TokMinus
  | "*" -> TokTimes
  | "/" -> TokDiv
  | "^" -> TokExp
  | "(" | "begin" -> TokLpar
  | ")" | "end" -> TokRpar
  | "}>" -> TokCloseML
  | _ -> assert false

(** Assuming on of the states in argument are in a [Is] state, outputs the corresponding token. *)
let token_of (state_id : 'a lexing_state) (state_lit : 'b lexing_state) (state_keyword : 'c lexing_state) (i_line : int) : token = match state_id, state_lit, state_keyword with
  | _, _, Is (_, s) -> Keyword (keyword_of_string s)
  | _, Is (Bool, "true"), _ -> Lit TokTrue
  | _, Is (Bool, "false"), _ -> Lit TokTrue
  | _, Is (Int, s), _ -> Lit (TokInt (int_of_string s))
  | _, Is (String, s), _ -> (let len = String.length s in assert (len >= 2); Lit (TokStr (String.sub s 1 (String.length s - 2)))) (* the string where we removed the quotes *)
  | Is (_, s), _, _ -> Id s
  | _, _, _ -> raise (LexingError (Printf.sprintf "Couldn't tokenize on line %d" i_line))

  exception FoundClosingMlBracket of int * (tokenization * bool * int)

let ml_lexer (s : string) : int * token list =
  let init_state = {
    state_id = CouldBe ((), "") ;
    state_lit = CouldBe (String, "") ;
    state_keyword = CouldBe ((keywords_trie, keywords_trie), "") ;
    tokenized = []
  } in
  let i, final_state = try begin
      -1,
      string_fold_lefti begin fun (current_state, last_char_was_whitespace, i_line) (i_s, c) ->
        let new_i_line = if c = '\n' then i_line + 1 else i_line in
        let c_is_whitespace = List.mem c whitespaces in
        if c_is_whitespace && last_char_was_whitespace then
          (current_state, true, new_i_line)
        else begin
          Printf.printf "state: %s --- c: %c\n%!" (string_of_tokenization current_state) c;
          let new_state_id = eat_letter_lex_identifier current_state.state_id c in
          let new_state_lit = eat_letter_lex_literal current_state.state_lit c in
          let new_state_keyword = eat_letter_lex_keyword current_state.state_keyword c in
          match new_state_id, new_state_lit, new_state_keyword with
            | CouldBe (_, ""), CouldBe (_, ""), CouldBe (_, "") -> (* the previous state must have lexed a token *) (* FIXME must be refined. We must remember past state and see if it's complementary i.e. ..., Is ..., ... -> CouldBe (_, "x"), CouldBe (_, ""), CouldBe (_, "x") also means a token must be outputed *)
              let new_tok = token_of current_state.state_id current_state.state_lit current_state.state_keyword new_i_line in
              let new_state = ({state_id = new_state_id ; state_lit = new_state_lit ; state_keyword = new_state_keyword ; tokenized = new_tok :: current_state.tokenized},
                c_is_whitespace, new_i_line) in
              if new_tok = Keyword TokCloseML then
                raise (FoundClosingMlBracket (i_s, new_state))
              else
                new_state
            | _, _, _ -> ({state_id = new_state_id ; state_lit = new_state_lit ; state_keyword = new_state_keyword ; tokenized = current_state.tokenized}, c_is_whitespace, new_i_line)
        end
      end (init_state, false, 1) (s ^ " ") (* we add a blank space at the end as a sentinel *)
    end
  with
    | FoundClosingMlBracket (i, final_state) -> (i, final_state)
  in
  match final_state with
    | (final_tokenization, _, _) -> i, List.rev final_tokenization.tokenized

let lexer (s : string) : token list =
  (* TODO read until <{ is found, launch ml_lexer, pick up the reading at i and loop that way *)
  let i, tokenized = ml_lexer s in
  Printf.printf "%d, %!" i;
  (* Printf.printf "%c\n" s.[i]; *)
  tokenized

(* TODO replace certain [assert false] by a LexerError with appropriate text, including line number *)
(* TODO add line number in tokens information *)
(* TODO allow to parse anything between }> ... <{ *)
(* TODO lex fstring *)

(* Tests *)

let () =
  let s = if Array.length Sys.argv > 1 then
      Sys.argv.(1)
    else begin
      let s_aux = "let x = 5 in  x" in
      String.init (String.length s_aux) (fun i -> if i = 12 then '\n' else s_aux.[i])
    end
  in
  Printf.printf "raw: %s\n" s;
  Printf.printf "lexed: %s\n" (string_of_list string_of_token (lexer s))