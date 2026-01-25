include Prelexer
open Utils
open Trie

exception LexingError of string

(* Some helpers functions to filter characaters *)

let is_char_num (c : char) : bool =
  int_of_char '0' <= int_of_char c && int_of_char c <= int_of_char '9'

let is_char_lowercase_letter (c : char) : bool =
  int_of_char 'a' <= int_of_char c && int_of_char c <= int_of_char 'z'

let is_char_uppercase_letter (c : char) : bool =
  int_of_char 'A' <= int_of_char c && int_of_char c <= int_of_char 'Z'

let is_first_character_of_id (c : char) : bool =
  c = '_' || (is_char_lowercase_letter c)

let is_character_of_num (c : char) : bool =
  c = '_' || (is_char_num c)

let is_character_of_id (c : char) : bool =
  c = '\'' || c = '_' || (is_char_num c) || (is_char_lowercase_letter c) || (is_char_uppercase_letter c)

(** While we are lexing a string [s], let [l] be the list of its character in reverse order, either it can be completed to become a token of types either [x1] or ... or [xn]: [CouldBe ([x1; ...; xn], l)], either it is a token [Is l] *)
type 'a lexing_state = CouldBe of 'a list * char list | Is of 'a * char list (* FIXME maybe merge CouldBe and IsNot; CouldBe ([],_) can represent IsNot *)

(* lexing identifiers *)
let eat_letter_lex_identifier (st : unit lexing_state) (c : char) : unit lexing_state = match st, c with
  | CouldBe (_, []), c -> if is_first_character_of_id c then Is ((), [c]) else CouldBe ([], [c])
  | Is ((), s'), c -> if is_character_of_id c then Is ((), c :: s') else CouldBe ([], c :: s')
  | CouldBe ([], s'), c -> CouldBe ([], (c :: s'))
  | _, _ -> assert false (* undefined state *)

(* lexing literals *)
type literals_kind = Fstring | String | Int

let rec eat_letter_lex_literal (st : literals_kind lexing_state) (c : char) : literals_kind lexing_state = match st, c with
  | CouldBe (_, []), c ->
    if is_char_num c then Is (Int, [c])
    else if c = '"' then CouldBe ([String], ['\"'])
    else if c = 'f' then CouldBe ([Fstring], ['f']) else CouldBe ([], [c])
  | Is (Int, s'), c -> if is_character_of_num c then Is (Int, c :: s') else CouldBe ([], c :: s')
  | CouldBe ([String], s'), '\"' -> Is (String, '\"' :: s') (* TODO take into account escaped quote : should not be considered as the end of the string *)
  | CouldBe ([String], s'), c -> CouldBe ([String], c :: s')
  | Is (String, s'), c -> CouldBe ([], c :: s')
  | CouldBe ([], s'), c -> CouldBe ([], c :: s')
  | CouldBe ([Fstring], ['f']), '"' -> CouldBe ([Fstring], ['"'; 'f'])
  | CouldBe ([Fstring], c1 :: c2 :: fstr), '\"' -> Is (Fstring, '\"' :: c1 :: c2 :: fstr) (* we need at least two characters to open a fstring: f and a quote *)
  | CouldBe ([Fstring], c1 :: c2 :: fstr), c -> CouldBe ([Fstring], c :: c1 :: c2 :: fstr)
  | Is (Fstring, s'), c -> CouldBe ([], c :: s')
  | _, _ -> assert false (* undefined state *)

(** Lexer *)

let token_of_ml_pretoken (s : string) : raw_token = match StringMap.find_opt s keywords_map with
  | Some raw_tok -> raw_tok
  | None -> (* Then it's either a number, string literal or an identifier *)
    let initial_id_state : unit lexing_state = (CouldBe ([], [])) in
    let initial_lit_state : literals_kind lexing_state = (CouldBe ([], [])) in
    let final_states : unit lexing_state * literals_kind lexing_state =
      String.fold_left
        (fun (id_state, lit_state) c -> (eat_letter_lex_identifier id_state c, eat_letter_lex_literal lit_state c))
        (initial_id_state, initial_lit_state) s
    in match final_states with
      | Is ((), l), _ -> (assert (s = string_of_char_list (List.rev l)); Id s)
      | _, Is (Int, l) -> (assert (s = string_of_char_list (List.rev l)); Lit (TokInt (int_of_string s)))
      | _, Is (String, l) -> (assert (s = string_of_char_list (List.rev l)); assert (s.[0] = '"'); assert (s.[String.length s - 1] = '"'); Lit (TokStr (String.sub s 1 (String.length s - 2))))
      | _, Is (Fstring, l) -> (assert (s = string_of_char_list (List.rev l)); assert (s.[0] = 'f'); assert (s.[1] = '"'); assert (s.[String.length s - 1] = '"');
        Lit (TokFstr (String.sub s 2 (String.length s - 3))))
      | _, _ -> assert false


let pre_token_lexer (pretok : pre_token) : token = match pretok with
  | PretokHtml (i_line, s) -> (i_line, TokHtml s)
  | PretokMl (i_line, s) -> (i_line, token_of_ml_pretoken s)

let pre_tokens_lexer (l : pre_token list) : token list = List.map pre_token_lexer l

let lexer (s : string) : token list = pre_tokens_lexer (prelexer s 0 (String.length s))

(* Tests *)

(* let () =
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
    else List.nth tests 4
  in
  Printf.printf "raw: %s\n" s;
  Printf.printf "lexed: %s\n" (string_of_list string_of_token (lexer s)) *)