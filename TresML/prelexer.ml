include Lexic
open Utils
open Trie
open Symbols

let debug = false

exception PrelexingError of string
exception LexingError of string
let symbols_trie = List.fold_left (fun tr keyword -> Trie.add_word tr keyword) Trie.empty symbols

let whitespaces : char list = ['\r'; '\n'; ' '; '\t']

(** A pretoken is either some html code and a line number or a ml code pretoken and a line number *)
type pre_token = PretokHtml of int * string | PretokMl of int * string | PretokFstr of int * string

let string_of_pre_token (pretok : pre_token) : string = match pretok with
  | PretokHtml (i, s) -> Printf.sprintf "l.%d Html:%s" i s
  | PretokMl (i, s) -> Printf.sprintf "l.%d Ml:%s" i s
  | PretokFstr (i, s) -> Printf.sprintf "l.%d Fstr:%s" i s

(** Returns the line number and the next non white space index *)
let rec next_non_white_space_idx (s : string) (i : int) (n : int) (line_number : int) : int * int =
  if i = n then (line_number, n)
else if s.[i] = '\n' then next_non_white_space_idx s (i + 1) n (line_number + 1)
  else if List.mem s.[i] whitespaces then next_non_white_space_idx s (i + 1) n line_number
  else (line_number, i)

(* Some helpers functions to filter characaters *)

let is_char_num (c : char) : bool =
  int_of_char '0' <= int_of_char c && int_of_char c <= int_of_char '9'

let is_char_lowercase_letter (c : char) : bool =
  int_of_char 'a' <= int_of_char c && int_of_char c <= int_of_char 'z'

let is_char_uppercase_letter (c : char) : bool =
  int_of_char 'A' <= int_of_char c && int_of_char c <= int_of_char 'Z'

let is_first_character_of_Id (c : char) : bool = is_char_uppercase_letter c

let is_first_character_of_id (c : char) : bool =
  (c = '_') || (is_char_lowercase_letter c)

let is_character_of_num (c : char) : bool =
  (c = '_') || (is_char_num c)

let is_character_of_id (c : char) : bool =
  (c = '\'') || (c = '_') || (is_char_num c) || (is_char_lowercase_letter c) || (is_char_uppercase_letter c)


(** [lex_now ['>'; '-'; 'y'; 'e'; 'h'] ['>'; '-'] ...] returns [("hey", "->")] *)
let lex_now (char_acc : char list) (symbol_acc : char list) (cur_symbol_lex : trie) (line_number : int) : (int * string) * (int * string) =
  if is_final cur_symbol_lex then begin
    (* symbol_acc is a lexed symbol, suffix of char_acc *)
    let rec extract_symbol (symbol_buf : char list) (symbol_acc : char list) (buf : char list) (line_number : int) : (int * string) * (int * string) = match symbol_buf, buf with
      | [], buf' -> ((line_number, string_of_char_list symbol_acc), (line_number, string_of_char_list (List.rev buf')))
      | c1 :: sym_buf', c2 :: buf' -> if c1 = c2 then extract_symbol sym_buf' (c1 :: symbol_acc) buf' line_number else assert false
      | _, _ -> assert false
    in
    extract_symbol symbol_acc [] char_acc line_number
  end else begin
    ((line_number, ""), (line_number, string_of_char_list (List.rev char_acc)))
  end

let update_word_list_pretok ((w1, w2) : (int * string) * (int * string)) (lexed : pre_token list) : pre_token list = match w1, w2 with
  | (_, ""), (_, "") -> Printf.fprintf stderr "Hum.\n%!"; lexed (* FIXME find out why it hums (TEST 6 among others) *)
  | (i, w), (_, "") | (_, ""), (i, w) -> (PretokMl (i, w)) :: lexed
  | (i1, w1), (i2, w2) -> (PretokMl (i1, w1)) :: (PretokMl (i2, w2)) :: lexed

(** [split_comment s line_number i n = (after_comment_line_number, after_comment_i)] reads from [s] until a closing TresML comment bracket is found.
  [after_comment_line_number] is the line number of the closing comment bracket.
  [after_comment_i] is the index of the first character after the closing comment bracket. *)
let rec split_comment (s : string) (line_number : int) (i : int) (n : int) : int * int =
  if i + len_close_ml_comment - 1 < n && String.sub s i len_close_ml_comment = close_ml_comment then
    (line_number, i + len_close_ml_comment)
  else if i + 1 < n && s.[i] = '\\' && s.[i + 1] = 'n' then
    split_comment s (line_number + 1) (i + 2) n
  else
    split_comment s line_number (i + 1) n

(** splits [s] on tokens-to-be from [i] (inclusive) to [n] (exclusive).
  This function is built from a function per kinds of language (strings, comment, html, ML, ...) that are mutually recursive. *)
let prelexer_all (s : string) (i : int) (n : int) : pre_token list =
  (** [read_symbol s i n line_number = c] where [c] is a unicode character, as a string, if we could read a substring of the form [name1.name2. ... .namen] in [s] from [i]. In this case [name1.name2. ... .namen] designate [c] according to TODO *)
  let read_symbol (s : string) (i : int) (n : int) (line_number : int) : int * int * char list =
    let rec read_symbol_acc (s : string) (i : int) (n : int) (line_number : int) (name_acc : char list) (cur_sym_trie : (char list) SymbolsTrie.t) : int * int * char list =
      assert (i <= n); 
      if i = n || (not (is_char_lowercase_letter s.[i]) && s.[i] <> '.') then
        let final_sym_trie = match SymbolsTrie.read_opt cur_sym_trie [string_of_char_list (List.rev name_acc)] with
          | Some next_tr -> next_tr
          | None -> raise (PrelexingError (Printf.sprintf "line %d: Undefined symbol." line_number))
        in
        match SymbolsTrie.find_opt final_sym_trie [] with
          | None -> raise (PrelexingError (Printf.sprintf "line %d: Undefined symbol." line_number))
          | Some sym -> (i, line_number, sym)
      else if s.[i] = '.' then
        match SymbolsTrie.read_opt cur_sym_trie [string_of_char_list (List.rev name_acc)] with
          | Some next_tr -> read_symbol_acc s (i + 1) n line_number [] next_tr
          | None -> raise (PrelexingError (Printf.sprintf "line %d: Undefined symbol." line_number))
      else (* it's an alphabetic character *)
        read_symbol_acc s (i + 1) n line_number (s.[i] :: name_acc) cur_sym_trie
    in
    read_symbol_acc s i n line_number [] tml_symbols_trie
  in
  (** returns [(i, line_number, lst)] with [i] the next character from which to lex, [line_number] the current line number at the [i]-th character and [lst] the list of pretoken found so far *)
  let rec split_ml (s : string) (prelex_acc : pre_token list) (i : int) (n : int) (line_number : int) : int * int * pre_token list =
    let rec split_string (s : string) (acc : char list) (line_number : int) (i : int) (n : int) : int * int * string =
      assert (i <= n);
      if i = n then begin
        raise (PrelexingError "End of string expected.")
      end else begin
        let next_line_number = if s.[i] = '\n' then line_number + 1 else line_number in
        if (s.[i] = '"' && not (0 < i && s.[i - 1] = '\\')) then
          (i+1, next_line_number, string_of_char_list (List.rev ('"' :: acc)))
        else if s.[i] = '#' && not (0 < i && s.[i - 1] = '\\') then
          let next_i, line_number, sym = read_symbol s (i + 1) n line_number in
          split_string s (List.rev_append sym acc) line_number next_i n
        else
          split_string s (s.[i] :: acc) line_number (i + 1) n
      end
    in
    let rec split_fstring
      (s : string)
      (fstring_acc : char list) (lexed_acc : pre_token list)
      (orig_lex : trie)
      (line_number : int) (i : int) (n : int)
      (split_html_rec : string -> char list -> pre_token list -> int -> int -> int -> int * int * pre_token list) : int * int * pre_token list =
      assert (i <= n);
      if i = n then begin
        raise (PrelexingError "End of fstring expected.")
      end else begin
        let next_line_number = if s.[i] = '\n' then line_number + 1 else line_number in
        if (s.[i] = '"' && not(0 < i && s.[i - 1] = '\\')) then
          split_ml_symbols_whitespace s [] [] ((PretokMl (next_line_number, "\"")) :: (PretokFstr (next_line_number, string_of_char_list (List.rev fstring_acc))) :: lexed_acc) (i+1) n orig_lex orig_lex next_line_number split_html_rec
        else if s.[i] = '%' && (i+1 < n && s.[i + 1] = '{') && not (0 < i && s.[i - 1] = '\\') then (* TODO change this in a similar way as detection of comments. *)
          split_ml_symbols_whitespace s [] [] ((PretokMl (next_line_number, "%{")) :: (PretokFstr (next_line_number, string_of_char_list (List.rev fstring_acc))) :: lexed_acc) (i+2) n orig_lex orig_lex next_line_number split_html_rec
        else if s.[i] = '#' && not (0 < i && s.[i - 1] = '\\') then
          let next_i, line_number, sym = read_symbol s (i + 1) n line_number in
          split_fstring s (List.rev_append sym fstring_acc) lexed_acc orig_lex line_number next_i n split_html_rec
        else
          split_fstring s (s.[i] :: fstring_acc) lexed_acc orig_lex line_number (i + 1) n split_html_rec
      end
    and split_ml_symbols_whitespace
      (s : string)
      (cur_word_acc : char list) (cur_symbol_acc : char list)
      (lexed_acc : pre_token list)
      (i : int) (n : int)
      (orig_lex : trie) (lex_state : trie)
      (line_number : int)
      (split_html_rec : string -> char list -> pre_token list -> int -> int -> int -> int * int * pre_token list)
        : int * int * pre_token list =
      let next_line_number, i_nonwhitespace = next_non_white_space_idx s i n line_number in
      if cur_word_acc = [] && i_nonwhitespace < n && s.[i_nonwhitespace] = '"' then begin
        (* we stop lexing keywords, lexing a string *)
        let next_i, str_line_number, str_lit = split_string s ['"'] next_line_number (i_nonwhitespace + 1) n in
        let lexed_acc_with_string = update_word_list_pretok ((0, ""), (str_line_number, str_lit)) lexed_acc in
        split_ml_symbols_whitespace s [] [] lexed_acc_with_string next_i n orig_lex orig_lex next_line_number split_html_rec
      end else if cur_word_acc = [] && i_nonwhitespace + len_open_ml_comment - 1 < n && String.sub s i len_open_ml_comment = open_ml_comment then begin
        (* we stop lexing keywords, lexing a comment *)
        let after_comment_line_number, next_i = split_comment s next_line_number (i_nonwhitespace + len_open_ml_comment) n in
        split_ml_symbols_whitespace s [] [] lexed_acc next_i n orig_lex orig_lex after_comment_line_number split_html_rec
      end else begin
        let next_lex_state, next_symbol_acc = if i < n then match eat_letter_opt lex_state s.[i] with
            | Some tr -> tr, s.[i] :: cur_symbol_acc
            | None -> orig_lex, []
          else
            orig_lex, cur_symbol_acc
        in
        if i_nonwhitespace != i || i_nonwhitespace = n || (is_final lex_state && not (is_final next_lex_state)) then begin
          (* we have to output a pretoken now *)
          let sym_lexed, word_lexed = lex_now cur_word_acc cur_symbol_acc lex_state line_number in
          let new_lexed_acc : pre_token list = update_word_list_pretok (sym_lexed, word_lexed) lexed_acc in
          (* either we detect the beginning of html code, we lex html from now on... *)
          if (snd sym_lexed) = close_ml_bracket then
            split_html_rec s [] new_lexed_acc i n line_number
          else if (snd sym_lexed) = open_html_bracket then
            split_html_rec s [] new_lexed_acc i n line_number
          (* ... either we detect the beginning of fstring... *)
          else if (snd sym_lexed) = close_ml_fstr_bracket then
            split_fstring s [] new_lexed_acc orig_lex line_number i n split_html_rec
          else if (snd sym_lexed) = open_fstring then
            split_fstring s [] new_lexed_acc orig_lex line_number i n split_html_rec
          else
            (* ... either there is nothing more to lex... *)
            if i_nonwhitespace = n then
              (n, line_number, new_lexed_acc)
            (* ... or we continue lexing ml *)
            else begin
              split_ml_symbols_whitespace s [] [] new_lexed_acc i_nonwhitespace n orig_lex orig_lex next_line_number split_html_rec
            end
        end else
          split_ml_symbols_whitespace s (s.[i] :: cur_word_acc) next_symbol_acc lexed_acc (i + 1) n orig_lex next_lex_state next_line_number split_html_rec
      end
    in
    split_ml_symbols_whitespace s [] [] prelex_acc i n symbols_trie symbols_trie line_number split_html
  and split_html (s: string) (cur_html_acc : char list) (prelex_acc : pre_token list) (i : int) (n : int) (line_number : int) : int * int * pre_token list =
    assert (i <= n); 
    if i = n then begin
      let last_html_pretok : pre_token = PretokHtml (line_number, string_of_char_list (List.rev cur_html_acc)) in
      (i, line_number, last_html_pretok :: prelex_acc)
    end else if i + len_close_html_bracket - 1 < n && (String.sub s i len_close_html_bracket = close_html_bracket) then begin
      (* We can encounter a HTML-closing bracket, we then need to switch to prelexing ML *)
      let last_html_pretok : pre_token = PretokHtml (line_number, string_of_char_list (List.rev cur_html_acc)) in
      if debug then Printf.fprintf stderr "Now prelexing ml\n%!";
      split_ml s ((PretokMl (line_number, close_html_bracket)) :: last_html_pretok :: prelex_acc) (i + len_close_html_bracket) n line_number
    end else if i + len_open_ml_bracket - 1 < n && (String.sub s i len_open_ml_bracket = open_ml_bracket) then begin
      (* We can also encounter a ML-opening bracket, that requires to do the same thing *)
      let html_pretok : pre_token = PretokHtml (line_number, string_of_char_list (List.rev cur_html_acc)) in
      if debug then Printf.fprintf stderr "Now prelexing ml\n%!";
      split_ml s (html_pretok :: prelex_acc) i n line_number
    end else if s.[i] = '#' && not (0 < i && s.[i - 1] = '\\') then
      let next_i, next_line_number, sym = read_symbol s (i + 1) n line_number in
      split_html s (List.rev_append sym cur_html_acc) prelex_acc next_i n next_line_number
    else
      let next_line_number = if s.[i] = '\n' then line_number + 1 else line_number in
      split_html s (s.[i] :: cur_html_acc) prelex_acc (i + 1) n next_line_number
  in
  let last_idx, last_line_number, toklst = split_html s [] [] i n 1 in
  if last_idx != String.length s then
    raise (PrelexingError "Unexpected end of HTML.")
  else
    List.rev toklst