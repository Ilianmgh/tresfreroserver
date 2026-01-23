open Utils
open Trie

exception LexingError of string
let open_ml_bracket = "<{"

let close_ml_bracket = "}>"

let len_open_ml_bracket = String.length open_ml_bracket

let symbols : string list = ["="; "->"; "&&"; "||"; "="; ">"; "<"; ">="; "<="; "<>"; "+"; "-"; "*"; "/"; "^"; open_ml_bracket; close_ml_bracket; "("; ")"; ";"]
let symbols_trie = List.fold_left (fun tr keyword -> Trie.add_word tr keyword) Trie.empty symbols

let whitespaces : char list = ['\r'; '\n'; ' '; '\t']

(** A pretoken is either some html code and a line number or a ml code pretoken and a line number *)
type pre_token = PretokHtml of int * string | PretokMl of int * string

let string_of_pre_token (pretok : pre_token) : string = match pretok with
  | PretokHtml (i, s) -> Printf.sprintf "Html %d|%s" i s
  | PretokMl (i, s) -> Printf.sprintf "Ml %d|%s" i s

(** Returns the line number and the next non white space index *)
let rec next_non_white_space_idx (s : string) (i : int) (n : int) (line_number : int) : int * int =
  if i = n then (line_number, n)
else if s.[i] = '\n' then next_non_white_space_idx s (i + 1) n (line_number + 1)
  else if List.mem s.[i] whitespaces then next_non_white_space_idx s (i + 1) n line_number
  else (line_number, i)

(** splits [s] on tokens-to-be from [i] (inclusive) to [n] (exclusive) *)
let prelexer (s : string) (i : int) (n : int) : pre_token list =
    (** returns [(i, line_number, lst)] with [i] the next character from which to lex, [line_number] the current line number at the [i]-th character and [lst] the list of pretoken found so far *)
  let rec split_ml (s : string) (i : int) (n : int) (line_number : int) : int * int * (int * string) list =
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
    in
    let update_word_list ((w1, w2) : (int * string) * (int * string)) (lexed : (int * string) list) : (int * string) list = match w1, w2 with
      | (_, ""), (_, "") -> Printf.fprintf stderr "Hum.\n%!"; lexed
      | (i, w), (_, "") | (_, ""), (i, w) -> (i, w) :: lexed
      | (i1, w1), (i2, w2) -> (i1, w1) :: (i2, w2) :: lexed
    in
    let rec split_ml_symbols_whitespace (s : string) (cur_word_acc : char list) (cur_symbol_acc : char list) (lexed_acc : (int * string) list) (i : int) (n : int) (orig_lex : trie) (lex_state : trie) (line_number : int) : int * int * (int * string) list =
      let next_line_number, i_nonwhitespace = next_non_white_space_idx s i n line_number in
      let next_lex_state, next_symbol_acc = if i < n then match eat_letter_opt lex_state s.[i] with
          | Some tr -> tr, s.[i] :: cur_symbol_acc
          | None -> orig_lex, []
        else
          orig_lex, cur_symbol_acc
      in
      if i_nonwhitespace != i || i_nonwhitespace = n || (is_final lex_state && not (is_final next_lex_state)) then begin
        (* we have to output a pretoken now *)
        let sym_lexed, word_lexed = lex_now cur_word_acc cur_symbol_acc lex_state line_number in
        let new_lexed_acc : (int * string) list = update_word_list (sym_lexed, word_lexed) lexed_acc in
        if (snd sym_lexed) = close_ml_bracket then
          (i_nonwhitespace, line_number, new_lexed_acc)
        else
          if i_nonwhitespace = n then
            (n, line_number, new_lexed_acc)
          else begin
            split_ml_symbols_whitespace s [] [] new_lexed_acc i_nonwhitespace n orig_lex orig_lex next_line_number
          end
      end else
        split_ml_symbols_whitespace s (s.[i] :: cur_word_acc) next_symbol_acc lexed_acc (i + 1) n orig_lex next_lex_state next_line_number
    in
    split_ml_symbols_whitespace s [] [] [] i n symbols_trie symbols_trie line_number
  and split_html (s: string) (cur_html_acc : char list) (prelex_acc : pre_token list) (i : int) (n : int) (line_number : int) : pre_token list =
    assert (i <= n);
    if i = n then begin
      let last_html_pretok : pre_token = PretokHtml (line_number, string_of_char_list (List.rev cur_html_acc)) in
      last_html_pretok :: prelex_acc
    end else if i + len_open_ml_bracket - 1 >= n then (* there's not enough space to lex another ML opening bracket *)
      let next_line_number = if s.[i] = '\n' then line_number + 1 else line_number in
      split_html s (s.[i] :: cur_html_acc) prelex_acc (i + 1) n next_line_number
    else if String.sub s i len_open_ml_bracket = open_ml_bracket then begin
      let html_pretok : pre_token = PretokHtml (line_number, string_of_char_list (List.rev cur_html_acc)) in
      Printf.fprintf stderr "Now lexing ml\n%!";
      let next_i, next_line_number, ml_lexed = split_ml s i n line_number in
      Printf.fprintf stderr "End lexing ml\n%!";
      assert (next_i > i);
      let new_prelex_acc : pre_token list = (List.map (fun (i, w) -> PretokMl (i, w)) ml_lexed) @ (html_pretok :: prelex_acc) in
      split_html s [] new_prelex_acc next_i n next_line_number
    end else
      let next_line_number = if s.[i] = '\n' then line_number + 1 else line_number in
      split_html s (s.[i] :: cur_html_acc) prelex_acc (i + 1) n next_line_number
  in
  List.rev (split_html s [] [] i n 1)

(* Tests *)

let () =
  let s = if Array.length Sys.argv > 1 then
      Sys.argv.(1)
    else begin
      let s_aux = "<{let x = 5 in  x}>" in
      String.init (String.length s_aux) (fun i -> if i = 14 then '\n' else s_aux.[i])
    end
  in
  Printf.printf "raw: %s\n" s;
  Printf.printf "lexed: %s\n" (string_of_list string_of_pre_token (prelexer s 0 (String.length s)))