open Utils

(** READING & PARSING SYMBOL LIST *)

(** [fold_lefti_file_line f init fd = res] is a fold_lefti function on file [fd], line by line:
  If [fd] corresponds to a file with lines
  line_1
  ...
  line_n
  then [res = f n (... (f 1 acc line_1) ...) line_n]
*) 
let fold_lefti_file_line (f : int -> 'acc -> string -> 'acc) (acc : 'acc) (fd : in_channel) : 'acc =
  let rec fold_lefti_file_line_cnt_lines (f : int -> 'acc -> string -> 'acc) (acc : 'acc) (line_number : int) (fd : in_channel) : 'acc =
    try
      let cur_line = input_line fd in
      fold_lefti_file_line_cnt_lines f (f line_number acc cur_line) (line_number + 1) fd
    with
      End_of_file -> acc
  in
  fold_lefti_file_line_cnt_lines f acc 1 fd

let preprocess_codex_syms (s : string) : string =
  Str.global_replace (Str.regexp "\\vs{[0-9]*}") "\u{X}"
    (Str.global_replace (Str.regexp "\\vs{text}$") "\u{FE0E}"
      (Str.global_replace (Str.regexp "\\vs{emoji}$") "\u{FE0F}" s))

(** [parse_symbols path = l] where [l] is a list of pair [(name, sym)] with [name] a human-friendly name for [sym].
  [name] is represented as a list of strings, [sym] is reprensented as a list of (ASCII) character. *)
let parse_symbols (path : string) : (string list * char list) list =
  let symbols_in = open_in path in
  let _, _, symbols_list = fold_lefti_file_line begin fun line_number (submodule, cur_parent_namespace, symbols_list_acc) line ->
      (* Assuming a line is either:
        - An blank line, or a comment starting by [//] at the very beginning of the line;
        - a non-tabulated toplevel symbol name of the form [name sym] or [name]; or
        - a modifier of the form [.name1.name2.___.namen sym], whose line may start with whitespaces; or
        - an indicator that the following line is deprecated, starting by [@deprecated].
        Notice a symbol is mandatory for modifiers but not especially for toplevel names. *)
      let is_within_submodule = match submodule with | Some _ -> true | None -> false in
      if is_within_submodule && line.[0] = '}' then
        (None, cur_parent_namespace, symbols_list_acc)
      else begin
        let len_line = String.length line in
        let line = if is_within_submodule then begin
            assert (line.[0] = ' ' && line.[1] = ' ');
            String.sub line 2 (len_line - 2)
          end else
            line
        in
        let len_line = if is_within_submodule then len_line - 2 else len_line in
        if len_line > 3 && line.[0] = ' ' && line.[1] = ' ' && line.[2] = '.' then
          (* we match the line minus the first char i.e. without the first dot as a [name1.name2.___.namen sym] *)
          match submodule, cur_parent_namespace, String.split_on_char ' ' (String.sub line 3 (len_line - 3)) with
            | Some submodule_name, Some parent_name, [name; sym] ->
              (submodule, cur_parent_namespace,
              (submodule_name :: parent_name :: (String.split_on_char '.' name), char_list_of_string (preprocess_codex_syms sym)) :: symbols_list_acc)
            | None, Some parent_name, [name; sym] ->
              (submodule, cur_parent_namespace,
              (parent_name :: (String.split_on_char '.' name), char_list_of_string sym) :: symbols_list_acc)
            | None, None, truc -> ( Printf.fprintf stderr "AAAH: %s\n" (string_of_list (fun s -> s) truc);
            raise (Invalid_argument (Printf.sprintf "1.1 Failed to parse symbols from file %s at line %d: %s" path line_number line)))
            | Some _, None, truc -> ( Printf.fprintf stderr "AAAH: %s" (string_of_list (fun s -> s) truc);
            raise (Invalid_argument (Printf.sprintf "1.2 Failed to parse symbols from file %s at line %d: %s" path line_number line)))
            | _, _, truc -> ( Printf.fprintf stderr "AAAH: %s" (string_of_list (fun s -> s) truc);
            raise (Invalid_argument (Printf.sprintf "1.3 Failed to parse symbols from file %s at line %d: %s" path line_number line)))
        else if Str.string_match (Str.regexp "[ \t\r\x0C]*@deprecated") line 0 then (* TODO fix regexp ? *)
          (* If [@deprecated] is not tabulated *)
          if line.[0] = ' ' then begin
            assert (line.[1] = ' ');
            (submodule, cur_parent_namespace, symbols_list_acc)
          end else
            (submodule, None, symbols_list_acc)
        else if (len_line > 1 && (line.[0] = '/' && line.[1] = '/')) || Str.string_match (Str.regexp "^[ \t\r\x0C]*$") line 0 then
          (submodule, None, symbols_list_acc)
        else
          let split_on_space_line = String.split_on_char ' ' line in
          match split_on_space_line with
            | [submodule_name; "{"] -> (Some submodule_name, None, symbols_list_acc)
            | [name] -> (submodule, Some name, symbols_list_acc)
            | [name; sym] -> (submodule, Some name, ([name], char_list_of_string (preprocess_codex_syms sym)) :: symbols_list_acc)
            | _ -> raise (Invalid_argument (Printf.sprintf "2 Failed to parse symbols from file %s at line %d: \"%s\"" path line_number line))
      end
    end (None, None, []) symbols_in
  in 
  close_in symbols_in;
  symbols_list

(** OCAML'S REPRESENTATION OF THE SYMBOL LIST *)

(** [GeneralizedHierarchicDict] implements a dictionary of sequences of [EdgeLabels.t] to any type. *)
module type GeneralizedHierarchicDict = sig
  (** A key in the dictionary is a sequence of [key_elt] *)
  type key_elt
  type 'a t
  val empty : 'a t
  val read : 'a t -> key_elt list -> 'a t
  val read_opt : 'a t -> key_elt list -> ('a t) option
  val is_final : 'a t -> bool
  (** [find dict [x1; ...; xn] = v] if [[x1; ...; xn]] is bound to [v] in [dict] *)
  val find : 'a t -> key_elt list -> 'a
  (** [find_opt dict [x1; ...; xn] = Some v] iff [[x1; ...; xn]] is bound to [v] in [dict] *)
  val find_opt : 'a t -> key_elt list -> 'a option
  (** [add dict [k1; ...; kn] v = dict'] where [dict'] represents the same dictionary as [dict] except it also bind [[k1; ...; kn]] to [v].
    If [[k1; ...; kn]] was already bound in [dict], replaces this pre-existing binding. *)
  val add : 'a t -> key_elt list -> 'a -> 'a t
end

module GeneralizedTrie (M : Map.S) : GeneralizedHierarchicDict with type key_elt = M.key = struct
  type key_elt = M.key
  type 'a t = { curVal : 'a option ; next : ('a t) M.t }
  let empty : 'a t = { curVal = None ; next = M.empty }
  let rec read (tr : 'a t) (ks : key_elt list) : 'a t = match ks with
    | [] -> tr
    | k :: k_rest -> begin match M.find_opt k tr.next with
      | Some next_tr -> read next_tr k_rest
      | None -> raise (Invalid_argument "The key sequence leads to a dead-end.")
    end
  let rec read_opt (tr : 'a t) (ks : key_elt list) : ('a t) option = match ks with
    | [] -> Some tr
    | k :: k_rest -> begin match M.find_opt k tr.next with
      | Some next_tr -> read_opt next_tr k_rest
      | None -> None
    end
  let is_final (tr : 'a t) : bool = match tr.curVal with
    | None -> false
    | Some _ -> true
  let rec find (tr : 'a t) (ks : key_elt list) : 'a = match ks with
    | [] -> begin match tr.curVal with
      | None -> raise Not_found
      | Some x -> x
    end
    | k :: k_rest -> begin match M.find_opt k tr.next with
      | Some next_tr -> find next_tr k_rest
      | None -> raise (Invalid_argument "The key sequence leads to a dead-end.")
    end
  let rec find_opt (tr : 'a t) (ks : key_elt list) : 'a option = match ks with
    | [] -> tr.curVal
    | k :: k_rest -> begin match M.find_opt k tr.next with
      | Some next_tr -> find_opt next_tr k_rest
      | None -> None
    end
  let rec add (tr : 'a t) (ks : key_elt list) (v : 'a) : 'a t = match ks with
    | [] -> { curVal = Some v ; next = tr.next }
    | k :: k_rest -> let next_tr = begin match M.find_opt k tr.next with
        | Some next_tr -> next_tr
        | None -> empty
      end
      in
      { curVal = tr.curVal ; next = M.add k (add next_tr k_rest v) tr.next}
end

module SymbolsTrie = GeneralizedTrie(Map.Make(String))

let tml_symbols_trie = 
  let tml_symbols_list = parse_symbols "./codex/src/modules/sym.txt" in
  let only_symbols_trie = List.fold_left (fun syms_dict (name, sym) -> SymbolsTrie.add syms_dict ("sym" :: name) sym) SymbolsTrie.empty tml_symbols_list in
  let tml_emoji_list = parse_symbols "./codex/src/modules/emoji.txt" in
  List.fold_left (fun syms_dict (name, sym) -> SymbolsTrie.add syms_dict ("emoji" :: name) sym) only_symbols_trie tml_emoji_list 