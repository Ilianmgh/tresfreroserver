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
  Str.global_replace (Str.regexp {|\\vs{([[a-f][A-F][0-9]]*)}|}) "\u{\1}" (* Won't work, maybe switch to HTML encoding of unicode characters *)
    (Str.global_replace (Str.regexp {|\\vs{text}$|}) "\u{FE0E}"
      (Str.global_replace (Str.regexp {|\\vs{emoji}$|}) "\u{FE0F}" s))

(** [parse_symbols path = l] where [l] is a list of pair [(module_path, sym_name, sym)] with [sym_name] a human-friendly name for [sym] within [module_path].
  [name] is represented as a list of strings, [sym] is reprensented as a list of (ASCII) characters. *)
let parse_symbols (path : string) : (string list * string list * char list) list = (* TODO generalize to multiple-nested submodules *)
  let symbols_in = open_in path in
  let _, _, symbols_list = fold_lefti_file_line begin fun line_number (submodule, cur_parent_namespace, symbols_list_acc) line ->
      (* Assuming a line is either:
        - An blank line, or a comment starting by [//] at the very beginning of the line;
        - a non-tabulated toplevel symbol name of the form [name sym] or [name]; or
        - a modifier of the form [.name1.name2.___.namen sym], whose line may start with whitespaces; or
        - an indicator that the following line is deprecated, starting by [@deprecated]; or
        - a submodule name followed by a submodule, delimited by {/}.
        Notice a symbol is mandatory for modifiers but not especially for toplevel names. *)
      let is_within_submodule, submodules_list = match submodule with | Some submodule_name -> true, [submodule_name] | None -> false, [] in
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
            | _, Some parent_name, [name; sym] ->
              (submodule, cur_parent_namespace,
              (submodules_list, parent_name :: (String.split_on_char '.' name), char_list_of_string (preprocess_codex_syms sym)) :: symbols_list_acc)
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
            | [name; sym] -> (submodule, Some name, (submodules_list, [name], char_list_of_string (preprocess_codex_syms sym)) :: symbols_list_acc)
            | _ -> raise (Invalid_argument (Printf.sprintf "2 Failed to parse symbols from file %s at line %d: \"%s\"" path line_number line))
      end
    end (None, None, []) symbols_in
  in 
  close_in symbols_in;
  symbols_list

(** OCAML'S REPRESENTATION OF THE SYMBOL LIST *)

(** [GeneralizedHierarchicDict] implements a hierarchical dictionary of sequences of [EdgeLabels.t] to any type. *)
module type GeneralizedHierarchicDict = sig
  (** A key in the dictionary is a sequence of [key_elt] *)
  type key_elt
  type 'a t
  val empty : 'a t
  val read : 'a t -> key_elt list -> 'a t
  val read_opt : 'a t -> key_elt list -> ('a t) option
  (** [find dict [x1; ...; xn] = v] if [[x1; ...; xn]] is bound to [v] in [dict] *)
  val find : 'a t -> key_elt list -> 'a
  (** [find_opt dict [x1; ...; xn] = Some v] iff [[x1; ...; xn]] is bound to [v] in [dict] *)
  val find_opt : 'a t -> key_elt list -> 'a option
  (** [add dict [k1; ...; kn] v = dict'] where [dict'] represents the same dictionary as [dict] except it also bind [[k1; ...; kn]] to [v].
    If [[k1; ...; kn]] was already bound in [dict], replaces this pre-existing binding. *)
  val add : 'a t -> key_elt list -> key_elt list -> 'a -> 'a t
end

module GeneralizedTrie (M : Map.S) : GeneralizedHierarchicDict with type key_elt = M.key = struct
  exception UndefinedSymbol of string
  type key_elt = M.key
  type 'a symbol_names = (key_elt list * 'a) list
  (** If a symbol name was already read, then we only need to focus on the variations, and the variant is [Symbols].
    Otherwise we are reading module or symbol names and we're in the [Modules] case *)
  type 'a t = Modules of { curSymbols : ('a symbol_names) M.t ; submodules : ('a t) M.t } | Symbols of 'a symbol_names
  let empty : 'a t = Modules { curSymbols = M.empty ; submodules = M.empty }
  let read_one (cur : 'a t) (k : key_elt) : 'a t = match cur with
    | Modules tr -> begin match M.find_opt k tr.submodules with
      | Some next_tr -> next_tr (* k is a module name *)
      | None -> begin match M.find_opt k tr.curSymbols with (* k is a variation or a symbol name *)
        | Some syms -> Symbols syms
        | None -> raise (UndefinedSymbol "Symbol not found.")
      end
    end
    | Symbols lst -> Symbols (List.fold_left begin fun acc_rem_syms (sym_variations, sym) ->
      if List.mem k sym_variations then (sym_variations, sym) :: acc_rem_syms else acc_rem_syms (* we keep [sym] iff variation [k] _can_ lead to [sym] *)
    end [] lst)
  let extract_current_symbol (cur : 'a t) : 'a = match cur with
    | Symbols syms -> begin match List.fold_left begin fun (n_variations, closest_sym) (cur_variations, cur_sym) ->
        let cur_n_variations = List.length cur_variations in
        match closest_sym with
          | None -> (cur_n_variations, Some cur_sym)
          | Some _ -> (* n_variations is the actual number of variations to obtain closest_sym. *)
            if n_variations <= cur_n_variations then
              (n_variations, closest_sym)
            else
              (cur_n_variations, Some cur_sym)
      end (0, None) syms with
      | (_, Some final_sym) -> final_sym
      | (_, None) -> raise (UndefinedSymbol "Couldn't find a symbol with these variations.") (* TODO be more precise in error messages in general in this module. *)
    end
    | Modules _ -> raise (UndefinedSymbol "No default symbol for modules.")
  let rec read (cur : 'a t) (ks : key_elt list) : 'a t = match ks with
    | [] -> cur
    | k :: k_rem -> read (read_one cur k) k_rem
  let rec read_opt (cur : 'a t) (ks : key_elt list) : ('a t) option = try
      Some (read cur ks)
    with
      | UndefinedSymbol _ -> None
  let rec find (cur : 'a t) (ks : key_elt list) : 'a = match read_opt cur ks with
    | Some ks_sym -> extract_current_symbol ks_sym
    | _ -> raise (UndefinedSymbol "Symbol not found.")
  let rec find_opt (cur : 'a t) (ks : key_elt list) : 'a option = try
      Some (find cur ks)
    with
      | UndefinedSymbol _ -> None
  let rec add_symbol_variation (sym : 'a symbol_names) (ks : key_elt list) (v : 'a) : 'a symbol_names = (ks, v) :: sym
  let rec add (cur : 'a t) (module_list : key_elt list) (ks : key_elt list) (v : 'a) : 'a t = match module_list, ks, cur with
    | modu_k :: modu_k_rest, _, Modules tr -> let next_tr = begin match M.find_opt modu_k tr.submodules with
        | Some next_tr -> next_tr
        | None -> empty
      end
      in
      Modules { curSymbols = tr.curSymbols ; submodules = M.add modu_k (add next_tr modu_k_rest ks v) tr.submodules }
    | [], symname :: variant_suffixes, Modules tr -> let cur_sym = begin match M.find_opt symname tr.curSymbols with
        | Some sym_variants -> sym_variants
        | None -> []
      end
      in
      Modules { curSymbols = M.add symname (add_symbol_variation cur_sym ks v) tr.curSymbols  ; submodules = tr.submodules }
    | [], ks, Symbols sym -> Symbols (add_symbol_variation sym ks v)
    | _ :: _, _, Symbols _ -> raise (Invalid_argument "Can't add symbol module to a symbol name.")
    | _, [], Modules _ -> raise (Invalid_argument "There cannot be a default value to modules, try a nonempty list of names.")
end

module SymbolsTrie = GeneralizedTrie(Map.Make(String))

let tml_symbols_trie = 
  let tml_symbols_list = parse_symbols "./codex/src/modules/sym.txt" in
  let only_symbols_trie = List.fold_left (fun syms_dict (modules_name, name, sym) -> SymbolsTrie.add syms_dict ("sym" :: modules_name) name sym) SymbolsTrie.empty tml_symbols_list in
  let tml_emoji_list = parse_symbols "./codex/src/modules/emoji.txt" in
  List.fold_left (fun syms_dict (modules_name, name, sym) -> SymbolsTrie.add syms_dict ("emoji" :: modules_name) name sym) only_symbols_trie tml_emoji_list 