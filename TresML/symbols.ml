open Utils

(** READING SYMBOL LIST *)

(** TODO ! *)

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
  let tml_symbols_list = [(["arrow"; "squiggly"], char_list_of_string "⇝"); (["arrow"], ['-'; '>']) ] in
  List.fold_left (fun syms_dict (name, sym) -> SymbolsTrie.add syms_dict name sym) SymbolsTrie.empty tml_symbols_list