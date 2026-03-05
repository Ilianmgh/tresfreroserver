module type S = sig
  type key
  type 'a t
  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val add_to_sub : key list -> key -> 'a -> 'a t -> 'a t
  val add_and_path : key list -> key -> 'a -> 'a t -> 'a t
  val add_sub : key -> 'a t -> 'a t -> 'a t
  val find : key -> 'a t -> 'a
  val find_opt : key -> 'a t -> 'a option
  val submap : key -> 'a t -> 'a t
  val submap_opt : key -> 'a t -> 'a t option
  val supmap : 'a t -> 'a t
  val supmap_opt : 'a t -> 'a t option
  val supmap_namespace : key -> 'a t -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val is_empty : 'a t -> bool
  val iter : (key list -> key -> 'a -> unit) -> 'a t -> unit
  val fold : (key list -> key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
  val disjoint_union : 'a t -> 'a t -> 'a t
end

module Make = functor (M : Map.S) -> struct
  type key = M.key
  type +!'a t = { root : 'a M.t ; sub : ('a t) M.t ; parent : 'a t option }
  let empty_with_parent ?(parent : 'a t option = None) (() : unit) : 'a t = { root = M.empty ; sub = M.empty ; parent = parent }
  let empty : 'a t = empty_with_parent () ~parent:None
  let add (k : key) (v : 'a) (s : 'a t) : 'a t = { root = M.add k v s.root ; sub = s.sub ; parent = s.parent }
  let rec add_to_sub (prefix : key list) (k : key) (v : 'a) (s : 'a t) : 'a t = match prefix with (* TODO make tail-rec *)
    | [] -> add k v s
    | toplevel_module :: prefix_rem -> begin match M.find_opt toplevel_module s.sub with
      | None -> Printf.fprintf stderr "1\n"; raise Not_found
      | Some s' -> { root = s.root ; sub = M.add toplevel_module (add_to_sub prefix_rem k v s') s.sub ; parent = s.parent }
    end
  let rec add_and_path (prefix : key list) (k : key) (v : 'a) (s : 'a t) : 'a t = match prefix with (* TODO make tail-rec *)
    | [] -> add k v s
    | toplevel_module :: prefix_rem -> begin match M.find_opt toplevel_module s.sub with
      | None -> { root = s.root ; sub = M.add toplevel_module (add_and_path prefix_rem k v empty) s.sub ; parent = s.parent }
      | Some s' -> { root = s.root ; sub = M.add toplevel_module (add_and_path prefix_rem k v s') s.sub ; parent = s.parent }
    end
  let add_sub (k : key) (sub_s : 'a t)  (s : 'a t) : 'a t = { root = s.root ; sub = M.add k { root = sub_s.root ; sub = sub_s.sub ; parent = Some s } s.sub ; parent = s.parent } (* TODO double usage here: we set parents twice, once on add/map/... and once on access to the chils. See where we can afford to not care about parents; maybe we simply need to put them here *)
  let rec find (k : key) (s : 'a t) : 'a = match M.find_opt k s.root with
    | Some v -> v
    | None -> begin match s.parent with
      | None -> Printf.fprintf stderr "2\n"; raise Not_found
      | Some p -> find k p
    end
  let rec find_opt (k : key) (s : 'a t) : 'a option = match M.find_opt k s.root with
    | Some v -> Some v
    | None -> begin match s.parent with
      | None -> None
      | Some p -> find_opt k p
    end
  let submap (subk : key) (s : 'a t) : 'a t = let child = M.find subk s.sub in { root = child.root ; sub = child.sub ; parent = Some s }
  let submap_opt (subk : key) (s : 'a t) : 'a t option = match M.find_opt subk s.sub with
    | None -> None
    | Some child -> Some { root = child.root ; sub = child.sub ; parent = Some s }
  let supmap (s : 'a t) : 'a t = match s.parent with
    | None -> Printf.fprintf stderr "3\n"; raise Not_found
    | Some p -> p
  let supmap_opt (s : 'a t) : 'a t option = s.parent
  let supmap_namespace (namespace : key) (s : 'a t) : 'a t = match s.parent with
    | None -> Printf.fprintf stderr "4\n"; raise Not_found
    | Some p -> p (* FIXME see if we want to leave the parent unchanged. The actual way, everythinf added to [s] will be discarded because we don't put [s] back as a child of [p], so [s] is restored to its last value when we called [submap] to get it. *)
  let rec map_change_parents (f : 'a -> 'b) (new_parent : 'b t option) (s : 'a t) : 'b t =
    let mapped_root = 
      { root = M.map f s.root ;
        sub = M.empty;
        parent = None}
    in
    let new_sub = M.map (map_change_parents f (Some mapped_root)) s.sub in
    { root = mapped_root.root ; sub = new_sub ; parent = new_parent }
  let rec map (f : 'a -> 'b) (s : 'a t) : 'b t = map_change_parents f begin match s.parent with
      | None -> None
      | Some p -> Some (map f p)
    end s (* FIXME not sure it works with the two-sided pointers *)
  let is_empty (s : 'a t) : bool = M.is_empty s.root && M.is_empty s.sub (* maybe we rather want to explore the hierarchy tree and see if each node has an empty map *)
  let rec iter (f : key list -> key -> 'a -> unit) (s : 'a t) : unit = (* TODO add prefix when exploring children *)
    M.iter (fun k v -> f [] k v) s.root;
    M.iter (fun k sub_s -> iter f sub_s) s.sub
  let fold (f : key list -> key -> 'a -> 'acc -> 'acc) (s : 'a t) (acc : 'acc) : 'acc =
    let rec fold_acc_prefix (cur_prefix : key list) (f : key list -> key -> 'a -> 'acc -> 'acc) (s : 'a t) (acc : 'acc) : 'acc =
      let current_folded = M.fold (f cur_prefix) s.root acc in
      M.fold (fun subname submodule acc -> fold_acc_prefix (subname :: cur_prefix) f submodule acc) s.sub current_folded
    in
    fold_acc_prefix [] f s acc
  let disjoint_union (s1 : 'a t) (s2 : 'a t) : 'a t =
    { root = M.union (fun _ _ -> raise (Invalid_argument "Non-disjoint union")) s1.root s2.root
    ; sub = M.union (fun _ _ -> raise (Invalid_argument "Non-disjoint union")) s1.sub s2.sub
    ; parent = None}
end