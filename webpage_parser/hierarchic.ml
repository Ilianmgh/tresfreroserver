module type S = sig
  type key
  type 'a t
  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val add_to_sub : key list -> key -> 'a -> 'a t -> 'a t
  val add_sub : key -> 'a t -> 'a t -> 'a t
  val find_root : key -> 'a t -> 'a
  val find_root_opt : key -> 'a t -> 'a option
  val submap : key -> 'a t -> 'a t
  val submap_opt : key -> 'a t -> 'a t option
  val map : ('a -> 'b) -> 'a t -> 'b t
  val is_empty : 'a t -> bool
  val iter : (key list -> key -> 'a -> unit) -> 'a t -> unit
  val fold : (key list -> key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
end

(** A hierarchic map: a collection of maps with parental links: each map has submap identified by their _names_, which are also of the type of the map's keys *)
module Make = functor (M : Map.S) -> struct
  type key = M.key
  type +!'a t = { root : 'a M.t ; sub : ('a t) M.t ; parent : 'a t option }
  let empty_with_parent ?(parent : 'a t option = None) : 'a t = { root = M.empty ; sub = M.empty ; parent = parent }
  let empty : 'a t = empty_with_parent ~parent:None
  let add (k : key) (v : 'a) (s : 'a t) : 'a t = { root = M.add k v s.root ; sub = s.sub ; parent = s.parent }
  let rec add_to_sub (prefix : key list) (k : key) (v : 'a) (s : 'a t) : 'a t = match prefix with (* TODO make tail-rec *)
    | [] -> add k v s
    | toplevel_module :: prefix_rem -> begin match M.find_opt toplevel_module s.sub with
      | None -> raise Not_found
      | Some s' -> { root = s.root ; sub = M.add toplevel_module (add_to_sub prefix_rem k v s') s.sub ; parent = s.parent }
    end
  let add_sub (k : key) (sub_s : 'a t)  (s : 'a t) : 'a t = { root = s.root ; sub = M.add k { root = sub_s.root ; sub = sub_s.sub ; parent = Some s } s.sub ; parent = s.parent }
  let rec find (k : key) (s : 'a t) : 'a = match M.find_opt k s.root with
    | Some v -> v
    | None -> begin match s.parent with
      | None -> raise Not_found
      | Some p -> find k p
    end
  let rec find_opt (k : key) (s : 'a t) : 'a option = match M.find_opt k s.root with
    | Some v -> Some v
    | None -> begin match s.parent with
      | None -> None
      | Some p -> find_opt k p
    end
  let submap (subk : key) (s : 'a t) : 'a t = M.find subk s.sub (* FIXME Maybe catch error and add a local error ? *)
  let submap_opt (subk : key) (s : 'a t) : 'a t option = M.find_opt subk s.sub
  let rec map (f : 'a -> 'b) (s : 'a t) : 'b t = { root = M.map f s.root ; sub = M.map (map f) s.sub ; parent = s.parent}
  let is_empty (s : 'a t) : bool = M.is_empty s.root && M.is_empty s.sub (* maybe we rather want to explore the hierarchy tree and see if each node has an empty map *)
  let rec iter (f : key -> key -> 'a -> unit) (s : 'a t) : unit = (* TODO add prefix when exploring children *)
    M.iter (fun k v -> f k k v) s.root;
    M.iter (fun k sub_s -> iter f sub_s) s.sub
  let fold (f : key list -> key -> 'a -> 'acc -> 'acc) (s : 'a t) (acc : 'acc) : 'acc = Printf.fprintf stderr "TODO Hierarchic.fold\n%!"; acc
end