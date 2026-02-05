module type S = sig
  type key
  type 'a t
  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val add_sub : key -> 'a t -> 'a t -> 'a t
  val find_root : key -> 'a t -> 'a
  val find_root_opt : key -> 'a t -> 'a option
  val find_sub : key -> 'a t -> 'a t
  val find_sub_opt : key -> 'a t -> 'a t option
  val map : ('a -> 'a) -> 'a t -> 'a t
end

module Make = functor (M : Map.S) -> struct
  type key = M.key
  type +!'a t = { root : 'a M.t ; sub : ('a t) M.t }
  let empty : 'a t = { root = M.empty ; sub = M.empty }
  let add (k : key) (v : 'a) (s : 'a t) : 'a t = { root = M.add k v s.root ; sub = s.sub }
  let add_sub (k : key) (new_sub : 'a t) (s : 'a t) : 'a t = { root = s.root ; sub = M.add k new_sub s.sub }
  let find_root (k : key) (s : 'a t) : 'a = M.find k s.root
  let find_root_opt (k : key) (s : 'a t) : 'a option = M.find_opt k s.root
  let find_sub (subk : key) (s : 'a t) : 'a t = M.find subk s.sub
  let find_sub_opt (subk : key) (s : 'a t) : 'a t option = M.find_opt subk s.sub
  let rec map (f : 'a -> 'a) (s : 'a t) : 'a t = { root = M.map f s.root ; sub = M.map (map f) s.sub}
end