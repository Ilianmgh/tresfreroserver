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

module Make (M : Map.S) : S with type key = M.key
