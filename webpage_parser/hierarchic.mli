(** A hierarchic map: a collection of maps with parental links: each map has submap identified by their _names_, which are also of the type of the map's keys *)
module type S = sig
  type key
  type 'a t
  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  (** [add_to_sub prefixes k s] adds a binding from [k] to [s] to a submap of [s], designated by the chain of submodules [prefixes] *)
  val add_to_sub : key list -> key -> 'a -> 'a t -> 'a t
  (** [add_sub k s' s] adds [s'] as a submap of [s], with name [k] *)
  val add_sub : key -> 'a t -> 'a t -> 'a t
  (** [find k s = v] where [k] is bound to [v] at the current namespace in [s] *)
  val find : key -> 'a t -> 'a
  (** Same as [find] but with an option *)
  val find_opt : key -> 'a t -> 'a option
  (** [submap namespace s = s'] where [s'] is the sub-environment of [s] with additional namespace [namespace] *)
  val submap : key -> 'a t -> 'a t
  (** Same as [submap] but with an option *)
  val submap_opt : key -> 'a t -> 'a t option
  (** [map f s = s'] maps [f] to each element of [s]. Should only be called on a root. *)
  val map : ('a -> 'b) -> 'a t -> 'b t
  val is_empty : 'a t -> bool
  (** [iter f s] applies [f] to each element of [s] *)
  val iter : (key list -> key -> 'a -> unit) -> 'a t -> unit
  (** [fold f s] TODO *)
  val fold : (key list -> key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
end

module Make (M : Map.S) : S with type key = M.key
