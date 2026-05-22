type 'a t

val empty : unit -> 'a t
val is_empty : 'a t -> bool

val get : 'a t -> int -> 'a option
val set : 'a t -> int -> 'a option -> unit
