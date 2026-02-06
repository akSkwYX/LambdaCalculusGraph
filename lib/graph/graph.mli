module type Element = sig
  type t

  val eq : t -> t -> bool

  val to_string : t -> string
end

module type Graph = sig
  module Elem : Element

  type t

  val empty : t
  val is_empty : t -> bool

  val order : t -> int
  val size : t -> int

  val add_node : t -> unit
  val add_edge : int -> int -> t -> unit
  val force_add_edge : int -> int -> t -> unit

  val to_pdf : t -> (int, Elem.t) Hashtbl.t -> Elem.t list -> string -> unit
end

module Graph (Elem : Element) : Graph with module Elem = Elem
