module type Ordered = sig
  type t

  val eq : t -> t -> bool 
  val lt : t -> t -> bool

  val to_string : t -> string
end

module type Heap = sig
  module Elem : Ordered
  
  type heap

  val empty : heap
  val is_empty : heap -> bool

  val insert : Elem.t -> heap -> heap
  val extract : heap -> (Elem.t * heap)
  val change_priority : Elem.t -> Elem.t -> heap -> heap
  val prio_insert : Elem.t -> Elem.t -> heap -> heap

  val to_string : heap -> string
end

module Queue (Element : Ordered) : Heap with type Elem.t = Element.t
