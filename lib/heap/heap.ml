module type Ordered = sig
  type t

  val eq : t -> t -> bool 
  val lt : t -> t -> bool

  val compare_eq : t -> t -> bool

  val to_string : t -> string
end

module type Heap = sig
  module Elem : Ordered
  
  type heap

  val empty : heap
  val is_empty : heap -> bool

  val insert : Elem.t -> heap -> unit
  val extract : heap -> Elem.t
  val change_priority : Elem.t -> Elem.t -> heap -> unit
  val prio_insert : Elem.t -> Elem.t -> heap -> unit

  val to_string : heap -> string
end

module BinaryHeap (Element : Ordered) : Heap with type Elem.t = Element.t = struct
  module Elem = Element

  let ( &= ) = Elem.eq
  let ( &< ) = Elem.lt

  (* Size of heap, Length of array, array*)
  type heap = { mutable size: int; mutable length: int; mutable elts: Elem.t array }

  let empty = {size = 0; length = 0; elts = [||]}
  let is_empty h = h.size = 0

  let parent i = (i - 1) / 2
  let left_child i = 2 * i + 1
  let right_child i = 2 * i + 2

  let swap h i j =  
    let temp = h.(i) in
    h.(i) <- h.(j);
    h.(j) <- temp

  let rec bubble_up arr i =
    if i > 0 then
      let p = parent i in
      if arr.(i) &< arr.(p) then
        (swap arr i p;
         bubble_up arr p)
      else if arr.(i) &= arr.(p) then
        if Elem.compare_eq arr.(i) arr.(p) then
          (swap arr i p;
           bubble_up arr p)

  let rec bubble_down arr n i =
    let l = left_child i in
    let r = right_child i in
    let smallest = 
      if l < n && (arr.(l) &< arr.(i) || (arr.(l) &= arr.(i) && Elem.compare_eq arr.(l) arr.(i))) then l else i in
    let smallest =
      if r < n && (arr.(r) &< arr.(smallest) || (arr.(r) &= arr.(i) && Elem.compare_eq arr.(r) arr.(i))) then r else smallest in
    if smallest <> i then
      (swap arr i smallest;
       bubble_down arr n smallest)

  let insert e (h : heap) =
    if h.size = 0 then 
      (h.length <- 1; h.elts <- [|e|])
    else if h.size = h.length then 
      (h.length <- 2 * h.length; 
      h.elts <- Array.init (2 * h.length) (fun i -> if i < h.size then h.elts.(i) else e))
    else 
      h.elts.(h.size) <- e
    ; bubble_up h.elts h.size
    ; h.size <- h.size + 1

  let extract h =
    if h.size = 0 then raise (Invalid_argument "Empty heap")
    else 
      let e = h.elts.(0) in
      h.elts.(0) <- h.elts.(h.size-1);
      bubble_down h.elts (h.size-1) 0;
      e

  let change_priority_i i new_e h =
    if new_e &< h.elts.(i) then (h.elts.(i) <- new_e; bubble_up h.elts i)
    else (h.elts.(i) <- new_e; bubble_down h.elts h.size i)

  let find_index e n arr =
    let rec loop i =
      if i >= n then None
      else if arr.(i) &= e then Some i
      else loop (i+1)
    in
    loop 0

  let change_priority old_e new_e h =
    find_index old_e h.size h.elts
    |> Option.get
    |> fun i -> change_priority_i i new_e h

  let prio_insert old_e new_e h =
    match find_index old_e h.size h.elts with
    | Some i -> change_priority_i i new_e h
    | None -> insert new_e h

  let to_string h =
    let rec aux i acc =
      if i >= h.size then acc
      else aux (i + 1) (acc ^ (Elem.to_string h.elts.(i)) ^ "\n")
    in
    aux 0 ""
end


module Queue (Element : Ordered) : Heap with type Elem.t = Element.t = BinaryHeap (Element)
