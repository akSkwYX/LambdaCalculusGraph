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

module BinaryHeap (Element : Ordered) : Heap with type Elem.t = Element.t = struct
  module Elem = Element

  let ( &= ) = Elem.eq
  let ( &< ) = Elem.lt

  (* Size of heap, Length of array, array*)
  type heap = int * int * Elem.t array

  let empty = (0, 0, [||])
  let is_empty (n, _, _) = n = 0

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

  let rec bubble_down arr n i =
    let l = left_child i in
    let r = right_child i in
    let smallest = 
      if l < n && arr.(l) &< arr.(i) then l else i in
    let smallest = 
      if r < n && arr.(r) &< arr.(smallest) then r else smallest in
    if smallest <> i then
      (swap arr i smallest;
       bubble_down arr n smallest)

  let insert e ((n, m, arr) as h) =
    if n = 0 then (1, 1, [|e|])
    else 
    let (n, m, arr) = if n = m then (n, 2 * m, Array.init (2 * m) (fun i -> if i < n then arr.(i) else e)) 
                      else (arr.(n) <- e; h) in
    bubble_up arr n; (n+1, m, arr)
    
  let extract (n, m, arr) =
    if n = 0 then raise (Invalid_argument "Empty heap")
    else 
      let e = arr.(0) in
      arr.(0) <- arr.(n-1);
      bubble_down arr (n-1) 0;
      (e, (n-1, m, arr))

  let change_priority_i i new_e ((n, _, arr) as h) =
    if new_e &< arr.(i) then (arr.(i) <- new_e; bubble_up arr i)
    else (arr.(i) <- new_e; bubble_down arr n i);
    h

  let find_index e n arr =
    let rec loop i =
      if i >= n then None
      else if arr.(i) &= e then Some i
      else loop (i+1)
    in
    loop 0

  let change_priority old_e new_e ((n, _, arr) as h) =
    find_index old_e n arr 
    |> Option.get
    |> fun i -> change_priority_i i new_e h

  let prio_insert old_e new_e ((n, _, arr) as h) =
    match find_index old_e n arr with
    | Some i -> change_priority_i i new_e h
    | None -> insert new_e h

  let to_string (n, _, arr) =
    let rec aux i acc =
      if i >= n then acc
      else aux (i + 1) (acc ^ (Elem.to_string arr.(i)) ^ " ")
    in
    aux 0 ""
end


module Queue (Element : Ordered) : Heap with type Elem.t = Element.t = BinaryHeap (Element)
