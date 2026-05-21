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
      h.size <- h.size - 1;
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

module BinomialHeap (Element : Ordered) : Heap with type Elem.t = Element.t = struct
  module Elem = Element

  let ( &= ) = Elem.eq
  let ( &< ) = Elem.lt
  let ( &<< ) = Elem.compare_eq

  type tree = Node of int * Elem.t * tree list

  type heap = (tree list) ref

  let rank = function
    | Node (r, _, _) -> r

  let root = function
    | Node (_, e, _) -> e

  let empty = ref []
  let is_empty h = !h = []

  let mergeTree s t =
    match s, t with
    | Node (r, e_s, children_s), Node (_, e_t, children_t) ->
      if e_s &< e_t then
        Node (r, e_s, t :: children_s)
      else if e_t &< e_s then
        Node (r, e_t, s :: children_t)
      else if e_s &<< e_s then
        Node (r, e_s, t :: children_s)
      else
        Node (r, e_t, s :: children_t)

  let rec insTree t h =
    match h with
    | [] -> [t]
    | t' :: tl ->
        if rank t < rank t' then t :: h
        else insTree (mergeTree t t') tl

  let insert e h =
    h := insTree (Node (0, e, [])) !h

  let rec merge u v = match u, v with
    | [], h | h, [] -> h
    | t_u :: tl_u, t_v :: tl_v ->
        if rank t_u < rank t_v then t_u :: merge tl_u v
        else if rank t_v < rank t_u then t_v :: merge u tl_v
        else insTree (mergeTree t_u t_v) (merge tl_u tl_v)

  let rec removeMinTree h =
    match h with
    | [] -> raise (Invalid_argument "Empty heap")
    | [t] -> t, []
    | t :: tl ->
        let t', tl' = removeMinTree tl in
        if root t &< root t' then t, tl
        else if root t' &< root t then t', t :: tl'
        else if root t &<< root t' then t, tl
        else t', t :: tl'

  let extract h =
    let (Node(_, e, children), h') = removeMinTree !h in
    h := merge (List.rev children) h'; e

  let rec change_priority_tree old_e new_e (Node (r, e, children)) =
    if old_e &= e then
      Some (Node (r, new_e, children))
    else
      let rec loop previous_children next_children =
        match next_children with
        | [] -> None
        | t :: tl -> 
          let t' = change_priority_tree old_e new_e t in
          (match t' with
          | None -> loop (t :: previous_children) tl
          | Some t' -> Some (previous_children, t', tl))
      in
      match loop [] children with
      | None -> None
      | Some (previous_children, (Node(r', e', new_children) as t), next_children) ->
          if e &< e' then
            Some (Node (r, e, List.rev_append previous_children (t :: next_children)))
          else if e' &< e then
            Some (Node (r, e', List.rev_append previous_children (Node(r', e, new_children) :: next_children)))
          else if e &<< e' then
            Some (Node (r, e, List.rev_append previous_children (t :: next_children)))
          else
            Some (Node (r, e', List.rev_append previous_children (Node(r', e, new_children) :: next_children)))


  let change_priority old_e new_e h =
    let rec loop previous_trees next_trees =
      match next_trees with
      | [] -> previous_trees
      | t :: tl ->
        match change_priority_tree old_e new_e t with
        | None -> loop (t :: previous_trees) tl
        | Some t' -> List.rev_append previous_trees (t' :: tl)
    in
    h := loop [] !h

  let prio_insert old_e new_e h =
    let rec loop previous_trees next_trees =
      match next_trees with
      | [] -> None
      | t :: tl ->
        match change_priority_tree old_e new_e t with
        | None -> loop (t :: previous_trees) tl
        | Some t' -> Some (List.rev_append previous_trees (t' :: tl))
    in
    match loop [] !h with
    | None -> insert new_e h
    | Some h' -> h := h'

  let to_string _ = failwith "todo"
end

module TwoThreeHeap (Element : Ordered) : Heap with type Elem.t = Element.t = struct
  module Elem = Element

  let ( &= ) = Elem.eq
  let ( &< ) = Elem.lt
  let ( &<< ) = Elem.compare_eq



end

module Queue (Element : Ordered) : Heap with type Elem.t = Element.t = BinaryHeap(Element)
