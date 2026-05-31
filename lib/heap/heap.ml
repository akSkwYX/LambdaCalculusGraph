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

  val size : heap -> int

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
  let ( &<< ) = Elem.compare_eq

  (* Size of heap, Length of array, array*)
  type heap = { mutable size: int; mutable length: int; mutable elts: Elem.t array }

  let size h = h.size

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
      if arr.(i) &< arr.(p) || (arr.(i) &= arr.(p) && arr.(i) &<< arr.(p)) then
        (swap arr i p;
         bubble_up arr p)

  let rec bubble_down arr n i =
    let l = left_child i in
    let r = right_child i in
    let smallest = 
      if l < n && (arr.(l) &< arr.(i) || (arr.(l) &= arr.(i) && arr.(l) &<< arr.(i))) then l else i in
    let smallest =
      if r < n && (arr.(r) &< arr.(smallest) || (arr.(r) &= arr.(smallest) && arr.(r) &<< arr.(smallest))) then r else smallest in
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
      bubble_down h.elts h.size 0;
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

  type heap = {mutable trees : tree list; mutable size : int}

  let size h = h.size

  let rank = function
    | Node (r, _, _) -> r

  let root = function
    | Node (_, e, _) -> e

  let empty = {trees = []; size = 0}
  let is_empty h = h.size = 0

  let mergeTree s t =
    match s, t with
    | Node (r, e_s, children_s), Node (_, e_t, children_t) ->
      if e_s &< e_t then
        Node (r, e_s, t :: children_s)
      else if e_t &< e_s then
        Node (r, e_t, s :: children_t)
      else if e_s &<< e_t then
        Node (r, e_s, t :: children_s)
      else
        Node (r, e_t, s :: children_t)

  let rec insTree t trees =
    match trees with
    | [] -> [t]
    | t' :: tl ->
        if rank t < rank t' then t :: trees
        else insTree (mergeTree t t') tl

  let insert e h =
    h.trees <- insTree (Node (0, e, [])) h.trees;
    h.size <- h.size + 1

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
    let (Node(_, e, children), h') = removeMinTree h.trees in
    h.trees <- merge (List.rev children) h';
    h.size <- h.size - 1;
    e

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
    h.trees <- loop [] h.trees

  let prio_insert old_e new_e h =
    let rec loop previous_trees next_trees =
      match next_trees with
      | [] -> None
      | t :: tl ->
        match change_priority_tree old_e new_e t with
        | None -> loop (t :: previous_trees) tl
        | Some t' -> Some (List.rev_append previous_trees (t' :: tl))
    in
    match loop [] h.trees with
    | None -> insert new_e h
    | Some h' -> h.trees <- h'

  let rec tree_to_string (Node(r, e, children)) =
    "rank : " ^ string_of_int r ^ ", e : " ^ (Elem.to_string e) ^ "\n" ^ "  " ^
    (String.concat "\n" (List.map tree_to_string children))

  let to_string h =
    (String.concat "\n" (List.map tree_to_string h.trees))
end

(* module FibonacciHeap (Element : Ordered) : Heap with type Elem.t = Element.t = struct *)
(*   module Elem = Element *)
(**)
(*   let ( &< ) = Elem.lt *)
(*   let ( &= ) = Elem.eq *)
(*   let ( &<< ) = Elem.compare_eq *)
(**)
(*   type tree = Node of Elem.t * tree list | Empty *)
(**)
(*   let root = function *)
(*     | Node (r, _) -> r *)
(*     | Empty -> raise (Invalid_argument "Empty tree") *)
(**)
(*   (* s <--> s *) *)
(*   (* s <--> a <--> b <--> c <--> s *) *)
(*   type 'a dlist = {mutable previous : 'a dlist; mutable e : 'a; mutable next : 'a dlist} *)
(*   let dlist_empty () = *)
(*     let rec s = {previous = s; e = Obj.magic (); next = s} in *)
(*     s *)
(**)
(*   type heap = {mutable min_tree : tree dlist option; mutable trees : tree dlist; mutable size : int} *)
(**)
(*   let size h = h.size *)
(**)
(*   let empty = {min_tree = None; trees = dlist_empty (); size = 0} *)
(*   let is_empty h = h.size = 0 *)
(**)
(*   let merge u v = *)
(*     {min_tree = (match u.min_tree, v.min_tree with *)
(*       | None, t | t, None -> t *)
(*       | Some t, Some t' -> *)
(*           (if root t.e &< root t'.e then Some t *)
(*           else if root t'.e &< root t.e then Some t' *)
(*           else if root t.e &<< root t'.e then Some t *)
(*           else Some t')); *)
(*       trees =  *)
(*         (u.trees.previous.next <- v.trees.next; *)
(*         u.trees.previous <- v.trees.previous; *)
(*         u.trees); *)
(*       size = u.size + v.size *)
(*     } *)
(**)
(*   let insert = failwith "" *)
(**)
(*   let extract = failwith "" *)
(**)
(*   let change_priority = failwith "" *)
(**)
(*   let prio_insert = failwith "" *)
(**)
(*   let to_string = failwith "" *)
(* end *)

module Queue (Element : Ordered) : Heap with type Elem.t = Element.t = BinomialHeap(Element)
