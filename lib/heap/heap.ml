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

  val empty : unit -> heap
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

  let empty () = {size = 0; length = 0; elts = [||]}
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

  let empty () = {trees = []; size = 0}
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

module FibonacciHeap (Element : Ordered) : Heap with type Elem.t = Element.t = struct
  module Elem = Element

  let ( &< ) = Elem.lt
  let ( &= ) = Elem.eq
  let ( &<< ) = Elem.compare_eq

  (* s <--> s *)
  (* s <--> a <--> b <--> c <--> s *)
  type 'a dlist = {mutable previous : 'a dlist; e : 'a; mutable next : 'a dlist}
  let dlist_empty () =
    let rec s = {previous = s; e = Obj.magic (); next = s} in
    s
  
  let dlist_merge a b =
    if b.next != b then begin
      a.previous.next <- b.next;
      b.next.previous <- a.previous;
      a.previous <- b.previous;
      b.previous.next <- a
    end

  let dlist_remove d =
    d.previous.next <- d.next;
    d.next.previous <- d.previous

  let dlist_iter f d =
    let rec aux d' =
      if d' == d then ()
      else (f d'; aux d'.next)
    in
    aux d.next

  type tree = {mutable parent : tree option; mutable self_link : tree dlist; mutable degree : int; mutable mark : bool; mutable e : Elem.t; children : tree dlist}

  let tree_merge (t : tree dlist) (t' : tree dlist) =
    let t, t' = 
      if t.e.e &< t'.e.e then t, t'
      else if t'.e.e &< t.e.e then t', t
      else if t.e.e &<< t'.e.e then t, t'
      else t', t
    in
    t.e.degree <- t.e.degree + 1;
    t'.e.parent <- Some t.e;
    let nlink = {previous = t.e.children; e = t'.e; next = t.e.children.next} in
    t'.e.self_link <- nlink;
    t.e.children.next.previous <- nlink;
    t.e.children.next <- nlink;
    t

  type heap = {mutable min_tree : tree dlist option; mutable trees : tree dlist; mutable size : int}

  let size h = h.size

  let empty () = {min_tree = None; trees = dlist_empty (); size = 0}
  let is_empty h = h.size = 0

  let merge u v =
    if is_empty u then 
      (u.min_tree <- v.min_tree;
      u.trees <- v.trees;
      u.size <- v.size)
    else if is_empty v then ()
    else
    (u.min_tree <- (match u.min_tree, v.min_tree with
      | None, t | t, None -> t
      | Some t, Some t' ->
          (if t.e.e &< t'.e.e then Some t
          else if t'.e.e &< t.e.e then Some t'
          else if t.e.e &<< t'.e.e then Some t
          else Some t'));
    u.trees <-
        (dlist_merge u.trees v.trees;
        u.trees);
    u.size <- u.size + v.size)

  let insert e h =
    let dl = dlist_empty () in
    dl.next <- {previous = dl; e = {parent = None; self_link = dlist_empty (); degree = 0; mark = false; e = e; children = dlist_empty ()}; next = dl};
    dl.previous <- dl.next;
    merge h {min_tree = Some dl.next; trees = dl; size = 1}

  let extract h =
    if h.size = 1 then
      let min = (Option.get h.min_tree).e.e in
      dlist_remove (Option.get h.min_tree);
      h.min_tree <- None;
      h.size <- 0;
      min
    else begin
    match h.min_tree with
    | None -> raise (Invalid_argument "Empty heap")
    | Some min_tree ->
      let min = min_tree.e.e in
      let children = min_tree.e.children in
      dlist_remove min_tree;
      dlist_iter (fun child -> child.e.parent <- None) children;
      dlist_merge h.trees children;
      let rank_array = Array.make 64 None in
      dlist_iter (fun t -> 
        let t = ref t in
        let d_t = ref !t.e.degree in
        while Option.is_some rank_array.(!d_t) do
          t := tree_merge !t (Option.get rank_array.(!d_t));
          rank_array.(!d_t) <- None;
          d_t := !t.e.degree
        done;
        rank_array.(!d_t) <- Some !t) h.trees
      ;
      h.trees <- dlist_empty ();
      let first_index = Option.get @@ Array.find_index Option.is_some rank_array in
      let t = Option.get rank_array.(first_index) in
      t.previous <- h.trees;
      t.next <- h.trees.next;
      h.trees.next.previous <- t;
      h.trees.next <- t;
      rank_array.(first_index) <- None;
      h.min_tree <- Some (Array.fold_left (fun (min_tree : tree dlist) t -> 
        match t with 
        | None -> min_tree 
        | Some t ->
          t.previous <- h.trees;
          t.next <- h.trees.next;
          t.e.self_link <- dlist_empty ();
          h.trees.next.previous <- t;
          h.trees.next <- t;
          if t.e.e &< min_tree.e.e then t
          else if min_tree.e.e &< t.e.e then min_tree
          else if t.e.e &<< min_tree.e.e then t
          else min_tree) t rank_array)
      ;
      h.size <- h.size - 1;
      min end

  let change_priority old_e new_e h = 
    let rec find_in_tree (t : tree dlist) =
      (* 1. Check the current node *)
      if t.e.e &= old_e then
        begin
        dlist_remove t;
        t.e.parent <- None;
        t.e.self_link <- dlist_empty ();
        t.e.mark <- false;
        t.e.e <- new_e;
        t.previous <- h.trees;
        t.next <- h.trees.next;
        h.trees.next.previous <- t;
        h.trees.next <- t;
        if new_e &< (Option.get h.min_tree).e.e
          || new_e &= (Option.get h.min_tree).e.e && new_e &<< (Option.get h.min_tree).e.e then h.min_tree <- Some t;
        
        let parent_opt = ref t.e.parent in
        while Option.is_some !parent_opt && (Option.get !parent_opt).mark do
          let parent = Option.get !parent_opt in
          dlist_remove parent.self_link;
          parent.degree <- parent.degree - 1;
          parent.mark <- false;
          parent.self_link.previous <- h.trees;
          parent.self_link.next <- h.trees.next;
          h.trees.next.previous <- parent.self_link;
          h.trees.next <- parent.self_link;
          parent.self_link <- dlist_empty ();
          parent_opt := parent.parent;
          parent.parent <- None
        done;
        (match !parent_opt with
        | None -> ()
        | Some p -> p.mark <- true);
        true
        end
      else

        let rec aux d =
          if d == t.e.children then false
          else if find_in_tree d then true
          else aux d.next
        in
        aux t.e.children.next
    in

    let rec find_in_dlist d =
      if d == h.trees then raise Not_found
      else if find_in_tree d then ()
      else find_in_dlist d.next
    in
    
    find_in_dlist h.trees.next


  let prio_insert old_e new_e h =
    try change_priority old_e new_e h with
    | Not_found -> insert new_e h

  let to_string h =
  if h.size = 0 then "Empty Heap"
  else
    let buf = Buffer.create 1024 in
    
    (* Helper to recursively format a tree node with indentation *)
    let rec format_tree indent (t : tree dlist) =
      if t.e.parent = None then 
        Buffer.add_string buf (Printf.sprintf "%s• Node(val: %s, deg: %d, mark: %b)\n" 
          indent (Elem.to_string t.e.e) t.e.degree t.e.mark)
      else
        Buffer.add_string buf (Printf.sprintf "%s├── Node(val: %s, deg: %d, mark: %b)\n" 
          indent (Elem.to_string t.e.e) t.e.degree t.e.mark);
          
      (* Recurse through children if any exist *)
      let child_sentinel = t.e.children in
      let rec format_children curr =
        if curr == child_sentinel then ()
        else begin
          format_tree (indent ^ "    ") curr;
          format_children curr.next
        end
      in
      format_children child_sentinel.next
    in

    (* Format the root list *)
    Buffer.add_string buf (Printf.sprintf "--- Fibonacci Heap (Size: %d) ---\n" h.size);
    (match h.min_tree with
     | None -> Buffer.add_string buf "Min pointer: None\n"
     | Some m -> Buffer.add_string buf (Printf.sprintf "Min pointer points to: %s\n" (Elem.to_string m.e.e)));
    
    Buffer.add_string buf "Root List:\n";
    let rec format_roots curr =
      if curr == h.trees then ()
      else begin
        format_tree "  " curr;
        format_roots curr.next
      end
    in
    format_roots h.trees.next;
    Buffer.add_string buf "---------------------------------\n";
    Buffer.contents buf
end

module FibonacciHeapWithoutComparison (Element : Ordered) : Heap with type Elem.t = Element.t = struct
  module Elem = Element

  let ( &< ) = Elem.lt
  let ( &= ) = Elem.eq

  (* s <--> s *)
  (* s <--> a <--> b <--> c <--> s *)
  type 'a dlist = {mutable previous : 'a dlist; e : 'a; mutable next : 'a dlist}
  let dlist_empty () =
    let rec s = {previous = s; e = Obj.magic (); next = s} in
    s
  
  let dlist_merge a b =
    if b.next != b then begin
      a.previous.next <- b.next;
      b.next.previous <- a.previous;
      a.previous <- b.previous;
      b.previous.next <- a
    end

  let dlist_remove d =
    d.previous.next <- d.next;
    d.next.previous <- d.previous

  let dlist_iter f d =
    let rec aux d' =
      if d' == d then ()
      else (f d'; aux d'.next)
    in
    aux d.next

  type tree = {mutable parent : tree option; mutable self_link : tree dlist; mutable degree : int; mutable mark : bool; mutable e : Elem.t; children : tree dlist}

  let tree_merge (t : tree dlist) (t' : tree dlist) =
    let t, t' = 
      if t.e.e &< t'.e.e then t, t'
      else t', t
    in
    t.e.degree <- t.e.degree + 1;
    t'.e.parent <- Some t.e;
    let nlink = {previous = t.e.children; e = t'.e; next = t.e.children.next} in
    t'.e.self_link <- nlink;
    t.e.children.next.previous <- nlink;
    t.e.children.next <- nlink;
    t

  type heap = {mutable min_tree : tree dlist option; mutable trees : tree dlist; mutable size : int}

  let size h = h.size

  let empty () = {min_tree = None; trees = dlist_empty (); size = 0}
  let is_empty h = h.size = 0

  let merge u v =
    if is_empty u then 
      (u.min_tree <- v.min_tree;
      u.trees <- v.trees;
      u.size <- v.size)
    else if is_empty v then ()
    else
    (u.min_tree <- (match u.min_tree, v.min_tree with
      | None, t | t, None -> t
      | Some t, Some t' ->
          (if t.e.e &< t'.e.e then Some t
          else Some t'));
    u.trees <-
        (dlist_merge u.trees v.trees;
        u.trees);
    u.size <- u.size + v.size)

  let insert e h =
    let dl = dlist_empty () in
    dl.next <- {previous = dl; e = {parent = None; self_link = dlist_empty (); degree = 0; mark = false; e = e; children = dlist_empty ()}; next = dl};
    dl.previous <- dl.next;
    merge h {min_tree = Some dl.next; trees = dl; size = 1}

  let extract h =
    if h.size = 1 then
      let min = (Option.get h.min_tree).e.e in
      dlist_remove (Option.get h.min_tree);
      h.min_tree <- None;
      h.size <- 0;
      min
    else begin
    match h.min_tree with
    | None -> raise (Invalid_argument "Empty heap")
    | Some min_tree ->
      let min = min_tree.e.e in
      let children = min_tree.e.children in
      dlist_remove min_tree;
      dlist_iter (fun child -> child.e.parent <- None) children;
      dlist_merge h.trees children;
      let rank_array = Array.make 64 None in
      dlist_iter (fun t -> 
        let t = ref t in
        let d_t = ref !t.e.degree in
        while Option.is_some rank_array.(!d_t) do
          t := tree_merge !t (Option.get rank_array.(!d_t));
          rank_array.(!d_t) <- None;
          d_t := !t.e.degree
        done;
        rank_array.(!d_t) <- Some !t) h.trees
      ;
      h.trees <- dlist_empty ();
      let first_index = Option.get @@ Array.find_index Option.is_some rank_array in
      let t = Option.get rank_array.(first_index) in
      t.previous <- h.trees;
      t.next <- h.trees.next;
      h.trees.next.previous <- t;
      h.trees.next <- t;
      rank_array.(first_index) <- None;
      h.min_tree <- Some (Array.fold_left (fun (min_tree : tree dlist) t -> 
        match t with 
        | None -> min_tree 
        | Some t ->
          t.previous <- h.trees;
          t.next <- h.trees.next;
          t.e.self_link <- dlist_empty ();
          h.trees.next.previous <- t;
          h.trees.next <- t;
          if t.e.e &< min_tree.e.e then t
          else min_tree) t rank_array)
      ;
      h.size <- h.size - 1;
      min end

  let change_priority old_e new_e h = 
    let rec find_in_tree (t : tree dlist) =
      (* 1. Check the current node *)
      if t.e.e &= old_e then
        begin
        dlist_remove t;
        t.e.parent <- None;
        t.e.self_link <- dlist_empty ();
        t.e.mark <- false;
        t.e.e <- new_e;
        t.previous <- h.trees;
        t.next <- h.trees.next;
        h.trees.next.previous <- t;
        h.trees.next <- t;
        if new_e &< (Option.get h.min_tree).e.e then h.min_tree <- Some t;
        
        let parent_opt = ref t.e.parent in
        while Option.is_some !parent_opt && (Option.get !parent_opt).mark do
          let parent = Option.get !parent_opt in
          dlist_remove parent.self_link;
          parent.degree <- parent.degree - 1;
          parent.mark <- false;
          parent.self_link.previous <- h.trees;
          parent.self_link.next <- h.trees.next;
          h.trees.next.previous <- parent.self_link;
          h.trees.next <- parent.self_link;
          parent.self_link <- dlist_empty ();
          parent_opt := parent.parent;
          parent.parent <- None
        done;
        (match !parent_opt with
        | None -> ()
        | Some p -> p.mark <- true);
        true
        end
      else

        let rec aux d =
          if d == t.e.children then false
          else if find_in_tree d then true
          else aux d.next
        in
        aux t.e.children.next
    in

    let rec find_in_dlist d =
      if d == h.trees then raise Not_found
      else if find_in_tree d then ()
      else find_in_dlist d.next
    in
    
    find_in_dlist h.trees.next


  let prio_insert old_e new_e h =
    try change_priority old_e new_e h with
    | Not_found -> insert new_e h

  let to_string h =
  if h.size = 0 then "Empty Heap"
  else
    let buf = Buffer.create 1024 in
    
    (* Helper to recursively format a tree node with indentation *)
    let rec format_tree indent (t : tree dlist) =
      if t.e.parent = None then 
        Buffer.add_string buf (Printf.sprintf "%s• Node(val: %s, deg: %d, mark: %b)\n" 
          indent (Elem.to_string t.e.e) t.e.degree t.e.mark)
      else
        Buffer.add_string buf (Printf.sprintf "%s├── Node(val: %s, deg: %d, mark: %b)\n" 
          indent (Elem.to_string t.e.e) t.e.degree t.e.mark);
          
      (* Recurse through children if any exist *)
      let child_sentinel = t.e.children in
      let rec format_children curr =
        if curr == child_sentinel then ()
        else begin
          format_tree (indent ^ "    ") curr;
          format_children curr.next
        end
      in
      format_children child_sentinel.next
    in

    (* Format the root list *)
    Buffer.add_string buf (Printf.sprintf "--- Fibonacci Heap (Size: %d) ---\n" h.size);
    (match h.min_tree with
     | None -> Buffer.add_string buf "Min pointer: None\n"
     | Some m -> Buffer.add_string buf (Printf.sprintf "Min pointer points to: %s\n" (Elem.to_string m.e.e)));
    
    Buffer.add_string buf "Root List:\n";
    let rec format_roots curr =
      if curr == h.trees then ()
      else begin
        format_tree "  " curr;
        format_roots curr.next
      end
    in
    format_roots h.trees.next;
    Buffer.add_string buf "---------------------------------\n";
    Buffer.contents buf
end

module PairingHeapNoDecrease (Element : Ordered) : Heap with type Elem.t = Element.t = struct
  module Elem = Element

  let ( &< ) = Elem.lt
  let ( &<< ) = Elem.compare_eq

  type heap = {mutable root : Elem.t; mutable children : heap list; mutable size: int}

  let size h = h.size
  let empty () = {root = Obj.magic (); children = []; size = 0}
  let is_empty h = h.size = 0

  let merge h1 h2 =
    if h1.size = 0 then h2
    else if h2.size = 0 then h1
    else if h1.root &< h2.root then {root = h1.root; children = h2 :: h1.children; size = h1.size + h2.size}
    else if h2.root &< h1.root then {root = h2.root; children = h1 :: h2.children; size = h2.size + h1.size}
    else if h1.root &<< h2.root then {root = h1.root; children = h2 :: h1.children; size = h1.size + h2.size}
    else {root = h2.root; children = h1 :: h2.children; size = h2.size + h1.size}

  let insert e h =
    let h' = merge {root = e; children = []; size = 1} h in
    h.root <- h'.root;
    h.children <- h'.children;
    h.size <- h'.size

  let rec merge_pairs = function
    | [] -> empty ()
    | [x] -> x
    | h1 :: h2 :: t -> merge (merge h1 h2) (merge_pairs t)

  let extract h =
    if h.size = 0 then raise (Invalid_argument "Empty heap")
    else begin
      let min = h.root in
      let h' = merge_pairs h.children in
      h.root <- h'.root;
      h.children <- h'.children;
      h.size <- h'.size;
      min
      end

  let change_priority _ = failwith ""
  let prio_insert _ = failwith ""

  let to_string _ = ""
end

module Queue (Element : Ordered) : Heap with type Elem.t = Element.t = FibonacciHeap(Element)
