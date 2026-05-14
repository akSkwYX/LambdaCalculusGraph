(* Reduction + Reindexaction en meme temps ? *)
(* Transform every redex into dumb ones and derivate the term *)

open Graph

let ( #~ ) = Fun.compose

(* ----- Signatures ----- *)

module type Strategy = sig
  module Lt : LambdaTerm.LambdaTerm with type t = LambdaTerm.LambdaTerm.t
  module Graph : Graph

  val name : string

  val is_normal : Lt.t -> bool
  val distance : ?acc:int -> ?res_list:Lt.t list -> Lt.t -> int
  val reduce : Lt.t -> Lt.t
  val reduce_safer : Lt.t -> Lt.t
  val reduce_step : Lt.t -> Lt.t
  val reduce_graph : string -> (int list, int) Hashtbl.t * Graph.t * int
end

module type NoStrategy = sig
  module S : Strategy
  module Lt : LambdaTerm.LambdaTerm
  module Queue : Heap.Heap with type Elem.t = int * Lt.t
  module LHashtbl : Hashtbl.S with type key = Lt.t
  module Graph : Graph with type Elem.t = Lt.t

  val is_normal : Lt.t -> bool
  val reduce_step : Lt.t -> Lt.t list
  val reduce_graph : string -> int LHashtbl.t * Graph.t
  val astar : bool -> Lt.t -> (int * int * Lt.t) LHashtbl.t * Graph.t * Lt.t list * int
  val brack : int -> Lt.t -> LambdaTerm.LambdaBottomTerm.t
end

module type PartialStrategy = sig
  module Lt : LambdaTerm.LambdaTerm with type t = LambdaTerm.LambdaTerm.t

  val name : string

  val is_normal : Lt.t -> bool
  val reduce_step : Lt.t -> Lt.t
end

(* ----- Functors ----- *)

module FStrategy (Ps : PartialStrategy) : Strategy = struct
  module Lt = Ps.Lt
  module Graph = Graph(struct
    type t = Lt.t
    let eq = Lt.alpha_eq
    let to_string = Lt.to_string end)

  let name = Ps.name
  let is_normal = Ps.is_normal
  let reduce_step = Ps.reduce_step

  let rec distance ?(acc=0) ?(res_list=[]) t =
    if is_normal t then acc
    else
      let next_t = Ps.reduce_step t in
      if List.exists (fun x -> Lt.eq x next_t) res_list then
        max_int (* cycle detected, return infinity *) 
      else
        distance ~acc:(acc+1) ~res_list:(t :: res_list) next_t

  let rec reduce term =
    if is_normal term then term else reduce (Ps.reduce_step term)

  let reduce_safer term =
    let rec aux res_list t =
      if is_normal t then t
      else
        let next_t = Ps.reduce_step t in
        if List.exists (fun x -> Lt.eq x next_t) res_list then
          (print_endline "Found cycle while reducting term"; t) (* cycle detected, return current term *)
        else
          aux (t :: res_list) next_t
    in
    aux [] term

  let reduce_graph _ =
    (* let term = Lt.of_string s in *)
    (* let term_db = Lt.deBruijn_index term in *)
    (* let graph = Graph.empty in *)
    (* let (node_map : (int list, int) Hashtbl.t) = Hashtbl.create 10 in *)
    (* let () = Hashtbl.add node_map term_db 0; *)
    (*          Graph.add_node graph in *)
    (* let node_id = ref 1 in *)
    (* let rec aux t = *)
    (*   let t_db = Lt.deBruijn_index t in *)
    (*   let next_t = Ps.reduce_step t in *)
    (*   let next_t_db = Lt.deBruijn_index next_t in *)
    (*   if Hashtbl.mem node_map next_t_db then  *)
    (*     (Graph.add_edge (Hashtbl.find node_map t_db) (Hashtbl.find node_map next_t_db) graph;  *)
    (*     graph) *)
    (*   else *)
    (*     let () = Hashtbl.add node_map next_t_db !node_id; *)
    (*              Graph.force_add_edge (Hashtbl.find node_map t_db) !node_id graph; *)
    (*              node_id := !node_id + 1 *)
    (*     in *)
    (*     aux next_t *)
    (* in *)
    (* let g = aux term in *)
    (* node_map, g, !node_id - 1 *)
    failwith "Tests ongoing"
end

module PartialLeftInnermostStrategy : PartialStrategy = struct
  module Lt = LambdaTerm.LambdaTerm

  let name = "Li"

  let rec is_normal = function
      Lt.Var _ -> true
    | Lt.Fun (_, body) -> is_normal body
    | Lt.App (Fun _,_) -> false
    | Lt.App (t1,t2) -> is_normal t1 && is_normal t2

  let rec reduce_step t = 
    match t with
    | Lt.Var v -> Lt.Var v
    | Lt.Fun (v, body) -> 
      Lt.Fun (v, reduce_step body)
    | Lt.App (Lt.Fun (v, body), t2) ->
      if is_normal body && is_normal t2 then
        Lt.substitute v t2 body
      else if is_normal body then
        Lt.App (Lt.Fun (v, body), reduce_step t2)
      else
        Lt.App (Lt.Fun (v, reduce_step body), t2)
    | Lt.App (t1, t2) ->
      if is_normal t1 then
        Lt.App (t1, reduce_step t2)
      else
        Lt.App (reduce_step t1, t2)
end

module LeftInnermostStrategy : Strategy = FStrategy(PartialLeftInnermostStrategy)
let leftInnermostStrategy = (module LeftInnermostStrategy : Strategy)

module PartialWeakLeftInnermostStrategy : PartialStrategy = struct
  module Lt = LambdaTerm.LambdaTerm

  let name = "Wli"

  let rec is_normal = function
      Lt.Var _ -> true
    | Lt.Fun _ -> true
    | Lt.App (Fun _,_) -> false
    | Lt.App (t1,t2) -> is_normal t1 && is_normal t2

  let rec reduce_step t = 
    match t with
    | Lt.Var v -> Lt.Var v
    | Lt.Fun (v, body) -> Lt.Fun (v, body)
    | Lt.App (Lt.Fun (v, body), t2) ->
      if is_normal body && is_normal t2 then
        Lt.substitute v t2 body
      else if is_normal body then
        Lt.App (Lt.Fun (v, body), reduce_step t2)
      else
        Lt.App (Lt.Fun (v, reduce_step body), t2)
    | Lt.App (t1, t2) ->
      if is_normal t1 then
        Lt.App (t1, reduce_step t2)
      else
        Lt.App (reduce_step t1, t2)
end

module WeakLeftInnermostStrategy : Strategy = FStrategy(PartialWeakLeftInnermostStrategy)
let weakLeftInnermostStrategy = (module WeakLeftInnermostStrategy : Strategy)

module PartialLeftOutermostStrategy : PartialStrategy = struct
  module Lt = LambdaTerm.LambdaTerm

  let name = "Lo"

  let rec is_normal = function
      Lt.Var _ -> true
    | Lt.Fun (_, body) -> is_normal body
    | Lt.App (Fun _,_) -> false
    | Lt.App (t1,t2) -> is_normal t1 && is_normal t2

  let reduce_step =
    let rec aux : Lt.t -> int * Lt.t = function
      Var v -> -1, Var v
    | Fun (v, body) -> let lvl, res = aux body in (if lvl = -1 then lvl else lvl + 1), Fun (v, res)
    | App (Fun (v, body),t2) -> 0, Lt.substitute v t2 body
    | App (t1, t2) ->
      let lvl1, res1 = aux t1 in
      let lvl2, res2 = aux t2 in
      if lvl1 = -1 && lvl2 = -1 then -1, App (t1, t2)
      else if lvl2 = -1 || (lvl1 <> -1 && lvl1 <= lvl2) then lvl1 + 1, App (res1, t2)
      else lvl2 + 1, App (t1, res2)
    in
    snd #~ aux
end

module LeftOutermostStrategy : Strategy = FStrategy(PartialLeftOutermostStrategy)
let leftOutermostStrategy = (module LeftOutermostStrategy : Strategy)

module PartialWeakLeftOutermostStrategy : PartialStrategy = struct
  module Lt = LambdaTerm.LambdaTerm

  let name = "Wlo"

  let rec is_normal = function
      Lt.Var _ -> true
    | Lt.Fun _ -> true
    | Lt.App (Fun _,_) -> false
    | Lt.App (t1,t2) -> is_normal t1 && is_normal t2

  let reduce_step =
    let rec aux : Lt.t -> int * Lt.t = function
      Var v -> -1, Var v
    | Fun (v, body) -> -1, Fun (v, body)
    | App (Fun (v, body),t2) -> 0, Lt.substitute v t2 body
    | App (t1, t2) ->
      let lvl1, res1 = aux t1 in
      let lvl2, res2 = aux t2 in
      if lvl1 = -1 && lvl2 = -1 then -1, App (t1, t2)
      else if lvl2 = -1 || (lvl1 <> -1 && lvl1 <= lvl2) then lvl1 + 1, App (res1, t2)
      else lvl2 + 1, App (t1, res2)
    in
    snd #~ aux
end

module WeakLeftOutermostStrategy : Strategy = FStrategy(PartialWeakLeftOutermostStrategy)
let weakLeftOutermostStrategy = (module WeakLeftOutermostStrategy : Strategy)

module FNoStrategy (S : Strategy) :NoStrategy = struct
  module Lt = S.Lt
  module S = S
  module LHashtbl = Hashtbl.Make(struct
    type t = Lt.t
    let equal = Lt.alpha_eq
    let hash = Fun.compose Hashtbl.hash Lt.to_deBruijn end)
  module Graph = Graph(struct
    type t = Lt.t
    let eq = Lt.alpha_eq
    let to_string = Lt.to_string end)

  let rec is_normal = function
      Lt.Var _ -> true
    | Lt.Fun (_, body) -> is_normal body
    | Lt.App (Fun _,_) -> false
    | Lt.App (t1,t2) -> is_normal t1 && is_normal t2

  let rec reduce_step : Lt.t -> Lt.t list = function
    | Var _ -> []
    | Fun (v, body) -> List.map ((fun t -> Fun (v, t)) : Lt.t -> Lt.t) (reduce_step body)
    | App (Fun (v, body), t2) ->
      Lt.substitute v t2 body :: (List.map (fun t -> Lt.App (Fun (v, t), t2)) (reduce_step body)) @
      (List.map (fun t -> Lt.App (Fun (v, body), t)) (reduce_step t2))
    | App (t1, t2) ->
      (List.map (fun t -> Lt.App (t, t2)) (reduce_step t1)) @
      (List.map (fun t -> Lt.App (t1, t)) (reduce_step t2))


  let reduce_graph _ =
    failwith "Tests ongoing"

    (* let term = Lt.of_deBruijn (Lt.deBruijn_index (Lt.of_string s)) in *)
    (**)
    (* (* Structures initialisation *) *)
    (* let graph = ref Graph.empty in *)
    (* let (node_map : (int list, int) Hashtbl.t) = Hashtbl.create (List.length (Lt.deBruijn_index term)) in *)
    (* let () = Hashtbl.add node_map (Lt.deBruijn_index term) 0; *)
    (*          graph := Graph.add_node !graph |> fst in *)
    (* let node_id = ref 1 in *)
    (* let found_normal_form = ref false in *)
    (**)
    (* (* Calculation of max term *) *)
    (* let normal_term = S.reduce_safer (S.Lt.of_string s) in *)
    (* let normal_term_db = S.Lt.deBruijn_index (normal_term) in *)
    (* let () = print_endline (Lt.deBruijn_to_string normal_term_db) in *)
    (**)
    (* (* Graph construction *) *)
    (* let one_step t = *)
    (*   if is_normal t then [] *)
    (*   else if !found_normal_form then  *)
    (*     let t_db = Lt.deBruijn_index t in  *)
    (*     let node_t = Hashtbl.find node_map t_db in  *)
    (*     let next_ts = reduce_step t in *)
    (*     List.iter (fun next_t -> *)
    (*       let next_t_db = Lt.deBruijn_index next_t in *)
    (*       if Hashtbl.mem node_map next_t_db then *)
    (*         Graph.add_edge node_t (Hashtbl.find node_map next_t_db) !graph *)
    (*       else *)
    (*         (graph := Graph.force_add_edge node_t !node_id !graph; *)
    (*         Hashtbl.add node_map next_t_db !node_id; *)
    (*         node_id := !node_id + 1)) next_ts; [] *)
    (*   else *)
    (*     let t_db = Lt.deBruijn_index t in *)
    (*     let node_t = Hashtbl.find node_map t_db in *)
    (*     let next_ts = reduce_step t in *)
    (*     List.fold_left (fun acc next_t -> *)
    (*       let next_t_db = Lt.deBruijn_index next_t in *)
    (*       (if List.equal (Int.equal) next_t_db normal_term_db then found_normal_form := true); *)
    (*       if Hashtbl.mem node_map next_t_db then *)
    (*         (Graph.add_edge node_t (Hashtbl.find node_map next_t_db) !graph; *)
    (*         acc) *)
    (*       else *)
    (*         (graph := Graph.force_add_edge node_t !node_id !graph; *)
    (*         Hashtbl.add node_map next_t_db !node_id; *)
    (*         node_id := !node_id + 1; *)
    (*         next_t :: acc) *)
    (*     ) [] next_ts *)
    (* in *)
    (**)
    (* let rec aux t_list = *)
    (*   match t_list with *)
    (*   | [] -> () *)
    (*   | _ -> *)
    (*     let new_terms = List.fold_left (fun acc t -> one_step t @ acc) [] t_list in *)
    (*     let new_terms = List.sort Lt.compare_length new_terms in *)
    (*     aux new_terms *)
    (* in *)
    (* aux [term]; node_map, !graph *)


  (* Heuristiques *)
  (* Non admissible : Lx.x *)
  let [@warning "-32"] h_trivial = Lt.length

  (* Non admissible : (Lx.f)((Lx.x)(Lx.x)) *)
  let [@warning "-32"] rec h_redex_count : Lt.t -> int = function
    | Var _ -> 0
    | Fun (_, body) -> h_redex_count body
    | App (Fun (_, body), t2) -> 1 + h_redex_count body + h_redex_count t2
    | App (t1, t2) -> h_redex_count t1 + h_redex_count t2

  (* Non admissible ? (Preuve à faire) *)
  let [@warning "-32"] rec h_redex_left_count : Lt.t -> int = function
    | Var _ -> 0
    | Fun (_, body) -> h_redex_left_count body
    | App (Fun (_, body), _) -> 1 + h_redex_left_count body
    | App (t1, _) -> h_redex_left_count t1

  (* Non admissible *)
  let [@warning "-32"] rec h_greedy_parallel : Lt.t -> int = function
    | Var _ -> 0
    | Fun (_, body) -> h_greedy_parallel body
    | App (Fun _, _) -> 1
    | App (t1, t2) -> h_greedy_parallel t1 + h_greedy_parallel t2

  (* Non admissible : factoriel 1 *)
  let [@warning "-32"] h_leftoutermost = LeftOutermostStrategy.distance

  let file = open_in_bin "heuristic_data"
  let heuristic_hashtbl : int LHashtbl.t = Marshal.from_channel file
  let () = close_in file

  module Lbt = LambdaTerm.LambdaBottomTerm
  
  let rec sel i =
    if i = 1 then sel_1
    else if i = 2 then sel_2
    else sel_3
    (* else sel_4 *)

  and sel_1 _ = None

  and sel_2 : Lt.t -> int option = function
    | Var _ -> Some 0
    | Fun (x, body) -> if List.mem x (Lbt.free_vars @@ brack 2 body) then Some 1 else None
    | App _ -> None

  and sel_3 : Lt.t -> int option = function
    | Var _ -> Some 0
    | Fun (x, body) -> if List.mem x (Lbt.free_vars @@ brack 3 body) then Some 1
                       else (match sel_3 body with | Some x when x > 0 -> Some (x + 1) | _ -> Some 0)
    | App (t1, _) -> (match sel_3 t1 with 
      | Some 0 -> Some 0
      | Some x when x > 1 -> Some (x - 1) 
      | _ -> None)

  (* Not sure I understood what is lengthtail *)
  (* and sel_4 : Lt.t -> int option = function *)
  (*   | Var _ -> Some 0 *)
  (*   | Fun (x, body) -> if List.mem x (Lt.free_vars @@ brack 4 body) then Some 1 *)
  (*                      else (match sel_4 body with | None -> None | Some x when x > 0 -> Some (x+1) | _ -> Some 0) *)
  (*   | App (t1, t2) -> (match sel_4 t1 with  *)
  (*     | Some x when x <> 1 -> Some (x-1)  *)
  (*     | Some 1 ->  *)
  (*       (match sel_4 t2 with | Some sel_4_t2 -> *)
  (*         (match lengthtail t1 with *)
  (*         | Some x when sel_4_t2 > x -> Some (sel_4_t2 - x) *)
  (*         | _ -> None) *)
  (*       | _ -> None) *)
  (*     | _ -> None) *)
  (**)
  (* and lengthtail = () *)

  and brack i : Lt.t -> Lbt.t = function
    | Var x -> Var x
    | Fun (x, body) -> Fun (x, brack i body)
    | App (t1, t2) -> 
      match sel i t2 with 
      | Some 1 -> App (brack i t1, brack i t2) 
      | _ -> App (brack i t1, Bottom)

  let [@warning "-32"] h_spine_redex i t =
    let redex_list = Lt.redex_list t in
    let redex_list' = Lbt.redex_list (brack i t) in
    List.fold_left (fun acc r ->
      if List.exists (LambdaTerm.alpha_compare r) redex_list' then acc + 1 else acc
    ) 0 redex_list

  let [@warning "-32"] h_preload t =
    try LHashtbl.find heuristic_hashtbl t
    with Not_found -> h_spine_redex 3 t

  module Queue = Heap.Queue (struct 
    type t = int * Lt.t
    let eq (_, t1) (_, t2) = Lt.alpha_eq t1 t2
    let lt (i, _) (j, _) = i < j
    let to_string (i, t) = "(" ^ string_of_int i ^ ", " ^ (Lt.to_string t) ^ ")"
    let compare_eq (_, t1) (_, t2) = h_trivial t1 < h_trivial t2 end)

  let astar construct_graph term =
    let h : Lt.t -> int = h_leftoutermost in
    
    (* Structures initialisation *)
    let graph = ref Graph.empty in
    let () = if construct_graph then Graph.add_node !graph in
    (*Hashtbl represent : 
      -key : term,
      -value : node_id, distance of astar algorithm, predecessor*)
    let (node_map : (int * int * Lt.t) LHashtbl.t) = LHashtbl.create 10000 in
    let () = LHashtbl.add node_map term (0, 0, Var "-1") in
    let q = Queue.empty in
    let mutex_queue = Mutex.create () in
    let () = Queue.insert (0, term) q in
    let node_id = ref 1 in
    let found_normal_form = ref false in

    let limited = false in
    let max_step = 10000 in
    let step = ref 1 in

    (* Calculation of normal term *)
    let normal_term = S.reduce_safer term in
    let () = print_endline (Lt.to_string normal_term) in

    let one_step t =
      if limited && !step >= max_step then ()
      else if is_normal t then ()
      else if !found_normal_form then ()
      else begin 
        let node_t, d_t, _ = LHashtbl.find node_map t in
        let next_ts = reduce_step t in
        List.iter (fun next_t ->
          if Lt.alpha_eq next_t normal_term then found_normal_form := true;
          Mutex.lock mutex_queue;
          if LHashtbl.mem node_map next_t then
            (let node_next_t, d_next_t, _ = LHashtbl.find node_map next_t in
            (if construct_graph then Graph.add_edge node_t node_next_t !graph);
            if d_t + 1 < d_next_t then
              (LHashtbl.replace node_map next_t (node_next_t, d_t + 1, t);
              Queue.prio_insert (0, next_t) (d_t + 1 + (h next_t), next_t) q))
          else
            (if construct_graph then Graph.force_add_edge node_t !node_id !graph;
            LHashtbl.add node_map next_t (!node_id, d_t+1, t);
            (if construct_graph then node_id := !node_id + 1);
            Queue.insert (d_t + 1 + (h next_t), next_t) q);
          incr step;
          Mutex.unlock mutex_queue
        ) next_ts
        end
    in
    let rec aux () =
      if Queue.is_empty q || !found_normal_form || (limited && !step >= max_step) then ()
      else
        let (_, t) = Mutex.lock mutex_queue; 
        (* print_string "Step : "; print_int !step; print_newline (); *)
        (* print_string (Queue.to_string q); *)
        Queue.extract q in
        Mutex.unlock mutex_queue; one_step t; aux ()
    in

    let thread_number = 16 in
    let domain_arr = Array.make thread_number (Domain.spawn aux) in
    for i = 0 to thread_number - 1 do
      Domain.join domain_arr.(i)
    done;

    try let (_, s, n) = LHashtbl.find node_map normal_term in
    let rec path = function
      | Lt.Var "-1" -> []
      | father -> 
        let (_,_,f) = LHashtbl.find node_map father in
        if Lt.alpha_eq father f then []
        else father :: path f
    in
    node_map, !graph, (List.rev (normal_term :: path n)), s with
    | Not_found -> node_map, !graph, [], -1
end

module LONoStrategy = FNoStrategy(LeftOutermostStrategy)
let loNoStrategy = (module LONoStrategy : NoStrategy)

module LINoStrategy = FNoStrategy(LeftInnermostStrategy)
let liNoStrategy = (module LINoStrategy : NoStrategy)

module WLONoStrategy = FNoStrategy(WeakLeftOutermostStrategy)
let wloNoStrategy = (module WLONoStrategy : NoStrategy)

module WLINoStrategy = FNoStrategy(WeakLeftInnermostStrategy)
let wliNoStrategy = (module WLINoStrategy : NoStrategy)
