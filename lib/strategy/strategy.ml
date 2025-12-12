(* Reduction + Reindexaction en meme temps ? *)
(* Transform every redex into dumb ones and derivate the term *)

open Graph

let ( #~ ) = Fun.compose

module type Strategy = sig
  module Lt : LambdaTerm.LambdaTerm

  val name : string

  val is_normal : Lt.t -> bool
  val distance : ?acc:int -> ?res_list:Lt.t list -> Lt.t -> int
  val reduce : Lt.t -> Lt.t
  val reduce_safer : Lt.t -> Lt.t
  val reduce_step : Lt.t -> Lt.t
  val reduce_graph : string -> (int list, int) Hashtbl.t * Graph.t * int
end

module type NoStrategy = sig
  module Lt : LambdaTerm.LambdaTerm
  module S : Strategy

  val is_normal : Lt.t -> bool
  val reduce_step : Lt.t -> Lt.t list
  val reduce_graph : string -> (int list, int) Hashtbl.t * Graph.t
  val astar : bool -> string -> (int list, int * int * int list) Hashtbl.t * Graph.t * Lt.t list * int
  val idastar : string -> Lt.t list * int
end

module type BaseStrategy = sig
  module Lt : LambdaTerm.LambdaTerm

  val substitute : string -> Lt.t -> Lt.t -> Lt.t
end

module type PartialStrategy = sig
  include BaseStrategy

  val name : string

  val is_normal : Lt.t -> bool
  val reduce_step : Lt.t -> Lt.t
end

module BaseStrategy (Lt : LambdaTerm.LambdaTerm) : BaseStrategy = struct
  module Lt = Lt

  let rec free_vars = function
    | Lt.Var v -> [v]
    | Lt.Fun (v, body) -> List.filter ((<>) v) (free_vars body)
    | Lt.App (t1,t2) -> free_vars t1 @ free_vars t2

  let rec bound_vars = function
    | Lt.Var _ -> []
    | Lt.Fun (v, body) -> v :: (bound_vars body)
    | Lt.App (t1,t2) -> bound_vars t1 @ bound_vars t2

  let find_fresh_var used_vars =
    let rec aux n =
      let candidate = "x" ^ string_of_int n in
      if List.exists ((=) candidate) used_vars then aux (n+1)
      else candidate
    in
    aux 0

  let rec substitute (x : string) (with_v : Lt.t) (in_u : Lt.t) =
    let free_in_v = free_vars with_v in
    match in_u with 
    | Lt.Var v when v = x -> with_v
    | Lt.Var _ -> in_u
    | Lt.Fun (v, _) when v = x -> in_u (* do not substitute bound variables *)
    | Lt.Fun (v, body) when List.exists ((=) v) free_in_v ->
      let used_vars = (free_in_v @ (free_vars body) @ (bound_vars body)) in
      let fresh_v = find_fresh_var used_vars in
      let renamed_body = substitute v (Var fresh_v) body in
      Lt.Fun (fresh_v, substitute x with_v renamed_body)
    | Lt.Fun (v, body) -> Lt.Fun (v, substitute x with_v body)
    | Lt.App (t1,t2) -> App (substitute x with_v t1, substitute x with_v t2)
end

module FStrategy (Ps : PartialStrategy) : Strategy = struct
  module Lt = Ps.Lt

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

  let reduce_graph s =
    let term = Lt.of_string s in
    let term_db = Lt.deBruijn_index term in
    let graph = ref Graph.empty in
    let (node_map : (int list, int) Hashtbl.t) = Hashtbl.create 10 in
    let () = Hashtbl.add node_map term_db 0;
             graph := Graph.add_node !graph |> fst in
    let node_id = ref 1 in
    let rec aux t =
      let t_db = Lt.deBruijn_index t in
      let next_t = Ps.reduce_step t in
      let next_t_db = Lt.deBruijn_index next_t in
      if Hashtbl.mem node_map next_t_db then 
        (Graph.add_edge (Hashtbl.find node_map t_db) (Hashtbl.find node_map next_t_db) !graph; 
        !graph)
      else
        let () = Hashtbl.add node_map next_t_db !node_id;
                 graph := Graph.force_add_edge (Hashtbl.find node_map t_db) !node_id !graph;
                 node_id := !node_id + 1
        in
        aux next_t
    in
    let g = aux term in
    node_map, g, !node_id - 1
end

module PartialLeftInnermostStrategy : PartialStrategy = struct
  include BaseStrategy(LambdaTerm.LambdaTerm)

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
        substitute v t2 body
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

module PartialWeakLeftInnermostStrategy : PartialStrategy = struct
  include BaseStrategy(LambdaTerm.LambdaTerm)

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
        substitute v t2 body
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

module PartialLeftOutermostStrategy : PartialStrategy = struct
  include BaseStrategy(LambdaTerm.LambdaTerm)

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
    | App (Fun (v, body),t2) -> 0, substitute v t2 body
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

module PartialWeakLeftOutermostStrategy : PartialStrategy = struct
  include BaseStrategy(LambdaTerm.LambdaTerm)

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
    | App (Fun (v, body),t2) -> 0, substitute v t2 body
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

module FNoStrategy (S : Strategy) :NoStrategy = struct
  include (BaseStrategy(LambdaTerm.LambdaTerm) : BaseStrategy)
  module S = S

  let rec is_normal = function
      Lt.Var _ -> true
    | Lt.Fun (_, body) -> is_normal body
    | Lt.App (Fun _,_) -> false
    | Lt.App (t1,t2) -> is_normal t1 && is_normal t2

  let rec reduce_step : Lt.t -> Lt.t list = function
    | Var _ -> []
    | Fun (v, body) -> List.map ((fun t -> Fun (v, t)) : Lt.t -> Lt.t) (reduce_step body)
    | App (Fun (v, body), t2) ->
      substitute v t2 body :: (List.map (fun t -> Lt.App (Fun (v, t), t2)) (reduce_step body)) @
      (List.map (fun t -> Lt.App (Fun (v, body), t)) (reduce_step t2))
    | App (t1, t2) ->
      (List.map (fun t -> Lt.App (t, t2)) (reduce_step t1)) @
      (List.map (fun t -> Lt.App (t1, t)) (reduce_step t2))


  let reduce_graph s =
    let term = Lt.of_deBruijn (Lt.deBruijn_index (Lt.of_string s)) in

    (* Structures initialisation *)
    let graph = ref Graph.empty in
    let (node_map : (int list, int) Hashtbl.t) = Hashtbl.create (List.length (Lt.deBruijn_index term)) in
    let () = Hashtbl.add node_map (Lt.deBruijn_index term) 0;
             graph := Graph.add_node !graph |> fst in
    let node_id = ref 1 in
    let found_normal_form = ref false in

    (* Calculation of max term *)
    let normal_term = S.reduce_safer (S.Lt.of_string s) in
    let normal_term_db = S.Lt.deBruijn_index (normal_term) in
    let () = print_endline (Lt.deBruijn_to_string normal_term_db) in

    (* Graph construction *)
    let one_step t =
      if is_normal t then []
      else if !found_normal_form then 
        let t_db = Lt.deBruijn_index t in 
        let node_t = Hashtbl.find node_map t_db in 
        let next_ts = reduce_step t in
        List.iter (fun next_t ->
          let next_t_db = Lt.deBruijn_index next_t in
          if Hashtbl.mem node_map next_t_db then
            Graph.add_edge node_t (Hashtbl.find node_map next_t_db) !graph
          else
            (graph := Graph.force_add_edge node_t !node_id !graph;
            Hashtbl.add node_map next_t_db !node_id;
            node_id := !node_id + 1)) next_ts; []
      else
        let t_db = Lt.deBruijn_index t in
        let node_t = Hashtbl.find node_map t_db in
        let next_ts = reduce_step t in
        List.fold_left (fun acc next_t ->
          let next_t_db = Lt.deBruijn_index next_t in
          (if List.equal (Int.equal) next_t_db normal_term_db then found_normal_form := true);
          if Hashtbl.mem node_map next_t_db then
            (Graph.add_edge node_t (Hashtbl.find node_map next_t_db) !graph;
            acc)
          else
            (graph := Graph.force_add_edge node_t !node_id !graph;
            Hashtbl.add node_map next_t_db !node_id;
            node_id := !node_id + 1;
            next_t :: acc)
        ) [] next_ts
    in

    let rec aux t_list =
      match t_list with
      | [] -> ()
      | _ ->
        let new_terms = List.fold_left (fun acc t -> one_step t @ acc) [] t_list in
        let new_terms = List.sort Lt.compare_length new_terms in
        aux new_terms
    in
    aux [term]; node_map, !graph


  (* Heuristiques *)
  (* Non admissible : Lx.x *)
  let [@warning "-32"] h_trivial = List.length

  (* Non admissible : (Lx.f)((Lx.x)(Lx.x)) *)
  let rec redex_count : Lt.t -> int = function
    | Var _ -> 0
    | Fun (_, body) -> redex_count body
    | App (Fun (_, body), t2) -> 1 + redex_count body + redex_count t2
    | App (t1, t2) -> redex_count t1 + redex_count t2
  let [@warning "-32"] h_redex_count = redex_count #~ Lt.of_deBruijn

  (* Non admissible ? (Preuve à faire) *)
  let rec redex_left_count : Lt.t -> int = function
    | Var _ -> 0
    | Fun (_, body) -> redex_left_count body
    | App (Fun (_, body), _) -> 1 + redex_left_count body
    | App (t1, _) -> redex_left_count t1
  let [@warning "-32"] h_redex_left_count = redex_left_count #~ Lt.of_deBruijn

  (* Non admissible *)
  let rec greedy_parallel : Lt.t -> int = function
    | Var _ -> 0
    | Fun (_, body) -> greedy_parallel body
    | App (Fun _, _) -> 1
    | App (t1, t2) -> greedy_parallel t1 + greedy_parallel t2
  let [@warning "-32"] h_greedy_parallel = greedy_parallel #~ Lt.of_deBruijn

  (* Non admissible : factoriel 1 *)
  let [@warning "-32"] h_leftoutermost = LeftOutermostStrategy.distance #~ LeftOutermostStrategy.Lt.of_deBruijn


  let astar construct_graph s =
    let h = h_redex_left_count in
    let term_db = Lt.deBruijn_index (Lt.of_string s) in
    let term = Lt.of_deBruijn term_db in
    let module Queue = Heap.Queue in
    
    (* Structures initialisation *)
    let graph = ref Graph.empty in
    let () = if construct_graph then graph := Graph.add_node !graph |> fst in
    let (node_map : (int list, int * int * int list) Hashtbl.t) = Hashtbl.create (List.length (Lt.deBruijn_index term)) in
    let () = Hashtbl.add node_map (Lt.deBruijn_index term) (0, 0, []) in
    let q = Queue.insert term_db 0 Queue.empty in
    let node_id = ref 1 in
    let found_normal_form = ref false in

    (* Calculation of normal term *)
    let normal_term = S.reduce_safer (S.Lt.of_string s) in
    let normal_term_db = S.Lt.deBruijn_index (normal_term) in
    let () = print_endline (Lt.deBruijn_to_string normal_term_db) in

    let one_step t q =
      if is_normal t then q
      else 
      let t_db = Lt.deBruijn_index t in
      let node_t, d_t, _ = Hashtbl.find node_map t_db in
      let next_ts = reduce_step t in
      if !found_normal_form then Queue.empty
      else
        (List.fold_left (fun q next_t ->
          let next_t_db = Lt.deBruijn_index next_t in
          (if List.equal (Int.equal) next_t_db normal_term_db then found_normal_form := true);
          if Hashtbl.mem node_map next_t_db then
            (let node_next_t, d_next_t, _ = Hashtbl.find node_map next_t_db in
            (if construct_graph then Graph.add_edge node_t node_next_t !graph);
            if d_t <= d_next_t then
              (Hashtbl.replace node_map next_t_db (node_next_t, d_t + 1, t_db);
              Queue.change_or_insert next_t_db (d_t + 1 + (h next_t_db)) q)
            else
              q)
          else
            (if construct_graph then (graph := Graph.force_add_edge node_t !node_id !graph);
            Hashtbl.add node_map next_t_db (!node_id, d_t+1, t_db);
            (if construct_graph then node_id := !node_id + 1);
            Queue.insert next_t_db (d_t + 1 + (h next_t_db)) q)
        ) q next_ts)
    in

    let rec aux q =
      if Queue.is_empty q then ()
      else
        let t_db, q = Queue.extract_min q in
        aux (one_step (Lt.of_deBruijn t_db) q)
    in
    let () = aux q in
    let (_, s, n) = Hashtbl.find node_map normal_term_db in
    let rec path = function
      | [] -> []
      | father -> 
        let (_,_,f) = Hashtbl.find node_map father in
        Lt.of_deBruijn father :: path f
    in
    node_map, !graph, (List.rev (Lt.of_deBruijn normal_term_db :: path n)), s


  let idastar s =
    let h = h_redex_count in
    let term_db = Lt.deBruijn_index (Lt.of_string s) in
    let found = ref false in
    
    let normal_term = S.reduce_safer (S.Lt.of_string s) in
    let normal_term_db = S.Lt.deBruijn_index (normal_term) in

    let rec dfs path bound : int list list * int =
      let current_cost = List.length path - 1 in
      let current_term_db = List.hd path in
      let cost = current_cost + h current_term_db in
      if cost > bound then (List.rev path, cost)
      else if List.equal ( = ) current_term_db normal_term_db then (found := true; (List.rev path, cost))
      else
        let min_bound = ref max_int in
        let min_path = ref [] in
        let next_terms = reduce_step (Lt.of_deBruijn current_term_db) in
        let alr_found = ref false in
        List.iter (fun next_t ->
          let next_t_db = Lt.deBruijn_index next_t in
          if not (List.mem next_t_db path) then
            let (t_path, t_bound) = dfs (next_t_db :: path) bound in
            if !found && not !alr_found then (alr_found := true; min_bound := t_bound; min_path := t_path)
            else if !found then ()
            else if t_bound < !min_bound then (min_bound := t_bound; min_path := t_path)
        ) next_terms;
        (!min_path, !min_bound)
    in

    let rec loop bound =
      let t_path, t_bound = dfs [term_db] bound in
      if !found then t_path, bound
      else if t_bound = max_int then [], max_int
      else loop t_bound
    in
    let (path, length) = loop (h term_db) in ((List.map (Lt.of_deBruijn) path), length)
end

module LONoStrategy = FNoStrategy(LeftOutermostStrategy)
module LINoStrategy = FNoStrategy(LeftInnermostStrategy)
module WLONoStrategy = FNoStrategy(WeakLeftOutermostStrategy)
module WLINoStrategy = FNoStrategy(WeakLeftInnermostStrategy)
