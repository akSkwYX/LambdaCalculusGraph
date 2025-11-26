open Graph

let ( #~ ) = Fun.compose

module type Strategy = sig
  module Lt : LambdaTerm.LambdaTerm

  val is_normal : Lt.t -> bool
  val distance : ?acc:int -> Lt.t -> Lt.t -> int
  val reduce : Lt.t -> Lt.t
  val reduce_safer : Lt.t -> Lt.t
  val reduce_string : string -> string
  val reduce_string_step_by_step : string -> string list
  val reduce_string_graph : string -> (int list, int) Hashtbl.t * Graph.t
end

module type NoStrategy = sig
  module Lt : LambdaTerm.LambdaTerm

  val is_normal : Lt.t -> bool
  val reduce_step : Lt.t -> Lt.t list
  val reduce_graph : string -> (int list, int) Hashtbl.t * Graph.t
  val astar : string -> (int list, int * int) Hashtbl.t * Graph.t * int
end

module type BaseStrategy = sig
  module Lt : LambdaTerm.LambdaTerm

  val substitute : string -> Lt.t -> Lt.t -> Lt.t
end

module type PartialStrategy = sig
  include BaseStrategy

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

  let is_normal = Ps.is_normal

  let rec distance ?(acc=0) t1 t2 =
    if Lt.eq t1 t2 then acc
    else if is_normal t1 then max_int
    else distance ~acc:(acc + 1) (Ps.reduce_step t1) t2

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

  let reduce_string s =
    let term = Lt.of_string s in
    let reduced_term = reduce term in
    Lt.to_string reduced_term

  let reduce_string_step_by_step s =
    let term = Lt.of_string s in
    let rec aux t acc =
      if is_normal t then (t :: acc)
      else aux (Ps.reduce_step t) (t :: acc)
    in
    let steps = aux term [] in
    List.rev_map Lt.to_string steps

  let reduce_string_graph s =
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
    node_map, aux term
end

module PartialLeftInnermostStrategy : PartialStrategy = struct
  include BaseStrategy(LambdaTerm.LambdaTerm)

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

module PartialLeftExternalStrategy : PartialStrategy = struct
  include BaseStrategy(LambdaTerm.LambdaTerm)

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

module LeftExternalStrategy : Strategy = FStrategy(PartialLeftExternalStrategy)

module NoStrategy : NoStrategy = struct
  include (BaseStrategy(LambdaTerm.LambdaTerm) : BaseStrategy)

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
    let found_les_form = ref false in

    (* Calculation of max term *)
    let reduced_les_term = LeftExternalStrategy.reduce_safer (LeftExternalStrategy.Lt.of_string s) in
    let reduced_les_term_db = LeftExternalStrategy.Lt.deBruijn_index (reduced_les_term) in
    let () = print_endline (Lt.deBruijn_to_string reduced_les_term_db) in

    (* Graph construction *)
    let one_step t =
      if is_normal t then []
      else if !found_les_form then 
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
          (if List.equal (Int.equal) next_t_db reduced_les_term_db then found_les_form := true);
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


  let astar s =
    let h t = List.length t in
    let term_db = Lt.deBruijn_index (Lt.of_string s) in
    let term = Lt.of_deBruijn term_db in
    let module Queue = Heap.Queue in
    
    (* Structures initialisation *)
    let graph = ref Graph.empty in
    let (node_map : (int list, int * int) Hashtbl.t) = Hashtbl.create (List.length (Lt.deBruijn_index term)) in
    let () = Hashtbl.add node_map (Lt.deBruijn_index term) (0, 0);
             graph := Graph.add_node !graph |> fst in
    let q = Queue.insert term_db 0 Queue.empty in
    let node_id = ref 1 in
    let found_les_form = ref false in

    (* Calculation of normal term *)
    let reduced_les_term = LeftExternalStrategy.reduce_safer (LeftExternalStrategy.Lt.of_string s) in
    let reduced_les_term_db = LeftExternalStrategy.Lt.deBruijn_index (reduced_les_term) in
    let () = print_endline (Lt.deBruijn_to_string reduced_les_term_db) in

    let one_step t q =
      if is_normal t then q
      else 
      let t_db = Lt.deBruijn_index t in
      let node_t, d_t = Hashtbl.find node_map t_db in
      let next_ts = reduce_step t in
      if !found_les_form then
        (List.iter (fun next_t ->
          let next_t_db = Lt.deBruijn_index next_t in
          if Hashtbl.mem node_map next_t_db then
            Graph.add_edge node_t (fst (Hashtbl.find node_map next_t_db)) !graph
          else
            (graph := Graph.force_add_edge node_t !node_id !graph;
            Hashtbl.add node_map next_t_db (!node_id, d_t+1);
            node_id := !node_id + 1)
        ) next_ts; q)
      else
        (List.fold_left (fun q next_t ->
          let next_t_db = Lt.deBruijn_index next_t in
          (if List.equal (Int.equal) next_t_db reduced_les_term_db then found_les_form := true);
          if Hashtbl.mem node_map next_t_db then
            (let node_next_t, d_next_t = Hashtbl.find node_map next_t_db in
            Graph.add_edge node_t node_next_t !graph;
            if d_t <= d_next_t then
              (Hashtbl.replace node_map next_t_db (node_next_t, d_t);
              Queue.change_or_insert next_t_db (d_t + 1 + (h next_t_db)) q)
            else
              q)
          else
            (graph := Graph.force_add_edge node_t !node_id !graph;
            Hashtbl.add node_map next_t_db (!node_id, d_t+1);
            node_id := !node_id + 1;
            Queue.insert next_t_db (d_t + 1 + (h next_t_db)) q)
        ) q next_ts)
    in

    let rec aux q =
      if Queue.is_empty q then ()
      else
        let t_db, q = Queue.extract_min q in
        aux (one_step (Lt.of_deBruijn t_db) q)
    in
    aux q; node_map, !graph, (snd (Hashtbl.find node_map reduced_les_term_db))
end
