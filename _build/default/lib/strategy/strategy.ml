open Graph

module type Strategy = sig
  module Lt : LambdaTerm.LambdaTerm

  val is_normal : Lt.t -> bool
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
end

module type BaseStrategy = sig
  module Lt : LambdaTerm.LambdaTerm

  val is_normal : Lt.t -> bool
  val substitute : Lt.t -> Lt.t -> Lt.t -> Lt.t
end

module type PartialStrategy = sig
  include BaseStrategy

  val reduce_step : Lt.t -> Lt.t
end

module BaseStrategy (Lt : LambdaTerm.LambdaTerm) : BaseStrategy = struct
  module Lt = Lt

  let rec is_normal = function
      Lt.Var _ -> true
    | Lt.Fun (_, body) -> is_normal body
    | Lt.App t_list -> 
      let rec aux = function
        | [] -> true
        | Lt.Fun _ :: _ :: _ -> false
        | h :: t -> is_normal h && aux t
      in
      aux t_list
    | Lt.Empty -> true

  let rec free_vars = function
    | Lt.Var v -> [v]
    | Lt.Fun (v, body) -> List.filter (fun x -> not (Lt.Token.eq x v)) (free_vars body)
    | Lt.App t_list -> List.flatten (List.map free_vars t_list)
    | Lt.Empty -> []

  let find_fresh_var used_vars =
    let rec aux n =
      let candidate = Lt.Token.create_var ("x" ^ string_of_int n) in
      if List.exists (Lt.Token.eq candidate) used_vars then aux (n+1)
      else candidate
    in
    aux 0

  let rec bound_vars = function
    | Lt.Var _ -> []
    | Lt.Fun (v, body) -> v :: (bound_vars body)
    | Lt.App t_list -> List.flatten (List.map bound_vars t_list)
    | Lt.Empty -> []

  let rec substitute (x : Lt.t) (with_v : Lt.t) (in_u : Lt.t) =
    let xv = match x with 
    | Lt.Var v -> v
    | _ -> raise (Invalid_argument "First argument must be a variable")
    in
    let free_in_v = free_vars with_v in
    match in_u with 
    | Lt.Var v when Lt.Token.eq v xv -> with_v
    | Lt.Var _ -> in_u
    | Lt.Fun (v, _) when Lt.Token.eq v xv -> in_u (* do not substitute bound variables *)
    | Lt.Fun (v, body) when List.exists (Lt.Token.eq v) free_in_v ->
      let used_vars = (free_in_v @ (free_vars body) @ (bound_vars body)) in
      let fresh_v = find_fresh_var used_vars in
      let renamed_body = substitute (Var v) (Var fresh_v) body in
      Lt.Fun (fresh_v, substitute x with_v renamed_body)
    | Lt.Fun (v, body) -> Lt.Fun (v, substitute x with_v body)
    | Lt.App t_list -> (match (List.map (substitute x with_v) t_list) with
                       | [t] -> t
                       | l -> Lt.App l)
    | Lt.Empty -> Lt.Empty
end

module FStrategy (Ps : PartialStrategy) : Strategy = struct
  module Lt = Ps.Lt

  let is_normal = Ps.is_normal

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

  let rec reduce_step = function
    | Lt.Var v -> Lt.Var v
    | Lt.Fun (v, body) -> Lt.Fun (v, reduce_step body)
    | Lt.App t_list ->
      begin
      let rec aux = function
        | [] -> raise (Invalid_argument "Got a term with an empty application")
        | [t] -> [reduce_step t]
        | Lt.Fun (v, body) :: u :: l ->
          if is_normal body then
            if is_normal u then
              (substitute (Lt.Var v) u body) :: l
            else
              Lt.Fun (v, body) :: (reduce_step u) :: l
          else
            Lt.Fun (v, reduce_step body) :: u :: l
        | t :: l ->
          if is_normal t then
            t :: aux l
          else
            reduce_step t :: l
      in
      match aux t_list with
      | [t] -> t
      | l -> Lt.App l
      end
    | Lt.Empty -> Lt.Empty
end

module LeftInnermostStrategy : Strategy = FStrategy(PartialLeftInnermostStrategy)

module PartialLeftExternalStrategy : PartialStrategy = struct
  include BaseStrategy(LambdaTerm.LambdaTerm)

  let rec reduce_step : Lt.t -> Lt.t = function
    | Empty -> Empty
    | Var v -> Var v
    | Fun (v, body) -> Fun (v, reduce_step body)
    | App t_list ->
      begin
      let rec aux has_found_redex = function
        | [] -> raise (Invalid_argument "Got a term with an empty application")
        | [t] -> false, [reduce_step t]
        | t1 :: t2 :: l ->
          match t1 with
          | Fun (v, body) -> true, substitute (Var v) t2 body :: l
          | _ -> let has_found_redex, result = aux has_found_redex (t2 :: l) in has_found_redex, t1 :: result
      in
      match aux false t_list with
      | _, [t] -> t
      | b, l when not b ->
        (let rec aux2 = function
          | [] -> []
          | h :: l ->
            (match reduce_step h with
            | t' when t' = h -> h :: aux2 l
            | t' -> t' :: l)
        in
        match aux2 l with
        | [t] -> t
        | l -> App l)
      | _, l -> App l
      end
end

module LeftExternalStrategy : Strategy = FStrategy(PartialLeftExternalStrategy)

module NoStrategy : NoStrategy = struct
  include (BaseStrategy(LambdaTerm.LambdaTerm) : BaseStrategy)

  let rec reduce_step : Lt.t -> Lt.t list = function
    | Empty -> []
    | Var _ -> []
    | Fun (v, body) -> List.map ((fun t -> Fun (v, t)) : Lt.t -> Lt.t) (reduce_step body)
    | App [t] -> reduce_step t
    | App t_list ->
      let rec aux found_first_apply before after =
        match after with
        | [] -> []
        | h :: t ->
          match h with
          | Lt.Fun (v, body) when not found_first_apply ->
            (match t with
            | [] -> aux false (before @ [h]) t
            | _ ->
              (match (before @ [substitute (Lt.Var v) (List.hd t) body] @ try List.tl t with Failure _ -> []) with
              | [t] -> t 
              | l  -> Lt.App l) :: aux true (before @ [h]) t)
          | _ ->
          if is_normal h then
            aux found_first_apply (before @ [h]) t
          else
            (List.map (fun term -> Lt.App (before @ [term] @ t)) (reduce_step h)) 
            @ aux found_first_apply (before @ [h]) t
      in
      aux false [] t_list

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
    (* Temporary solution *)
    let reduced_les_term_db = (-2) :: reduced_les_term_db @ [-3] in
    let () = print_endline (Lt.deBruijn_to_string reduced_les_term_db) in

    (* Graph construction *)
    let one_step t =
      if is_normal t then []
      else if !found_les_form then (print_endline "Found les form"; [])
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
end
