open Graph

module type Strategy = sig
  module Lt : LambdaTerm.LambdaTerm

  val is_normal : Lt.t -> bool
  val reduce : Lt.t -> Lt.t
  val reduce_safer : Lt.t -> Lt.t
  val reduce_string : string -> string
  val reduce_string_step_by_step : string -> string list
  val reduce_string_graph : string -> (string, int) Hashtbl.t * Graph.t
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
      List.for_all (function 
        | Lt.Fun _ -> false 
        | t -> is_normal t) t_list
    | Lt.Empty -> true

  let rec substitute (x : Lt.t) (with_v : Lt.t) (in_u : Lt.t) =
    let xv = match x with 
    | Lt.Var v -> v
    | _ -> raise (Invalid_argument "First argument must be a variable")
    in
    match in_u with 
    | Lt.Var v when Lt.Token.eq v xv -> with_v
    | Lt.Var _ -> in_u
    | Lt.Fun (v, _) when Lt.Token.eq v xv -> in_u (* do not substitute bound variables *)
    | Lt.Fun (v, body) -> Lt.Fun (v, substitute x with_v body)
    | Lt.App t_list -> Lt.App (List.map (substitute x with_v) t_list)
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
          t (* cycle detected, return current term *)
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
    let graph = ref Graph.empty in
    let (node_map : (string, int) Hashtbl.t) = Hashtbl.create 10 in
    let next_node_id = ref 1 in
    let rec aux t =
      if is_normal t then !graph
      else
        let next_t = Ps.reduce_step t in
        let next_t_str = Lt.to_string next_t in
        if Hashtbl.mem node_map next_t_str then 
          (Graph.add_edge (Hashtbl.find node_map (Lt.to_string t)) (Hashtbl.find node_map next_t_str) !graph; 
          !graph)
        else
          (let node_id = !next_node_id in
          next_node_id := !next_node_id + 1;
          Hashtbl.add node_map next_t_str node_id;
          graph := Graph.force_add_edge (Hashtbl.find node_map (Lt.to_string t)) node_id !graph;
          aux next_t)
    in
    (Hashtbl.add node_map (Lt.to_string term) 0;
    graph := Graph.add_node !graph |> fst;
    node_map, aux term)
end

module PartialLeftInnermostStrategy : PartialStrategy = struct
  include BaseStrategy(LambdaTerm.Left)

  let apply t1 t2 =
    match t1 with
    | Lt.Fun (v, body) -> substitute (Lt.Var v) t2 body
    | _ -> raise (Invalid_argument "Trying to apply a non-function")

  let rec reduce_step = function
    | Lt.Var v -> Lt.Var v
    | Lt.Fun (v, body) -> Lt.Fun (v, reduce_step body)
    | Lt.App [t1; t2] ->
      begin
      if not ( is_normal t1 ) then
        Lt.App [reduce_step t1; t2]
      else if not ( is_normal t2 ) then
        Lt.App [t1; reduce_step t2]
      else
        apply t1 t2
      end
    | Lt.App _ -> raise (Invalid_argument "Trying to use Left Innermost Strategy on a non parenthesized term")
    | Lt.Empty -> Lt.Empty
end

module LeftInnermostStrategy : Strategy = FStrategy(PartialLeftInnermostStrategy)

module PartialLeftExternalStrategy : PartialStrategy = struct
  include BaseStrategy(LambdaTerm.Right)

  let rec reduce_step : Lt.t -> Lt.t = function
    | Empty -> Empty
    | Var v -> Var v
    | Fun (v, body) -> Fun (v, reduce_step body)
    | App [t1; t2] ->
      (match t1 with
      | Fun (v, body) -> substitute (Var v) t2 body
      | Var v -> App [Var v; reduce_step t2]
      | App _ -> App [reduce_step t1; t2]
      | Empty -> t2)
    | App _ -> raise (Invalid_argument "Trying to use Left External Strategy on a non parenthesized term")
end

module LeftExternalStrategy : Strategy = FStrategy(PartialLeftExternalStrategy)

module NoStrategy : NoStrategy = struct
  include (BaseStrategy(LambdaTerm.NoParenthesis) : BaseStrategy)

  let rec list_without_last = function
    | [] -> []
    | [_] -> []
    | h :: t -> h :: (list_without_last t)

  let list_replace_i_and_next i e l =
    let rec aux k = function
      | [] -> []
      | [t] -> [t]
      | _ :: _ :: l when k = i -> e :: l
      | h :: t -> h :: (aux (k+1) t)
    in
    aux 0 l

  let list_replace_i i e l =
    let rec aux k = function
      | [] -> []
      | _ :: t when k = i -> e :: t
      | h :: t -> h :: (aux (k+1) t)
    in
    aux 0 l

  let rec reduce_step : Lt.t -> Lt.t list = function
    | Empty -> []
    | Var _ -> []
    | Fun (v, body) -> List.map ((fun t -> Fun (v, t)) : Lt.t -> Lt.t) (reduce_step body)
    | App [t] -> reduce_step t
    | App t_list ->
      let rec aux acc i = function
        | [] -> acc
        | [t] -> (List.map (fun t -> Lt.App (list_without_last t_list @ [t])) (reduce_step t)) @ acc
        | t1 :: t2 :: l ->
          let acc = match t1 with
          | Fun (v, body) ->
            let new_term = substitute (Var v) t2 body in
            let new_app = list_replace_i_and_next i new_term t_list in
            let doesnt_apply_reduced_t1 = reduce_step t1 in
            let acc = List.fold_left 
                (fun acc t -> ((Lt.App (list_replace_i i t t_list)) :: acc))
                acc doesnt_apply_reduced_t1 in
            (Lt.App new_app) :: acc
          | App _ ->
            let reduced_t1 = reduce_step t1 in
            List.fold_left (fun acc t -> ((Lt.App (list_replace_i i t t_list)) :: acc)) acc reduced_t1
          | _ ->
            acc
          in
          aux acc (i+1) (t2 :: l)
      in
      aux [] 0 t_list

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

    (* Graph construction *)
    let one_step t =
      if is_normal t then []
      else if !found_les_form then []
      else
        let t_db = Lt.deBruijn_index t in
        let node_t = Hashtbl.find node_map t_db in
        let next_ts = reduce_step t in
        List.fold_left (fun acc next_t ->
          let next_t_db = Lt.deBruijn_index next_t in
          if List.equal (Int.equal) next_t_db reduced_les_term_db then found_les_form := true;
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
        aux new_terms
    in
    aux [term]; node_map, !graph
end
