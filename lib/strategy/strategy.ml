open Graph

module type Strategy = sig
  module Lt : LambdaTerm.LambdaTerm

  val is_normal : Lt.t -> bool
  val reduce : Lt.t -> Lt.t
  val reduce_string : string -> string
  val reduce_string_step_by_step : string -> string list
  val reduce_string_graph : string -> (string, int) Hashtbl.t * Graph.t
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
    | Lt.App (Lt.Fun _, _) -> false
    | Lt.App (t1, t2) -> is_normal t1 && is_normal t2
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
    | Lt.App (t1, t2) -> Lt.App (substitute x with_v t1, substitute x with_v t2)
    | Lt.Empty -> Lt.Empty
end

module FStrategy (Ps : PartialStrategy) : Strategy = struct
  module Lt = Ps.Lt

  let is_normal = Ps.is_normal

  let rec reduce term =
    if is_normal term then term else reduce (Ps.reduce_step term)

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
    | Lt.App (t1, t2) ->
      begin
      if not ( is_normal t1 ) then
        Lt.App (reduce_step t1, t2)
      else if not ( is_normal t2 ) then
        Lt.App (t1, reduce_step t2)
      else
        apply t1 t2
      end
    | Lt.Empty -> Lt.Empty
end

module LeftInnermostStrategy : Strategy = FStrategy(PartialLeftInnermostStrategy)

module PartialLeftExternalStrategy : PartialStrategy = struct
  include BaseStrategy(LambdaTerm.Right)

  (* (Lx.Ly.xy)(Lz.z)v -> (Ly.(Lz.z)y)v -> (Lz.z)v -> v *)
  let apply t1 t2 =
    match t1 with
    | Lt.Fun (v, body) -> substitute (Lt.Var v) t2 body
    | _ -> raise (Invalid_argument "Trying to apply a non-function")

  let rec reduce_step = function
    | Lt.Var v -> Lt.Var v
    | Lt.Fun (v, body) -> Lt.Fun (v, reduce_step body)
    | Lt.App (t1, t2) ->
      begin
      try apply t1 t2 with
      | Invalid_argument _ ->
        if not ( is_normal t1 ) then
          Lt.App (reduce_step t1, t2)
        else if not ( is_normal t2 ) then
          Lt.App (t1, reduce_step t2)
        else
          Lt.App (t1, t2)
      end
    | Lt.Empty -> Lt.Empty
end

module LeftExternalStrategy : Strategy = FStrategy(PartialLeftExternalStrategy)
