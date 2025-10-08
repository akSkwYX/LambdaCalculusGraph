module type Strategy = sig
  module Lt : LambdaTerm.LambdaTerm

  val is_normal : Lt.t -> bool
  val reduce : Lt.t -> Lt.t
  val reduce_safer : Lt.t -> Lt.t
  val reduce_string : string -> string
  val reduce_string_step_by_step : string -> string list
  val reduce_string_graph : string -> (string, int) Hashtbl.t * Graph.Graph.t
end

module type NoStrategy = sig
  module Lt : LambdaTerm.LambdaTerm

  val is_normal : Lt.t -> bool
  val reduce_step : Lt.t -> Lt.t list
  val reduce_graph : string -> (int list, int) Hashtbl.t * Graph.Graph.t
end

module LeftInnermostStrategy : Strategy
module LeftExternalStrategy : Strategy
module NoStrategy : NoStrategy
