module type Strategy = sig
  module Lt : LambdaTerm.LambdaTerm

  val name : string

  val is_normal : Lt.t -> bool
  val distance : ?acc:int -> ?res_list:Lt.t list -> Lt.t -> int
  val reduce : Lt.t -> Lt.t
  val reduce_safer : Lt.t -> Lt.t
  val reduce_step : Lt.t -> Lt.t
  val reduce_graph : string -> (int list, int) Hashtbl.t * Graph.Graph.t * int
end

module type NoStrategy = sig
  module Lt : LambdaTerm.LambdaTerm
  module S : Strategy

  val is_normal : Lt.t -> bool
  val reduce_step : Lt.t -> Lt.t list

  val reduce_graph : string -> (int list, int) Hashtbl.t * Graph.Graph.t
  val astar : bool -> string -> (int list, int * int * int list) Hashtbl.t * Graph.Graph.t * Lt.t list * int
  val idastar : string -> Lt.t list * int
end

module LeftInnermostStrategy : Strategy
module LeftOutermostStrategy : Strategy
module WeakLeftOutermostStrategy : Strategy
module WeakLeftInnermostStrategy : Strategy

module LONoStrategy : NoStrategy
module LINoStrategy : NoStrategy
module WLONoStrategy : NoStrategy
module WLINoStrategy : NoStrategy
