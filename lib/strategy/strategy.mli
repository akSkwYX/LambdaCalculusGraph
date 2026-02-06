module type Strategy = sig
  module Lt : LambdaTerm.LambdaTerm with type t = LambdaTerm.LambdaTerm.t
  module Graph : Graph.Graph

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
  module Graph : Graph.Graph with type Elem.t = Lt.t

  val is_normal : Lt.t -> bool
  val reduce_step : Lt.t -> Lt.t list
  val reduce_graph : string -> int LHashtbl.t * Graph.t
  val astar : bool -> Lt.t -> (int * int * Lt.t) LHashtbl.t * Graph.t * Lt.t list * int
  val idastar : string -> Lt.t list * int

  val brack : int -> Lt.t -> LambdaTerm.LambdaBottomTerm.t
end

module LeftInnermostStrategy : Strategy
module LeftOutermostStrategy : Strategy
module WeakLeftOutermostStrategy : Strategy
module WeakLeftInnermostStrategy : Strategy

module LONoStrategy : NoStrategy
module LINoStrategy : NoStrategy
module WLONoStrategy : NoStrategy
module WLINoStrategy : NoStrategy
