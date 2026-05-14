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

  val brack : int -> Lt.t -> LambdaTerm.LambdaBottomTerm.t
end

module LeftInnermostStrategy : Strategy
val leftInnermostStrategy : (module Strategy)
module LeftOutermostStrategy : Strategy
val leftOutermostStrategy : (module Strategy)
module WeakLeftOutermostStrategy : Strategy
val weakLeftOutermostStrategy : (module Strategy)
module WeakLeftInnermostStrategy : Strategy
val weakLeftInnermostStrategy : (module Strategy)

module LONoStrategy : NoStrategy
val loNoStrategy : (module NoStrategy)
module LINoStrategy : NoStrategy
val liNoStrategy : (module NoStrategy)
module WLONoStrategy : NoStrategy
val wloNoStrategy : (module NoStrategy)
module WLINoStrategy : NoStrategy
val wliNoStrategy : (module NoStrategy)
