module type LambdaTermAbstract = sig
  module Token = Token.Token

  type t

  exception Parsing_error of string

  val alpha_eq : t -> t -> bool
  val eq : t -> t -> bool

  val iter : (t -> unit) -> t -> unit
  val map : (t -> t) -> t -> t
  val fold_left : ('a -> t -> 'a) -> 'a -> t -> 'a
  val filter : (t -> bool) -> t -> t list

  val length : t -> int

  val free_vars : t -> string list
  val bound_vars : t -> string list
  val substitute : string -> t -> t -> t

  val redex_list : t -> t list

  val to_string : t -> string
  val to_string_tree : ?indent : int -> t -> string
  val to_ugly_string : t -> string

  val of_string : string -> t
  
  val to_deBruijn : ?binders : string list -> t -> int list
end

module type LambdaTerm = sig
  type t = Var of string | Fun of string * t | App of t * t

  include LambdaTermAbstract with type t := t
end

module type LambdaBottomTerm = sig
  type t = Var of string | Fun of string * t | App of t * t | Bottom

  include LambdaTermAbstract with type t := t
end

module LambdaTerm : LambdaTerm
module LambdaBottomTerm : LambdaBottomTerm

val alpha_compare : LambdaTerm.t -> LambdaBottomTerm.t -> bool
