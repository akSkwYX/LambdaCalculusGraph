module type LambdaTerm = sig
  module Token = Token.Token

  type t = Var of string | Fun of string * t | App of t * t

  exception Parsing_error of string

  val eq : t -> t -> bool
  val compare_length : t -> t -> int

  val deBruijn_index : ?binders:string list -> t -> int list
  val of_deBruijn : ?binders:int -> int list -> t

  val to_string : t -> string
  val to_string_tree : ?indent : int -> t -> string
  val to_ugly_string : t -> string
  val of_string : string -> t

  val free_vars : t -> string list
  val bound_vars : t -> string list
  val substitute : string -> t -> t -> t

  val redex_list : t -> t list

  val deBruijn_to_string : int list -> string
end

module LambdaTerm : LambdaTerm
