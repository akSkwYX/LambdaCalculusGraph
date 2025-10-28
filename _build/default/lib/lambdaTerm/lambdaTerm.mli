module type LambdaTerm = sig
  module Token = Token.Token

  type t = Var of Token.t | Fun of Token.t * t | App of t list | Empty

  exception Parsing_error of string

  val eq : t -> t -> bool

  val deBruijn_index : t -> int list
  val deBruijn_string : t -> string
  val of_deBruijn : int list -> t
  val of_deBruijn_string : string -> t
  val deBruijn_to_string : int list -> string

  val to_string : t -> string
  val to_string_tree : ?indent : int -> t -> string
  val to_ugly_string : t -> string
  val to_format_string : t -> string
  val of_string : string -> t
end

(* module Left : LambdaTerm *)
(* module Right : LambdaTerm *)
module LambdaTerm : LambdaTerm
