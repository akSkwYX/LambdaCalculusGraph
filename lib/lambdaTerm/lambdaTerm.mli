module type LambdaTerm = sig
  module Token = Token.Token

  type t = Var of Token.t | Fun of Token.t * t | App of t list | Empty

  exception Parsing_error of string

  val eq : t -> t -> bool
  val compare_length : t -> t -> int

  val deBruijn_index : t -> int list
  val of_deBruijn : int list -> t

  val to_string : t -> string
  val to_string_tree : ?indent : int -> t -> string
  val to_ugly_string : t -> string
  val of_string : string -> t

  (* Compositions *)
  val deBruijn_to_string : int list -> string
end

(* module Left : LambdaTerm *)
(* module Right : LambdaTerm *)
module LambdaTerm : LambdaTerm
