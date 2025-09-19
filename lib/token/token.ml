module type Token = sig
  type lexeme = Var of string | Lambda | LParen | RParen | Dot
  type t = {lexeme : lexeme; index : int}

  val index : t -> int
  val lexeme : t -> lexeme

  val eq : t -> t -> bool

  val to_string : t -> string
  val of_string : string -> t
  val list_of_string : string -> t list
end

module Token = struct
  type lexeme = Var of string | Lambda | LParen | RParen | Dot
  type t = {lexeme : lexeme; index : int}

  exception Lexing_error of string

  let index t = t.index
  let lexeme t = t.lexeme

  let eq t1 t2 =
    match t1.lexeme, t2.lexeme with
    | Var v1, Var v2 -> v1 = v2
    | Lambda, Lambda -> true
    | LParen, LParen -> true
    | RParen, RParen -> true
    | Dot, Dot -> true
    | _ -> false

  let to_string = function
    | { lexeme = Var v; index = _ } -> v
    | { lexeme = Lambda; index = _ } -> "λ"
    | { lexeme = LParen; index = _ } -> "("
    | { lexeme = RParen; index = _ } -> ")"
    | { lexeme = Dot; index = _ } -> "."

  let of_string s =
    if String.length s <> 1 then
      raise (Invalid_argument ("Token string must be of length 1 : " ^ s))
    else
    match s.[0] with
    | 'L' -> { lexeme = Lambda; index = (-1) }
    | '(' -> { lexeme = LParen; index = (-1) }
    | ')' -> { lexeme = RParen; index = (-1) }
    | '.' -> { lexeme = Dot; index = (-1) }
    | c when Char.code c >= Char.code 'a' && Char.code c <= Char.code 'z' ->
      { lexeme = Var (String.make 1 c); index = (-1) }
    | _ -> raise (Invalid_argument ("Invalid token string : " ^ s))

  let list_of_string s =
    let len = String.length s in
    let rec lex_lambda k current =
      if current >= len then k [] else
      match s.[ current ] with
      | 'L' -> lex_lambda (fun x -> k ({ lexeme = Lambda; index = current } :: x)) (current + 1)
      | '(' -> lex_lambda (fun x -> k ({ lexeme = LParen; index = current } :: x)) (current + 1)
      | ')' -> lex_lambda (fun x -> k ({ lexeme = RParen; index = current } :: x)) (current + 1)
      | '.' -> lex_lambda (fun x -> k ({ lexeme = Dot; index = current } :: x)) (current + 1)
      | ' ' | '\n' -> lex_lambda k (current + 1)
      | c when Char.code c >= Char.code 'a' && Char.code c <= Char.code 'z' ->
        lex_lambda (fun x -> k ({ lexeme = Var (String.make 1 c); index = current } :: x)) (current + 1)
      | c -> raise (Lexing_error ("[ " ^ (string_of_int current) ^ " ] Unexpected character : " ^ (String.make 1 c)))
    in
    try lex_lambda Fun.id 0 with
    | Lexing_error msg -> print_endline msg; exit 1
end
