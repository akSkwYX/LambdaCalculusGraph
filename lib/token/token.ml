module type Token = sig
  type lexeme = 
    Var of string | Lambda | LParen | RParen | Dot 
    | Coma | N of int
    | Number of int | Succ | Plus | Time | True | False | LAngle | RAngle
    | P1 | P2 | P
  and t = {lexeme : lexeme; index : int}

  val index : t -> int
  val lexeme : t -> lexeme

  val eq : t -> t -> bool

  val to_string : t -> string
  val to_ugly_string : t -> string
  val list_of_string : string -> t list
  val of_string : string -> t
  val create_var : string -> t
end

module Token = struct
  type lexeme = 
    Var of string | Lambda | LParen | RParen | Dot 
    | Coma | N of int
    | Number of int | Succ | Plus | Time | True | False | LAngle | RAngle
    | P1 | P2 | P
  and t = {lexeme : lexeme; index : int}

  exception Lexing_error of string

  let index t = t.index
  let lexeme t = t.lexeme

  let eq t1 t2 =
    t1.lexeme = t2.lexeme

  let to_string = function
    | { lexeme = Var v; index = _ } -> v
    | { lexeme = Lambda; index = _ } -> "λ"
    | { lexeme = LParen; index = _ } -> "("
    | { lexeme = RParen; index = _ } -> ")"
    | { lexeme = Dot; index = _ } -> "."
    | { lexeme = Coma; index = _ } -> ","
    | { lexeme = N n; index = _ } -> string_of_int n
    | { lexeme = Number n; index = _ } -> "[" ^ string_of_int n ^ "]"
    | { lexeme = Succ; index = _ } -> "[S]"
    | { lexeme = Plus; index = _ } -> "[+]"
    | { lexeme = Time; index = _ } -> "[*]"
    | { lexeme = True; index = _ } -> "[T]"
    | { lexeme = False; index = _ } -> "[F]"
    | { lexeme = LAngle; index = _ } -> "<"
    | { lexeme = RAngle; index = _ } -> ">"
    | { lexeme = P1; index = _ } -> "π1"
    | { lexeme = P2; index = _ } -> "π2"
    | { lexeme = P; index = _ } -> "[P]"

  let to_ugly_string = function
    | { lexeme = Var v; index = _ } -> "{ lexeme = Var \"" ^ v ^ "\"; index = Token.index t }"
    | { lexeme = Lambda; index = _ } -> "{ lexeme = Lambda; index = Token.index t }"
    | { lexeme = LParen; index = _ } -> "{ lexeme = LParen; index = Token.index t }"
    | { lexeme = RParen; index = _ } -> "{ lexeme = RParen; index = Token.index t }"
    | { lexeme = Dot; index = _ } -> "{ lexeme = Dot; index = Token.index t }"
    | { lexeme = Coma; index = _ } -> "{ lexeme = Coma; index = Token.index t }"
    | { lexeme = N n; index = _ } -> "{ lexeme = N " ^ string_of_int n ^ "; index = Token.index t }"
    | { lexeme = Number n; index = _ } -> "{ lexeme = Number " ^ string_of_int n ^ "; index = Token.index t }"
    | { lexeme = Succ; index = _ } -> "{ lexeme = Succ; index = Token.index t }"
    | { lexeme = Plus; index = _ } -> "{ lexeme = Plus; index = Token.index t }"
    | { lexeme = Time; index = _ } -> "{ lexeme = Time; index = Token.index t }"
    | { lexeme = True; index = _ } -> "{ lexeme = True; index = Token.index t }"
    | { lexeme = False; index = _ } -> "{ lexeme = False; index = Token.index t }"
    | { lexeme = LAngle; index = _ } -> "{ lexeme = LAngle; index = Token.index t }"
    | { lexeme = RAngle; index = _ } -> "{ lexeme = RAngle; index = Token.index t }"
    | { lexeme = P1; index = _ } -> "{ lexeme = P1; index = Token.index t }"
    | { lexeme = P2; index = _ } -> "{ lexeme = P2; index = Token.index t }"
    | { lexeme = P; index = _ } -> "{ lexeme = P; index = Token.index t }"

  let rec find_n s len acc pos =
    if pos < len then
      let nc = s.[ pos ] in
      if Char.code nc >= 48 && Char.code nc <= 57 then
        find_n s len (acc * 10 + (Char.code nc - 48)) (pos + 1)
      else
        (acc, pos)
    else
      (acc, pos)

  let list_of_string s =
    let len = String.length s in
    let rec lex_lambda k current =
      if current >= len then k [] else
      match s.[ current ] with
      | 'L' -> lex_lambda (fun x -> k ({ lexeme = Lambda; index = current } :: x)) (current + 1)
      | '(' -> lex_lambda (fun x -> k ({ lexeme = LParen; index = current } :: x)) (current + 1)
      | ')' -> lex_lambda (fun x -> k ({ lexeme = RParen; index = current } :: x)) (current + 1)
      | '.' -> lex_lambda (fun x -> k ({ lexeme = Dot; index = current } :: x)) (current + 1)
      | ',' -> lex_lambda (fun x -> k ({ lexeme = Coma; index = current } :: x)) (current + 1)
      | '<' -> lex_lambda (fun x -> k ({ lexeme = LAngle; index = current } :: x)) (current + 1)
      | '>' -> lex_lambda (fun x -> k ({ lexeme = RAngle; index = current } :: x)) (current + 1)
      | ' ' | '\n' -> lex_lambda k (current + 1)
      | '[' ->
        begin
        let index_closed = try String.index_from s (current + 1) ']' with
          | Not_found -> raise (Lexing_error ("Unmatched '[' at position " ^ string_of_int current))
        in
        let inner = String.sub s (current + 1) (index_closed - current - 1) in
        let lexeme = match inner with
        | "" -> raise (Lexing_error ("Empty brackets at position " ^ string_of_int current))
        | "S" -> Succ
        | "+" -> Plus
        | "*" -> Time
        | "T" -> True
        | "F" -> False
        | "P1" -> P1
        | "P2" -> P2
        | "P" -> P
        | _ ->
          let n = try int_of_string inner with
            | Failure _ -> raise (Lexing_error ("Invalid token in brackets at position " ^ string_of_int current))
          in Number n
        in
        lex_lambda (fun x -> k ({ lexeme = lexeme; index = current } :: x)) (index_closed + 1)
        end
      | c when Char.code c >= 48 && Char.code c <= 57 ->
        let (n, next_pos) = find_n s len (Char.code c - 48) (current + 1) in
        lex_lambda (fun x -> k ({ lexeme = N n; index = current } :: x)) next_pos
      | c when Char.code c >= Char.code 'a' && Char.code c <= Char.code 'z' ->
        lex_lambda (fun x -> k ({ lexeme = Var (String.make 1 c); index = current } :: x)) (current + 1)
      | c -> raise (Lexing_error ("[ " ^ (string_of_int current) ^ " ] Unexpected character : " ^ (String.make 1 c)))
    in
    try lex_lambda Fun.id 0 with
    | Lexing_error msg -> print_endline msg; exit 1

  let of_string s =
    match list_of_string s with
    | [t] -> t
    | [] -> raise (Lexing_error "Empty string")
    | _ -> raise (Lexing_error "Multiple tokens in string")

  let create_var v = { lexeme = Var v; index = -1 }
end
