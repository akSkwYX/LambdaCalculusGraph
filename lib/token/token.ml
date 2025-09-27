module type Token = sig
  type lexeme = Var of string | Lambda | LParen | RParen | Dot | Coma | N of int
  type t = {lexeme : lexeme; index : int}

  val index : t -> int
  val lexeme : t -> lexeme

  val eq : t -> t -> bool

  val to_string : t -> string
  val to_ugly_string : t -> string
  val of_string : string -> t
  val list_of_string : string -> t list
end

module Token = struct
  type lexeme = 
    Var of string | Lambda | LParen | RParen | Dot 
    | Coma | N of int (* Syntactic sugar *)
    | Number of int | Succ | Plus | Time (* Defined term *)
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
    | Coma, Coma -> true
    | N n1, N n2 -> n1 = n2
    | Number n1, Number n2 -> n1 = n2
    | Succ, Succ -> true
    | Plus, Plus -> true
    | Time, Time -> true
    | _ -> false

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

  let of_string s =
    if String.length s = 1 then
      match s.[0] with
      | 'L' -> { lexeme = Lambda; index = (-1) }
      | '(' -> { lexeme = LParen; index = (-1) }
      | ')' -> { lexeme = RParen; index = (-1) }
      | '.' -> { lexeme = Dot; index = (-1) }
      | ',' -> { lexeme = Coma; index = (-1) }
      | c when Char.code c >= 48 && Char.code c <= 57 ->
        { lexeme = N (Char.code c - 48); index = (-1) }
      | c when Char.code c >= 97 && Char.code c <= 122 ->
        { lexeme = Var (String.make 1 c); index = (-1) }
      | _ -> raise (Invalid_argument ("Invalid token string : " ^ s))
    else if String.length s >= 3 && s.[0] = '[' && s.[String.length s - 1] = ']' then
      let inner = String.sub s 1 (String.length s - 2) in
      match inner with
      | "S" -> { lexeme = Succ; index = (-1) }
      | "+" -> { lexeme = Plus; index = (-1) }
      | "*" -> { lexeme = Time; index = (-1) }
      | _ -> (try let n = int_of_string inner in { lexeme = Number n; index = (-1) }
              with Failure _ ->
                raise (Invalid_argument ("Invalid token string : " ^ s)) )
    else
      raise (Invalid_argument ("Invalid token string : " ^ s))

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
      | ' ' | '\n' -> lex_lambda k (current + 1)
      | '[' ->
        begin
        if current + 2 >= len then raise (Lexing_error "Unmatched '[' at end of input") else
        match s.[ current + 1 ] with
        | 'S' | '+' | '*' as t ->
          if s.[current + 2] = ']' then
            let lexeme = match t with
              | 'S' -> Succ
              | '+' -> Plus
              | '*' -> Time
              | _ -> failwith "Unreachable"
            in
            lex_lambda (fun x -> k ({ lexeme = lexeme; index = current } :: x)) (current + 3)
          else
            raise (Lexing_error ("Unmatched '[' at position " ^ string_of_int current))
        | c when Char.code c >= 48 && Char.code c <= 57 -> 
          let (n, next_pos) = find_n s len (Char.code c - 48) (current + 2) in
          if next_pos < len && s.[ next_pos ] = ']' then
            lex_lambda (fun x -> k ({ lexeme = Number n; index = current } :: x)) (next_pos + 1)
          else
            raise (Lexing_error ("Unmatched '[' at position " ^ string_of_int current))
        | _ -> raise (Lexing_error ("[ " ^ (string_of_int current) ^ " ] Unexpected character after '[' : " ^ (String.make 1 s.[ current + 1 ])))
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
end
