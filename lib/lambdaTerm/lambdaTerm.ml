open Token

module type LambdaTerm = sig
  module Token = Token

  type t = Var of Token.t | Fun of Token.t * t | App of t * t | Empty

  exception Parsing_error of string

  val eq : t -> t -> bool

  val to_string : t -> string
  val to_string_tree : ?indent : int -> t -> string
  val of_string : string -> t
end

module Base = struct
  module Token = Token

  type t = Var of Token.t | Fun of Token.t * t | App of t * t | Empty

  exception Parsing_error of string

  let rec eq t1 t2 =
    match (t1, t2) with
    | (Var v1, Var v2) -> Token.eq v1 v2
    | (Fun (v1, body1), Fun (v2, body2)) -> Token.eq v1 v2 && eq body1 body2
    | (App (t1a, t1b), App (t2a, t2b)) -> eq t1a t2a && eq t1b t2b
    | (Empty, Empty) -> true
    | _ -> false
  
  let rec to_string = function
      Var v -> Token.to_string v
    | Fun (v, body) -> "λ" ^ Token.to_string v ^ "." ^ to_string body
    | App (Var v1, Var v2) -> Token.to_string v1 ^ Token.to_string v2
    | App (Var v, t) -> Token.to_string v ^ "(" ^ to_string t ^ ")"
    | App (t, Var v) -> "(" ^ to_string t ^ ")" ^ Token.to_string v
    | App (t1, t2) -> "(" ^ to_string t1 ^ ")(" ^ to_string t2 ^ ")"
    | Empty -> ""

  let rec to_string_tree ?(indent=0) term =
    let indentation = String.make (indent * 2) ' ' in
    match term with
    | Var v -> indentation ^ "Var(" ^ Token.to_string v ^ ")\n"
    | Fun (v, body) ->
        indentation ^ "Fun(" ^ Token.to_string v ^ ")\n" ^
        (to_string_tree ~indent:(indent + 1) body)
    | App (t1, t2) ->
        indentation ^ "App(\n" ^
        (to_string_tree ~indent:(indent + 1) t1) ^
        (to_string_tree ~indent:(indent + 1) t2) ^
        indentation ^ ")\n"
    | Empty -> indentation ^ "Empty\n"
end

module Left : LambdaTerm = struct
  include Base

  let of_string s =
    let tokens = Token.list_of_string s in
    let rec parse (res : t) (tokens : Token.t list) : t * Token.t list =
      match tokens with
      | [] -> res, []
      | [{lexeme=Var _; _} as v] -> (
        match res with
        | Empty -> Var v, []
        | _ -> App (res, Var v), [] )
      | {lexeme=Var _; _} as v :: rest -> (
        match res with
        | Empty -> parse (Var v) rest
        | _ -> parse (App (res, Var v)) rest )
      | {lexeme=Lambda; _} :: ({lexeme=Var _; _} as v) :: {lexeme=Dot; _} :: body -> (
        let (body_term, remaining) = parse Empty body in
        match res with
        | Empty -> Fun (v, body_term), remaining
        | _ -> App (res, (Fun (v, body_term))), remaining )
      | {lexeme=LParen; _} :: rest ->
        let (subterm, remaining) = parse Empty rest in
        (match remaining with
        | {lexeme=RParen; _} :: rest_after_paren -> 
            (match res with
            | Empty -> parse subterm rest_after_paren
            | _ -> parse (App (res, subterm)) rest_after_paren)
        | [] -> raise (Parsing_error "Unclosed parenthesis at end of term")
        | t :: _ -> raise (Parsing_error ("[ " ^ (string_of_int (Token.index t)) ^ " ] 
                                          Expected closing parenthesis"))
        )
      | ({lexeme=RParen; _} :: _) as l -> res, l
      | t :: _ -> raise (Parsing_error ("[ " ^ (string_of_int (Token.index t)) ^ " ] 
                                        Unexpected token: " ^ (Token.to_string t)))
    in
    try parse Empty tokens |> fst with
    | Parsing_error msg -> print_endline msg; exit 1
end

module Right : LambdaTerm = struct
  include Base

  let of_string s =
    let tokens = Token.list_of_string s in
    let rec parse : Token.t list -> t * Token.t list = function
      | [] -> Empty, []
      | [{lexeme=Var _;_} as v] -> Var v, []
      | ({lexeme=Var _;_} as v) :: rest -> 
        begin
        let next, rest = parse rest in
        match next with
        | Empty -> Var v, rest
        | _ -> App(Var v, next), rest
        end
      | {lexeme = Lambda;_} :: ({lexeme=Var _;_} as v) :: {lexeme=Dot;_} :: body ->
        let body_term, rest = parse body in
        Fun (v, body_term), rest
      | {lexeme=LParen;_} :: rest ->
        begin
        let inner_term, inner_rest = parse rest in
        match inner_rest with
        | [] -> raise (Parsing_error "Unclosed parenthesis at end of term")
        | [{lexeme=RParen;_}] -> inner_term, []
        | {lexeme=RParen;_} :: rest -> let next, next_rest = parse rest in 
                                       App (inner_term, next), next_rest
        | t :: _ -> raise (Parsing_error ("[ " ^ (string_of_int (Token.index t)) ^ " ] Unclosed parenthesis"))
        end
      | ({lexeme=RParen;_} :: _) as l -> Empty, l
      | t :: _ -> raise (Parsing_error ("[ " ^ (string_of_int (Token.index t)) ^ " ] Unexpected token : "
                                        ^ (Token.to_string t)))
        
    in
    try parse tokens |> fst with
    | Parsing_error msg -> print_endline msg; exit 1
end
