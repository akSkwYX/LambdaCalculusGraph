open Token

module type LambdaTerm = sig
  module Token = Token

  type t = Var of Token.t | Fun of Token.t * t | App of t * t | Empty

  exception Parsing_error of string

  val eq : t -> t -> bool

  val deBruijn_index : t -> int list
  val deBruijn_string : t -> string
  val of_deBruijn : int list -> t
  val of_deBruijn_string : string -> t

  val to_string : t -> string
  val to_string_tree : ?indent : int -> t -> string
  val to_ugly_string : t -> string
  val of_string : string -> t
end

module Base = struct
  module Token = Token

  type t = Var of Token.t | Fun of Token.t * t | App of t * t | Empty

  exception Parsing_error of string

  let deBruijn_index t = 
    let rec aux binders = function
      | Var v ->
        let idx = List.find_index (Token.eq v) binders in
        (match idx with
         | Some i -> [i+1]
         | None -> [0])
      | Fun (v, body) -> (-1) :: (aux (v :: binders) body)
      | App (t1, t2) -> (aux binders t1) @ (aux binders t2)
      | Empty -> []
    in
    aux [] t

  let deBruijn_string t =
    let indices = deBruijn_index t in
    String.concat " " (List.map (fun x -> if x=(-1) then "λ" else string_of_int x ) indices)

  let of_deBruijn indices =
    let rec aux binders = function
      | [] -> Empty
      | -1 :: t ->
        let (v : Token.t) = {lexeme=Var ("x" ^ string_of_int (List.length binders)); index = -1} in
        Fun (v, aux (v :: binders) t)
      | [n] when n > 0 ->
        let v = List.nth binders (n - 1) in
        Var v
      | [0] ->
        let (v : Token.t) = {lexeme=Var ("f" ^ string_of_int (List.length binders)); index = -1} in
        Var v
      | n :: t when n > 0 ->
        let v = List.nth binders (n - 1) in
        App (Var v, aux binders t)
      | 0 :: t ->
        let (v : Token.t) = {lexeme=Var ("f" ^ string_of_int (List.length binders)); index = -1} in
        App (Var v, aux binders t)
      | _ -> raise (Parsing_error "Negative index in de Bruijn indices")
    in
    aux [] indices

  let of_deBruijn_string s =
    let indices = String.split_on_char ' ' s in
    of_deBruijn (List.map (fun x -> if x = "λ" then -1 else int_of_string x) indices)

  let eq t1 t2 =
    List.equal (=) (deBruijn_index t1) (deBruijn_index t2) 

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

  let rec to_ugly_string term =
    match term with
    | Var v -> "Var " ^ Token.to_ugly_string v
    | Fun (v, body) -> "Fun (" ^ Token.to_ugly_string v ^ ", " ^ to_ugly_string body ^ ")"
    | App (t1, t2) -> "App (" ^ to_ugly_string t1 ^ ", " ^ to_ugly_string t2 ^ ")"
    | Empty -> "Empty"
end

module Left : LambdaTerm = struct
  include Base

  let parse_param l =
    let rec aux res (l : Token.t list) =
      match l with
      | [] -> raise (Parsing_error "Expected parameter at end of term")
      | ({lexeme=Var _; _} as v) :: {lexeme=Coma; _} :: t -> aux (v :: res) t
      | ({lexeme=Var _; _} as v) :: {lexeme=Dot; _} :: t -> v :: res, t
      | h :: _ -> raise (Parsing_error ("[ " ^ (string_of_int (Token.index h)) ^ " ] Unexpected token when parsing parameters") )
    in
    aux [] l

  let make_fun params body =
    let rec aux res params =
      match params with
      | [] -> res
      | h :: t -> aux (Fun (h, res)) t
    in
    match params with
    | [] -> raise (Parsing_error "No parameters found when constructing function")
    | h :: t -> aux (Fun (h, body)) t

  let compose_lambda_aux f n =
    let rec aux k n =
      if n <= 0 then k (Var {lexeme=Var "x"; index = (-1)}) else aux (fun x -> k (App(f, x))) (n - 1)
    in
    Fun ({lexeme=Var "x"; index = (-1)}, aux Fun.id n)

  let compose_lambda t (l : Token.t list) =
    match l with
    | {lexeme=N n; _} :: rest -> compose_lambda_aux t n, rest
    | _ -> t, l

  let extend (t : Token.t) =
    match t.lexeme with
    | Number n ->
      Fun ({lexeme=Var "f"; index = t.index},
            compose_lambda_aux (Var {lexeme=Var "f"; index = t.index}) n)
    | Succ -> 
      Fun ({ lexeme = Var "n"; index = Token.index t }, 
           Fun ({ lexeme = Var "f"; index = Token.index t }, 
                Fun ({ lexeme = Var "x"; index = Token.index t }, 
                     App (Var { lexeme = Var "f"; index = Token.index t }, 
                     App (App (Var { lexeme = Var "n"; index = Token.index t }, 
                               Var { lexeme = Var "f"; index = Token.index t }), 
                          Var { lexeme = Var "x"; index = Token.index t })))))
    | Plus ->
      Fun ({ lexeme = Var "m"; index = Token.index t },
           Fun ({ lexeme = Var "n"; index = Token.index t },
                Fun ({ lexeme = Var "f"; index = Token.index t }, 
                     Fun ({ lexeme = Var "x"; index = Token.index t }, 
                          App (App (Var { lexeme = Var "m"; index = Token.index t }, 
                                    Var { lexeme = Var "f"; index = Token.index t }), 
                               App (App (Var { lexeme = Var "n"; index = Token.index t }, 
                                         Var { lexeme = Var "f"; index = Token.index t }), 
                                    Var { lexeme = Var "x"; index = Token.index t }))))))
    | Time ->
      Fun ({ lexeme = Var "m"; index = Token.index t }, 
           Fun ({ lexeme = Var "n"; index = Token.index t }, 
                Fun ({ lexeme = Var "f"; index = Token.index t }, 
                     App (Var { lexeme = Var "m"; index = Token.index t }, 
                          App (Var { lexeme = Var "n"; index = Token.index t }, 
                               Var { lexeme = Var "f"; index = Token.index t })))))
    | _ -> raise (Parsing_error ("[ " ^ (string_of_int (Token.index t)) ^ " ] Cannot extend token: " ^ (Token.to_string t)))

  let of_string s =
    let tokens = Token.list_of_string s in
    let rec parse (res : t) (tokens : Token.t list) : t * Token.t list =
      match tokens with
      | [] -> res, []
      (* | [{lexeme=Var _; _} as v] -> ( *)
      (*   match res with *)
      (*   | Empty -> Var v, [] *)
      (*   | _ -> App (res, Var v), [] ) *)
      | {lexeme=Var _; _} as v :: rest -> (
        let vn, rest = compose_lambda (Var v) rest in
        match res with
        | Empty -> parse vn rest
        | _ -> parse (App (res, vn)) rest )
      | ({lexeme=Number _; _} as t) :: rest 
      | ({lexeme=Succ; _} as t) :: rest 
      | ({lexeme=Plus; _} as t) :: rest
      | ({lexeme=Time; _} as t) :: rest -> (
        let tn, rest = compose_lambda (extend t) rest in
        match res with
        | Empty -> parse tn rest
        | _ -> parse (App (res, tn)) rest )
      | {lexeme=Lambda; _} :: paramsxbody ->
        let params, body = parse_param paramsxbody in
        let (body_term, remaining) = parse Empty body in
        let f = make_fun params body_term in 
        let fn, remaining = compose_lambda f remaining in (
        match res with
        | Empty -> fn, remaining
        | _ -> App (res, fn), remaining )
      | {lexeme=LParen; _} :: rest ->
        let (subterm, remaining) = parse Empty rest in
        (match remaining with
        | {lexeme=RParen; _} :: rest_after_paren -> 
            let subtermn, rest_after_n = compose_lambda subterm rest_after_paren in
            (match res with
            | Empty -> parse subtermn rest_after_n
            | _ -> parse (App (res, subtermn)) rest_after_n)
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
