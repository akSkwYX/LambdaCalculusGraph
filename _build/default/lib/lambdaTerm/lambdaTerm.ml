open Token

let ( #~ ) = Fun.compose

module type LambdaTerm = sig
  module Token = Token

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

  (* Compositions *)
  val deBruijn_to_string : int list -> string
end

module Base = struct
  module Token = Token

  type t = Var of string | Fun of string * t | App of t * t

  exception Parsing_error of string

  let rec to_string_tree ?(indent=0) term =
    let indentation = String.make (indent * 2) ' ' in
    match term with
    | Var v -> indentation ^ "Var( " ^ v ^ " )\n"
    | Fun (v, body) ->
        indentation ^ "Fun(" ^ v ^ ")\n" ^
        (to_string_tree ~indent:(indent + 1) body)
    | App (t1, t2)->
        indentation ^ "App(\n" ^
        to_string_tree ~indent:(indent+1) t1 ^ "\n" ^
        to_string_tree ~indent:(indent+1) t2 ^ "\n" ^
        indentation ^ ")\n"

  let compress = function
    | Fun (n1, Fun (f1, Fun (x1, App (Var f2, App (App (Var n2, Var f3), Var x2)))))
      when n1 = n2 && f1 = f2 && f2 = f3 && x1 = x2 ->
      "[S]"
    | Fun (m1, Fun (n1, Fun (f1, Fun (x1, App (App (Var m2, Var f2), App (App (Var n2, Var f3), Var x2))))))
      when m1 = m2 && n1 = n2 && f1 = f2 && f2 = f3 && x1 = x2 -> 
      "[+]"
    | Fun (m1, Fun (n1, Fun (f1, App (Var m2, App (Var n2, Var f2)))))
      when m1 = m2 && n1 = n2 && f1 = f2 ->
      "[*]"
    | Fun (t1, Fun (_, Var t2))
      when t1 = t2 ->
      "[T]"
    | Fun (_, Fun (f1, Var f2))
      when f1 = f2 ->
      "[F]"
    | Fun (s1, App (Var s2, Fun (t1, Fun (_, Var t2))))
      when s1 = s2 && t1 = t2 ->
      "[P1]"
    | Fun (s1, App (Var s2, Fun (_, Fun (f1, Var f2))))
      when s1 = s2 && f1 = f2 ->
      "[P2]"
    | Fun (n1, Fun (f1, Fun (x1, App (App (App (Var n2, Fun (g1, Fun (h1, App (Var h2, App (Var g2, Var f2))))), Fun (_, Var x2)), Fun (u1, Var u2)))))
      when n1 = n2 && f1 = f2 && x1 = x2 && g1 = g2 && h1 = h2 && u1 = u2 -> 
      "[P]"
    | Fun (n1, App (Var n2, App (Fun (_, Fun (_, Fun (f1, Var f2))), Fun (t1, Fun (_, Var t2)))))
      when n1 = n2 && f1 = f2 && t1 = t2 ->
      "[IsZero]"
    | App (Fun (g1, Fun (h1, App (Var h2, App (Var g2, App (Var g3, Var h3))))), Fun (g1', Fun (h1', App (Var h2', App (Var g2', App (Var g3', Var h3'))))))
      when g1 = g2 && g2 = g3 && h1 = h2 && h2 = h3 && g1' = g2' && g2' = g3' && h1' = h2' && h2' = h3' ->
      "[Y]"
    | Fun (f, Fun (x, n)) ->
      let rec is_number k = function
        | Var v when v = x -> true, k
        | App (Var v, t) when v = f -> is_number (k+1) t
        | _ -> false, 0
      in
      let is_num, n = is_number 0 n in
      if is_num then "[" ^ string_of_int n ^ "]"
      else raise (Invalid_argument "Cannot compress term")
    | _ -> raise (Invalid_argument "Cannot compress term")

  let rec to_string t =
    try compress t with
    | Invalid_argument _ ->
      match t with
      | Var v -> v
      | Fun (v, body) -> "λ" ^ v ^ "." ^ to_string body
      | App (t1, t2) ->
        (match t1 with
        | Var v -> v
        | _ -> "(" ^ to_string t1 ^ ")") ^
        (match t2 with
        | Var v -> v
        | _ -> "(" ^ to_string t2 ^ ")")

  (* -1 : free var 
     n >= 0 : bound var with id n
     -2 : lambda
     -3 : left parenthesis
     -4 : right parenthesis *)

  let rec deBruijn_index ?(binders=[]) = function
    | Var v ->
      begin
      match List.find_index ((=) v) binders with
       | Some i -> [i]
       | None -> [-1]
      end
    | Fun (v, body) -> (-2) :: (deBruijn_index ~binders:(v :: binders) body)
    | App (t1, t2) -> 
      (-3) :: deBruijn_index ~binders:binders t1 @ (-4) :: (-3) :: deBruijn_index ~binders:binders t2 @ [(-4)]

  let rec drop_parens ?(acc=[]) ?(i=0) : int list -> int list * int list = function
    | [] -> List.rev acc, []
    | (-3) as tok :: t -> drop_parens ~acc:(tok :: acc) ~i:(i+1) t
    | (-4) as tok :: t ->
      if i = 0 then List.rev acc, t
      else drop_parens ~acc:(tok :: acc) ~i:(i-1) t
    | h :: t -> drop_parens ~acc:(h :: acc) ~i:i t

  let rec of_deBruijn ?(binders=0) = function
    | [] -> raise (Parsing_error "Unexpected end of de Bruijn index list")
    | [-1] -> Var "f"
    | -1 :: rest ->
      let t2 = of_deBruijn ~binders:binders rest in
      App (Var "f", t2)
    | [n] when n >= 0 -> Var ("b" ^ string_of_int (binders - n - 1))
    | n :: rest when n >= 0 ->
      let t2 = of_deBruijn ~binders:binders rest in
      App (Var ("b" ^ string_of_int n), t2)
    | -2 :: rest -> Fun ("b" ^ string_of_int binders, of_deBruijn ~binders:(binders + 1) rest)
    | -3 :: rest ->
      begin
      let t1, rest_after_t1 = drop_parens rest in
      let t1 = of_deBruijn ~binders:binders t1 in
      match rest_after_t1 with
      | [] -> t1
      | _ -> (let t2 = of_deBruijn ~binders:binders rest_after_t1 in
             App (t1, t2))
      end
    | -4 :: _ -> raise (Parsing_error "Unexpected closing parenthesis in de Bruijn index list")
    | _ -> raise (Parsing_error "Invalid de Bruijn index list")

  let eq t1 t2 =
    List.equal (=) (deBruijn_index t1) (deBruijn_index t2) 

  let length =
    Fun.compose List.length deBruijn_index

  let compare_length t1 t2 =
    length t1 - length t2

  let rec to_ugly_string term =
    match term with
    | Var v -> "Var \"" ^ v ^ "\""
    | Fun (v, body) -> "Fun (\"" ^ v ^ "\", " ^ to_ugly_string body ^ ")"
    | App (t1, t2) -> 
      "App (" ^ to_ugly_string t1 ^ ", " ^ to_ugly_string t2 ^ ")"

  let rec parse_params : Token.t list -> string list * Token.t list = function
    | [] -> raise (Parsing_error "Expected parameter at end of term")
    | {lexeme=Var v; _} :: {lexeme=Coma; _} :: t ->
      let params, body = parse_params t in
      v :: params, body
    | {lexeme=Var v; _} :: {lexeme=Dot; _} :: t -> [v], t
    | h :: _ -> raise (Parsing_error ("[ " ^ (string_of_int (Token.index h)) ^ " ] Unexpected token when parsing parameters") )

  let make_fun params body =
    let rec aux params =
      match params with
      | [] -> body
      | h :: t -> Fun(h, aux t)
    in
    match params with
    | [] -> raise (Parsing_error "No parameters found when constructing function")
    | h :: t -> (Fun (h, aux t))


  let deBruijn_to_string = to_string #~ of_deBruijn
end

module LambdaTerm : LambdaTerm = struct
  include Base

  (* let compose_lambda t (l : Token.t list) = *)
  (*   match l with *)
  (*   | {lexeme=N n; _} :: rest -> *)
  (*     let rec aux = function *)
  (*     | 0 -> Var "t" *)
  (*     | k -> App (t, aux (k-1)) *)
  (*     in *)
  (*     Fun ("t", aux n), rest *)
  (*   | _ -> t, l *)

  let extend (t : Token.t) =
    match t.lexeme with
    | Number n ->
      let rec aux = function
      | 0 -> Var "x"
      | k -> App (Var "f", aux (k-1))
      in
      Fun ("f", Fun ("x", aux n))
    | Succ -> 
      Fun ("n", Fun ("f", Fun ("x", App (Var "f", App (App (Var "n", Var "f"), Var "x")))))
    | Plus -> 
      Fun ("m", Fun ("n", Fun ("f", Fun ("x", App (App (Var "m", Var "f"), App (App (Var "n", Var "f"), Var "x"))))))
    | Time ->
      Fun ("m", Fun ("n", Fun ("f", App (Var "m", App (Var "n", Var "f")))))
    | True ->
      Fun ("t", Fun ("f", Var "t"))
    | False ->
      Fun ("t", Fun ("f", Var "f"))
    | P1 ->
      Fun ("s", App (Var "s", Fun ("t", Fun ("f", Var "t"))))
    | P2 ->
      Fun ("s", App (Var "s", Fun ("t", Fun ("f", Var "f"))))
    | P -> 
      Fun ("n", Fun ("f", Fun ("x", App (App (App (Var "n", Fun ("g", Fun ("h", App (Var "h", App (Var "g", Var "f"))))), Fun ("u", Var "x")), Fun ("u", Var "u")))))
    | IsZero ->
      Fun ("n", App (Var "n", App (Fun ("z", Fun ("t", Fun ("f", Var "f"))), Fun ("t", Fun ("f", Var "t")))))
    | Y ->
      App (Fun ("g", Fun ("h", App (Var "h", App (Var "g", App (Var "g", Var "h"))))), Fun ("g", Fun ("h", App (Var "h", App (Var "g", App (Var "g", Var "h"))))))
    | _ -> raise (Parsing_error ("[ " ^ (string_of_int (Token.index t)) ^ " ] Cannot extend token: " ^ (Token.to_string t)))

  let of_string s =
    
    let rec drop_parens ?(acc=[]) ?(i=0) : Token.t list -> Token.t list * Token.t list = function
      | [] -> raise (Parsing_error "Unmatched opening parenthesis")
      | {lexeme=LParen; _} as tok :: t -> drop_parens ~acc:(tok :: acc) ~i:(i+1) t
      | {lexeme=RParen; _} as tok :: t ->
        if i = 0 then List.rev acc, t
        else drop_parens ~acc:(tok :: acc) ~i:(i-1) t
      | h :: t -> drop_parens ~acc:(h :: acc) ~i:i t
    in

    let rec parse_couple ?(t1=[]) ?(t2=[]) ?(i=0) ?(b=false) 
      : Token.t list -> Token.t list * Token.t list * Token.t list = function
      | [] -> raise (Parsing_error "Unmatched opening angle bracket")
      | {lexeme=LAngle; _} as tok :: rest -> 
        if not b then parse_couple ~t1:(tok :: t1) ~t2:t2 ~i:(i+1) ~b:b rest
        else parse_couple ~t1:t1 ~t2:(tok :: t2) ~i:(i+1) ~b:b rest
      | {lexeme=RAngle; _} as tok :: rest ->
        if i = 0 then List.rev t1, List.rev t2, rest
        else if not b then parse_couple ~t1:(tok :: t1) ~t2:t2 ~i:(i-1) ~b:b rest
        else parse_couple ~t1:t1 ~t2:(tok :: t2) ~i:(i-1) ~b:b rest
      | {lexeme=Coma; _} as tok :: rest ->
        if not b && i = 0 then parse_couple ~t1:t1 ~t2:t2 ~i:i ~b:true rest
        else if not b then parse_couple ~t1:(tok :: t1) ~t2:t2 ~i:i ~b:b rest
        else parse_couple ~t1:t1 ~t2:(tok :: t2) ~i:i ~b:b rest
      | h :: rest ->
        if not b then parse_couple ~t1:(h :: t1) ~t2:t2 ~i:i ~b:b rest
        else parse_couple ~t1:t1 ~t2:(h :: t2) ~i:i ~b:b rest
    in

    let tokens = Token.list_of_string s in
    let rec parse : Token.t list -> t = function
    | [] -> raise (Parsing_error "Unexpected end of term") 
    | {lexeme=Var v; _} :: rest ->
      (match rest with
      | [] -> Var v
      | _ -> App (Var v, parse rest))
    | ({lexeme=Number _; _} as t) :: rest
    | ({lexeme=Succ; _} as t) :: rest
    | ({lexeme=Plus; _} as t) :: rest
    | ({lexeme=Time; _} as t) :: rest
    | ({lexeme=True; _} as t) :: rest
    | ({lexeme=False; _} as t) :: rest
    | ({lexeme=P1; _} as t) :: rest 
    | ({lexeme=P2; _} as t) :: rest
    | ({lexeme=P; _} as t) :: rest 
    | ({lexeme=IsZero; _} as t) :: rest
    | ({lexeme=Y; _} as t) :: rest ->
      (match rest with
      | [] -> extend t
      | _ -> App (extend t, parse rest))
    | {lexeme=Lambda; _} :: paramsxbody ->
      let params, body = parse_params paramsxbody in
      let body_term = parse body in
      make_fun params body_term
    | {lexeme=LParen; _} :: rest ->
      begin
      let t1, t1_rest = drop_parens rest in
      let t1 = parse t1 in
      match t1_rest with
      | [] -> t1
      | _ -> (let t2 = parse t1_rest in
              App (t1, t2))
      end
    | {lexeme=LAngle; _} :: rest ->
      let t1, t2, rest = parse_couple rest in
      let t1 = parse t1 in
      let t2 = parse t2 in
      (match rest with
      | [] -> Fun ("s", App (App (Var "s", t1), t2))
      | _ -> App (Fun ("s", App (App (Var "s", t1), t2)), parse rest))
    | {lexeme=RParen; _} :: _ -> raise (Parsing_error ("Unexpected closing parenthesis"))
    | {lexeme=RAngle; _} :: _ -> raise (Parsing_error ("Unexpected closing angle"))
    | t -> raise (Parsing_error ("Unexpected token when parsing term : " ^ (Token.to_string (List.hd t))))
    in
    parse tokens
end
