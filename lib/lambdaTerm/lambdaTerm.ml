open Token
let ( #~ ) = Fun.compose

module type LambdaTermAbstract = sig
  module Token = Token

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

module LambdaTerm : LambdaTerm = struct
  module Token = Token

  type t = Var of string | Fun of string * t | App of t * t
  exception Parsing_error of string

  let alpha_eq t1 t2 =
    let rec aux binders1 binders2 t1 t2 =
      match t1, t2 with
      | Var v1, Var v2 ->
        begin
        match List.find_index ((=) v1) binders1, List.find_index ((=) v2) binders2 with
        | Some i, Some j when i = j -> true
        | None, None -> true
        | _ -> false
        end
      | Fun (v1, body1), Fun (v2, body2) ->
        aux (v1 :: binders1) (v2 :: binders2) body1 body2
      | App (s1, s2), App (t1, t2) ->
        aux binders1 binders2 s1 t1 && aux binders1 binders2 s2 t2
      | _ -> false
    in
    aux [] [] t1 t2

  let [@warning "-32"] rec eq t1 t2 =
    match t1, t2 with
    | Var v1, Var v2 -> v1 = v2
    | Fun (v1, body1), Fun (v2, body2) -> v1 = v2 && eq body1 body2
    | App (s1, s2), App (t1, t2) -> eq s1 t1 && eq s2 t2
    | _ -> false

  let rec iter f term =
    match term with
    | Var _ -> f term
    | Fun (_, body) -> f term; iter f body
    | App (t1, t2) -> f term; iter f t1; iter f t2

  let rec map f term =
    match term with
    | Var _ -> f term
    | Fun (v, body) -> f (Fun (v, map f body))
    | App (t1, t2) -> f (App (map f t1, map f t2))

  let rec fold_left f acc term =
    match term with
    | Var _ -> f acc term
    | Fun (_, body) -> fold_left f (f acc term) body
    | App (t1, t2) -> fold_left f (fold_left f (f acc term) t1) t2

  let rec filter f term =
    match term with
    | Var _ -> if f term then [term] else []
    | Fun (_, body) ->
      let rest = filter f body in
      if f term then term :: rest else rest
    | App (t1, t2) ->
      let rest1 = filter f t1 in
      let rest2 = filter f t2 in
      if f term then term :: (rest1 @ rest2) else (rest1 @ rest2)

  let length = fold_left (fun acc _ -> acc + 1) 0

  let rec free_vars = function
    | Var v -> [v]
    | Fun (v, body) -> List.filter ((<>) v) (free_vars body)
    | App (t1,t2) -> free_vars t1 @ free_vars t2

  let rec bound_vars = function
    | Var _ -> []
    | Fun (v, body) -> v :: (bound_vars body)
    | App (t1,t2) -> bound_vars t1 @ bound_vars t2

  let find_fresh_var used_vars =
    let rec aux n =
      let candidate = "x" ^ string_of_int n in
      if List.exists ((=) candidate) used_vars then aux (n+1)
      else candidate
    in
    aux 0

  let rec substitute (x : string) (with_v : t) (in_u : t) =
    let free_in_v = free_vars with_v in
    match in_u with 
    | Var v when v = x -> with_v
    | Var _ -> in_u
    | Fun (v, _) when v = x -> in_u
    | Fun (v, body) when List.exists ((=) v) free_in_v ->
      let used_vars = (free_in_v @ (free_vars body) @ (bound_vars body)) in
      let fresh_v = find_fresh_var used_vars in
      let renamed_body = substitute v (Var fresh_v) body in
      Fun (fresh_v, substitute x with_v renamed_body)
    | Fun (v, body) -> Fun (v, substitute x with_v body)
    | App (t1,t2) -> App (substitute x with_v t1, substitute x with_v t2)

  let redex_list t =
    let rec aux res = function
    | Var _ -> res
    | Fun (_, body) -> aux res body
    | App (Fun(_, body), t2) as h -> aux (aux (if List.mem h res then res else h :: res) body) t2
    | App (t1, t2) -> aux (aux res t1) t2
    in
    aux [] t

  (* To Delete *)
  (* let rec redex_list = function *)
  (*   | Var _ -> [] *)
  (*   | Fun (_, body) -> redex_list body *)
  (*   | App (Fun(x, body), t2) -> App (Fun(x, body), t2) :: ((redex_list body) @ (redex_list t2)) *)
  (*   | App (t1, t2) -> redex_list t1 @ redex_list t2 *)

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

  let rec compress = function
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
    | Fun (x1, Var x2) when x1 = x2 ->
      "[I]"
    | Fun (f, Fun (x, n)) ->
      let rec is_number k = function
        | Var v when v = x -> true, k
        | App (Var v, t) when v = f -> is_number (k+1) t
        | _ -> false, 0
      in
      let is_num, n = is_number 0 n in
      if is_num then "[" ^ string_of_int n ^ "]"
      else raise (Invalid_argument "Cannot compress term")
    | Fun (z1, App (App (Var z2, u), v)) when z1 = z2 ->
      "<" ^ to_string u ^ "," ^ to_string v ^ ">"
    | _ -> raise (Invalid_argument "Cannot compress term")

  and to_string t =
    try compress t with
    | Invalid_argument _ ->
      match t with
      | Var v -> v
      | Fun (v, body) -> "λ" ^ v ^ "." ^ to_string body
      | App (t1, t2) ->
        (match t1 with
        | Var v -> v
        | _ -> (try compress t1 with | Invalid_argument _ -> "(" ^ to_string t1 ^ ")")) ^
        (match t2 with
        | Var v -> v
        | _ -> (try compress t2 with | Invalid_argument _ -> "(" ^ to_string t2 ^ ")"))

  (* -1 : free var 
     n >= 0 : bound var with id n
     -2 : lambda
     -3 : left parenthesis
     -4 : right parenthesis *)

  (* let rec drop_parens ?(acc=[]) ?(i=0) : int list -> int list * int list = function *)
  (*   | [] -> List.rev acc, [] *)
  (*   | (-3) as tok :: t -> drop_parens ~acc:(tok :: acc) ~i:(i+1) t *)
  (*   | (-4) as tok :: t -> *)
  (*     if i = 0 then List.rev acc, t *)
  (*     else drop_parens ~acc:(tok :: acc) ~i:(i-1) t *)
  (*   | h :: t -> drop_parens ~acc:(h :: acc) ~i:i t *)
  (**)
  (* let rec of_deBruijn ?(binders=0) = function *)
  (*   | [] -> raise (Parsing_error "Unexpected end of de Bruijn index list") *)
  (*   | [-1] -> Var "f" *)
  (*   | -1 :: rest -> *)
  (*     let t2 = of_deBruijn ~binders:binders rest in *)
  (*     App (Var "f", t2) *)
  (*   | [n] when n >= 0 -> Var ("b" ^ string_of_int (binders - n - 1)) *)
  (*   | n :: rest when n >= 0 -> *)
  (*     let t2 = of_deBruijn ~binders:binders rest in *)
  (*     App (Var ("b" ^ string_of_int n), t2) *)
  (*   | -2 :: rest -> Fun ("b" ^ string_of_int binders, of_deBruijn ~binders:(binders + 1) rest) *)
  (*   | -3 :: rest -> *)
  (*     begin *)
  (*     let t1, rest_after_t1 = drop_parens rest in *)
  (*     let t1 = of_deBruijn ~binders:binders t1 in *)
  (*     match rest_after_t1 with *)
  (*     | [] -> t1 *)
  (*     | _ -> (let t2 = of_deBruijn ~binders:binders rest_after_t1 in *)
  (*            App (t1, t2)) *)
  (*     end *)
  (*   | -4 :: _ -> raise (Parsing_error "Unexpected closing parenthesis in de Bruijn index list") *)
  (*   | _ -> raise (Parsing_error "Invalid de Bruijn index list") *)

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
      Fun ("n", App (App (Var "n", Fun ("z", Fun ("t", Fun ("f", Var "f")))), Fun ("t", Fun ("f", Var "t"))))
    | Y ->
      App (Fun ("g", Fun ("h", App (Var "h", App (Var "g", App (Var "g", Var "h"))))), Fun ("g", Fun ("h", App (Var "h", App (Var "g", App (Var "g", Var "h"))))))
    | I ->
      Fun ("x", Var "x")
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

    (* Ca marche probablement pas ça *)
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
    | ({lexeme=Y; _} as t) :: rest
    | ({lexeme=I; _} as t) :: rest ->
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

  let rec to_deBruijn ?(binders=[]) = function
    | Var v ->
      begin
      match List.find_index ((=) v) binders with
       | Some i -> [i]
       | None -> [-1]
      end
    | Fun (v, body) -> (-2) :: (to_deBruijn ~binders:(v :: binders) body)
    | App (t1, t2) -> 
      (-3) :: to_deBruijn ~binders:binders t1 @ (-4) :: (-3) :: to_deBruijn ~binders:binders t2 @ [(-4)]
end

module LambdaBottomTerm : LambdaBottomTerm = struct
  module Token = Token

  type t = Var of string | Fun of string * t | App of t * t | Bottom

  exception Parsing_error of string

  let alpha_eq t1 t2 =
    let rec aux binders1 binders2 t1 t2 =
      match t1, t2 with
      | Var v1, Var v2 ->
        begin
        match List.find_index ((=) v1) binders1, List.find_index ((=) v2) binders2 with
        | Some i, Some j when i = j -> true
        | None, None -> true
        | _ -> false
        end
      | Fun (v1, body1), Fun (v2, body2) ->
        aux (v1 :: binders1) (v2 :: binders2) body1 body2
      | App (s1, s2), App (t1, t2) ->
        aux binders1 binders2 s1 t1 && aux binders1 binders2 s2 t2
      | _ -> false
    in
    aux [] [] t1 t2

  let [@warning "-32"] rec eq t1 t2 =
    match t1, t2 with
    | Var v1, Var v2 -> v1 = v2
    | Fun (v1, body1), Fun (v2, body2) -> v1 = v2 && eq body1 body2
    | App (s1, s2), App (t1, t2) -> eq s1 t1 && eq s2 t2
    | Bottom, Bottom -> true
    | _ -> false

  let rec iter f term =
    match term with
    | Var _ -> f term
    | Bottom -> f term
    | Fun (_, body) -> f term; iter f body
    | App (t1, t2) -> f term; iter f t1; iter f t2

  let rec map f term =
    match term with
    | Var _ -> f term
    | Bottom -> f term
    | Fun (v, body) -> f (Fun (v, map f body))
    | App (t1, t2) -> f (App (map f t1, map f t2))

  let rec fold_left f acc term =
    match term with
    | Var _ -> f acc term
    | Bottom -> f acc term
    | Fun (_, body) -> fold_left f (f acc term) body
    | App (t1, t2) -> fold_left f (fold_left f (f acc term) t1) t2

  let rec filter f term =
    match term with
    | Var _ -> if f term then [term] else []
    | Bottom -> if f term then [term] else []
    | Fun (_, body) ->
      let rest = filter f body in
      if f term then term :: rest else rest
    | App (t1, t2) ->
      let rest1 = filter f t1 in
      let rest2 = filter f t2 in
      if f term then term :: (rest1 @ rest2) else (rest1 @ rest2)

  let length = fold_left (fun acc _ -> acc + 1) 0

  let rec free_vars = function
    | Var v -> [v]
    | Bottom -> []
    | Fun (v, body) -> List.filter ((<>) v) (free_vars body)
    | App (t1,t2) -> free_vars t1 @ free_vars t2

  let rec bound_vars = function
    | Var _ -> []
    | Bottom -> []
    | Fun (v, body) -> v :: (bound_vars body)
    | App (t1,t2) -> bound_vars t1 @ bound_vars t2

  let find_fresh_var used_vars =
    let rec aux n =
      let candidate = "x" ^ string_of_int n in
      if List.exists ((=) candidate) used_vars then aux (n+1)
      else candidate
    in
    aux 0

  let rec substitute (x : string) (with_v : t) (in_u : t) =
    let free_in_v = free_vars with_v in
    match in_u with 
    | Bottom -> Bottom
    | Var v when v = x -> with_v
    | Var _ -> in_u
    | Fun (v, _) when v = x -> in_u
    | Fun (v, body) when List.exists ((=) v) free_in_v ->
      let used_vars = (free_in_v @ (free_vars body) @ (bound_vars body)) in
      let fresh_v = find_fresh_var used_vars in
      let renamed_body = substitute v (Var fresh_v) body in
      Fun (fresh_v, substitute x with_v renamed_body)
    | Fun (v, body) -> Fun (v, substitute x with_v body)
    | App (t1,t2) -> App (substitute x with_v t1, substitute x with_v t2)

  let redex_list t =
    let rec aux res = function
    | Var _ -> res
    | Bottom -> res
    | Fun (_, body) -> aux res body
    | App (Fun(_, body), t2) as h -> aux (aux (if List.mem h res then res else h :: res) body) t2
    | App (t1, t2) -> aux (aux res t1) t2
    in
    aux [] t

  let rec compress = function
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
    | Fun (n1, App (App (Var n2, Fun (_, Fun (_, Fun (f1, Var f2)))), Fun (t1, Fun (_, Var t2))))
      when n1 = n2 && f1 = f2 && t1 = t2 ->
      "[IsZero]"
    | App (Fun (g1, Fun (h1, App (Var h2, App (Var g2, App (Var g3, Var h3))))), Fun (g1', Fun (h1', App (Var h2', App (Var g2', App (Var g3', Var h3'))))))
      when g1 = g2 && g2 = g3 && h1 = h2 && h2 = h3 && g1' = g2' && g2' = g3' && h1' = h2' && h2' = h3' ->
      "[Y]"
    | Fun (x1, Var x2) when x1 = x2 ->
      "[I]"
    | Fun (f, Fun (x, n)) ->
      let rec is_number k = function
        | Var v when v = x -> true, k
        | App (Var v, t) when v = f -> is_number (k+1) t
        | _ -> false, 0
      in
      let is_num, n = is_number 0 n in
      if is_num then "[" ^ string_of_int n ^ "]"
      else raise (Invalid_argument "Cannot compress term")
    | Fun (z1, App (App (Var z2, u), v)) when z1 = z2 ->
      "<" ^ to_string u ^ "," ^ to_string v ^ ">"
    | _ -> raise (Invalid_argument "Cannot compress term")

  and to_string t =
    try compress t with
    | Invalid_argument _ ->
      match t with
      | Var v -> v
      | Bottom -> "⊥"
      | Fun (v, body) -> "λ" ^ v ^ "." ^ to_string body
      | App (t1, t2) ->
        (match t1 with
        | Var v -> v
        | _ -> (try compress t1 with | Invalid_argument _ -> "(" ^ to_string t1 ^ ")")) ^
        (match t2 with
        | Var v -> v
        | _ -> (try compress t2 with | Invalid_argument _ -> "(" ^ to_string t2 ^ ")"))

  let rec to_string_tree ?(indent=0) term =
    let indentation = String.make (indent * 2) ' ' in
    match term with
    | Var v -> indentation ^ "Var( " ^ v ^ " )\n"
    | Bottom -> indentation ^ "Bottom\n"
    | Fun (v, body) ->
        indentation ^ "Fun(" ^ v ^ ")\n" ^
        (to_string_tree ~indent:(indent + 1) body)
    | App (t1, t2)->
        indentation ^ "App(\n" ^
        to_string_tree ~indent:(indent+1) t1 ^ "\n" ^
        to_string_tree ~indent:(indent+1) t2 ^ "\n" ^
        indentation ^ ")\n"

  let rec to_ugly_string term =
    match term with
    | Var v -> "Var \"" ^ v ^ "\""
    | Bottom -> "Bottom"
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
    | I ->
      Fun ("x", Var "x")
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

    (* Ca marche probablement pas ça *)
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
    | ({lexeme=Y; _} as t) :: rest
    | ({lexeme=I; _} as t) :: rest ->
      (match rest with
      | [] -> extend t
      | _ -> App (extend t, parse rest))
    | {lexeme=Bottom; _} :: rest ->
      (match rest with
      | [] -> Bottom
      | _ -> App (Bottom, parse rest))
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

  let rec to_deBruijn ?(binders=[]) = function
    | Bottom -> [(-5)]
    | Var v ->
      begin
      match List.find_index ((=) v) binders with
       | Some i -> [i]
       | None -> [-1]
      end
    | Fun (v, body) -> (-2) :: (to_deBruijn ~binders:(v :: binders) body)
    | App (t1, t2) -> 
      (-3) :: to_deBruijn ~binders:binders t1 @ (-4) :: (-3) :: to_deBruijn ~binders:binders t2 @ [(-4)]
end


let alpha_compare (t1 : LambdaTerm.t) (t2 : LambdaBottomTerm.t) =
  let rec aux binders1 binders2 t1 t2 =
    match t1, t2 with
    | LambdaTerm.Var v1, LambdaBottomTerm.Var v2 ->
      begin
      match List.find_index ((=) v1) binders1, List.find_index ((=) v2) binders2 with
      | Some i, Some j when i = j -> true
      | None, None -> true
      | _ -> false
      end
    | Fun (v1, body1), Fun (v2, body2) ->
      aux (v1 :: binders1) (v2 :: binders2) body1 body2
    | App (s1, s2), App (t1, t2) ->
      aux binders1 binders2 s1 t1 && aux binders1 binders2 s2 t2
    | _, Bottom -> true
    | _ -> false
  in
  aux [] [] t1 t2
