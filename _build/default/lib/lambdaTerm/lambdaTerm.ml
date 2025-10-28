open Token

module type LambdaTerm = sig
  module Token = Token

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

module Base = struct
  module Token = Token

  type t = Var of Token.t | Fun of Token.t * t | App of t list | Empty

  exception Parsing_error of string

  let rec to_string = function
      Var v -> Token.to_string v
    | Fun (v, body) -> "λ" ^ Token.to_string v ^ "." ^ to_string body
    | App t_list ->
      let rec aux acc = function
        | [] -> acc
        | Var v :: l -> aux (acc ^ Token.to_string v) l
        | t :: l -> aux (acc ^ "(" ^ to_string t ^ ")") l
      in
      aux "" t_list
    | Empty -> ""

  let deBruijn_to_string indices =
    String.concat " " (List.map (fun x -> 
      if x=(-1) then "λ" 
      else if x=(-2) then "("
      else if x=(-3) then ")"
      else string_of_int x ) indices)

  let deBruijn_index t = 
    let rec aux binders = function
      | Var v ->
        let idx = List.find_index (Token.eq v) binders in
        (match idx with
         | Some i -> [i+1]
         | None -> [0])
      | Fun (v, body) -> (-1) :: (aux (v :: binders) body)
      | App [x] -> aux binders x
      | App t_list -> 
        List.fold_left (fun acc t -> acc @ (-2) :: aux binders t @ [(-3)]) [] t_list
      | Empty -> []
    in
    let res = aux [] t in
    res

  let deBruijn_string t =
    let indices = deBruijn_index t in
    deBruijn_to_string indices

  let of_deBruijn indices =
    let rec aux binders : int list -> t list * int list = function
      | [] -> [], []
      | -3 :: t -> [], (-3) :: t
      | -1 :: t ->
        let (v : Token.t) = {lexeme=Var ("x" ^ string_of_int (List.length binders)); index = -1} in
        let body_list, remaining = aux (v :: binders) t in
        (match body_list with
        | [] -> raise (Parsing_error "Function with no body")
        | [body] -> [Fun (v, body)], remaining
        | body_list -> [Fun (v, App body_list)], remaining)
      | [0] ->
        let (v : Token.t) = {lexeme=Var ("f" ^ string_of_int (List.length binders)); index = -1} in
        [Var v], []
      | [n] when n > 0 ->
        let v = List.nth binders (n - 1) in
        [Var v], []
      | 0 :: t ->
        let (v : Token.t) = {lexeme=Var ("f" ^ string_of_int (List.length binders)); index = -1} in
        let t_list, remaining = aux binders t in 
        [App (Var v :: t_list)], remaining
      | n :: t when n > 0 ->
        let v = List.nth binders (n - 1) in
        let t_list, remaining = aux binders t in
        [App (Var v :: t_list)], remaining
      | -2 :: rest ->
        let (subterm_list, remaining) = aux binders rest in
        let subterm = (match subterm_list with
          | [] -> raise (Parsing_error "Empty parenthesized expression")
          | [t] -> t
          | t_list -> App t_list) in
        (match remaining with
        | -3 :: rest_after_paren -> 
            let t_list, remaining = aux binders rest_after_paren in subterm :: t_list, remaining
        | [] -> raise (Parsing_error "Unclosed parenthesis at end of term")
        | _ :: _ -> raise (Parsing_error "Expected closing parenthesis"))
      | _ -> raise (Parsing_error "Negative index in de Bruijn indices")
    in
    match aux [] indices |> fst with
    | [] -> Empty
    | [x] -> x
    | t_list -> App t_list

  let of_deBruijn_string s =
    let indices = String.split_on_char ' ' s in
    of_deBruijn (List.map (fun x -> if x = "λ" then -1 else int_of_string x) indices)

  let eq t1 t2 =
    List.equal (=) (deBruijn_index t1) (deBruijn_index t2) 


  let rec to_format_string = function
      Var v -> Token.to_string v
    | Fun (v, body) -> "\\lambda " ^ Token.to_string v ^ "." ^ to_format_string body
    | App t_list ->
      let rec aux acc = function
        | [] -> acc
        | Var v :: l -> aux (acc ^ Token.to_string v) l
        | t :: l -> aux (acc ^ "(" ^ to_format_string t ^ ")") l
      in
      aux "" t_list
    | Empty -> ""

  let rec to_string_tree ?(indent=0) term =
    let indentation = String.make (indent * 2) ' ' in
    match term with
    | Var v -> indentation ^ "Var(" ^ Token.to_string v ^ ")\n"
    | Fun (v, body) ->
        indentation ^ "Fun(" ^ Token.to_string v ^ ")\n" ^
        (to_string_tree ~indent:(indent + 1) body)
    | App t_list ->
        indentation ^ "App(\n" ^
        List.fold_left (fun acc t -> acc ^ to_string_tree ~indent:(indent + 1) t) "" t_list ^
        indentation ^ ")\n"
    | Empty -> indentation ^ "Empty\n"

  let rec to_ugly_string term =
    match term with
    | Var v -> "Var " ^ Token.to_ugly_string v
    | Fun (v, body) -> "Fun (" ^ Token.to_ugly_string v ^ ", " ^ to_ugly_string body ^ ")"
    | App t_list -> 
      let inner_string = List.fold_left (fun acc t -> acc ^ to_ugly_string t ^ "; ") "" t_list in
      "App [" ^ 
      String.sub inner_string 0 (String.length inner_string - 2)  ^ "]"
    | Empty -> "Empty"

  let parse_params l =
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
end

module LambdaTerm : LambdaTerm = struct
  include Base

  let compose_lambda t (l : Token.t list) =
    match l with
    | {lexeme=N n; _} :: rest ->
      let rec aux = function
      | 0 -> Var {lexeme=Var "t"; index=(-1)}
      | k -> App [t; aux (k-1)]
      in
      Fun ({lexeme=Var "t"; index=(-1)}, aux n), rest
    | _ -> t, l

  let rec extend (t : Token.t) =
    match t.lexeme with
    | Number n ->
      Fun ({ lexeme = Var "f"; index = Token.index t },
        Fun ({ lexeme = Var "x"; index = Token.index t },
          match compose_lambda (Var {lexeme=Var "f"; index=(-1)}) [{lexeme= N n; index=(-1)}] |> fst with
          | Fun _ as f -> App [f; Var { lexeme = Var "x"; index = Token.index t }]
          | _ -> failwith "Unreachable"))
    | Succ -> 
      Fun ({ lexeme = Var "n"; index = Token.index t }, 
        Fun ({ lexeme = Var "f"; index = Token.index t }, 
          Fun ({ lexeme = Var "x"; index = Token.index t },
            App [Var { lexeme = Var "f"; index = Token.index t }; 
                 App [Var { lexeme = Var "n"; index = Token.index t };
                      Var { lexeme = Var "f"; index = Token.index t }; 
                      Var { lexeme = Var "x"; index = Token.index t }]])))
    | Plus ->
      Fun ({ lexeme = Var "m"; index = Token.index t }, 
        Fun ({ lexeme = Var "n"; index = Token.index t }, 
          Fun ({ lexeme = Var "f"; index = Token.index t }, 
            Fun ({ lexeme = Var "x"; index = Token.index t }, 
              App [Var { lexeme = Var "m"; index = Token.index t };
                   Var { lexeme = Var "f"; index = Token.index t }; 
                   App [Var { lexeme = Var "n"; index = Token.index t };
                        Var { lexeme = Var "f"; index = Token.index t }; 
                        Var { lexeme = Var "x"; index = Token.index t }]]))))
    | Time ->
      Fun ({ lexeme = Var "m"; index = Token.index t },
        Fun ({ lexeme = Var "n"; index = Token.index t },
          Fun ({ lexeme = Var "f"; index = Token.index t },
            App [Var { lexeme = Var "m"; index = Token.index t };
                 App [Var { lexeme = Var "n"; index = Token.index t };
                      Var { lexeme = Var "f"; index = Token.index t }]])))
    | True ->
      Fun ({ lexeme = Var "x"; index = Token.index t },
        Fun ({ lexeme = Var "y"; index = Token.index t },
          Var { lexeme = Var "x"; index = Token.index t }))
    | False ->
      Fun ({ lexeme = Var "x"; index = Token.index t },
        Fun ({ lexeme = Var "y"; index = Token.index t },
          Var { lexeme = Var "y"; index = Token.index t }))
    | P1 ->
      Fun ({ lexeme = Var "s"; index = Token.index t },
        App [Var { lexeme = Var "s"; index = Token.index t };
             Fun ({ lexeme = Var "x"; index = Token.index t },
               Fun ({ lexeme = Var "y"; index = Token.index t },
                 Var { lexeme = Var "x"; index = Token.index t }))])
    | P2 ->
      Fun ({ lexeme = Var "s"; index = Token.index t },
        App [Var { lexeme = Var "s"; index = Token.index t };
             Fun ({ lexeme = Var "x"; index = Token.index t },
               Fun ({ lexeme = Var "y"; index = Token.index t },
                 Var { lexeme = Var "y"; index = Token.index t }))])
    (* | Couple u,v ->  *)
    (*   Fun ({ lexeme = Var "z"; index = Token.index t }, *)
    (*     App [Var { lexeme = Var "z"; index = Token.index t }; *)
    (*          of_string u; of_string v]) *)
    | _ -> raise (Parsing_error ("[ " ^ (string_of_int (Token.index t)) ^ " ] Cannot extend token: " ^ (Token.to_string t)))

  and of_string s =
    let tokens = Token.list_of_string s in
    let rec parse : Token.t list -> t list * Token.t list = function
    | [] -> [], []
    | [{lexeme=Var _; _} as v] -> [Var v], []
    | ({lexeme=Var _; _} as v) :: l -> let t_list, remaining = parse l in (Var v) :: t_list, remaining
    | ({lexeme=Plus; _} as t) :: l 
    | ({lexeme=Time; _} as t) :: l
    | ({lexeme=Succ; _} as t) :: l 
    | ({lexeme=True; _} as t) :: l
    | ({lexeme=False; _} as t) :: l
    | ({lexeme=P1; _} as t) :: l 
    | ({lexeme=P2; _} as t) :: l
    | ({lexeme=Number _; _} as t) :: l -> let t_list, remaining = parse l in (extend t) :: t_list, remaining
    | {lexeme=Lambda; index=i} :: l ->
      let params, body_l = parse_params l in
      let body_t_list, remaining = parse body_l in
      let body_t = (match body_t_list with
      | [] -> raise (Parsing_error ("[ " ^ (string_of_int i) ^ " ] Function with no body"))
      | [t] -> t
      | t_list -> App t_list) in
      let f, remaining = compose_lambda (make_fun params body_t) remaining in
      let t_list, remaining = parse remaining in f :: t_list, remaining
    | {lexeme=LParen; index = i} :: rest ->
      let (subterm_list, remaining) = parse rest in
      let subterm = (match subterm_list with
        | [] -> raise (Parsing_error ("[ " ^ (string_of_int i) ^ " ] Empty parenthesized expression"))
        | [t] -> t
        | t_list -> App t_list) in
      (match remaining with
      | {lexeme=RParen; _} :: rest_after_paren -> 
          let subtermn, rest_after_n = compose_lambda subterm rest_after_paren in
          let t_list, remaining = parse rest_after_n in subtermn :: t_list, remaining
      | [] -> raise (Parsing_error "Unclosed parenthesis at end of term")
      | t :: _ -> raise (Parsing_error ("[ " ^ (string_of_int (Token.index t)) ^ " ] 
                                        Expected closing parenthesis"))
      )
    | {lexeme=LAngle; index = i} :: rest ->
      let (subterm_list_u, remaining) = parse rest in
      let subterm_u = (match subterm_list_u with
        | [] -> raise (Parsing_error ("[ " ^ (string_of_int i) ^ " ] Empty angle-bracketed expression"))
        | [t] -> t
        | t_list -> App t_list) in
      (match remaining with
       | {lexeme=Coma; _} :: remaining ->
          let (subterm_list_v, remaining) = parse remaining in
          let subterm_v = (match subterm_list_v with
            | [] -> raise (Parsing_error ("[ " ^ (string_of_int i) ^ " ] Empty angle-bracketed expression"))
            | [t] -> t
            | t_list -> App t_list) in
          (match remaining with
          | {lexeme=RAngle; _} :: rest_after_angle ->
              let couple = 
                Fun ({ lexeme = Var "z"; index = i },
                  App [ App [ Var { lexeme = Var "z"; index = i };
                            subterm_u ]; subterm_v]) in
              let couplen, rest_after_n = compose_lambda couple rest_after_angle in
              let t_list, remaining = parse rest_after_n in couplen :: t_list, remaining
          | [] -> raise (Parsing_error "Unclosed angle bracket at end of term")
          | t :: _ -> raise (Parsing_error ("[ " ^ (string_of_int (Token.index t)) ^ " ] 
                                            Expected closing angle bracket"))
          )
       | [] -> raise (Parsing_error "Unclosed angle bracket at end of term")
       | t :: _ -> raise (Parsing_error ("[ " ^ (string_of_int (Token.index t)) ^ " ] 
                                         Expected comma in angle-bracketed expression"))
      )
    | ({lexeme=RParen; _} :: _) as l -> [], l
    | ({lexeme=RAngle; _} :: _) as l -> [], l
    | ({lexeme=Coma; _} :: _) as l -> [], l
    | t :: _ -> raise (Parsing_error ("[ " ^ (string_of_int (Token.index t)) ^ " ] 
                                      Unexpected token: " ^ (Token.to_string t)))
  in
  try (match parse tokens |> fst with
  | [] -> Empty
  | [t] -> t
  | t_list -> App t_list) with
  | Parsing_error msg -> print_endline msg; exit 1
end

(* module Left : LambdaTerm = struct *)
(*   include Base *)
(**)
(*   let compose_lambda_aux f n = *)
(*     let rec aux k n = *)
(*       if n <= 0 then k (Var {lexeme=Var "x"; index = (-1)}) else aux (fun x -> k (App[f; x])) (n - 1) *)
(*     in *)
(*     Fun ({lexeme=Var "x"; index = (-1)}, aux Fun.id n) *)
(**)
(*   let compose_lambda t (l : Token.t list) = *)
(*     match l with *)
(*     | {lexeme=N n; _} :: rest -> compose_lambda_aux t n, rest *)
(*     | _ -> t, l *)
(**)
(*   let extend (t : Token.t) = *)
(*     match t.lexeme with *)
(*     | Number n -> *)
(*       Fun ({lexeme=Var "f"; index = t.index}, *)
(*             compose_lambda_aux (Var {lexeme=Var "f"; index = t.index}) n) *)
(*     | Succ ->  *)
(*       Fun ({ lexeme = Var "n"; index = Token.index t },  *)
(*            Fun ({ lexeme = Var "f"; index = Token.index t },  *)
(*                 Fun ({ lexeme = Var "x"; index = Token.index t },  *)
(*                      App [Var { lexeme = Var "f"; index = Token.index t };  *)
(*                      App [App [Var { lexeme = Var "n"; index = Token.index t };  *)
(*                                Var { lexeme = Var "f"; index = Token.index t }];  *)
(*                           Var { lexeme = Var "x"; index = Token.index t }]]))) *)
(*     | Plus -> *)
(*       Fun ({ lexeme = Var "m"; index = Token.index t }, *)
(*            Fun ({ lexeme = Var "n"; index = Token.index t }, *)
(*                 Fun ({ lexeme = Var "f"; index = Token.index t },  *)
(*                      Fun ({ lexeme = Var "x"; index = Token.index t },  *)
(*                           App [App [Var { lexeme = Var "m"; index = Token.index t };  *)
(*                                     Var { lexeme = Var "f"; index = Token.index t }];  *)
(*                                App [App [Var { lexeme = Var "n"; index = Token.index t };  *)
(*                                          Var { lexeme = Var "f"; index = Token.index t }];  *)
(*                                     Var { lexeme = Var "x"; index = Token.index t }]])))) *)
(*     | Time -> *)
(*       Fun ({ lexeme = Var "m"; index = Token.index t },  *)
(*            Fun ({ lexeme = Var "n"; index = Token.index t },  *)
(*                 Fun ({ lexeme = Var "f"; index = Token.index t },  *)
(*                      App [Var { lexeme = Var "m"; index = Token.index t };  *)
(*                           App [Var { lexeme = Var "n"; index = Token.index t };  *)
(*                                Var { lexeme = Var "f"; index = Token.index t }]]))) *)
(*     | _ -> raise (Parsing_error ("[ " ^ (string_of_int (Token.index t)) ^ " ] Cannot extend token: " ^ (Token.to_string t))) *)
(**)
(*   let of_string s = *)
(*     let tokens = Token.list_of_string s in *)
(*     let rec parse (res : t) (tokens : Token.t list) : t * Token.t list = *)
(*       match tokens with *)
(*       | [] -> res, [] *)
(*       | {lexeme=Var _; _} as v :: rest -> ( *)
(*         let vn, rest = compose_lambda (Var v) rest in *)
(*         match res with *)
(*         | Empty -> parse vn rest *)
(*         | _ -> parse (App [res; vn]) rest ) *)
(*       | ({lexeme=Number _; _} as t) :: rest  *)
(*       | ({lexeme=Succ; _} as t) :: rest  *)
(*       | ({lexeme=Plus; _} as t) :: rest *)
(*       | ({lexeme=Time; _} as t) :: rest -> ( *)
(*         let tn, rest = compose_lambda (extend t) rest in *)
(*         match res with *)
(*         | Empty -> parse tn rest *)
(*         | _ -> parse (App [res; tn]) rest ) *)
(*       | {lexeme=Lambda; _} :: paramsxbody -> *)
(*         let params, body = parse_params paramsxbody in *)
(*         let (body_term, remaining) = parse Empty body in *)
(*         let f = make_fun params body_term in  *)
(*         let fn, remaining = compose_lambda f remaining in ( *)
(*         match res with *)
(*         | Empty -> fn, remaining *)
(*         | _ -> App [res; fn], remaining ) *)
(*       | {lexeme=LParen; _} :: rest -> *)
(*         let (subterm, remaining) = parse Empty rest in *)
(*         (match remaining with *)
(*         | {lexeme=RParen; _} :: rest_after_paren ->  *)
(*             let subtermn, rest_after_n = compose_lambda subterm rest_after_paren in *)
(*             (match res with *)
(*             | Empty -> parse subtermn rest_after_n *)
(*             | _ -> parse (App [res; subtermn]) rest_after_n) *)
(*         | [] -> raise (Parsing_error "Unclosed parenthesis at end of term") *)
(*         | t :: _ -> raise (Parsing_error ("[ " ^ (string_of_int (Token.index t)) ^ " ]  *)
(*                                           Expected closing parenthesis")) *)
(*         ) *)
(*       | ({lexeme=RParen; _} :: _) as l -> res, l *)
(*       | t :: _ -> raise (Parsing_error ("[ " ^ (string_of_int (Token.index t)) ^ " ]  *)
(*                                         Unexpected token: " ^ (Token.to_string t))) *)
(*     in *)
(*     try parse Empty tokens |> fst with *)
(*     | Parsing_error msg -> print_endline msg; exit 1 *)
(* end *)
(**)
(* module Right : LambdaTerm = struct *)
(*   include Base *)
(**)
(*   let of_string s = *)
(*     let tokens = Token.list_of_string s in *)
(*     let rec parse : Token.t list -> t * Token.t list = function *)
(*       | [] -> Empty, [] *)
(*       | [{lexeme=Var _;_} as v] -> Var v, [] *)
(*       | ({lexeme=Var _;_} as v) :: rest ->  *)
(*         begin *)
(*         let next, rest = parse rest in *)
(*         match next with *)
(*         | Empty -> Var v, rest *)
(*         | _ -> App[Var v; next], rest *)
(*         end *)
(*       | {lexeme = Lambda;_} :: ({lexeme=Var _;_} as v) :: {lexeme=Dot;_} :: body -> *)
(*         let body_term, rest = parse body in *)
(*         Fun (v, body_term), rest *)
(*       | {lexeme=LParen;_} :: rest -> *)
(*         begin *)
(*         let inner_term, inner_rest = parse rest in *)
(*         match inner_rest with *)
(*         | [] -> raise (Parsing_error "Unclosed parenthesis at end of term") *)
(*         | [{lexeme=RParen;_}] -> inner_term, [] *)
(*         | {lexeme=RParen;_} :: rest -> let next, next_rest = parse rest in  *)
(*                                        App [inner_term; next], next_rest *)
(*         | t :: _ -> raise (Parsing_error ("[ " ^ (string_of_int (Token.index t)) ^ " ] Unclosed parenthesis")) *)
(*         end *)
(*       | ({lexeme=RParen;_} :: _) as l -> Empty, l *)
(*       | t :: _ -> raise (Parsing_error ("[ " ^ (string_of_int (Token.index t)) ^ " ] Unexpected token : " *)
(*                                         ^ (Token.to_string t))) *)
(*     in *)
(*     try parse tokens |> fst with *)
(*     | Parsing_error msg -> print_endline msg; exit 1 *)
(* end *)
