module S = Strategy.LONoStrategy
module Lt = S.Lt

module LHashtbl = Hashtbl.Make(struct
  type t = Lt.t
  let equal = Lt.alpha_eq
  let hash = Fun.compose Hashtbl.hash Lt.to_deBruijn end)

let lambda_term_generator n =
  let rec generate_terms var depth =
    if depth = 0 then
      List.init (var+1) (fun i -> Lt.Var ("x"^(string_of_int i)))
    else
      List.map (fun t -> Lt.Fun ("x"^(string_of_int (var + 1)), t)) (generate_terms (var + 1) (depth - 1)) @
      List.concat @@ List.init (var+1) (fun i -> List.map (fun t -> Lt.Fun ("x"^(string_of_int i), t)) (generate_terms var (depth - 1))) @
      List.map (fun t1 ->
        List.map (fun t2 ->
          Lt.App (t1, t2)) (generate_terms var (depth - 1))
      ) (generate_terms var (depth - 1))
  in
  List.concat @@ List.init (n+1) (generate_terms 0)

let distance t =
  let (_, _, _, d) = S.astar false t in d

let generate_hashtble table n =
  List.iter (fun t -> if not @@ LHashtbl.mem table t then LHashtbl.add table t (distance t)) (lambda_term_generator n)

let t = LHashtbl.create 100000
let () = generate_hashtble t 3
let () = LHashtbl.iter (fun k v -> Printf.printf "%s -> %d\n" (Lt.to_string k) v) t

let file = open_out_bin "heuristic_data"
let () = Marshal.to_channel file t []
let () = close_out file

let file' = open_in_bin "heuristic_data"
let _ : int LHashtbl.t = Marshal.from_channel file'
let () = close_in file'
