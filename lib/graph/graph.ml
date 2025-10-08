module type Graph = sig
  type t

  val empty : t
  val is_empty : t -> bool

  val order : t -> int
  val size : t -> int
  val add_node : t -> t * int 
  val add_edge : int -> int -> t -> unit 
  val force_add_edge : int -> int -> t -> t
  val get_neighbors : int -> t -> int list
  val init_from_edges : (int * int) list -> t
  val to_string : t -> string
  val to_pdf : t -> (int, int list) Hashtbl.t -> string -> unit
end

module Graph : Graph = struct
  type t = int list array

  let empty = [||]
  let is_empty g = Array.length g = 0

  let order g = Array.length g
  let size g = Array.fold_left (fun acc neighbors -> acc + List.length neighbors) 0 g

  let add_node g = Array.append g [| [] |], Array.length g

  let add_edge v1 v2 g =
    let len = Array.length g in
    if v1 < 0 || v1 >= len || v2 < 0 || v2 >= len then
      raise (Invalid_argument "Node index out of bounds")
    else
      (* Remove this case if want to see each possible derivations *)
      if List.mem v2 g.(v1) then
        ()
      else
        g.(v1) <- v2 :: g.(v1)

  let force_add_edge v1 v2 g =
    let len = Array.length g in
    let g' = if v1 >= len || v2 >= len then
               let new_size = max (v1 + 1) (v2 + 1) in
               Array.init new_size (fun i -> if i < len then g.(i) else [])
             else g
    in
    add_edge v1 v2 g';
    g'

  let get_neighbors v g =
    if v < 0 || v >= Array.length g then
      raise (Invalid_argument "Node index out of bounds")
    else
      g.(v)

  let init_from_edges edges =
    let max_node = List.fold_left (fun acc (v1, v2) -> max acc (max v1 v2)) (-1) edges in
    let g = Array.make (max_node + 1) [] in
    List.iter (fun (v1, v2) -> add_edge v1 v2 g |> ignore) edges;
    g

  let to_string g =
    Array.fold_left (fun (acc, i) neighbors ->
       acc ^ (string_of_int i) ^ " : " ^
       (String.concat "; " (List.map string_of_int neighbors) ^ "\n"), i+1
    ) ("", 0) g |> fst

  let to_pdf (g : t) (name_map : (int, int list) Hashtbl.t) (filename : string) : unit =
    let format_node n = LambdaTerm.NoParenthesis.to_format_string @@ LambdaTerm.NoParenthesis.of_deBruijn n in
    let tex_file = open_out ("results/" ^ filename ^ ".tex") in
    let () = Printf.fprintf tex_file "
\\documentclass{article}
\\usepackage{tikz}
\\usetikzlibrary{graphs, graphdrawing}
\\usegdlibrary{trees}
\\begin{document}
\\tikz [every node/.style = draw] \\graph [grow down, tree layout] {
" in
    let () = Array.iteri (fun i neighbors ->
      let source = try format_node (Hashtbl.find name_map i) with Not_found -> string_of_int i in
      let targets = List.map (fun v ->
        try format_node (Hashtbl.find name_map v) with Not_found -> string_of_int v
      ) neighbors in
      List.iter (fun t -> Printf.fprintf tex_file "
  \"$%s$\" -> \"$%s$\";
" source t) targets
    ) g in
    let () = Printf.fprintf tex_file "};\n\\end{document}\n" in
    close_out tex_file;
    let _ = Sys.command ("lualatex -interaction=nonstopmode -output-directory=results results/" ^ filename ^ ".tex" ^ " > results/output.log") in
    ()
end
