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
  val to_pdf_dot : t -> (int, int list) Hashtbl.t -> string -> unit
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

  (* CHATGPT prompt *)

(* helpers *)
let escape_label s =
  let b = Buffer.create (String.length s + 8) in
  String.iter (fun c ->
    match c with
    | '"' -> Buffer.add_string b "\\\""  (* double quote *)
    | '\\' -> Buffer.add_string b "\\\\" (* backslash *)
    | '\n' -> Buffer.add_string b "\\n"  (* newline *)
    | _ -> Buffer.add_char b c
  ) s;
  Buffer.contents b

(* compute BFS layers (nodes at same reduction depth) from roots (nodes with indegree 0) *)
let compute_layers (g : int list array) : int list array =
  let n = Array.length g in
  let indeg = Array.make n 0 in
  Array.iter (fun nbrs -> List.iter (fun j -> if j >= 0 && j < n then indeg.(j) <- indeg.(j) + 1) nbrs) g;
  let roots = ref [] in
  for i = 0 to n - 1 do if indeg.(i) = 0 then roots := i :: !roots done;
  (* fallback: if no indegree-0 nodes, choose node 0 as root(s) *)
  if !roots = [] then roots := [0];
  let q = Queue.create () in
  let level = Array.make n (-1) in
  List.iter (fun r -> Queue.add r q; level.(r) <- 0) !roots;
  while not (Queue.is_empty q) do
    let u = Queue.take q in
    List.iter (fun v ->
      if v >= 0 && v < n && level.(v) = -1 then (
        level.(v) <- level.(u) + 1;
        Queue.add v q
      )
    ) g.(u)
  done;
  (* group nodes by level *)
  let maxl = Array.fold_left (fun a x -> max a x) 0 level in
  let groups = Array.init (maxl + 1) (fun _ -> []) in
  for i = 0 to n - 1 do
    if level.(i) >= 0 then groups.(level.(i)) <- i :: groups.(level.(i))
    else groups.(0) <- i :: groups.(0)  (* unreachable nodes put in level 0 to avoid losing them *)
  done;
  groups

(* write dot and produce pdf *)
let to_pdf_dot
    (g : int list array)
    (hst : (int, int list) Hashtbl.t)
    (filename : string)
  : unit =
  let format_node n =
    try LambdaTerm.LambdaTerm.to_string @@ LambdaTerm.LambdaTerm.of_deBruijn (Hashtbl.find hst n)
    with Not_found -> string_of_int n
  in
  let dot_path = "results/" ^ filename ^ ".dot" in
  let pdf_path = "results/" ^ filename ^ ".pdf" in
  let oc = open_out dot_path in
  (* header with layout options tuned for clarity *)
  Printf.fprintf oc "digraph G {\n";
  Printf.fprintf oc "  graph [splines=true, overlap=false, rankdir=LR, nodesep=0.7, ranksep=1.0];\n";
  Printf.fprintf oc "  node [shape=box, style=rounded, fontname=\"DejaVu Sans\", fontsize=10, margin=0.08];\n";
  Printf.fprintf oc "  edge [arrowsize=0.8];\n\n";
  (* nodes *)
  Array.iteri (fun i _ ->
    let label = escape_label (format_node i) in
    Printf.fprintf oc "  n%d [label=\"%s\"];\n" i label
  ) g;
  Printf.fprintf oc "\n";
  (* edges *)
  Array.iteri (fun i nbrs ->
    List.iter (fun j -> Printf.fprintf oc "  n%d -> n%d;\n" i j) nbrs
  ) g;
  Printf.fprintf oc "\n";
  (* same-rank groups by BFS layers (helps horizontal alignment) *)
  (try
     let layers = compute_layers g in
     Array.iter (fun nodes ->
       match nodes with
       | [] -> ()
       | _ ->
         Printf.fprintf oc "  { rank = same; ";
         List.iter (fun i -> Printf.fprintf oc "n%d; " i) nodes;
         Printf.fprintf oc "}\n"
     ) layers
   with _ -> ());
  Printf.fprintf oc "}\n";
  close_out oc;
  (* call dot to create a PDF; check exit code if you want robust error handling *)
  let cmd = Printf.sprintf "dot -Tpdf -Gcharset=UTF-8 -o %s %s" pdf_path dot_path in
  ignore (Sys.command cmd);
  Printf.printf "Wrote %s and %s\n%!" dot_path pdf_path

  (* End *)

  let to_pdf (g : t) (name_map : (int, int list) Hashtbl.t) (filename : string) : unit =
    let format_node n = LambdaTerm.LambdaTerm.to_format_string @@ LambdaTerm.LambdaTerm.of_deBruijn n in
    let tex_file = open_out ("results/" ^ filename ^ ".tex") in
    let () = Printf.fprintf tex_file "
\\documentclass[landscape]{article}
\\usepackage[margin=0.5in]{geometry}
\\usepackage{tikz}
\\usetikzlibrary{graphs, graphdrawing}
\\usegdlibrary{trees}
\\begin{document}
\\tikzset{font=\\small}
\\begin{tikzpicture}[every node/.style = draw, scale = 1.2] \\graph [grow down, tree layout] {
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
    let () = Printf.fprintf tex_file "};\n\\end{tikzpicture}\n\\end{document}\n" in
    close_out tex_file;
    let _ = Sys.command ("lualatex -interaction=nonstopmode -output-directory=results results/" ^ filename ^ ".tex" ^ " > results/output.log") in
    ()
end
