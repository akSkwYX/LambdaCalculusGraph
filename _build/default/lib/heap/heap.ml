module type Heap = sig
  type t

  val empty : t
  val is_empty : t -> bool
  val insert : int list -> int -> t -> t
  val extract_min : t -> (int list * t)
  val change_or_insert : int list -> int -> t -> t
  val to_string : t -> string
end

module Queue : Heap = struct
  type t = (int list * int) list
  let empty = []
  let is_empty = (=) []

  let insert e p h =
    let rec aux acc = function
      | [] -> List.rev ((e, p) :: acc)
      | (e', p') :: t as lst ->
        if p < p' then
          List.rev_append acc ((e, p) :: lst)
        else
          aux ((e', p') :: acc) t
    in
    aux [] h

  let extract_min = function
    | [] -> failwith "Heap is empty"
    | (e, _) :: t -> (e, t)

  let change e p h =
    let rec aux acc = function
      | [] -> raise (Invalid_argument "Trying to change priority of an element which doesn't appear in heap")
      | (e', p') :: t ->
        if e' = e then
          List.rev_append acc ((e, p) :: t)
        else
          aux ((e', p') :: acc) t
    in
    aux [] h

  let change_or_insert e p h =
    if List.mem (e,p) h then
      change e p h
    else
      insert e p h

  let to_string h =
    let rec aux = function
      | [] -> ""
      | (e, p) :: t ->
        let rest = aux t in
        let entry_str = Printf.sprintf "([%s], %d)" (String.concat "; " (List.map string_of_int e)) p in
        if rest = "" then entry_str else entry_str ^ " :: " ^ rest
    in
    "[" ^ aux h ^ "]"
end
