type 'a t = {
  mutable size : int;
  mutable capacity : int;
  mutable arr : 'a option array;
}

let empty () = { size = 0; capacity = 0; arr = [||] }
let is_empty d = d.size = 0

let get d i =
  if i < 0 || i >= d.size then raise (Invalid_argument "Index out of bounds")
  else d.arr.(i)

let set d i x =
  if i < 0 then raise (Invalid_argument "Negative index")
  else
    let new_capacity = ref (max (2 * d.capacity) (i + 1)) in
    while !new_capacity < i + 1 do
      new_capacity := 2 * !new_capacity
    done;
    let new_arr = Array.make !new_capacity None in
    for k = 0 to i do
      if k < d.capacity then
        new_arr.(k) <- d.arr.(k)
    done;
    new_arr.(i) <- x;
    d.size <- d.size + 1;
    d.capacity <- !new_capacity;
    d.arr <- new_arr
