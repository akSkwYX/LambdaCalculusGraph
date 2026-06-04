module IntElem = struct
  type t = int

  let eq a b = a = b
  let lt a b = a < b
  let compare_eq a b = a <= b
  let to_string = string_of_int
end

module TestQueue = Heap.Queue(IntElem)

let elem_testable = Alcotest.testable (fun ppf x -> Format.pp_print_string ppf (IntElem.to_string x)) IntElem.eq

(* --- The Test Cases --- *)

let test_empty () =
  let h = TestQueue.empty () in
  Alcotest.(check bool) "New heap is empty" true (TestQueue.is_empty h)

let test_insert_and_extract () =
  let h = TestQueue.empty () in
  TestQueue.insert 5 h;
  TestQueue.insert 3 h;
  TestQueue.insert 10 h;

  print_endline @@ TestQueue.to_string h;
  
  Alcotest.(check bool) "Heap is not empty after inserts" false (TestQueue.is_empty h);
  
  Alcotest.(check elem_testable) "First extract should be min element" 3 (TestQueue.extract h);
  print_endline @@ TestQueue.to_string h;
  Alcotest.(check elem_testable) "Second extract" 5 (TestQueue.extract h);
  print_endline @@ TestQueue.to_string h;
  Alcotest.(check elem_testable) "Third extract" 10 (TestQueue.extract h);
  print_endline @@ TestQueue.to_string h;
  Alcotest.(check bool) "Heap is empty again" true (TestQueue.is_empty h)

let test_change_priority () =
  let h = TestQueue.empty () in
  TestQueue.insert 10 h;
  TestQueue.insert 20 h;
  
  print_endline @@ TestQueue.to_string h;
  TestQueue.change_priority 20 5 h;
  print_endline @@ TestQueue.to_string h;

  Alcotest.(check elem_testable) "Extracted element should now be the updated one" 5 (TestQueue.extract h)

let () =
  let open Alcotest in
  run "Priority Queue Tests" [
    "basic_operations", [
      test_case "Empty heap properties" `Quick test_empty;
      test_case "Insert and extract order" `Quick test_insert_and_extract;
      test_case "Change priority behavior" `Quick test_change_priority;
    ];
  ]
