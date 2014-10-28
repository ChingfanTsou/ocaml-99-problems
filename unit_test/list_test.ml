open OUnit2
  
let test01_nom test_ctxt = assert_equal
    (Some "d")
    (List_prb.Prb_01.last [ "a" ; "b" ; "c" ; "d" ])

let test01_null test_ctxt = assert_equal
    None (List_prb.Prb_01.last [])

let test20_nom test_ctxt = assert_equal
    ["a"; "c"; "d"]
    (List_prb.Prb_20.remove_at 1 ["a";"b";"c";"d"])

let test21_1 test_ctxt = assert_equal
    (List_prb.Prb_21.insert_at "alfa" 1 ["a";"b";"c";"d"])
    ["a"; "alfa"; "b"; "c"; "d"]

let test21_3 test_ctxt = assert_equal
    (List_prb.Prb_21.insert_at "alfa" 3 ["a";"b";"c";"d"])
    ["a"; "b"; "c"; "alfa"; "d"]

let test21_4 test_ctxt = assert_equal
    (List_prb.Prb_21.insert_at "alfa" 4 ["a";"b";"c";"d"]) ["a"; "b"; "c"; "d"; "alfa"]

let test22_asc test_ctxt = assert_equal
    (List_prb.Prb_22.range 4 9) [4;5;6;7;8;9]

let test22_des test_ctxt = assert_equal
    (List_prb.Prb_22.range 9 4) [9;8;7;6;5;4]

let test22_sgl test_ctxt = assert_equal
    (List_prb.Prb_22.range 5 5) [5]

let suite =
  "list_test">:::
  ["01_non_null_list">:: test01_nom;
   "01_null_list">:: test01_null;
   "20_nom">:: test20_nom;
   "21_1">:: test21_1;
   "21_3">:: test21_3;
   "21_4">:: test21_4;
   "22_asc">:: test22_asc;
   "22_des">:: test22_des;
   "22_sgl">:: test22_sgl]

let () =
  run_test_tt_main suite
