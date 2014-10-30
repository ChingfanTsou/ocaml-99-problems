open OUnit2
open List_prb
  
let test01_nom test_ctxt = assert_equal
    (Some "d")
    (Prb_01.last [ "a" ; "b" ; "c" ; "d" ])

let test01_null test_ctxt = assert_equal
    None (Prb_01.last [])

let test11_nom test_ctxt =
  let open Prb_11 in
  assert_equal
    (encode
       ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"])
    [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
     Many (4, "e")]

let test11_null test_ctxt = assert_equal
    (Prb_11.encode [])
    []

let test20_nom test_ctxt = assert_equal
    ["a"; "c"; "d"]
    (Prb_20.remove_at 1 ["a";"b";"c";"d"])

let test21_1 test_ctxt = assert_equal
    (Prb_21.insert_at "alfa" 1 ["a";"b";"c";"d"])
    ["a"; "alfa"; "b"; "c"; "d"]

let test21_3 test_ctxt = assert_equal
    (Prb_21.insert_at "alfa" 3 ["a";"b";"c";"d"])
    ["a"; "b"; "c"; "alfa"; "d"]

let test21_4 test_ctxt = assert_equal
    (Prb_21.insert_at "alfa" 4 ["a";"b";"c";"d"]) ["a"; "b"; "c"; "d"; "alfa"]

let test22_asc test_ctxt = assert_equal
    (Prb_22.range 4 9) [4;5;6;7;8;9]

let test22_des test_ctxt = assert_equal
    (Prb_22.range 9 4) [9;8;7;6;5;4]

let test22_sgl test_ctxt = assert_equal
    (Prb_22.range 5 5) [5]

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
