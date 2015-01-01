open OUnit2
open List_prb
  
let test01_nom test_ctxt = assert_equal
    (Some "d")
    (Prb_01.last [ "a" ; "b" ; "c" ; "d" ])

let test01_null test_ctxt = assert_equal
    None (Prb_01.last [])

let test02_nom test_ctxt = assert_equal
    (Prb_02.last_two [ "a" ; "b" ; "c" ; "d" ])
    (Some ("c", "d"))

let test02_one test_ctxt = assert_equal
    (Prb_02.last_two [ "a" ])
    None

let test02_null test_ctxt = assert_equal
    (Prb_02.last_two [])
    None

let test03_nom test_ctxt = assert_equal
    (Prb_03.at 3 [ "a" ; "b"; "c"; "d"; "e" ])
    (Some "c")

let test03_null test_ctxt = assert_equal
    (Prb_03.at 3 [ "a" ])
    None

let test04_nom test_ctxt = assert_equal
    (Prb_04.length [ "a" ; "b" ; "c"])
    3

let test04_null test_ctxt = assert_equal
    (Prb_04.length [])
    0

let test05_nom test_ctxt = assert_equal
    (Prb_05.rev ["a" ; "b" ; "c"])
    ["c"; "b"; "a"]

let test05_null test_ctxt = assert_equal
    (Prb_05.rev [])
    []

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

(* let test23 test_ctxt = *)
(*   let l = Prb_23.rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3 in *)
(*   if List.length l <> 3 then *)
(*     assert_failure "Wrong number of selected elements." *)
(*   else *)
(*     List.fold_left (fun x test_flag -> *)
(*       if (List.mem )) *)
      

let suite =
  "list_test">:::
  ["01_last_nom">:: test01_nom;
   "01_last_null">:: test01_null;
   "02_last_two_nom">:: test02_nom;
   "02_last_two_one">:: test02_one;
   "02_last_two_null">:: test02_null;
   "03_at_nom">:: test03_nom;
   "03_at_null">:: test03_null;
   "04_length_nom">:: test04_nom;
   "04_length_null">:: test04_null;
   "05_rev_nom">:: test05_nom;
   "05_rev_null">:: test05_null;
   "20_remove_at_nom">:: test20_nom;
   "21_insert_at_1">:: test21_1;
   "21_insert_at_3">:: test21_3;
   "21_insert_at_4">:: test21_4;
   "22_range_asc">:: test22_asc;
   "22_range_des">:: test22_des;
   "22_range_sgl">:: test22_sgl]

let () =
  run_test_tt_main suite
