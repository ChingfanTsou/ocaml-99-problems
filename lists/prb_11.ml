(*   Modified run-length encoding.
     Modify the result of the previous
   problem in such a way that if an
   element has no duplicates it is
   simply copied into the result list.
     Only elements with duplicates
   are transferred as (N E) lists.
 *)

(*   Since OCaml lists are homogeneous,
   one needs to define a type to hold both
   single elements and sub-lists.
 *)
type 'a rle_t =
  | One of 'a
  | Many of int * 'a

let encode_m l =
  let mdf pcg =
    let Many (n,code) = pcg in
    if n = 1 then
      One code
    else
      pcg in
  let rec ec_aux d_l pcg c_l =
    match d_l with
    | [] -> (mdf pcg)::c_l
    | x :: t ->
       let Many (n,y) = pcg in
       if n = 0 || x = y then
	 ec_aux t (Many(n+1,y)) c_l
       else
	 ec_aux t (Many(1,x)) ((mdf pcg)::c_l) in
  if l = [] then
    []
  else
    List.rev (ec_aux l (Many(0,List.hd l)) [])
