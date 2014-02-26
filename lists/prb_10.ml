(* Run-length encoding of a list. *)
let encode l =
  let rec ec_aux d_l pcg c_l =
    match d_l with
    | [] -> pcg::c_l
    | x :: t ->
       let n,y = pcg in
       if n = 0 || x = y then
	 ec_aux t (n+1,y) c_l
       else
	 ec_aux t (1,x) (pcg::c_l) in
  if l = [] then
    []
  else
    List.rev (ec_aux l (0,List.hd l) [])
