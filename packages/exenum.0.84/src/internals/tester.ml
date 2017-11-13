open Exen
open Convenience

(* Generic version that can be instantiated to a Lwt version and a non-lwt version. *)					
let gen_tester print ?tos bind unit exen ?(from=bigzero) ?upto ?(verbose_period=10000) ~len f =
  
  assert (len > 0) ;
  assert (verbose_period > 0) ;

  let bigverbose_period = boi verbose_period in

  let test_current count index =
    let verbose = verbose_period > 0 && Z.equal bigzero (bigmod count bigverbose_period) in
    let test_value = get exen index in
    
    bind (if verbose then print (Printf.sprintf "Test number %s, index #%s ... %s" (sob count) (sob index)
                                   (match tos with None -> "" | Some tos -> "with " ^ tos test_value ^ " ... "))
          else unit)
      
	 (fun () ->
	  bind (f test_value)
	       (fun () -> if verbose then print "done\n" else unit))
  in
  
  (* for loop, in which we double the current index every len iterations. *)
  let rec iterate count current remaining =
    (* End of this part? If yes, current := current * 2. *)
    if remaining = 0 then
      iterate count (2 **. current) len

    else
      (* Have we reached the upper bound? *)
      let continue =
	match upto with
	| None -> true
	| Some upper_bound -> big_compare current upper_bound < 0
      in

      if continue then
	begin
	  bind (test_current count current)
	       (fun () -> iterate (succ count) (succ current) (remaining - 1))
	end
      else
	(* We're done. *)
	unit
  in

  iterate bigzero from len
