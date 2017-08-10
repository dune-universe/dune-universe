let protect ~(f:unit -> 'a) ~(finally:unit -> unit) : 'a =
  let finally_executed : bool ref = ref false in
  let res : 'a =
    try
      f ()
    with
    | e -> finally_executed := true; finally (); raise e in
  if !finally_executed then
    res
  else
    (finally (); res)
;;
