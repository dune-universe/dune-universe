module Option = struct
  let bind e f = match e with
    | Some v -> f v
    | None -> None
  let return v = Some v
end

let t1 = 
  let open Option in do_
  ; x <-- return 1
  ; return x

let t2 = Option.do_
  ; x <-- return 1
  ; return x

let t3 = Option.do_
  ; x <-- return 1
  ; ();  print_string "hello"
  ; return x

let t5 = 
  let open Option in
  let%m x = return 1 in
  return x

let t6 = Option.do_
  ; [%p? (x,y) as z] <-- return (1,2)
  ; return (x + y, z)

let t7 = Option.do_
  ; x <-- return 1
  ; ( y <-- return 2       (* do_; still governs this *)
    ; z <-- return 3
    ; return (x + y, z) )

(*
let t8 =
  Option.do_
  ; x <-- return 1
  ; ( y <-- return 2       (* error *)
    ; z <-- return 3 )
  ; return (x + y, z)
*)

let t9 =
  Option.do_
  ; x <-- return 1
  ; (); (print_string "hello"; print_endline " world") (* this is ok *) 
  ; return x

let t10 =
  Option.do_
  ; x <-- return 1
  ; y <-- (Option.do_
          ; z <-- return 2   
          ; return z
          )
  ; return (x + y)

let t11 = Option.do_;
  x <-- return 1;
  let y = x + x in
  let module M = struct let q = 1 end in
  let open M in
  let%m w = return 1 in
  z <-- return (y + q + w);
  return z

let t12 = Option.do_
  ; return 1 (* bind is not used but there is no warning 26 *)

let t13 =
  match () with
  | () -> do_;
      Option.return 1
        
    
