open Printf


(* FIRST PART *)
type ('a,'b) t = OK of 'a | Error of 'b
[@@deriving gt ~options:{show}]

let () =
  let show fa fb (e: (_,_) t) =
    GT.transform t (new show_t_t (GT.lift fa) (GT.lift fb)) () e in
  printf "%s\n%!" (show (GT.show GT.int) (GT.show GT.string) (OK 1));
  printf "%s\n%!" (show (GT.show GT.int) (GT.show GT.string) (Error "error1"));
  ()


(* SECOND PART *)
type 'a     t2 = ('a, GT.string) t
[@@deriving gt ~options:{show}]

let () =
  let show fa (e: _ t2) =
    GT.transform t2 (new show_t2_t fa) () e in
  printf "%s\n%!" (show (GT.lift @@ GT.show GT.float) (OK 2.));
  printf "%s\n%!" (show (GT.lift @@ GT.show GT.int) (Error "error2"));
  ()


(* THIRD PART *)
type t3 = GT.char t2
[@@deriving gt ~options:{show}]

let () =
  let show (e: t3) = GT.transform t3 (new show_t3_t) () e in
  printf "%s\n%!" (show (OK '3'));
  printf "%s\n%!" (show (Error "error3"));
  ()
