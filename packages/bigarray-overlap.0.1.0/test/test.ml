let triple a b c =
  Alcotest.testable
    (fun ppf (x, y, z) ->
       let pp_a = Alcotest.pp a
       and pp_b = Alcotest.pp b
       and pp_c = Alcotest.pp c in
       Fmt.pf ppf "@[<hov>(%a,@ %a,@ %a)@]"
         pp_a x pp_b y pp_c z)
    (fun (u, v, w) (x, y, z) ->
       let eq_a = Alcotest.equal a
       and eq_b = Alcotest.equal b
       and eq_c = Alcotest.equal c in
       eq_a u x && eq_b v y && eq_c w z)

let test w i v0 v1 =
  Alcotest.(check w) (Fmt.strf "%d" i) v0 v1

open Bigarray_compat

module Make (B : sig
        type layout

        val layout : layout Bigarray_compat.layout
        val sub1 : ('a, 'b, layout) Array1.t -> int -> int -> ('a, 'b, layout) Array1.t
        val sub2 : ('a, 'b, layout) Array2.t -> int -> int -> ('a, 'b, layout) Array2.t
        val sub3 : ('a, 'b, layout) Array3.t -> int -> int -> ('a, 'b, layout) Array3.t
 end) = struct
  let tests () =
    let a = Array0.create Int B.layout in
    let b = Array0.create Int B.layout in

    test Alcotest.bool 1 (Overlap.array0 a b) false ;
    test Alcotest.bool 2 (Overlap.array0 b a) false ;
    test Alcotest.bool 3 (Overlap.array0 a a) true ;
    test Alcotest.bool 4 (Overlap.array0 b b) true ;

    let v = Array1.create Int B.layout 10 in
    let w = Alcotest.(option (triple int int int)) in

    let ab = B.sub1 v 5 5 in
    let cd = B.sub1 v 0 5 in
    test w 5 (Overlap.array1 ab cd) None ;

    let ab = B.sub1 v 0 5 in
    let cd = B.sub1 v 5 5 in
    test w 6 (Overlap.array1 ab cd) None ;

    let ab = B.sub1 v 0 6 in
    let cd = B.sub1 v 5 5 in
    test w 7 (Overlap.array1 ab cd) (Some (1, 5, 0)) ;

    let ab = B.sub1 v 5 5 in
    let cd = B.sub1 v 0 6 in
    test w 8 (Overlap.array1 ab cd) (Some (1, 0, 5)) ;

    let ab = B.sub1 v 0 8 in
    let cd = B.sub1 v 2 8 in
    test w 9 (Overlap.array1 ab cd) (Some (6, 2, 0)) ;

    let ab = B.sub1 v 0 10 in
    let cd = B.sub1 v 2 8 in
    test w 10 (Overlap.array1 ab cd) (Some (8, 2, 0)) ;

    let ab = B.sub1 v 0 10 in
    let cd = B.sub1 v 2 6 in
    test w 11 (Overlap.array1 ab cd) (Some (6, 2, 0)) ;

    let ab = B.sub1 v 0 8 in
    let cd = B.sub1 v 0 10 in
    test w 12 (Overlap.array1 ab cd) (Some (8, 0, 0)) ;

    let ab = B.sub1 v 0 10 in
    let cd = B.sub1 v 0 10 in
    test w 13 (Overlap.array1 ab cd) (Some (10, 0, 0)) ;

    let ab = B.sub1 v 0 10 in
    let cd = B.sub1 v 0 8 in
    test w 14 (Overlap.array1 ab cd) (Some (8, 0, 0)) ;

    let ab = B.sub1 v 2 6 in
    let cd = B.sub1 v 0 10 in
    test w 15 (Overlap.array1 ab cd) (Some (6, 0, 2)) ;

    let ab = B.sub1 v 2 8 in
    let cd = B.sub1 v 0 10 in
    test w 16 (Overlap.array1 ab cd) (Some (8, 0, 2)) ;

    let ab = B.sub1 v 2 8 in
    let cd = B.sub1 v 0 8 in
    test w 17 (Overlap.array1 ab cd) (Some (6, 0, 2)) ;

    let v = Array2.create Int B.layout 10 10 in
    let w = Alcotest.(option (triple int (pair int int) (pair int int))) in

    let ab = B.sub2 v 5 5 in
    let cd = B.sub2 v 0 5 in
    test w 18 (Overlap.array2 ab cd) None ;
    let ab = B.sub2 v 0 5 in
    let cd = B.sub2 v 5 5 in
    test w 19 (Overlap.array2 ab cd) None ;
    let ab = B.sub2 v 0 6 in
    let cd = B.sub2 v 5 5 in
    test w 20 (Overlap.array2 ab cd) (Some (10, (5, 0), (0, 0))) ;
    let ab = B.sub2 v 5 5 in
    let cd = B.sub2 v 0 6 in
    test w 21 (Overlap.array2 ab cd) (Some (10, (0, 0), (5, 0))) ;
    let ab = B.sub2 v 0 8 in
    let cd = B.sub2 v 2 8 in
    test w 22 (Overlap.array2 ab cd) (Some (60, (2, 0), (0, 0))) ;
    let ab = B.sub2 v 0 10 in
    let cd = B.sub2 v 2 8 in
    test w 23 (Overlap.array2 ab cd) (Some (80, (2, 0), (0, 0))) ;
    let ab = B.sub2 v 0 10 in
    let cd = B.sub2 v 2 6 in
    test w 24 (Overlap.array2 ab cd) (Some (60, (2, 0), (0, 0))) ;
    let ab = B.sub2 v 0 8 in
    let cd = B.sub2 v 0 10 in
    test w 25 (Overlap.array2 ab cd) (Some (80, (0, 0), (0, 0))) ;
    let ab = B.sub2 v 0 10 in
    let cd = B.sub2 v 0 10 in
    test w 26 (Overlap.array2 ab cd) (Some (100, (0, 0), (0, 0))) ;
    let ab = B.sub2 v 0 10 in
    let cd = B.sub2 v 0 8 in
    test w 27 (Overlap.array2 ab cd) (Some (80, (0, 0), (0, 0))) ;
    let ab = B.sub2 v 2 6 in
    let cd = B.sub2 v 0 10 in
    test w 28 (Overlap.array2 ab cd) (Some (60, (0, 0), (2, 0))) ;
    let ab = B.sub2 v 2 8 in
    let cd = B.sub2 v 0 10 in
    test w 29 (Overlap.array2 ab cd) (Some (80, (0, 0), (2, 0))) ;
    let ab = B.sub2 v 2 8 in
    let cd = B.sub2 v 0 8 in
    test w 30 (Overlap.array2 ab cd) (Some (60, (0, 0), (2, 0))) ;

    let v = Array3.create Int B.layout 10 10 10 in
    let w = Alcotest.(option (triple int (triple int int int) (triple int int int))) in

    let ab = B.sub3 v 5 5 in
    let cd = B.sub3 v 0 5 in
    test w 31 (Overlap.array3 ab cd) None ;
    let ab = B.sub3 v 0 5 in
    let cd = B.sub3 v 5 5 in
    test w 32 (Overlap.array3 ab cd) None ;
    let ab = B.sub3 v 0 6 in
    let cd = B.sub3 v 5 5 in
    test w 33 (Overlap.array3 ab cd) (Some (100, (5, 0, 0), (0, 0, 0))) ;
    let ab = B.sub3 v 5 5 in
    let cd = B.sub3 v 0 6 in
    test w 34 (Overlap.array3 ab cd) (Some (100, (0, 0, 0), (5, 0, 0))) ;
    let ab = B.sub3 v 0 8 in
    let cd = B.sub3 v 2 8 in
    test w 35 (Overlap.array3 ab cd) (Some (600, (2, 0, 0), (0, 0, 0))) ;
    let ab = B.sub3 v 0 10 in
    let cd = B.sub3 v 2 8 in
    test w 36 (Overlap.array3 ab cd) (Some (800, (2, 0, 0), (0, 0, 0))) ;
    let ab = B.sub3 v 0 10 in
    let cd = B.sub3 v 2 6 in
    test w 37 (Overlap.array3 ab cd) (Some (600, (2, 0, 0), (0, 0, 0))) ;
    let ab = B.sub3 v 0 8 in
    let cd = B.sub3 v 0 10 in
    test w 38 (Overlap.array3 ab cd) (Some (800, (0, 0, 0), (0, 0, 0))) ;
    let ab = B.sub3 v 0 10 in
    let cd = B.sub3 v 0 10 in
    test w 39 (Overlap.array3 ab cd) (Some (1000, (0, 0, 0), (0, 0, 0))) ;
    let ab = B.sub3 v 0 10 in
    let cd = B.sub3 v 0 8 in
    test w 40 (Overlap.array3 ab cd) (Some (800, (0, 0, 0), (0, 0, 0))) ;
    let ab = B.sub3 v 2 6 in
    let cd = B.sub3 v 0 10 in
    test w 41 (Overlap.array3 ab cd) (Some (600, (0, 0, 0), (2, 0, 0))) ;
    let ab = B.sub3 v 2 8 in
    let cd = B.sub3 v 0 10 in
    test w 42 (Overlap.array3 ab cd) (Some (800, (0, 0, 0), (2, 0, 0))) ;
    let ab = B.sub3 v 2 8 in
    let cd = B.sub3 v 0 8 in
    test w 43 (Overlap.array3 ab cd) (Some (600, (0, 0, 0), (2, 0, 0))) ;
  ;;
end

let c_layout =
  let module C_overlap = Make(struct
    type layout = c_layout

    let layout = C_layout
    let sub1 = Array1.sub
    let sub2 = Array2.sub_left
    let sub3 = Array3.sub_left end) in
  Alcotest.test_case "C overlap" `Quick C_overlap.tests

let fortran_layout =
  let module Fortran_overlap = Make(struct
    type layout = fortran_layout

    let layout = Fortran_layout
    let sub1 t a b = Array1.sub t (a+1) b
    let sub2 t a b = Array2.sub_right t (a+1) b
    let sub3 t a b = Array3.sub_right t (a+1) b end) in
  Alcotest.test_case "FORTRAN overlap" `Quick Fortran_overlap.tests



let () =
  Alcotest.run "overlap"
    [ "overlap", [ c_layout; fortran_layout; ] ]
