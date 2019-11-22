open! Import

let print_t_list t =
  List.iter t ~f:(fun (n,b) -> Printf.printf "%s %i\n" n b);
  printf "\n"

module Simple = struct
  type 'a t =
    { a : 'a }
  [@@deriving sexp_of, hardcaml]

  let%expect_test "simple" =
    print_t_list (to_list t);
    [%expect {|
      a 1 |} ]
end

module Set_bits = struct
  type 'a t =
    { a : 'a [@bits 12]
    ; b : 'a [@bits 0] }
  [@@deriving sexp_of, hardcaml]

  let%expect_test "set bits" =
    print_t_list (to_list t);
    [%expect {|
      a 12
      b 0 |} ]

  let%expect_test "iter" =
    iter ~f:(fun si -> print_s [%sexp (si : string * int)]) t;
    [%expect {|
      (a 12)
      (b 0) |} ]

  let%expect_test "iter2" =
    iter2 ~f:(fun si i -> print_s [%sexp ((si, i) : (string * int) * int)]) t { a=5; b=3 };
    [%expect {|
      ((a 12) 5)
      ((b 0) 3) |} ]

  let%expect_test "map" =
    print_t_list (to_list @@ map ~f:(fun (n,b) -> n,b+1) t);
    [%expect {|
      a 13
      b 1 |} ]

  let%expect_test "map2" =
    print_t_list (to_list @@ map2 ~f:(fun (n,b) c -> n,b+c) t { a=5; b=3 });
    [%expect {|
      a 17
      b 3 |} ]

  let%expect_test "[map] order" =
    ignore (map ~f:(fun si -> print_s [%sexp (si : string * int)]) t);
    [%expect {|
      (a 12)
      (b 0) |} ]

  let%expect_test "[map2] order" =
    ignore (map2 ~f:(fun si i -> print_s [%sexp ((si, i) : (string * int) * int)])
              t { a=5; b=3 });
    [%expect {|
      ((a 12) 5)
      ((b 0) 3) |} ]
end

module Rtlname = struct
  type 'a t =
    { hello : 'a [@rtlname "WORLD"] }
  [@@deriving sexp_of, hardcaml]

  let%expect_test "rtlname" =
    print_t_list (to_list t);
    [%expect {|
      WORLD 1 |} ]
end

module Nesting = struct
  type 'a t =
    { a : 'a [@bits 2]
    ; b : 'a Simple.t
    ; c : 'a Rtlname.t }
  [@@deriving sexp_of, hardcaml]

  let%expect_test "Nesting" =
    print_t_list (to_list t);
    [%expect {|
      a 2
      a 1
      WORLD 1 |} ]
end

module Rtlprefix = struct
  type 'a t =
    { world : 'a          [@rtlprefix "hello_"]
    ; foo   : 'a          [@rtlprefix "hello_"] [@rtlname "WORLD"]
    ; x     : 'a Simple.t [@rtlprefix "i_"]
    ; y     : 'a Simple.t [@rtlprefix "i_"]                        [@rtlmangle true] }
  [@@deriving sexp_of, hardcaml]

  let%expect_test "rtlprefix" =
    print_t_list (to_list t);
    [%expect {|
      hello_world 1
      hello_WORLD 1
      i_a 1
      i_y_a 1|} ]
end

module Rtlsuffix = struct
  type 'a t =
    { hello : 'a                              [@rtlsuffix "_world"]
    ; foo   : 'a          [@rtlname "hello_"] [@rtlsuffix "WORLD"]
    ; x     : 'a Simple.t                     [@rtlsuffix "_o"]
    ; y     : 'a Simple.t                     [@rtlsuffix "_o"]    [@rtlmangle true] }
  [@@deriving sexp_of, hardcaml]

  let%expect_test "rtlsuffix" =
    print_t_list (to_list t);
    [%expect {|
      hello_world 1
      hello_WORLD 1
      a_o 1
      y_a_o 1 |} ]
end

module Arrays = struct
  type 'a t =
    { x : 'a array [@length 1]
    ; y : 'a array [@length 3] [@bits 5]
    ; z : 'a array [@length 2] [@rtlname "Z"] }
  [@@deriving sexp_of, hardcaml]

  let%expect_test "arrays" =
    print_t_list (to_list t);
    [%expect {|
      x0 1
      y0 5
      y1 5
      y2 5
      Z0 1
      Z1 1 |} ]
end

module Array_with_module = struct
  module M = struct
    type 'a t =
      { foo : 'a }
    [@@deriving sexp_of, hardcaml]
  end

  type 'a t =
    { x : 'a M.t array [@length 1] }
  [@@deriving sexp_of, hardcaml]

  let%expect_test _ =
    print_t_list (to_list t);
    [%expect {|
      foo0 1 |}];
  ;;
end

module Lists = struct
  type 'a t =
    { x : 'a list [@length 1]
    ; y : 'a list [@length 3] [@bits 5]
    ; z : 'a list [@length 2] [@rtlname "Z"] }
  [@@deriving sexp_of, hardcaml]

  let%expect_test "lists" =
    print_t_list (to_list t);
    [%expect {|
      x0 1
      y0 5
      y1 5
      y2 5
      Z0 1
      Z1 1 |} ]
end

module List_with_module = struct
  module M = struct
    type 'a t =
      { foo : 'a }
    [@@deriving sexp_of, hardcaml]
  end

  type 'a t =
    { x : 'a M.t list [@length 1] }
  [@@deriving sexp_of, hardcaml]

  let%expect_test _ =
    print_t_list (to_list t);
    [%expect {|
      foo0 1 |}];
  ;;
end

module Rtlprefix_option = struct
  type 'a t =
    { a : 'a [@rtlprefix "X"]
    ; b : 'a }
  [@@deriving sexp_of, hardcaml ~rtlprefix:"i_"]

  let%expect_test "rtlprefix_option" =
    print_t_list (to_list t);
    [%expect {|
      Xa 1
      i_b 1 |} ]
end

module Rtlsuffix_option = struct
  type 'a t =
    { a : 'a [@rtlsuffix "X"]
    ; b : 'a }
  [@@deriving sexp_of, hardcaml ~rtlsuffix:"_o"]

  let%expect_test "rtlsuffix_option" =
    print_t_list (to_list t);
    [%expect {|
      aX 1
      b_o 1 |} ]
end

module Rtlmangle_option = struct
  type 'a t =
    { a : 'a [@bits 2]
    ; b : 'a Simple.t
    ; c : 'a Rtlname.t }
  [@@deriving sexp_of, hardcaml ~rtlmangle:true]

  let%expect_test "rtlmangle option" =
    print_t_list (to_list t);
    [%expect {|
      a 2
      b_a 1
      c_WORLD 1 |} ]
end

module Options = struct
  module N = struct type 'a t = { n : 'a}[@@deriving sexp_of, hardcaml] end

  type 'a t =
    { a : 'a     [@rtlprefix "P"] [@rtlname "N"] [@rtlsuffix "S"]
    ; b : 'a     [@rtlprefix "P"]                [@rtlsuffix "S"]
    ; c : 'a     [@rtlprefix "P"]
    ; d : 'a     [@rtlprefix "P"] [@rtlname "N"]
    ; e : 'a                      [@rtlname "N"] [@rtlsuffix "S"]
    ; f : 'a                                     [@rtlsuffix "S"]
    ; g : 'a
    ; h : 'a                      [@rtlname "N"]
    ; i : 'a N.t [@rtlprefix "P"]                [@rtlsuffix "S"]
    ; j : 'a N.t [@rtlprefix "P"]
    ; k : 'a N.t                                 [@rtlsuffix "S"]
    ; l : 'a N.t }
  [@@deriving
    sexp_of
  , hardcaml
      ~rtlmangle:true
      ~rtlprefix:"p"
      ~rtlsuffix:"s"]
  let%expect_test "options and overrides" =
    print_t_list (to_list t);
    [%expect {|
      PNS 1
      PbS 1
      Pcs 1
      PNs 1
      pNS 1
      pfS 1
      pgs 1
      pNs 1
      Pi_nS 1
      Pj_ns 1
      pk_nS 1
      pl_ns 1 |} ]
end

module Extended : sig
  type 'a t =
    { foo : 'a }
  [@@deriving sexp_of, hardcaml]
end = struct
  type 'a t =
    { foo : 'a }
  [@@deriving sexp_of, hardcaml]
end

let%expect_test "extended" =
  print_s [%sexp (Extended.port_widths : int Extended.t)];
  [%expect {| ((foo 1)) |}];
  print_s [%sexp (Extended.port_names : string Extended.t)];
  [%expect {| ((foo foo)) |}]
;;

module Bar : sig
  type 'a t =
    { bar : 'a }
  [@@deriving sexp_of, hardcaml ~ast]
end = struct
  type 'a t =
    { bar : 'a
    (** bar documentation *)
    }
  [@@deriving sexp_of, hardcaml ~ast]
end

module Foo : Hardcaml.Interface.S_with_ast = struct
  type 'a t =
    { foo : 'a
    (** foo documentation *)
    ; bar : 'a Bar.t
    ; lst : 'a list[@length 7]
    ; arr : 'a array[@length 0]
    ; lstm : 'a Bar.t list[@length 7]
    (** lstm documentation *)
    ; arrm : 'a Bar.t array[@length 0]
    }
  [@@deriving sexp_of, hardcaml ~ast]
end

let%expect_test "ast" =
  print_s [%sexp (Foo.ast : Hardcaml.Interface.Ast.t)];
  [%expect {|
    (((name foo)
      (type_ (
        Signal
        (bits    1)
        (rtlname foo)))
      (sequence ())
      (doc (" foo documentation ")))
     ((name bar)
      (type_ (
        Module
        (name Bar)
        (ast ((
          (name bar)
          (type_ (
            Signal
            (bits    1)
            (rtlname bar)))
          (sequence ())
          (doc (" bar documentation ")))))))
      (sequence ())
      (doc      ()))
     ((name lst)
      (type_ (
        Signal
        (bits    1)
        (rtlname lst)))
      (sequence ((
        (kind   List)
        (length 7))))
      (doc ()))
     ((name arr)
      (type_ (
        Signal
        (bits    1)
        (rtlname arr)))
      (sequence ((
        (kind   Array)
        (length 0))))
      (doc ()))
     ((name lstm)
      (type_ (
        Module
        (name Bar)
        (ast ((
          (name bar)
          (type_ (
            Signal
            (bits    1)
            (rtlname bar)))
          (sequence ())
          (doc (" bar documentation ")))))))
      (sequence ((
        (kind   List)
        (length 7))))
      (doc (" lstm documentation ")))
     ((name arrm)
      (type_ (
        Module
        (name Bar)
        (ast ((
          (name bar)
          (type_ (
            Signal
            (bits    1)
            (rtlname bar)))
          (sequence ())
          (doc (" bar documentation ")))))))
      (sequence ((
        (kind   Array)
        (length 0))))
      (doc ())))|}]
;;
