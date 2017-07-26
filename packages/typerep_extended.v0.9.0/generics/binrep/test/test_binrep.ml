
open Core
open Typerep_experimental.Std

(* NB: in this file only the "safe" versions (i.e. the ml ones) of the reader and writer
   are manually checked.  This is OK not to test the "unsafe" versions because the safe
   versions in reality call the unsafe ones (so the unsafe one are in fact * checked as
   well, although it's not obvious at first.)  *)

let equal = Pervasives.(=)

let%test_module _ = (module struct
  open Bin_prot.Type_class

  let buf =
    Bin_prot.Common.create_buf 65535
  ;;

  let compose_inverts_is_ident writer reader value : bool =
    ignore (writer.write buf ~pos:0 value) ;
    let value' = reader.read buf ~pos_ref:(ref 0) in
    equal value value'
  ;;

  (* ---------------------------------------------------------------------------------- *)
  let check value typerep =
    let `generic writer = Binrep.bin_writer_t typerep in
    let `generic reader = Binrep.bin_reader_t typerep in
    compose_inverts_is_ident writer reader value
    (* we lose the "diff" done in the sexp version, but printing a sexp is easy and
      useful, printing a binary buffer, not so much. *)
  ;;

  let check_untyped value typerep =
    let str = Type_struct.of_typerep typerep in
    let `generic writer = Binrep.Tagged.bin_writer_t str in
    let `generic reader = Binrep.Tagged.bin_reader_t str in
    let untyped_value =
      let `generic converter = Tagged.Of_typed.of_typerep typerep in
      converter value
    in
    assert ( compose_inverts_is_ident writer reader untyped_value )
  ;;

  (* ---------------------------------------------------------------------------------- *)
  let check_reader buffer typerep trusted_reader =
    let `generic reader = Binrep.bin_reader_t typerep in
    equal
      (trusted_reader.read buffer ~pos_ref:(ref 0))
      (reader.read buffer ~pos_ref:(ref 0))
  ;;

  let check_untyped_reader buffer typerep trusted_reader =
    let `generic reader = Binrep.Tagged.bin_reader_t (Type_struct.of_typerep typerep) in
    let `generic to_typed = Tagged.Typed_of.of_typerep typerep in
    assert (
      equal
        (trusted_reader.read buffer ~pos_ref:(ref 0))
        (to_typed (reader.read buffer ~pos_ref:(ref 0)))
    )
  ;;

  (* ---------------------------------------------------------------------------------- *)
  let check_writer value typerep trusted_reader =
    let `generic writer = Binrep.bin_writer_t typerep in
    ignore (writer.write buf ~pos:0 value) ;
    equal value (trusted_reader.read buf ~pos_ref:(ref 0))
  ;;

  let check_untyped_writer value typerep trusted_reader =
    let `generic writer   = Binrep.Tagged.bin_writer_t (Type_struct.of_typerep typerep) in
    let `generic of_typed = Tagged.Of_typed.of_typerep typerep in
    ignore (writer.write buf ~pos:0 (of_typed value)) ;
    assert ( equal value (trusted_reader.read buf ~pos_ref:(ref 0)) )
  ;;

  (* ---------------------------------------------------------------------------------- *)
  let check_untyped_shaper typerep trusted_shape =
    let `generic shaper = Binrep.Tagged.bin_shape_t (Type_struct.of_typerep typerep) in
    [%test_result: Bin_prot.Shape.Canonical.t]
      (Bin_shape_lib.Bin_shape.eval (shaper ()))
      ~expect:(Bin_shape_lib.Bin_shape.eval trusted_shape)
  ;;

  let check_typed_shaper typerep trusted_shape =
    let `generic shaper = Binrep.bin_shape_t typerep in
    [%test_result: Bin_prot.Shape.Canonical.t]
      (Bin_shape_lib.Bin_shape.eval (shaper ()))
      ~expect:(Bin_shape_lib.Bin_shape.eval trusted_shape)
  ;;

  let check_shape typerep trusted_shape =
    check_untyped_shaper typerep trusted_shape;
    check_typed_shaper typerep trusted_shape;
  ;;

  (* ---------------------------------------------------------------------------------- *)
  let test_sequence_typed ~value ~typerep ~trusted_reader ~trusted_writer =
    assert (check value typerep) ;
    ignore (trusted_writer.write buf ~pos:0 value) ;
    assert (check_reader buf typerep trusted_reader) ;
    assert (check_writer value typerep trusted_reader);
  ;;

  let test_sequence_obj_typed ~value ~typerep ~trusted_reader ~trusted_writer =
    let typerep = Type_struct.recreate_dynamically_typerep_for_test typerep in
    test_sequence_typed ~value ~typerep ~trusted_reader ~trusted_writer
  ;;

  let test_sequence_untyped ~value ~typerep ~trusted_reader ~trusted_writer =
    check_untyped value typerep ;

    ignore (trusted_writer.write buf ~pos:0 value) ;
    check_untyped_reader buf typerep trusted_reader;

    check_untyped_writer value typerep trusted_reader
  ;;

  (* ---------------------------------------------------------------------------------- *)
  let full_cycle ~value ~typerep ~trusted_reader ~trusted_writer =
    let str = Type_struct.of_typerep typerep in

    let `generic t_writer = Binrep.bin_writer_t typerep in
    let `generic t_reader = Binrep.bin_reader_t typerep in

    let `generic u_writer = Binrep.Tagged.bin_writer_t str in
    let `generic u_reader = Binrep.Tagged.bin_reader_t str in

    begin
      ignore (trusted_writer.write buf ~pos:0 value) ;

      let read_value = t_reader.read buf ~pos_ref:(ref 0) in

      ignore (t_writer.write buf ~pos:0 read_value) ;

      let read_untyped = u_reader.read buf ~pos_ref:(ref 0) in

      ignore (u_writer.write buf ~pos:0 read_untyped) ;

      let final_read = trusted_reader.read buf ~pos_ref:(ref 0) in

      assert (equal final_read value)
    end

  (* ---------------------------------------------------------------------------------- *)
  let test_sequence ~value ~typerep ~trusted_reader ~trusted_writer ~trusted_shape =
    test_sequence_typed ~value ~typerep ~trusted_reader ~trusted_writer;
    test_sequence_obj_typed ~value ~typerep ~trusted_reader ~trusted_writer;
    test_sequence_untyped ~value ~typerep ~trusted_reader ~trusted_writer;
    full_cycle ~value ~typerep ~trusted_reader ~trusted_writer;
    check_shape typerep trusted_shape;
  ;;

  let%test_unit _ =
    let module M = struct
      type t = int [@@deriving typerep, bin_io]
    end in
    test_sequence
      ~value:5
      ~typerep:M.typerep_of_t
      ~trusted_reader:M.bin_reader_t
      ~trusted_writer:M.bin_writer_t
      ~trusted_shape:M.bin_shape_t
  ;;

  let%test_unit _ =
    let module M = struct
      type t = int32 [@@deriving typerep, bin_io]
    end in
    let value = Int32.of_int_exn 5 in
    test_sequence
      ~value
      ~typerep:M.typerep_of_t
      ~trusted_reader:M.bin_reader_t
      ~trusted_writer:M.bin_writer_t
      ~trusted_shape:M.bin_shape_t
  ;;

  let%test_unit _ =
    let module M = struct
      type t = int64 [@@deriving typerep, bin_io]
    end in
    let value = Int64.of_int_exn 5 in
    test_sequence
      ~value
      ~typerep:M.typerep_of_t
      ~trusted_reader:M.bin_reader_t
      ~trusted_writer:M.bin_writer_t
      ~trusted_shape:M.bin_shape_t
  ;;

  let%test_unit _ =
    let module M = struct
      type t = char [@@deriving typerep, bin_io]
    end in
    let value = 'c' in
    test_sequence
      ~value
      ~typerep:M.typerep_of_t
      ~trusted_reader:M.bin_reader_t
      ~trusted_writer:M.bin_writer_t
      ~trusted_shape:M.bin_shape_t
  ;;

  let%test_unit _ =
    let module M = struct
      type t = float [@@deriving typerep, bin_io]
    end in
    let value = 543.02 in
    test_sequence
      ~value
      ~typerep:M.typerep_of_t
      ~trusted_reader:M.bin_reader_t
      ~trusted_writer:M.bin_writer_t
      ~trusted_shape:M.bin_shape_t
  ;;

  let%test_unit _ =
    let module M = struct
      type t = string [@@deriving typerep, bin_io]
    end in
    let value = "Hello, world!" in
    test_sequence
      ~value
      ~typerep:M.typerep_of_t
      ~trusted_reader:M.bin_reader_t
      ~trusted_writer:M.bin_writer_t
      ~trusted_shape:M.bin_shape_t
  ;;

  let%test_unit _ =
    let module M = struct
      type t = bool [@@deriving typerep, bin_io]
    end in
    let test value =
      test_sequence
        ~value
        ~typerep:M.typerep_of_t
        ~trusted_reader:M.bin_reader_t
        ~trusted_writer:M.bin_writer_t
        ~trusted_shape:M.bin_shape_t
    in
    test true;
    test false
  ;;

  let%test_unit _ =
    let module M = struct
      type t = unit [@@deriving typerep, bin_io]
    end in
    test_sequence
      ~value:()
      ~typerep:M.typerep_of_t
      ~trusted_reader:M.bin_reader_t
      ~trusted_writer:M.bin_writer_t
      ~trusted_shape:M.bin_shape_t
  ;;

  let%test_unit _ =
    let module M = struct
      type 'a t = 'a option [@@deriving typerep, bin_io]
    end in
    let test value =
      let typerep = M.typerep_of_t typerep_of_int in
      let trusted_reader = M.bin_reader_t bin_reader_int in
      let trusted_writer = M.bin_writer_t bin_writer_int in
      let trusted_shape = M.bin_shape_t bin_shape_int in
      test_sequence
        ~value
        ~typerep
        ~trusted_reader
        ~trusted_writer
        ~trusted_shape
    in
    test None;
    test (Some 5)
  ;;

  let%test_unit _ =
    let module M = struct
      type 'a t = 'a list [@@deriving typerep, bin_io]
    end in
    let test value =
      let typerep = M.typerep_of_t typerep_of_int in
      let trusted_reader = M.bin_reader_t bin_reader_int in
      let trusted_writer = M.bin_writer_t bin_writer_int in
      let trusted_shape = M.bin_shape_t bin_shape_int in
      test_sequence
        ~value
        ~typerep
        ~trusted_reader
        ~trusted_writer
        ~trusted_shape
    in
    test [] ;
    test [1;2;6;5;4;3]
  ;;

  let%test_unit _ =
    let module M = struct
      type 'a t = 'a array [@@deriving typerep, bin_io]
    end in
    let test value =
      let typerep = M.typerep_of_t typerep_of_int in
      let trusted_reader = M.bin_reader_t bin_reader_int in
      let trusted_writer = M.bin_writer_t bin_writer_int in
      let trusted_shape = M.bin_shape_t bin_shape_int in
      test_sequence
        ~value
        ~typerep
        ~trusted_reader
        ~trusted_writer
        ~trusted_shape
    in
    test [||] ;
    test [|1;2;6;5;4;3|]
  ;;

  let%test_unit _ =
    let module M = struct
      type 'a t = 'a ref [@@deriving typerep, bin_io]
    end in
    let value = ref 6 in
    let typerep = M.typerep_of_t typerep_of_int in
    let trusted_reader = M.bin_reader_t bin_reader_int in
    let trusted_writer = M.bin_writer_t bin_writer_int in
    let trusted_shape = M.bin_shape_t bin_shape_int in
    test_sequence
      ~value
      ~typerep
      ~trusted_reader
      ~trusted_writer
      ~trusted_shape
  ;;

  let%test_unit _ =
    let module M = struct
      type t = {foo:int; bar:float} [@@deriving typerep, bin_io]
    end in
    let value = { M. foo = 5 ; bar = 43.25 } in
    let typerep = M.typerep_of_t in
    let trusted_reader = M.bin_reader_t in
    let trusted_writer = M.bin_writer_t in
    let trusted_shape = M.bin_shape_t in
    test_sequence
      ~value
      ~typerep
      ~trusted_reader
      ~trusted_writer
      ~trusted_shape
  ;;

  let%test_unit _ =
    let module M = struct
      type 'a t = {foo:'a; bar:float} [@@deriving typerep, bin_io]
    end in
    let value = { M. foo = 5 ; bar = 43.25 } in
    let typerep = M.typerep_of_t typerep_of_int in
    let trusted_reader = M.bin_reader_t bin_reader_int in
    let trusted_writer = M.bin_writer_t bin_writer_int in
    let trusted_shape = M.bin_shape_t bin_shape_int in
    test_sequence
      ~value
      ~typerep
      ~trusted_reader
      ~trusted_writer
      ~trusted_shape
  ;;

  let%test_unit _ =
    let module M = struct
      type ('a, 'b) t = ('a * 'b) [@@deriving typerep, bin_io]
    end in
    let value = (5,45.67) in
    let typerep = M.typerep_of_t typerep_of_int typerep_of_float in
    let trusted_reader = M.bin_reader_t bin_reader_int bin_reader_float in
    let trusted_writer = M.bin_writer_t bin_writer_int bin_writer_float in
    let trusted_shape = M.bin_shape_t bin_shape_int bin_shape_float in
    test_sequence
      ~value
      ~typerep
      ~trusted_reader
      ~trusted_writer
      ~trusted_shape
  ;;

  let%test_unit _ =
    let module M = struct
      type ('a, 'b, 'c) t = ('a * 'b * 'c) [@@deriving typerep, bin_io]
    end in
    let value = (5,45,3.1415) in
    let typerep = M.typerep_of_t typerep_of_int typerep_of_int typerep_of_float in
    let trusted_reader =
      M.bin_reader_t
        bin_reader_int
        bin_reader_int
        bin_reader_float
    in
    let trusted_writer =
      M.bin_writer_t
        bin_writer_int
        bin_writer_int
        bin_writer_float
    in
    let trusted_shape =
      M.bin_shape_t
        bin_shape_int
        bin_shape_int
        bin_shape_float
    in
    test_sequence
      ~value
      ~typerep
      ~trusted_reader
      ~trusted_writer
      ~trusted_shape
  ;;

  let%test_unit _ =
    let module M = struct
      type ('a, 'b, 'c, 'd) t = ('a * 'b * 'c * 'd) [@@deriving typerep, bin_io]
    end in
    let value = (5,45,3.14,42.25) in
    let typerep =
      M.typerep_of_t
        typerep_of_int
        typerep_of_int
        typerep_of_float
        typerep_of_float
    in
    let trusted_reader =
      M.bin_reader_t
        bin_reader_int
        bin_reader_int
        bin_reader_float
        bin_reader_float
    in
    let trusted_writer =
      M.bin_writer_t
        bin_writer_int
        bin_writer_int
        bin_writer_float
        bin_writer_float
    in
    let trusted_shape =
      M.bin_shape_t
        bin_shape_int
        bin_shape_int
        bin_shape_float
        bin_shape_float
    in
    test_sequence
      ~value
      ~typerep
      ~trusted_reader
      ~trusted_writer
      ~trusted_shape
  ;;

  let%test_unit _ =
    let module M = struct
      type ('a, 'b, 'c, 'd, 'e) t = ('a * 'b * 'c * 'd * 'e) [@@deriving typerep, bin_io]
    end in
    let value = (5,45,3.14,42.25,"hi") in
    let typerep =
      M.typerep_of_t
        typerep_of_int
        typerep_of_int
        typerep_of_float
        typerep_of_float
        typerep_of_string
    in
    let trusted_reader =
      M.bin_reader_t
        bin_reader_int
        bin_reader_int
        bin_reader_float
        bin_reader_float
        bin_reader_string
    in
    let trusted_writer =
      M.bin_writer_t
        bin_writer_int
        bin_writer_int
        bin_writer_float
        bin_writer_float
        bin_writer_string
    in
    let trusted_shape =
      M.bin_shape_t
        bin_shape_int
        bin_shape_int
        bin_shape_float
        bin_shape_float
        bin_shape_string
    in
    test_sequence
      ~value
      ~typerep
      ~trusted_reader
      ~trusted_writer
      ~trusted_shape
  ;;

  let%test_unit _ =
    let module M = struct
      type t =
        | Foo
        | Bar of int
        | Baz of int * int
        | Bax of (int * int)
        | Sna of { x : int; y : string }
        [@@deriving typerep, bin_io]
    end in
    let test value =
      test_sequence
        ~value
        ~typerep:M.typerep_of_t
        ~trusted_reader:M.bin_reader_t
        ~trusted_writer:M.bin_writer_t
        ~trusted_shape:M.bin_shape_t
    in
    test M.Foo;
    test (M.Bar  651);
    test (M.Baz (651,54));
    test (M.Bax (651,54));
    test (M.Sna { x = 42; y = "y" });
  ;;

  let%test_unit _ =
    let module A = struct
      type 'a t = Leaf of 'a | Node of 'a t * 'a t [@@deriving typerep, bin_io]
    end in
    let module M = struct
      type t = int A.t [@@deriving typerep, bin_io]

      let rec producer n =
        if n > 0
        then A.Node (producer (n-1), producer (n-1))
        else A.Leaf 0
      ;;

      let value = producer 15
    end in
    if false
    then
      test_sequence
        ~value:M.value
        ~typerep:M.typerep_of_t
        ~trusted_reader:M.bin_reader_t
        ~trusted_writer:M.bin_writer_t
        ~trusted_shape:M.bin_shape_t
  ;;

  let%test_unit _ =
    let module M = struct
      type t = [ `Foo | `Bar of int | `Other of string ] [@@deriving typerep, bin_io]
    end in
    let test value =
      test_sequence
        ~value
        ~typerep:M.typerep_of_t
        ~trusted_reader:M.bin_reader_t
        ~trusted_writer:M.bin_writer_t
        ~trusted_shape:M.bin_shape_t
    in
    test (`Foo);
    test (`Bar 13);
    test (`Other "FOOBAR")
  ;;
end)
