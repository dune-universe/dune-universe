open Linalg
module T = Proptest

module Make_mat_tests
    (N : T.Generic.Name)
    (Ring : Basic_intf.Lang.Ring with type 'a m = 'a)
    (S : Intf.Tensor with type 'a k = 'a and type 'a m = 'a and type pos = int)
    (M : Intf.Mat
           with type 'a k = 'a
            and type 'a m = 'a
            and type elt = Ring.t
            and type 'a shape = 'a S.t
            and type base_index = int)
    (L : sig
      type index = int * int

      val shape : index M.shape
    end)
    (M_gen : T.Generic.Generator with type t = L.index M.t)
    (K : T.Generic.Generator with type t = L.index)
    (Ring_gen : T.Generic.Generator with type t = M.elt) =
struct
  let n s = Printf.sprintf "%s: %s" N.name s

  module Vec_properties =
    Vector_tests.Make_vec_tests (N) (Ring) (S) (M)
      (struct
        type index = int * int

        let length = L.shape
      end)
      (M_gen)
      (K)
      (Ring_gen)

  (* split a matrix in a list of columns and reconcatenate *)
  let () =
    T.Generic.add_test
      (n "split and concat cols = identity")
      Crowbar.(
        map [M_gen.gen] @@ fun mat ->
        let (c, _r) =
          let shape = M.idim mat in
          (S.fst shape, S.snd shape)
        in
        let cols = List.init (S.dim c S.Path.empty) (M.col mat) in
        let cols = List.map M.of_col cols in
        let mat' =
          match cols with
          | hd :: tl -> List.fold_left M.concat_horiz hd tl
          | _ -> assert false
        in
        M_gen.(mat = mat'))

  (* split a matrix in a list of rows and reconcatenate *)
  let () =
    T.Generic.add_test
      (n "split and concat rows = identity")
      Crowbar.(
        map [M_gen.gen] @@ fun mat ->
        let (_c, r) =
          let shape = M.idim mat in
          (S.fst shape, S.snd shape)
        in
        let rows = List.init (S.dim r S.Path.empty) (M.row mat) in
        let rows = List.map M.of_row rows in
        let mat' =
          match rows with
          | hd :: tl -> List.fold_left M.concat_vert hd tl
          | _ -> assert false
        in
        M_gen.(mat = mat'))

  let () =
    T.Generic.add_test
      (n "swap_rows involutive")
      Crowbar.(
        map [M_gen.gen; K.gen; K.gen] @@ fun mat (_, r1) (_, r2) ->
        let mat' = M.swap_rows mat r1 r2 in
        let mat'' = M.swap_rows mat' r1 r2 in
        M_gen.(mat = mat''))

  let () =
    T.Generic.add_test
      (n "swap_cols involutive")
      Crowbar.(
        map [M_gen.gen; K.gen; K.gen] @@ fun mat (c1, _) (c2, _) ->
        let mat' = M.swap_cols mat c1 c2 in
        let mat'' = M.swap_cols mat' c1 c2 in
        M_gen.(mat = mat''))

  let () =
    T.Generic.add_test
      (n "mm id mat = mat")
      Crowbar.(
        map [M_gen.gen] @@ fun mat ->
        let r = S.snd @@ M.idim mat in
        let id = M.identity r in
        M_gen.(M.mm id mat = mat))

  let () =
    T.Generic.add_test
      (n "mm mat id = mat")
      Crowbar.(
        map [M_gen.gen] @@ fun mat ->
        let c = S.fst @@ M.idim mat in
        let id = M.identity c in
        M_gen.(M.mm mat id = mat))
end

module Make_array_backed_tests
    (N : T.Generic.Name)
    (Ring : Basic_intf.Lang.Ring with type 'a m = 'a)
    (S : Intf.Tensor with type 'a k = 'a and type 'a m = 'a and type pos = int)
    (M : Intf.Mat
           with type 'a k = 'a
            and type 'a m = 'a
            and type elt = Ring.t
            and type base_index = int
            and type 'a shape = 'a S.t)
    (A : Basic_intf.Lang.Array
           with type 'a m = 'a
            and type index = M.base_index
            and type elt = Ring.t)
    (L : sig
      val cols : M.base_index

      val rows : M.base_index
    end)
    (Ring_gen : T.Generic.Generator with type t = M.elt) =
struct
  module Array_L = struct
    let length = L.cols * L.rows
  end

  module Array_K = struct
    type t = int

    let gen = Crowbar.range Array_L.length

    let ( = ) = Int.equal
  end

  module BL = Basic_impl.Lang
  module Array_helpers =
    T.Array.Helpers (BL.Empty) (BL.Sequencing) (A) (Array_L) (Array_K)
      (Ring_gen)
  module Array_backed =
    Mat.Array_backed_column_major (BL.Empty) (BL.Codegen) (S) (BL.Bool) (BL.Int)
      (BL.Int)
      (A)
      (BL.Exn)
      (BL.Product)
      (BL.Sequencing)

  let reduce :
      type acc elt.
      (acc -> elt -> acc) -> acc -> ('i S.t, 'i, elt) Intf.vec -> acc =
    fun (type acc) f (init : acc) (Intf.Vec (s, v)) ->
     let module Storage = BL.Make_storage (struct
       type t = acc
     end) in
     S.fold
       (module Storage)
       (fun i acc ->
         let open BL.Sequencing in
         let* vi = v i in
         f acc vi)
       s
       init

  let shape = S.(tensor (rank_one L.cols) (rank_one L.rows))

  module M_gen = struct
    type t = (int * int) M.t

    let gen =
      Crowbar.map [Array_helpers.array_gen] @@ fun a ->
      Array_backed.in_of_array shape a

    let ( = ) v1 v2 = reduce ( && ) true (M.map2 Ring_gen.( = ) v1 v2)
  end

  module K_gen = struct
    type t = int * int

    let gen = Crowbar.(map [range L.cols; range L.rows] @@ fun c r -> (c, r))

    let ( = ) (a, b) (a', b') = Int.equal a a' && Int.equal b b'
  end

  include
    Make_mat_tests (N) (Ring) (S) (M)
      (struct
        type index = int * int

        let shape = shape
      end)
      (M_gen)
      (K_gen)
      (Ring_gen)
end

module Q_array = Vector_tests.Q_array

module Q_tests_square =
  Make_array_backed_tests
    (struct
      let name = "Linalg.Mat (square)"
    end)
    (Basic_impl.Lang.Rational)
    (Tensor.Int)
    (Mat.Rational)
    (Q_array)
    (struct
      let cols = 5

      let rows = 5
    end)
    (T.Generators.Q)

module Q_tests_rect1 =
  Make_array_backed_tests
    (struct
      let name = "Linalg.Mat (3 cols, 5 rows)"
    end)
    (Basic_impl.Lang.Rational)
    (Tensor.Int)
    (Mat.Rational)
    (Q_array)
    (struct
      let cols = 3

      let rows = 5
    end)
    (T.Generators.Q)

module Q_tests_rect2 =
  Make_array_backed_tests
    (struct
      let name = "Linalg.Mat (5 cols, 3 rows)"
    end)
    (Basic_impl.Lang.Rational)
    (Tensor.Int)
    (Mat.Rational)
    (Q_array)
    (struct
      let cols = 5

      let rows = 3
    end)
    (T.Generators.Q)
