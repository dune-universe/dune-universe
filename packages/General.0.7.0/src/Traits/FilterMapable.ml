(* [ToList.|ToArray.|][filter|map|filter_map|flat_map][|_i|_acc] *)

#include "FilterMapable.signatures.ml"

module ToContainer(C: sig type 'a t end) = struct
  #include "FilterMapable.signatures.ToContainer.ml"

  module Tests = struct
    open Testing

    module Make1(M: sig
      type 'a t

      val of_list: 'a list -> 'a t
    end)(ToContainer: sig
      include S1 with type 'a t := 'a M.t
    end)(Container: sig
      val of_list: 'a list -> 'a C.t
      val to_list: 'a C.t -> 'a list
    end) = struct
      open ToContainer

      let test = "FilterMapable.ToContainer" >:: [
        "map" >: (lazy (check_int_list ~expected:[2; 7; 4; 0] ([1; 6; 3; -1] |> M.of_list |> map ~f:((+) 1) |> Container.to_list)));
        "map_i" >: (lazy (check_int_list ~expected:[1; 7; 5; 2] ([1; 6; 3; -1] |> M.of_list |> map_i ~f:(fun ~i x -> i + x) |> Container.to_list)));
        "map_acc" >: (lazy (check_int_list ~expected:[43; 48; 255; 755] ([1; 6; 3; -1] |> M.of_list |> map_acc ~acc:42 ~f:(fun ~acc x -> (acc * x, x + acc)) |> Container.to_list)));
        "filter" >: (lazy (check_int_list ~expected:[3; 15; 9] ([1; 3; 4; 15; 9; 7] |> M.of_list |> filter ~f:(fun x -> x mod 3 = 0) |> Container.to_list)));
        "filter_i" >: (lazy (check_int_list ~expected:[3; 15] ([3; 4; 15; 9; 7] |> M.of_list |> filter_i ~f:(fun ~i x -> i mod 2 = 0 && x mod 3 = 0) |> Container.to_list)));
        "filter_acc" >: (lazy (check_int_list ~expected:[3; 9] ([2; 3; 4; 15; 9; 7] |> M.of_list |> filter_acc ~acc:42 ~f:(fun ~acc x -> (acc + x, acc mod 2 = 0 && x mod 3 = 0)) |> Container.to_list)));
        "filter_map" >: (lazy (check_int_list ~expected:[4; 16; 10] ([1; 3; 4; 15; 9; 7] |> M.of_list |> filter_map ~f:(fun x -> Option.some_if' (x mod 3 = 0) (x + 1)) |> Container.to_list)));
        "filter_map_i" >: (lazy (check_int_list ~expected:[4; 16] ([3; 4; 15; 9; 7] |> M.of_list |> filter_map_i ~f:(fun ~i x -> Option.some_if' (i mod 2 = 0 && x mod 3 = 0) (x + 1)) |> Container.to_list)));
        "filter_map_acc" >: (lazy (check_int_list ~expected:[4; 10] ([2; 3; 4; 15; 9; 7] |> M.of_list |> filter_map_acc ~acc:42 ~f:(fun ~acc x -> (acc + x, Option.some_if' (acc mod 2 = 0 && x mod 3 = 0) (x + 1))) |> Container.to_list)));
        "flat_map" >: (lazy (check_int_list
          ~expected:[1; 2; 4; 3; 6; 9; 4; 8; 12; 16]
          (
            [1; 2; 0; 3; 4]
            |> M.of_list
            |> flat_map ~f:(fun x ->
              IntRange.make x
              |> IntRange.ToList.map ~f:(fun n -> x * (n + 1))
              |> Container.of_list
            )
            |> Container.to_list
          )
        ));
        "flat_map_i" >: (lazy (check_int_list
          ~expected:[1; 3; 6; 6; 12; 18; 8; 16; 24; 32]
          (
            [1; 2; 0; 3; 4]
            |> M.of_list
            |> flat_map_i ~f:(fun ~i x ->
              IntRange.make x
              |> IntRange.ToList.map ~f:(fun n -> (x + i) * (n + 1))
              |> Container.of_list
            )
            |> Container.to_list
          )
        ));
        "flat_map_acc" >: (lazy (check_int_list
          ~expected:[43; 86; 88; 255; 258; 261; 1012; 1016; 1020; 1024]
          (
            [1; 2; 0; 3; 4]
            |> M.of_list
            |> flat_map_acc ~acc:42 ~f:(fun ~acc x ->
              let acc = acc * (x + 1)
              and ys =
                IntRange.make x
                |> IntRange.ToList.map ~f:(fun n -> acc + x * (n + 1))
                |> Container.of_list
              in
              (acc, ys)
            )
            |> Container.to_list
          )
        ));
      ]
    end
  end
end

module ToList = ToContainer(struct type 'a t = 'a list end)

module ToArray = ToContainer(struct type 'a t = 'a array end)

module Tests = struct
  open Testing

  module Make1(M: sig
    include S1

    val to_list: 'a t -> 'a list
    val of_list: 'a list -> 'a t
  end): sig
    val test: Test.t
  end = struct
    module T = ToContainer(M)
    include T.Tests.Make1(M)(M)(M)
  end
end
