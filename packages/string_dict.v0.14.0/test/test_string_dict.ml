open Base
open Expect_test_helpers_core

module Quickcheck = struct
  include Core_kernel.Quickcheck

  module String = Core_kernel.String
end

let%expect_test _ =
  Quickcheck.test (Quickcheck.String.Set.quickcheck_generator Quickcheck.String.quickcheck_generator)
    ~sizes:(Sequence.of_list [0; 1; 2; 3; 4; 5; 10; 100; 1000])
    ~trials:9
    ~f:(fun strings ->
      let assoc = List.mapi (Set.to_list strings) ~f:(fun i s -> (s, i)) in
      let map  = Map.of_alist_exn (module String) assoc in
      let dict = String_dict.of_alist_exn assoc         in
      let inputs =
        List.concat_map (Set.to_list strings) ~f:(fun str ->
          let len = String.length str in
          [ str
          ; str ^ "x"
          ; if len = 0 then
              "foo"
            else
              String.sub str ~pos:0 ~len:(len - 1)
          ])
      in
      (let from_map = Map.to_alist map in
       let from_dict = String_dict.to_alist dict in
       require [%here] ([%compare.equal: (string * int) list] from_map from_dict)
         ~if_false_then_print_s:
           (lazy [%sexp { from_map : (string * int) list
                        ; from_dict : (string * int) list
                        }]));
      List.iter inputs ~f:(fun str ->
        let from_map  = Map.find  map  str in
        let from_dict = String_dict.find dict str in
        require [%here] ([%compare.equal: int option] from_map from_dict)
          ~if_false_then_print_s:
            (lazy [%sexp { from_map  : int option
                         ; from_dict : int option
                         ; map       : int Map.M(String).t
                         }])))
;;


