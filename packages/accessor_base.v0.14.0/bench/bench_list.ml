open! Core
open! Import

let%bench_fun ("Accessor.iter List.each"[@indexed len = [ 1; 10; 1000; 10000 ]]) =
  let xs = List.init len ~f:Fn.id in
  fun () -> Accessor.iter Accessor.List.each xs ~f:ignore
;;

let%bench_fun ("Accessor.map List.each"[@indexed len = [ 1; 10; 1000; 10000 ]]) =
  let xs = List.init len ~f:Fn.id in
  fun () -> Accessor.map Accessor.List.each xs ~f:ignore
;;
