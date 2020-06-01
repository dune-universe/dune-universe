open! Core_kernel
open! Import

let year_exn =
  [%accessor
    Accessor.field ~get:Date.year ~set:(fun date year ->
      Date.create_exn ~y:year ~m:(Date.month date) ~d:(Date.day date))]
;;

let month_exn =
  [%accessor
    Accessor.field ~get:Date.month ~set:(fun date month ->
      Date.create_exn ~y:(Date.year date) ~m:month ~d:(Date.day date))]
;;

let day_exn =
  [%accessor
    Accessor.field ~get:Date.day ~set:(fun date day ->
      Date.create_exn ~y:(Date.year date) ~m:(Date.month date) ~d:day)]
;;
