open! Core_kernel
open! Import

let mapperi accessor at sexp_of_bt ~f =
  print_s [%message "mapi" ~_:(Accessor.mapi accessor at ~f : bt)]
;;

let mapper accessor at sexp_of_bt ~f =
  print_s [%message "map" ~_:(Accessor.map accessor at ~f : bt)]
;;

let many_getteri accessor at sexp_of_i sexp_of_a =
  print_s
    [%message
      "to_listi" ~_:(Accessor.to_listi accessor at : (i Accessor.Index.t * a) list)];
  let queue = Queue.create () in
  Accessor.iteri accessor at ~f:(fun i a -> Queue.enqueue queue (i, a));
  print_s [%message "iteri" ~_:(queue : (i Accessor.Index.t * a) Queue.t)]
;;

let many_getter accessor at sexp_of_a =
  print_s [%message "to_list" ~_:(Accessor.to_list accessor at : a list)];
  let queue = Queue.create () in
  Accessor.iter accessor at ~f:(Queue.enqueue queue);
  print_s [%message "iter" ~_:(queue : a Queue.t)]
;;

let manyi accessor at sexp_of_i sexp_of_a sexp_of_bt ~f =
  mapperi accessor at sexp_of_bt ~f;
  many_getteri accessor at sexp_of_i sexp_of_a
;;

let many accessor at sexp_of_a sexp_of_bt ~f =
  mapper accessor at sexp_of_bt ~f;
  many_getter accessor at sexp_of_a
;;

let getteri accessor at sexp_of_i sexp_of_a =
  print_s [%message "geti" ~_:(Accessor.geti accessor at : i Accessor.Index.t * a)]
;;

let getter accessor at sexp_of_a = print_s [%message "get" ~_:(at.@(accessor) : a)]

let field accessor at sexp_of_a sexp_of_bt ~f =
  many accessor at sexp_of_a sexp_of_bt ~f;
  getter accessor at sexp_of_a
;;

let fieldi accessor at sexp_of_i sexp_of_a sexp_of_bt ~f =
  manyi accessor at sexp_of_i sexp_of_a sexp_of_bt ~f;
  getteri accessor at sexp_of_i sexp_of_a
;;

let constructor accessor b sexp_of_bt =
  print_s [%message "construct" ~_:(Accessor.construct accessor b : bt)]
;;

let optional_getter accessor at sexp_of_a =
  print_s [%message "get_option" ~_:(Accessor.get_option accessor at : a option)];
  many_getter accessor at sexp_of_a
;;

let optional accessor at sexp_of_a sexp_of_bt ~f =
  optional_getter accessor at sexp_of_a;
  many accessor at sexp_of_a sexp_of_bt ~f
;;

let variant accessor b at sexp_of_a sexp_of_bt ~f =
  optional accessor at sexp_of_a sexp_of_bt ~f;
  constructor accessor b sexp_of_bt
;;

let isomorphism accessor b at sexp_of_a sexp_of_bt ~f =
  field accessor at sexp_of_a sexp_of_bt ~f;
  variant accessor b at sexp_of_a sexp_of_bt ~f
;;
