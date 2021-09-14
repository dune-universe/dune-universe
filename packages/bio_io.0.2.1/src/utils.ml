open! Base

let try0 f =
  match f () with
  | exception exn -> Or_error.error "Caught exception" exn Exn.sexp_of_t
  | result -> Or_error.return result

let try1 f a =
  match f a with
  | exception exn -> Or_error.error "Caught exception" exn Exn.sexp_of_t
  | result -> Or_error.return result

(* Run a fold-like function catching errors. *)
let try_fold f_ a ~init ~f =
  match f_ a ~init ~f with
  | exception exn -> Or_error.error "Caught exception" exn Exn.sexp_of_t
  | result -> Or_error.return result

(* Run a map-like function catching errors. Also good for iter-like
   functions. *)
let try_map f_ a ~f =
  match f_ a ~f with
  | exception exn -> Or_error.error "Caught exception" exn Exn.sexp_of_t
  | result -> Or_error.return result
