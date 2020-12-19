open! Core_kernel
open Thing

type t =
  { link : Thing.Link.t
  ; comment_forest : [ `Comment of Comment.t | `More_comments of More_comments.t ] list
  }
[@@deriving sexp]
