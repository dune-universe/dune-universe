open! Core
open! Async
open Reddit_api_kernel

val iter_comments
  :  Retry_manager.t
  -> comment_response:Comment_response.t
  -> f:(Thing.Comment.t -> unit Deferred.t)
  -> unit Deferred.t
