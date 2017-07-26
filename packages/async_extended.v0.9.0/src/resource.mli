(**
   A resource that can be [acquire]d to obtain [Handle]s that should be [release]d by the
   user later. Most resources can be acquired multiple times by multiple concurrent async
   jobs. What exactly happens when you [acquire] depends on the specific resource.

   Possible examples of resources:

   - File (acquire opens a file)
   - Tcp endpoints (acquire creates a connection)
   - Entitlements (the motivating use case)
   - Semaphore (acquire is blocking and takes a lock)
   - Slot (like semaphore, but acquire is non-blocking and fails)

   Definitions:

   It may be useful think to think about resources and dependencies between them
   in terms of *activation*s.
   Activation of resource [x] is a pair of events (and the corresponding period of time):
   (i) [x] has been successfully acquired (to obtain a handle [h]) and
   (ii) [Handle.release h] has been called.

   We say that activation [a] is enclosed into activation [b] whenever
   [a] begins before [b] and [a] ends after [b]
*)
open! Core
open! Async_kernel

module Raw : sig
  (**
     Handle is something you get when you [acquire] a resource and you use the handle to
     release the resource.

     Handle lifetime corresponds to a particular resource activation.
  *)
  module Handle : sig
    (**
       This type keeps a clean-up action, that has to be executed exactly once.
    *)
    type t

    (* The normal way to get a handle is to acquire a resource. This is a low-level
       function that can be used when defining a resource using [Resource.create]. *)
    val create : release:(unit -> unit Deferred.t) -> t

    (** Runs the clean-up action unless it has already been started.
        The result becomes determined whenever the cleanup action completes. *)
    val release : t -> unit Deferred.t

    (* Checks that a handle is released before being GCed

       If you release [check ~alert h] then [h] will also be released.
       If you lose the reference to [check ~alert h] without releasing it
       then [alert] will get called.
    *)
    val check : alert:(unit -> unit) -> t -> t
  end

  type ('a, 'e) t

  (**
     [acquire] and [Handle.release] are the low-level interface to accessing the resource.
     These should be used carefully: to make sure you do not resource leak you need to
     call [Handle.release] at least once on every handle returned by [acquire]. To ensure
     safety you also must stop using the ['a] after having called [release] on a
     corresponding handle.

     [create] is the low-level way of creating resources.
     Think of [create] and [acquire] as inverses: [create] wraps the resource
     implementation and [acquire] exposes it:
     1. [acquire (create f) = f ()]
     2. [create (fun () -> acquire r) = r]
  *)
  val create : (unit -> ('a * Handle.t, 'e) Result.t Deferred.t) -> ('a, 'e) t
  val acquire : ('a, 'e) t -> ('a * Handle.t, 'e) Result.t Deferred.t
end

type ('a, 'e) t = ('a, 'e) Raw.t

val create :
  acquire:(unit -> ('a, 'e) Result.t Deferred.t)
  -> release:('a -> unit Deferred.t)
  -> ('a, 'e) t

(**
   Access a resource without having to deal with Handle.t explicitly.
   The resource is acquired before [f] is called and released after [f] returns a result
   or raises an error to the enclosing monitor.

   [f] should not use ['a] after it raises or returns, whichever happens first.
*)
val with_ : ('a, 'e) t -> f:('a -> 'b Deferred.t) -> ('b, 'e) Result.t Deferred.t

(** Bind corresponds to resource dependency:
    when acquiring [x >>= f], resource [x] will be acquired, then [f] is going to
    be evaluated, then the result of [f] is going to be acquired.
    Releases will be done in the opposite order.
*)
include Monad.S2 with type ('a, 'e) t := ('a, 'e) t

val map_error : ('a, 'e1) t -> f:('e1 -> 'e2) -> ('a, 'e2) t
val fail : 'e -> ('a, 'e) t

(**
   The idea is the following:
   If you try to acquire a shared resource that's already been acquired,
   but not yet released then, instead of acquiring it again, you use the value acquired
   earlier. You only release the underlying resource when all handles to the shared
   resource get released.

   More precisely, if [y = shared x] and [x] is exclusively used here then:
   (i) every activation of [y] is enclosed into an activation of [x];
   (ii) at any time there is at most one activation of [x];
   (iii) activations of [x] are as short as possible otherwise.

    Beware [shared] is not referentially transparent in that [acquire (shared x)] followed
    by [acquire (shared x)] will acquire [x] twice, so you always want to bind the result
    to a variable: [let y = shared x in ... (* acquire y multiple times *)]

    As an example of what you can do with this, in [dart] library [shared] lets us:
    - Coalesce multiple requests for the same entitlement by the same user into one.
    - Only establish up to one connection to dart server per user.

    In general, it might be helpful when:
    - If acquiring the original resource is costly;
    - If acquiring [x] multiple times concurrently is not safe;
    - If multiple acquirings of [x] would block each other;
    - <your idea>.
*)
val shared : ('a, 'e) t -> ('a, 'e) t

module Memo (Key : Hashable) : sig
  (**
     [memo f] is similar to [Core.Memo.general (fun k -> shared (f k))].  The
     important difference is that the memo entry is only kept while the resource is
     acquired. In particular, once all handles of a particular key get released the memo
     entry gets forgotten and [f] gets applied again on next acquisition.  If [acquire]
     fails or function raises for a particular key, the result is not cached. *)
  val memo : (Key.t -> ('a, 'e) t) -> (Key.t -> ('a, 'e) t)
end

(** Delay all [release]s by the given amount and don't wait for them *)
val delayed_release : ('a, 'e) t -> delay:Time.Span.t -> ('a, 'e) t
