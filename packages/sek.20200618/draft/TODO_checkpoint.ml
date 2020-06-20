(**

  New feature: checkpoints, a form of restricted snapshot, providing
  the ability to recover unique ownership of the chunks in the
  sequence despite the fact that they are, at some point, shared between
  the main sequence and its checkpoint.

  The proposed checkpoint mechanism is incompatible with calls to [copy],
  [sub], [concat], [append], [split], [snapshot], [snapshot_and_clear].
  Before calling these functions, the user must call [discard_all s] to
  disable all valid checkpoints associated with [s]. This operation is not
  performed implicitly, because we want the programmer to be well aware that
  these functions invalidate checkpoints, rather than having operations on
  checkpoints that fail further on.

  The functions [copy_without_sharing] and [sub_without_sharing]
  could be provided, however they execute in linear time.

  The implementation of [clear] needs to be refined, to preserve the
  the owner version and the checkpoints. Another function called
  [clear_including_checkpoints] might be provided to discard everything.
  The specification of [assign] might need to be refined.

  The functions [take] and [drop] remain valid.


  The process is as follows:

  1. Create the checkpoint, by calling [r = checkpoint s].
     At this point, schunks are shared between [r] and [s].

     The checkpoint [r] remains untouched until it is either
     exploited by a [restore] operation, or discarded via
     a [discard] operation (see below).

     The sequence [s] can be modified. Modifications to schunks
     shared with [r] may lead to copy-on-write operations
     (for every [set] operation, and every [push] operation
     for which the support chunk cannot be reused.)

  2. At any point, it is possible to restore [s] in the state
     of the checkpoint [r], by calling [restore r s]. Note that
     the current state of [s] is lost.

     Importantly, all the chunks that were uniquely-owned by [s]
     at the time of executing [r = checkpoint s] are still
     uniquely-owned by [s] after the [restore] operation.

  3. At any point, instead of exploiting [restore], it is
     possible to throw away the checkpoint [r], by calling
     [discard r s], and recover, in [s], the unique ownership
     of all the chunks that were uniquely owned by [s] at the
     time of executing [r = checkpoint s].

     Importantly, all the chunks that have been allocated by [s]
     after the creation of the checkpoint [r] are still uniquely
     owned by [s] after the execution of [discard r s].

     It is not mandatory to eventually call [discard] on a checkpoint.
     A checkpoint may be silently dropped on the floor. In this case,
     [s] never gets back the unique ownership of the schunks shared
     with the checkpoint.

  4. At any point, instead of exploiting [restore] or [discard], it is
     possible to convert the checkpoint [r] into a snapshot, by calling
     [s' = snapshot_of_checkpoint r]. At this point, the sequences
     [s] and [s'] may be freely used independently from each other
     (they may share schunks, but in the same way as any other
     sequences.) After converting the checkpoint [r] to a snapshot,
     the checkpoint is invalidated and cannot be used for a [restore].


  Semantics for nested checkpoints:

  1. While working with a sequence [s], it is possible to create
     several checkpoints, [r1], [r2], ... [rn].

  2. It is possible to call [restore ri s] on any checkpoint [ri].
     The checkpoints [rj] with [j > i] are no longer usable, and the
     sequence [s] becomes exclusive owner not just of the schunks
     common with [ri], but also of all the schunks common with [rj]
     with [j > i].
     The checkpoints [rj] with [j < i] are still usable for [restore]
     operations.

  3. It is possible to call [discard] (for recovering full ownership
     of the schunks) only in reverse order, that is, [discard rn s],
     then [discard r_{n-1} s], etc.. up to [discard r1]. Recall that,
     as long as [discard] is not called on a checkpoint [r], the
     schunks shared between [s] and the checkpoint [r] are not
     uniquely-owned by [s].

  3'.For convenience, the function [discard_all s] invokes [discard]
     on all the valid checkpoints for [s].


  About discard: it would make sense to call [discard] on the oldest
  checkpoint. However, this would require a representation of owner
  as a list of version numbers, as opposed to an interval of version
  numbers. For example, if [r1] si a checkpoint a version [v1],
  [r2] a checkpoint at version [v2], and

  Design of the API: for backtracking algorithms, at a decision point
  where several possibilities are to be explored, it is useful to
  create a checkpoint before entering a branch, and restoring to that
  checkpoint upon return from the branch. For all branches but the last
  one, we could provide an optimized function [restore_and_keep_checkpoint r s],
  which is equivalent to [restore r s; let r = checkpoint s], but with
  the benefits that it reuses the allocated object [r], and moreover
  preserves its identity, saving the use of a reference in the code.
  An alternative API would provide the function [checkpoint_in r s] as
  a function that stores a checkpoint into an existing object [r], but
  this seems less practical for the user.

  Design of the API: it would be possible to offer in the API
  [restore r] and [discard r], by storing the pointer on [s] in
  the checkpoint, but this would obfuscate the code by not making
  explicit the modification to the state of [s]. Note that the user
  may implement the alternative API easily on top of the one provided:

    let checkpoint s = (checkpoint s, s)
    let restore (r, s) = restore r s
    let discard (r, s) = discard r s


  Implementation: we do not want to check upon every operation
  whether the sequence has active checkpoints, this would be too
  costly for operations such as [push]. Thus, if [v] is the version
  number of [s] initially, then after [r = checkpoint s], the
  version number of [s] should be set to [v+1]. At this point,
  updates to [s] may create uniquely-owned schunks at version [v+1].
  After [discard r s], the sequence [s] must thus uniquely own
  schunks whose version number is either [v] or [v+1]. Hence, to
  support the proposed feature, we must generalize the notion of
  "owner" from a single integer to a range of integers.

  Implementation: in order to prevent operations such as [copy]
  and [snapshot] to interfere with the checkpoint mechanism, we
  add a field called [last_checkpoint] to the ESek record, and
  use this field to maintain a linked list between a sequence
  and its valid checkpoints. Clearing this list invalidates all
  the checkpoints. It could automatically call the function
  [discard_all s].


*)


(*---------------------------------------------------------------*)
(** Implementation of version numbers with ownership on intervals *)

(** We present a design that allows [is_uniquely_owned] to be
    implemented as a single comparison operation. *)

(* in module Owner *)

(* [Owner.none] is the version number to be stored in schunks created
   directly by persistent sequences *)

let Owner.none = -1

(* [Owner.persistent] is the version number passed to the schunk module
   by the operations from the persistent sequence.

   In SChunk.create, if the argument [o] is [Owner.persistent], then
   the field [owner] of the schunk is set to [Owner.none]. *)

let Owner.persistent = max_int


(* In module ESek: ownership of chunks with version numbers in a range.
   The fields [last_checkpoint] represents the chained list of valid checkpoints.
   Invalidation is achieved by executing [s.last_checkpoint <- None]. *)

type 'a checkpoint = 'a ESek.t (* but abstract type in the API *)

type 'a ESek.t = { ... ;
              owner_low : int;
              owner_high : int;
              last_checkpoint : ('a ESek.checkpoint) option}
  (* Invariants to add to the [check] function:
     - [owner_low <= owner_high]
     - [check_checkpoints s], as defined further below *)

(* Invariants:
   -- [0 <= owner_low <= owner_high] is always true
   -- all schunks stored in the structure have a version number [v]
      such that [v <= owner_high].
   -- all schunks with version number [v] in the range
      [owner_low <= v <= owner_high] are uniquely-owned by the structure.
      All other schunks are considered potentially shared.
   -- for operations on schunks from the middle sequence, the value
      [owner_low] is provided as current version.
   -- Fresh schunks can be created with any version number in the range
      [owner_low...owner_high], it really does not matter. For simplicity,
      we use [owner_low], since this is already the value to be provided
      for operations on schunks from the middle sequence.
*)

(* in module PSek, no change needed, only interpret the owner field from
   the pair as [owner_max]. *)

type 'a PSek.t = { ... ;
                   owner_max : int; }

(** Invariants:
    -- when creating an empty persistent sequence, [owner_max = Owner.none = -1]
    -- all schunks stored in the persistent structure have a version number
       [v] such that [v <= owner_max].
    -- for operations manipulating schunks, the argument [Owner.persistent]
       is provided.

*)

(*---------------------------------------------------------------*)
(** Efficient implementation of [Owner.is_uniquely_owned].
    For ephemeral sequences, [o_of_sequence] is [owner_low].
    For persistent sequences, [o_of_sequence] is [owner_low]. *)

let Owner.is_uniquely_owned o_of_schunk o_of_sequence =
  o_of_sequence <= o_of_schunk

(** Justification:

    The full test for an ephemeral sequence
    [    o_of_chunk <> Owner.none
      && owner_low <= o_of_chunk <= owner_high]
    This test can be simplified to [owner_low <= o_of_chunk], because:
    1) it is necessarily the case that [o_of_chunk <= owner_high]
    since [owner_high] is an upper bound on the version numbers,
    2) we have [owner_low >= 0], so [owner_low <= o_of_chunk]
       implies [o_of_chunk <> Owner.none = -2].

    For a persistent sequence, we have:
    [of_o_sequence = Owner.persistent = max_int], thus the comparison
    [of_o_sequence <= o_of_chunk] always evaluates to false,
    thus schunks are never considered uniquely owned by persistent
    sequences.

*)


(*---------------------------------------------------------------*)
(* Protection for functions that are incompatible with checkpoints *)

let no_checkpoints s =
  s.last_checkpoint = None

let copy (s : 'a ESek.t) : 'a ESek.t =
  assert (no_checkpoints s);
  let owner = s.owner_high + 1 in
  s.owner_low <- owner;
  s.owner_high <- owner;
  { ... ; (* copy all fields from [s], including [last_snapshot], which is [None],
             and [owner_low] and [owner_high], which have been just modified. *)
    owner_low = owner;
    owner_high = owner }

let sub s = (* same for [split], [assign], [snapshot], [snapshot_and_clear]. *)
  assert (no_checkpoints s);
  ...

let concat s1 s2 = (* same for [append] *)
  assert (no_checkpoints s1);
  assert (no_checkpoints s2);
  ...


(*---------------------------------------------------------------*)
(* Implementation of conversion functions *)

let edit (s : 'a PSek.t) : 'a ESek.t =
  let owner = s.owner_max + 1 in
  { ... ;
    owner_low = owner;
    owner_high = owner }

let snapshot (s : 'a ESek.t) : 'a PSek.t =
  { ... ;
    owner_max = s.owner_high }


(*---------------------------------------------------------------*)
(* Sanity check function for the chain of checkpoints *)

let check_checkpoints s = (* mutually recursive with [check] *)
  match s.last_checkpoint with
  | None -> ()
  | Some r ->
     (* The ranges of ownership must be consecutive *)
     assert (r.owner_high + 1 = s.owner_low);
     (* The checkpoint must satisfy all the invariants of an ephemeral sequence,
        with the only exception that there might be shared schunks between
        [r] and [s]. TODO: how to deal with that? *)
     check r
     (* The call to [check r] should recursively invoke [check_checkpoints r]. *)


(*---------------------------------------------------------------*)
(* Implementation of discard operations *)

(* Internal: [is_valid_checkpoint r s] returns [true] if the operation
   [revert r s] or [discard r s] is legitimate. The checkpoint [r] must be
   an ancestor of [s] in the linked list implemented by the [last_checkpoint]
   fields. Its cost is linear, however the cost is amortized because
   every checkpoint created, if traversed by [is_valid_checkpoint], is
   being removed immediately afterwards from the chained list by [discard]. *)

let is_valid_checkpoint r s =
  (* [aux rlast] tests whether [r] belongs to the chain starting from its argument [s] *)
  let rec aux s =
    match s.last_checkpoint with
    | None -> false
    | Some r' -> (r == r') || (aux r')
    in
  aux s


(*---------------------------------------------------------------*)
(* Implementation of checkpoint functions *)

(** [checkpoint s] returns a checkpoint [r], which is a copy of [s],
    then updates the [owner] and [last_checkpoint] fields of [s]. *)

let checkpoint (s : 'a ESek.t) : 'a ESek.chekpoint =
  let r = { ...; (* copy of all [s] fields, including [last_checkpoint], [owner_low], and [owner_high] *) } in
    (* Note: the operation above could share code with [move_out_of], as it is similar but does not [fubar]. *)
  let owner = s.owner_high + 1 in
  s.owner_low <- owner;
  s.owner_high <- owner;
  s.last_checkpoint <- Some r;
  r

(** Exception raised if [discard] or [restore] is invoked on an invalid checkpoint *)

exception InvalidCheckpoint

(** [restore r s] sets [s] to the state [r], that is, to the state that
   [s] had when [r] was built, including with the unique ownership of its schunks. *)

let restore (r : 'a ESek.chekpoint) (s : 'a ESek.t) : unit =
  if not (is_valid_checkpoint r s)
    then raise InvalidCheckpoint;
  assign s r (* copy all field, including [owner_low], [owner_high], and [last_checkpoint]. *)

(** [discard r s] sets [s] to the state [r], that is, to the state that
   [s] had when [r] was built. It checks that [r] is a valid checkpoint
   and invokes [discard] on all the intermediate checkpoints that were
   built from [s] since the creation of [r].

   The operation [discard] is thus implemented recursively, by following
   the chains of [last_checkpoint] fields, and stopping after [r] is discarded.
   The value of [s.last_checkpoint] is then updated to [r.last_checkpoint].

   If [InvalidCheckpoint] is raised, no side effect is performed. *)

let discard (r : 'a ESek.chekpoint) (s : 'a ESek.t) : unit =
  let last = ref s.last_checkpoint in
  (* Note: I initially wrote the code using a recursive function, but it was less clear I found *)
  begin while true do
    match !last with
    | None -> raise InvalidCheckpoint
    | Some r' ->
        if r' != r then last := r'.last_checkpoint else raise Break
  done with Break -> () end;
  (* We reached [r], we recover unique ownership of all schunks created since [r]. *)
  s.owner_low <- r.owner_low;
  (* The remaining valid checkpoints are those created before [r]. *)
  s.last_checkpoint <- r.last_checkpoint

(** [discard_all s] is equivalent to calling [discard r s] on all valid checkpoints
    of [s], from most recent to oldest. *)

let rec discard_all s =
  (* Function [aux] follows the checkpoint chain *)
  let rec aux rlast =
    match rlast.last_checkpoint with
    | Some r -> aux r (* continue following the chain *)
    | None -> (* [rlast] is the last in the checkpoint chain *)
        (* Recover ownership of schunks uniquely owned by [rlast] at its time of creation,
           or allocated afterwards *)
        s.owner_low <- rlast.owner_low;
        (* Clear the checkpoint chain from [s] *)
        s.last_checkpoint <- None
    in
  (* If the chain of checkpoints is nonempty, follow it using [aux] *)
  match s.last_checkpoint with
  | None -> ()
  | Some r -> aux r


(*---------------------------------------------------------------*)
(** LATER Supporting a restricted form of [append] *)

(** The function [append] could be allowed, with some restrictions.
    [append s s'] can be used to extend a sequence [s] with a sequence [s']
    that has been freshly created, without invalidating the snapshots from [s].
    There are two simple cases that seem useful:
    - if [s'] has owner zero/none, then its schunks are transfered to [s] without
      unique ownership.
    - if [s'] has owner equal to that of [s], then its schunks are transfered to
      [s] with unique ownership.
    The latter is more useful, however it requires the possibility to create a
    sequence at a given owner, or the possibility to raise the owner value to
    that of another structure (recall [set_version]).

*)

(*---------------------------------------------------------------*)
(** LATER Supporting [snapshot_of_checkpoint *)

(** An operation that might be useful is, given a sequence [s] and a
    checkpoint [r], to convert [r] into a persistent sequence.
    Doing so necessarily invalidates [r] and all checkpoints prior
    to it. It thus requires updating the linked list made by the
    [last_checkpoint] fields so that it stops before reaching [r]. *)