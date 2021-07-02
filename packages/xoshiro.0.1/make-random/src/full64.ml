(** {1 Full64}

   This module contains one unique functor which, provided a type [state], a
   function [bits] that generates bits from the state and a few functions to
   initialise and manipulate the state, returns a module with the same signature
   as {!Stdlib.Random}. It is basically only a convenience on top of {!State30}
   and {!Init30}.

   This is the 64-bits version: the function [bits] is expected to return an
   [int64] in which all the bits are set and the function [init] takes an array
   of [int64] in which all the bits are set. This version is meant to be used
   for PRNGs whose state is made of 64-bits integers and who generate 64-bits
   outputs (eg. [xoshiro256++]). *)

external random_seed: unit -> int array = "caml_sys_random_seed"

module Make (B : Bits.Full64) : Sig.Full = struct

  (* The [bits] function that we receive generates 64 bits but we only want to
     provide 30-bits outputs. This could be done by tossing away 34 bits, but we
     can do better: when calling the [bits] function, we take 30 bits from it
     that we return, but we also save 30 bits for the next call. This way, we
     toss only 4 bits. Checking in which case we are is (usually) much faster
     than calling the provided [bits] function, and therefore we save time by
     doing so. *)

  type state =
    { b_state : B.state ;
      mutable second : int }

  (* The state of this functor wraps the given [B.state] in a structure keeping
     track of both the state and the 30 bits that we saved. [second] is either
     negative if there are no bits saved, or positive and containing 30 bits. *)

  (* Out of our 64-bits full interface [B], we generate a 30-bits provider [B30]
     which returns 30 bits at each call but saves 30 bits every time. *)

  module B30 = struct
    type nonrec state = state

    let u30mask = (1 lsl 30) - 1

    let bits state =
      if state.second > 0 then
        (
          let result = state.second in
          state.second <- -1;
          result
        )
      else
        (
          let result = B.bits state.b_state in
          state.second <- Int64.to_int result land u30mask;
          Int64.(to_int (shift_right_logical result 34))
        )
  end

  (* We now lift the state manipulation functions from our 64-bits full
     interface [B] to a state-only interface [B64]. *)

  module B64 = struct
    type nonrec state = state

    let new_state () =
      { b_state = B.new_state ();
        second = -1 }

    let assign state1 state2 =
      B.assign state1.b_state state2.b_state;
      state1.second <- state2.second

    let init_size = B.init_size

    let init state seed =
      B.init state.b_state seed;
      state.second <- -1

    let default_seed = B.default_seed
  end

  (* The functor can now be obtained by deriving the bits providers from [B30]
     and the state-manipulation functions from [B64]. *)

  module State = struct
    include State30.Make(B30)
    include Init64.Make(B64)
  end

  let bits () = State.bits State.default
  let int bound = State.int State.default bound
  let int32 bound = State.int32 State.default bound
  let nativeint bound = State.nativeint State.default bound
  let int64 bound = State.int64 State.default bound
  let float scale = State.float State.default scale
  let bool () = State.bool State.default

  let full_init seed = State.full_init State.default seed
  let init seed = State.full_init State.default [|seed|]
  let self_init () = full_init (random_seed ())

  let get_state () = State.copy State.default
  let set_state state = B64.assign State.default state
end
