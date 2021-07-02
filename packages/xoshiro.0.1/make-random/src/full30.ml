(** {1 Full30}

   This module contains one unique functor which, provided a type [state], a
   function [bits] that generates bits from the state and a few functions to
   initialise and manipulate the state, returns a module with the same signature
   as {!Stdlib.Random}. It is basically only a convenience on top of {!State30}
   and {!Init30}.

   This is the 30-bits version: the function [bits] is expected to return an
   [int] whose 30 lower bits {b only} are set, and the function [init] is
   expected to take an array of [int] whose 30 lower bits {b only} are set.
   This version is meant to be used for PRNGs whose state is made of 30-bits
   integers and who generate 30-bits outputs (eg. the one from the standard
   library). *)

external random_seed: unit -> int array = "caml_sys_random_seed"

module Make (B : Bits.Full30) : Sig.Full = struct
  module State = struct
    include State30.Make(B)
    include Init30.Make(B)
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
  let set_state state = B.assign State.default state
end
