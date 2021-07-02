module Make (B : Bits.Basic30) : Sig.Basic = struct
  include Full30.Make (struct
      type state = unit

      let new_state () = ()
      let assign () () = ()
      let init_size = 1
      let init () _ = ()
      let default_seed = 0

      let bits = B.bits
    end)
end
