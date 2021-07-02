let next x =
  let open Int64 in
  x := add !x 0x9e3779b97f4a7c15L;
  let z = !x in
  let z = mul (logxor z (shift_right_logical z 30)) 0xbf58476d1ce4e5b9L in
	let z = mul (logxor z (shift_right_logical z 27)) 0x94d049bb133111ebL in
	logxor z (shift_right_logical z 31)

include MakeRandom.Full64(struct
    type state = int64 ref
    let bits = next

    let new_state () = ref 0L
    let assign s1 s2 = s1 := !s2

    let init_size = 1
    let init state seed =
      state := seed.(0)

    let default_seed = 135801055
  end)
