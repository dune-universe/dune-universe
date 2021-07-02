(* We create a fake Random module copying just enough from the standard library.
   We check that calling MakeRandom on it returns indeed the same module
   altogether. *)

module FakeRandom = MakeRandom.Full30(struct
    type state = Random.State.t

    (* We will need to manipulate the internal state of the Random module. So
       here it is, as [actual_state]. We will do some dark [Obj.magic] later. *)

    type actual_state = { st : int array; mutable idx : int }

    let bits = Random.State.bits

    (* [new_state] and [assign] are copied directly from the standard library's
       implementation, with some [Obj.magic] added to let us manipulate the
       actual internal state. *)

    let new_state () =
      (Obj.magic { st = Array.make 55 0; idx = 0 } : state)

    let assign (st1 : state) (st2 : state) =
      let st1 = Obj.magic st1 in
      let st2 = Obj.magic st2 in
      Array.blit st2.st 0 st1.st 0 55;
      st1.idx <- st2.idx

    (* The functions of initialisation of [MakeRandom] has basically been copied
       from the standard library's [full_init] function. Asking [MakeRandom] to
       generate 55 integers and copying the state (similarly to [assign]) should
       therefore do the same thing. *)

    let init_size = 55

    let init (s : state) seed =
      let s = Obj.magic s in
      Array.blit seed 0 s.st 0 55;
      s.idx <- 0

    (* The default state of the standard library is obtained by calling [init
       27182818], which means that using this value as default seed should also
       do the same thing. *)

    let default_seed = 27182818
  end)
;;

(* Now we check that [Random] and [FakeRandom] behave indeed the same. *)

LibSameBits.run_on_full
  "std"  (module Random)
  "fake" (module FakeRandom)
