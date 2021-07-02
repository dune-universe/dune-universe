module LowLevel = struct
  type t = int64 array

  let of_int64_array a =
    if Array.length a <> 4 then
      invalid_arg "Xoshiro256plusplus.LowLevel.of_int64_array";
    Array.copy a

  let to_int64_array = Array.copy

  let rotl x k =
    let open Int64 in
    logor (shift_left x k) (shift_right_logical x (64 - k))

  let next s =
    let open Int64 in

    (* const uint64_t result = rotl(s[0] + s[3], 23) + s[0]; *)
    let result = Int64.add (rotl (Int64.add s.(0) s.(3)) 23) s.(0) in

    (* const uint64_t t = s[1] << 17; *)
    let t = shift_left s.(1) 17 in

    (* s[2] ^= s[0]; *)
    s.(2) <- logxor s.(2) s.(0);
    (* s[3] ^= s[1]; *)
    s.(3) <- logxor s.(3) s.(1);
    (* s[1] ^= s[2]; *)
    s.(1) <- logxor s.(1) s.(2);
    (* s[0] ^= s[3]; *)
    s.(0) <- logxor s.(0) s.(3);

    (* s[2] ^= t; *)
    s.(2) <- logxor s.(2) t;

    (* s[3] = rotl(s[3], 45); *)
    s.(3) <- rotl s.(3) 45;

    (* return result; *)
    result

  let jump = [|
    0x180ec6d33cfd0abaL; 0xd5a61266f0c9392cL;
    0xa9582618e03fc9aaL; 0x39abdc4529b1661cL;
  |]

  let jump s =
    let open Int64 in

	  let s0 = ref 0L in
	  let s1 = ref 0L in
	  let s2 = ref 0L in
	  let s3 = ref 0L in

   for i = 0 to 4 - 1 do
     for b = 0 to 64 - 1 do
       if logand jump.(i) (shift_left 1L b) <> 0L then
         (
           s0 := logxor !s0 s.(0);
           s1 := logxor !s1 s.(1);
           s2 := logxor !s2 s.(2);
           s3 := logxor !s3 s.(3)
         );
       ignore (next s)
     done
   done;

   s.(0) <- !s0;
   s.(1) <- !s1;
   s.(2) <- !s2;
   s.(3) <- !s3

	let long_jump = [|
    0x76e15d3efefdcbbfL; 0xc5004e441c522fb3L;
    0x77710069854ee241L; 0x39109bb02acbe635L;
  |]

 let long_jump s =
   let open Int64 in

	 let s0 = ref 0L in
	 let s1 = ref 0L in
	 let s2 = ref 0L in
	 let s3 = ref 0L in

   for i = 0 to 4 - 1 do
     for b = 0 to 64 - 1 do
       if logand long_jump.(i) (shift_left 1L b) <> 0L then
         (
           s0 := logxor !s0 s.(0);
           s1 := logxor !s1 s.(1);
           s2 := logxor !s2 s.(2);
           s3 := logxor !s3 s.(3)
         );
       ignore (next s)
     done
   done;

   s.(0) <- !s0;
   s.(1) <- !s1;
   s.(2) <- !s2;
   s.(3) <- !s3
end

include MakeRandom.Full64(struct
    type state = int64 array
    let bits = LowLevel.next

    let new_state () =
      Array.make 4 Int64.zero

    let assign state1 state2 =
      Array.blit state2 0 state1 0 4

    let init_size = 4
    let init state seed =
      assign state seed

    let default_seed = 135801055
  end)
