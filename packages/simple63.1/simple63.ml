(* Each row in the table below is an encoding scheme. Each
   encoding scheme is associated with an integer tag whose
   (4-bit) value is in the range of [0,15]. An encoding scheme
   is itself a sequence of integers representing the bit widths of
   integers that are encoded within a single 63-bit word.

       +----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
       |                  widths of elements encoded in a word, and their preponderance                    |
 +-----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
 | tag | 59 | 30 | 29 | 20 | 19 | 15 | 14 | 13 | 12 | 11 | 10 |  9 |  8 |  7 |  6 |  5 |  4 |  3 |  2 |  1 |
 +-----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
 |  15 |  1 |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |
 |  14 |    |  1 |  1 |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |
 |  13 |    |    |    |  2 |  1 |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |
 |  12 |    |    |    |    |    | 3  |  1 |    |    |    |    |    |    |    |    |    |    |    |    |    |
 |  11 |    |    |    |    |    |    |    |    |  4 |  1 |    |    |    |    |    |    |    |    |    |    |
 |  10 |    |    |    |    |    |    |    |    |    |    |  5 |  1 |    |    |    |    |    |    |    |    |
 |   9 |    |    |    |    |    |    |    |    |    |    |    |  3 |  4 |    |    |    |    |    |    |    |
 |   8 |    |    |    |    |    |    |    |    |    |    |    |    |  3 |  5 |    |    |    |    |    |    |
 |   7 |    |    |    |    |    |    |    |    |    |    |    |    |    |  5 |  4 |    |    |    |    |    |
 |   6 |    |    |    |    |    |    |    |    |    |    |    |    |    |    |  9 |  1 |    |    |    |    |
 |   5 |    |    |    |    |    |    |    |    |    |    |    |    |    |    |  1 |  9 |  2 |    |    |    |
 |   4 |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |  1 | 12 |  2 |    |    |
 |   3 |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |  2 | 15 |  3 |    |
 |   2 |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |  6 | 20 |  1 |
 |   1 |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    | 20 | 19 |
 |   0 |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    | 59 |
 +-----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+

   Since an OCaml integer consists of 63 (not 64) bits, and a 4 bits tag
   is used to encode the identity of a word's scheme, 59 bits remain
   to encode anywhere from 1 to 59 integers. The following
   table offers a different view of the encoding schemes associated
   with each tag. As with the previous table, each scheme is represented by
   a row in the table:

        +----------------------------------------------------------------------------------------------------------------------------+
        |                                                    sequence length                                                         |
 +------+----------------------------------------------------------------------------------------------------------------------------+
 |      |                        1   1     1         2             2                       3                                       5 |
 |  tag | 1  2  3  4  5  6 7 8 9 0 * 2 * * 5 * * * * 0 * * * * * * 7 * * * * * * * * * * * 9 * * * * * * * * * * * * * * * * * * * 9 |
 +------+----------------------------------------------------------------------------------------------------------------------------+
 |  15  | 59                                                                                                                         |
 |  14  | 30 29                                                                                                                      |
 |  13  | 20 20 19                                                                                                                   |
 |  12  | 15 15 15 14                                                                                                                |
 |  11  | 12 12 12 12 11                                                                                                             |
 |  10  | 10 10 10 10 10 9                                                                                                           |
 |   9  |  9  9  9  8  8 8 8                                                                                                         |
 |   8  |  8  8  8  7  7 7 7 7                                                                                                       |
 |   7  |  7  7  7  7  7 6 6 6 6                                                                                                     |
 |   6  |  6  6  6  6  6 6 6 6 6 5                                                                                                   |
 |   5  |  6  5  5  5  5 5 5 5 5 5 4 4                                                                                               |
 |   4  |  5  4  4  4  4 4 4 4 4 4 4 4 4 3 3                                                                                         |
 |   3  |  4  4  3  3  3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 2                                                                               |
 |   2  |  3  3  3  3  3 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1                                                                 |
 |   1  |  2  2  2  2  2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1                                         |
 |   0  |  1  1  1  1  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 |
 +------+----------------------------------------------------------------------------------------------------------------------------+

   These schemes were designed so that the following property holds:
   Consider a sequence of integers that can plausibly be encoded by a
   scheme whose tag is T. Any prefix of that sequence (some leading
   subsequence) can be encoded by at least one other scheme whose
   tag T' >= T.

   For example, consider a sequence of 5 integers that can be
   encoded by the scheme tag:11. A prefix of length 3 of that sequence
   could be encoded by the scheme tag:12 (whose capacity is 4 integers),
   and the scheme tag:13 (whose capacity is exactly 3 integer). The
   scheme tag:14 is not valid for that prefix, because that scheme can
   only accomodate two integers.

*)

let max_width = 59
let max_value = (1 lsl max_width) - 1

(* [tag_to_config] offers a more compact representation of the
   encoding scheme matrix above. *)
let tag_to_config = [|
  [ 1, max_width]; (* {width * count} *)
  [ 2, 20;  1, 19];
  [ 3,  6;  2, 20; 1, 1];
  [ 4,  2;  3, 15; 2, 3];
  [ 5,  1;  4, 12; 3, 2];
  [ 6,  1;  5,  9; 4, 2];
  [ 6,  9;  5,  1];
  [ 7,  5;  6,  4];
  [ 8,  3;  7,  5];
  [ 9,  3;  8,  4];
  [10,  5;  9,  1];
  [12,  4; 11,  1];
  [15,  3; 14,  1];
  [20,  2; 19,  1];
  [30,  1; 29,  1];
  [max_width,  1]
|]

(* [encoding_matrix.(tag).(len)] returns the bit-width of a the
   [len]-th integer in the encoding scheme associated with
   [tag]. If the sequence supported by that scheme is shorter
   than [len], then [None] is returned. *)
let encoding_matrix =
  Array.map (
    fun config ->
      let row = Array.make (max_width + 1) None in
      let _ = List.fold_left (
        fun i (width, count) ->
          for k = 0 to count - 1 do
            row.(i + k) <- Some width
          done;
          i + count
      ) 1 config in
      row
  ) tag_to_config

(* [tag_to_wdiths] is used in [decode_one_word_iter] and [decode_from_seq]
   *)
let tag_to_widths =
  let rec loop accu = function
    | [] -> List.rev accu
    | (_, 0) :: rest -> loop accu rest
    | (width, count) :: rest -> loop (width :: accu) ((width, count - 1) :: rest)
  in
  Array.map (
    fun config ->
      loop [] config
  ) tag_to_config

(* [tag_to_widths_rev_tl] is used in [encode_elements] *)
let tag_to_widths_rev_tl =
  Array.map (fun ws -> List.tl (List.rev ws)) tag_to_widths


(* [tag_to_len.(tag)] returns the length of the sequence (that is, the
   number of encoded integers) whose encoding scheme is
   associated with [tag] *)
let tag_to_len =
  Array.map (
    fun config ->
      List.fold_left (
        fun len (_, count) ->
          len + count
      ) 0 config
  ) tag_to_config

(* given the length of the integer sequence (that is, the number of
   integers) encoded thus far, and the width of the subsequent
   integer, return the tag of the scheme that can contain the
   augmented sequence, if any *)
let tag_of_width_f =
  let rec loop tag len width =
    if tag = 16 then
      None
    else
      match encoding_matrix.(tag).(len) with
      | None -> None
      | Some max_width ->
        if width <= max_width then
          Some tag
        else
          loop (tag + 1) len width
  in
  fun ~start_tag ~len ~width ->
    loop start_tag (len + 1) width

let tag_of_width =
  let a = Array.init 16 (
    fun start_tag ->
      Array.init max_width (
        fun len ->
          Array.init max_width (
            fun w1 ->
              let width = w1 + 1 in
              tag_of_width_f ~start_tag ~len ~width
          )
      )
  ) in
  fun ~start_tag ~len ~width ->
    if len < max_width && start_tag < 16 then
      a.(start_tag).(len).(width - 1)
    else
      None


(* [tag_of_len len] returns the tag associated with a sequence whose
   length is [len]. Raises [Invalid_argument] when [len] equals [0].
   *)
let tag_of_len =
  let tol = Array.make
 max_width (-1) in
  Array.iteri (
    fun tag len ->
      for k = 0 to len-1 do
        tol.(k) <- tag
      done
  ) tag_to_len;
  fun len ->
    if len < 1 then
      raise (Invalid_argument "tag_of_len")
    else
      tol.(len - 1)

let tag_mask = 15 lsl max_width

(* list [A; B; C] is encoded with A residing in the most significat
   bits and C in the least significant bits. An integer sequence [A;
   B; C] is presented to function [encode_elements] in reverse order
   (as [C; B; A]). [encode_elements] works by first encoding an
   element at the head of [elements_rev] in the least significant
   bits, and them shifts it (and all the previously encoded elements)
   left until the last element in the list [elements_rev] is reached.
   *)
let encode_elements =
  let rec loop widths elements word =
    match widths, elements with
    | [], [element] ->
      word lor element

    | width :: widths_rest, element :: elements_rest ->
      let word = (word lor element) lsl width in
      loop widths_rest elements_rest word

    | _ -> assert false
  in
  fun len elements ->
    let tag = tag_of_len len in
    let widths = tag_to_widths_rev_tl.(tag) in
    let word = loop widths elements 0 in

    (* add (with bitwise or) tag in bits 60-63 *)
    (tag lsl max_width) lor word

let decode_one_word_iter =
  let rec loop f word = function
    | width :: widths_rest ->
      (* mask has [width] [1]'s in the least significant bits, and
         [0]'s everywhere else *)
      let mask = (1 lsl width) - 1 in
      let element = word land mask in
      f element;
      (* discard [element] by shifting [word] [width] bits to the
         right *)
      let word = word lsr width in
      loop f word widths_rest

    | [] -> ()
  in
  fun f word ->
    let tag = (word land tag_mask) lsr max_width in
    let widths = tag_to_widths.(tag) in
    loop f word widths

let decode_from_seq =
  let open Seq in
  let rec decode_one_word word widths_rev in_seq () =
    match widths_rev with
    | width :: widths_rev_rest ->
      (* mask has [width] [1]'s in the least significant bits, and
         [0]'s everywhere else *)
      let mask = (1 lsl width) - 1 in
      let element = word land mask in
      (* discard [element] by shifting [word] [width] bits to the
         right *)
      let word = word lsr width in
      Cons (element, decode_one_word word widths_rev_rest in_seq)

    | [] ->
      start in_seq ()

  and start in_seq =
    match in_seq () with
    | Cons (word, in_seq) ->
      let tag = (word land tag_mask) lsr max_width in
      let widths_rev = tag_to_widths.(tag) in
      decode_one_word word widths_rev in_seq

    | Nil -> fun () -> Nil
  in
  start


(* [width_of_int x] returns the number of bits required to represent
   [x]. That is, it returns the (1-based) index of the most
   significant bit of [x]. The width of [0] is treated specially,
   returning [1]. *)
let width_of_int =
  let rec loop k x =
    if x = 0 then
      k
    else
      loop (k + 1) (x lsr 1)
  in
  fun x ->
    if x = 0 then
      1
    else
      loop 0 x

(* raised when a value is to be encoded is invalid. Valid inputs are
   integers whose bits 60-63 are zero. Invalid inputs therefore
   include all negative integers and positive integers greater than or
   equal to [1 lsl 59] = [576460752303423488] $\approx$ [5.8e17]. *)
exception Invalid of int

let encode_to_seq =
  let open Seq in
  let rec loop start_tag len elements_rev seq () =
    match seq () with
    | Nil ->
      if len = 0 then
        Nil
      else
        (* even though we've exhausted the input sequence, there are
           some trailing elements in [elements_rev] that we may still
           have to encode *)
        backtrack len elements_rev seq

    | Cons (element, seq) ->
      let width = width_of_int element in
      if width > max_width then
        raise (Invalid element)
      else
        match tag_of_width ~start_tag ~len ~width with
        | None ->
          (* by adding [int] whose width is [width], we are unable to
             encode the sequence gathered thus far, we must settle for
             encoding that sequence without the extra element. We
             first put it back into the sequence: *)
          let seq () = Cons (element, seq) in
          backtrack len elements_rev seq

        | Some tag ->
          loop tag (len + 1) (element :: elements_rev) seq ()

  and backtrack len elements_rev seq =
    (* we may not be able to encode all of [elements_rev] in one word,
       as its length may not correspond to a valid encoding
       scheme *)

    (* if the length [len] of [elements_rev] did correspond to a valid
       encoding scheme, then [len] and [len_of_valid_sequence] are
       equal. *)
    let tag_ = tag_of_len len in
    let len_of_valid_sequence = tag_to_len.(tag_) in
    if len_of_valid_sequence = len then
      let word = encode_elements len elements_rev in
      let seq = loop 0 0 [] seq in
      Cons (word, seq)
    else
      (* the length of [elements_rev] does not correspond to a valid
         encoding scheme. We have to trim it by removing some number
         of leading elements in list [elements_rev]. Those elements
         that have to trim are put back into [seq]. *)
      let tag_1 = tag_ + 1 in
      let len_just_right = tag_to_len.(tag_1) in
      let trim_n = len - len_just_right in
      assert (trim_n > 0);
      (* put [trim_n] elements back into [seq], and remove them
         from [elements_rev] *)
      let _, seq, elements = List.fold_left (
        fun (k, seq, elements) element ->
          if k < trim_n then
            k + 1, (fun () -> Cons ( element, seq )), elements
          else
            k + 1, seq, element :: elements
      ) (0, seq, []) elements_rev in
      (* reverse elements, as [encode_elements] expects elements in
         reverse order *)
      let elements_rev = List.rev elements in
      let word = encode_elements len_just_right elements_rev in
      let seq = loop 0 0 [] seq in
      Cons (word, seq)
  in

  fun seq ->
    loop 0 0 [] seq

(* [encode_len] returns the number of 63-bit words into which the
   sequence would be encoded. *)
let encode_len seq =
  Seq.fold_left (fun c _ -> c + 1) 0 (encode_to_seq seq)

type iba = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t

let decode_from_bigarray f ~n ~offset (a:iba) =
  for i = offset to offset + n - 1 do
    let word = a.{i} in
    decode_one_word_iter f word
  done

let encode_to_bigarray seq offset (a:iba) =
  Seq.fold_left (
    fun i word ->
      a.{i} <- word;
      i + 1
  ) offset (encode_to_seq seq)

(* TODO: support big-endian *)
let input_word ch =
  (* We allow the [End_of_file] exception to propagate for all but the
     first byte reads. *)
  let b8 =
    try
      input_byte ch
    with End_of_file ->
      raise Sys.Break
  in
  let b7 = input_byte ch in
  let b6 = input_byte ch in
  let b5 = input_byte ch in
  let b4 = input_byte ch in
  let b3 = input_byte ch in
  let b2 = input_byte ch in
  let b1 = input_byte ch in
  let word = (         b1) lsl 8 in
  let word = (word lor b2) lsl 8 in
  let word = (word lor b3) lsl 8 in
  let word = (word lor b4) lsl 8 in
  let word = (word lor b5) lsl 8 in
  let word = (word lor b6) lsl 8 in
  let word = (word lor b7) lsl 8 in
  let word = (word lor b8) in
  word


(* TODO: support big-endian *)
let output_word word ch =
  let b8 = (word land (0x7f lsl 56)) lsr 56 in (* note: 0x7f, rather than 0xff *)
  let b7 = (word land (0xff lsl 48)) lsr 48 in
  let b6 = (word land (0xff lsl 40)) lsr 40 in
  let b5 = (word land (0xff lsl 32)) lsr 32 in
  let b4 = (word land (0xff lsl 24)) lsr 24 in
  let b3 = (word land (0xff lsl 16)) lsr 16 in
  let b2 = (word land (0xff lsl  8)) lsr  8 in
  let b1 = (word land (0xff       ))        in
  output_byte ch b1;
  output_byte ch b2;
  output_byte ch b3;
  output_byte ch b4;
  output_byte ch b5;
  output_byte ch b6;
  output_byte ch b7;
  output_byte ch b8

let decode_from_channel =
  let rec loop f ch =
    let word = input_word ch in
    decode_one_word_iter f word;
    loop f ch
  in
  fun f ch ->
    try
      loop f ch
    with Sys.Break ->
      ()

let encode_to_channel seq ch =
  Seq.iter (
    fun word ->
      output_word word ch
  ) (encode_to_seq seq)

let%test _ =
  (* the inverse of the CDF of the exponential distribution *)
  let inv_cdf_exponential lambda f =
    -. log (1. -. f) /. lambda
  in

  (* draw random samples of the exponential distribution
     (parameterized by lambda) by uniformly sampling the inverse
     cumulative density function of that distribution *)
  let rand_exponential lambda =
    let f = Random.float 1.0 in
    inv_cdf_exponential lambda f
  in

  let int_trunc_rand_exp lambda =
    min max_value (truncate (rand_exponential lambda))
  in

  (* [gen m n ff] generates a list of [m] elements, where every [n]
     elements a new generating function is creating using [ff] *)
  let gen : int -> int -> (unit -> unit -> int) -> int list =
    let rec loop ff f i j n accu =
      if j = 0 then
        let f = ff () in
        loop ff f i n n accu
      else
      if i = 0 then
        accu
      else
        let x = f () in
        loop ff f (i - 1) (j - 1) n (x :: accu)
    in
    fun m n ff ->
      let f = ff () in
      loop ff f m n n []
  in

  let gen_rand_exponential () =
    let lambda = 1e-5 +. (Random.float 0.1) in
    fun () ->
      int_trunc_rand_exp lambda
  in

  (* synthesize a random sequence of positive integers consisting of
     elements drawn from the exponential distribution, so that small
     values are more prevalent than larger ones. Every [n] elements,
     switch to using a different (randomly generated) parameter
     [lambda]. *)
  let rand_list m n = gen m n gen_rand_exponential in

  let print_lists label_1 list_1 label_2 list_2 =
    Printf.printf "%s=\n%!" label_1;
    List.iter (Printf.printf "%d\n%!") list_1;
    print_newline ();
    Printf.printf "%s=\n" label_2;
    List.iter (Printf.printf "%d\n%!") list_2;
    print_newline ()
  in

  (* memory-map file into a bigarray *)
  let memory_map : string -> iba * int =
    let open Unix in
    let open Bigarray in
    fun path ->
      (* open the file for reading *)
      let fd = openfile path [O_RDONLY] 0o640 in

      let file_size = lseek fd 0 SEEK_END in
      let n_elements = file_size / 8 in

      let vec =
        let shared = false in
        let dims = [| n_elements |] in
        let ga = map_file fd int c_layout shared dims in
        Bigarray.array1_of_genarray ga
      in
      close fd;
      vec, n_elements
  in

  let decode_from_bigarray_to_list vec n_words =
    let y = ref [] in
    decode_from_bigarray ~n:n_words ~offset:0
      (fun el -> y := el :: !y) vec;
    List.rev !y
  in

  let m = 1_000_000 in
  (* every how many samples should we swith to a different value of
     lambda? *)
  let n = 1000 in

  let x = rand_list m n in
  assert (List.length x = m);
  let x_seq = List.to_seq x in

  let n_words = encode_len x_seq in
  let compression_ratio = (float n_words) /. (float m) in
  Printf.printf "compression ratio: %0.3f\n%!" compression_ratio;

  (* encode the input sequence first into another sequence, then
     convert the sequence into a list for easy comparison with the
     input. *)
  let x_from_list = List.of_seq (decode_from_seq (encode_to_seq x_seq)) in

  (* write the sequence to a temporary file using [encode_channel] *)
  let path = Filename.temp_file "test-" ".s63" in
  let ch = open_out_bin path in
  encode_to_channel x_seq ch;
  close_out ch;

  (* read the sequence from the temporary file using [decode_channel] *)
  let ch = open_in_bin path in
  let x_from_channel = ref [] in
  decode_from_channel (fun el -> x_from_channel := el :: !x_from_channel) ch;
  let x_from_channel = List.rev !x_from_channel in

  (* write the sequence to a bigarray, using [encode_bigarray] *)
  let vec : iba = Bigarray.(Array1.create int c_layout n_words) in
  let n_words' = encode_to_bigarray x_seq 0 vec in
  assert (n_words = n_words');

  (* read the sequence form bigarray [vec] using [decode_bigarray] *)
  let x_from_bigarray = decode_from_bigarray_to_list vec n_words in

  (* to make sure that the bigarray memory mappings and the byte
     orderings of the channel write operations yield same result, read
     the file written using [output_byte] using memory mapping *)
  let x_from_bigarray_mmap =
    let vec, n_words = memory_map path in
    decode_from_bigarray_to_list vec n_words
  in

  let b_from_list = x_from_list = x in

  if not b_from_list then
    print_lists "input" x "output-from-list" x_from_list;

  let b_from_channel = x_from_channel = x in

  if not b_from_channel then
    print_lists "input" x "output-from-ch" x_from_channel;

  let b_from_bigarray = x_from_bigarray = x in
  if not b_from_bigarray then
    print_lists "input" x "output-from-bigarray" x_from_bigarray;

  let b_from_bigarray_mmap = x_from_bigarray_mmap = x in
  if not b_from_bigarray then
    print_lists "input" x "output-from-bigarray-mmap" x_from_bigarray_mmap;

  b_from_list && b_from_channel && b_from_bigarray && b_from_bigarray_mmap
