open Tables
open Ctypes
open PosixTypes
open Foreign
open Ops
module Bigstring = Core_kernel.Bigstring

type bigstring = Bigstring.t

let add (a : char) (b : char) : char =
  ((int_of_char a) lxor (int_of_char b)) |> char_of_int

let sub (a : char) (b : char) : char =
  ((int_of_char a) lxor (int_of_char b)) |> char_of_int

let mul (a : char) (b : char) : char =
  mul_table.(int_of_char a).[int_of_char b]

let div (a : char) (b : char) : char =
  if      (int_of_char a) = 0 then
    char_of_int 0
  else if (int_of_char b) = 0 then
    failwith "Divisor is 0"
  else (
    let log_a = log_table.[a |> int_of_char] |> int_of_char in
    let log_b = log_table.[b |> int_of_char] |> int_of_char in
    let log_result = ref (log_a - log_b) in
    if !log_result < 0 then
      log_result := !log_result + 255;
    exp_table.[!log_result]
  )

let exp (a : char) (n : int) : char =
  if      n               = 0 then
    1 |> char_of_int
  else if (int_of_char a) = 0 then
    0 |> char_of_int
  else (
    let log_a = log_table.[a |> int_of_char] |> int_of_char in
    let log_result = ref (log_a * n) in
    while 255 <= !log_result do
      log_result := !log_result - 255;
    done;
    exp_table.[!log_result]
  )

let mul_slice_pure_ocaml
    ?(skip_to : int = 0)
    (c        : char)
    (input    : bigstring)
    (out      : bigstring)
  : unit =
  let mt = mul_table.(c |> int_of_char) in

  assert (Bigstring.length input = Bigstring.length out);

  let len = Bigstring.length input in

  for n = skip_to to (len) - 1 do
    out.&{n} <- mt.%[input.&{n}] |> int_of_char;
  done

let mul_slice_xor_pure_ocaml
    ?(skip_to : int = 0)
    (c        : char)
    (input    : bigstring)
    (out      : bigstring)
  : unit =
  let mt = mul_table.(c |> int_of_char) in

  assert (Bigstring.length input = Bigstring.length out);

  let len = Bigstring.length input in

  for n = skip_to to (len) - 1 do
    out.&{n} <- (mt.%[input.&{n}] |> int_of_char) lxor out.&{n};
  done

let slice_xor (input : bigstring) (out : bigstring) : unit =
  assert (Bigstring.length input = Bigstring.length out);

  let len = Bigstring.length input in

  for n = 0 to (len) - 1 do
    out.&{n} <- input.&{n} lxor out.&{n};
  done

let reedsolomon_gal_mul =
  foreign ~release_runtime_lock:true "reedsolomon_gal_mul"
    (ptr uint8_t @-> ptr uint8_t @-> ptr uint8_t @-> ptr uint8_t @-> size_t @-> returning size_t)

let reedsolomon_gal_mul_xor =
  foreign ~release_runtime_lock:true "reedsolomon_gal_mul_xor"
    (ptr uint8_t @-> ptr uint8_t @-> ptr uint8_t @-> ptr uint8_t @-> size_t @-> returning size_t)

let mul_slice (c : char) (input : bigstring) (out : bigstring) =
  let low  = Bigarray.Array2.slice_left mul_table_low  (c |> int_of_char) in
  let high = Bigarray.Array2.slice_left mul_table_high (c |> int_of_char) in

  assert (Bigstring.length input = Bigstring.length out);

  let size = Bigstring.length input in

  let bytes_done =
    (Unsigned.Size_t.to_int
       (reedsolomon_gal_mul
          (coerce (ptr char) (ptr uint8_t) (bigarray_start array1 low))
          (coerce (ptr char) (ptr uint8_t) (bigarray_start array1 high))
          (coerce (ptr char) (ptr uint8_t) (bigarray_start array1 input))
          (coerce (ptr char) (ptr uint8_t) (bigarray_start array1 out))
          (Unsigned.Size_t.of_int size)))
  in

  mul_slice_pure_ocaml ~skip_to:bytes_done c input out

let mul_slice_xor (c : char) (input : bigstring) (out : bigstring) =
  let low  = Bigarray.Array2.slice_left mul_table_low  (c |> int_of_char) in
  let high = Bigarray.Array2.slice_left mul_table_high (c |> int_of_char) in

  assert (Bigstring.length input = Bigstring.length out);

  let size = Bigstring.length input in

  let bytes_done =
    (Unsigned.Size_t.to_int
       (reedsolomon_gal_mul_xor
          (coerce (ptr char) (ptr uint8_t) (bigarray_start array1 low))
          (coerce (ptr char) (ptr uint8_t) (bigarray_start array1 high))
          (coerce (ptr char) (ptr uint8_t) (bigarray_start array1 input))
          (coerce (ptr char) (ptr uint8_t) (bigarray_start array1 out))
          (Unsigned.Size_t.of_int size)))
  in

  mul_slice_xor_pure_ocaml ~skip_to:bytes_done c input out
