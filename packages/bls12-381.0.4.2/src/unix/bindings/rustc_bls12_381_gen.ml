(*****************************************************************************)
(*                                                                           *)
(* Copyright (c) 2020-2021 Danny Willems <be.danny.willems@gmail.com>        *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

let c_headers = "#include \"rustc_bls12_381.h\""

module AllBindings (F : Cstubs.FOREIGN) = struct
  include Rustc_bls12_381_bindings.Fq12 (F)
  include Rustc_bls12_381_bindings.Fr (F)
  include Rustc_bls12_381_bindings.G1 (F)
  include Rustc_bls12_381_bindings.G2 (F)
  include Rustc_bls12_381_bindings.Pairing (F)
end

let () =
  let (ml_filename, c_filename) = (Sys.argv.(1), Sys.argv.(2)) in
  let c_out = open_out_bin c_filename in
  let ml_out = open_out_bin ml_filename in
  let c_formatter = Format.formatter_of_out_channel c_out in
  let ml_formatter = Format.formatter_of_out_channel ml_out in
  (* ML file generation *)
  Format.set_formatter_out_channel ml_out ;
  Cstubs.write_ml
    ml_formatter
    ~errno:Cstubs.ignore_errno
    ~concurrency:Cstubs.sequential
    ~prefix:"caml"
    (module AllBindings) ;
  Format.fprintf c_formatter "%s@\n" c_headers ;
  Cstubs.write_c
    c_formatter
    ~errno:Cstubs.ignore_errno
    ~concurrency:Cstubs.sequential
    ~prefix:"caml"
    (module AllBindings) ;
  close_out ml_out ;
  close_out c_out
