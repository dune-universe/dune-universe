(* File: fftw3.ml

   Objective Caml interface for FFTW.

   Copyright (C) 2005-

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: https://math.umons.ac.be/anum/software/

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation, with the
   special exception on linking described in file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details.
*)

open Bigarray

module type Sig = sig
  type float_elt
    (** Precision of float numbers. *)

  type complex_elt
    (** Precision of complex numbers. *)

  val float : (float, float_elt) Bigarray.kind
  val complex : (Complex.t, complex_elt) Bigarray.kind

  type 'a plan (* Immutable FFTW plan. *)
  type c2c
  type r2c
  type c2r
  type r2r

  type dir = Forward | Backward

  type measure =
    | Estimate
    | Measure
    | Patient
    | Exhaustive

  type r2r_kind =
    | R2HC | HC2R | DHT
    | REDFT00 | REDFT01 | REDFT10 | REDFT11
    | RODFT00 | RODFT01 | RODFT10 | RODFT11

  exception Failure of string

  val exec : 'a plan -> unit

  module Guru : sig

  end


  module Genarray :
  sig
    external create: ('a, 'b) kind -> 'c layout -> int array
      -> ('a, 'b, 'c) Bigarray.Genarray.t
      = "fftw3_ocaml_ba_create"

    type 'l complex_array = (Complex.t, complex_elt, 'l) Bigarray.Genarray.t
    type 'l float_array   = (float, float_elt, 'l) Bigarray.Genarray.t
    type coord = int array

    val dft : dir ->
      ?meas:measure ->
      ?destroy_input:bool -> ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi: coord list ->
      ?ni: coord -> ?ofsi: coord -> ?inci: coord -> 'l complex_array ->
      ?howmanyo: coord list ->
      ?no: coord -> ?ofso: coord -> ?inco: coord -> 'l complex_array
      -> c2c plan

    val r2c : ?meas:measure ->
      ?destroy_input:bool -> ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi: coord list ->
      ?ni: coord -> ?ofsi: coord -> ?inci: coord -> 'l float_array ->
      ?howmanyo: coord list ->
      ?no: coord -> ?ofso: coord -> ?inco: coord -> 'l complex_array
      -> r2c plan

    val c2r : ?meas:measure ->
      ?destroy_input:bool -> ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi: coord list ->
      ?ni: coord -> ?ofsi: coord -> ?inci: coord -> 'l complex_array ->
      ?howmanyo: coord list ->
      ?no: coord -> ?ofso: coord -> ?inco: coord -> 'l float_array
      -> c2r plan

    val r2r : r2r_kind array ->
      ?meas:measure ->
      ?destroy_input:bool -> ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi: coord list ->
      ?ni: coord -> ?ofsi: coord -> ?inci: coord -> 'l float_array ->
      ?howmanyo: coord list ->
      ?no: coord -> ?ofso: coord -> ?inco: coord -> 'l float_array
      -> r2r plan
  end


  module Array1 :
  sig
    val create: ('a, 'b) kind -> 'c layout -> int -> ('a, 'b, 'c) Array1.t
    val of_array : ('a, 'b) kind -> 'c layout -> 'a array -> ('a, 'b, 'c) Array1.t
    type 'l complex_array = (Complex.t, complex_elt, 'l) Array1.t
    type 'l float_array   = (float, float_elt, 'l) Array1.t

    val dft : dir -> ?meas:measure ->
      ?destroy_input:bool -> ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi:int list ->
      ?ni:int -> ?ofsi:int -> ?inci:int -> 'l complex_array ->
      ?howmanyo:int list ->
      ?no:int -> ?ofso:int -> ?inco:int -> 'l complex_array
      -> c2c plan

    val r2c : ?meas:measure ->
      ?destroy_input:bool -> ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi: int list ->
      ?ni: int -> ?ofsi: int -> ?inci: int -> 'l float_array ->
      ?howmanyo: int list ->
      ?no: int -> ?ofso: int -> ?inco: int -> 'l complex_array
      -> r2c plan

    val c2r : ?meas:measure ->
      ?destroy_input:bool -> ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi: int list ->
      ?ni: int -> ?ofsi: int -> ?inci: int -> 'l complex_array ->
      ?howmanyo: int list ->
      ?no: int -> ?ofso: int -> ?inco: int -> 'l float_array
      -> c2r plan

    val r2r : r2r_kind -> ?meas:measure ->
      ?destroy_input:bool -> ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi:int list ->
      ?ni:int -> ?ofsi:int -> ?inci:int -> 'l float_array ->
      ?howmanyo:int list ->
      ?no:int -> ?ofso:int -> ?inco:int -> 'l float_array
      -> r2r plan
  end

  module Array2 :
  sig
    val create: ('a, 'b) kind -> 'c layout -> int -> int -> ('a, 'b, 'c) Array2.t
    type 'l complex_array = (Complex.t, complex_elt, 'l) Array2.t
    type 'l float_array   = (float, float_elt, 'l) Array2.t
    type coord = int * int

    val dft : dir ->
      ?meas:measure ->
      ?destroy_input:bool -> ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi: coord list ->
      ?ni: coord -> ?ofsi: coord -> ?inci: coord -> 'l complex_array ->
      ?howmanyo: coord list ->
      ?no: coord -> ?ofso: coord -> ?inco: coord -> 'l complex_array
      -> c2c plan

    val r2c : ?meas:measure ->
      ?destroy_input:bool -> ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi: coord list ->
      ?ni: coord -> ?ofsi: coord -> ?inci: coord -> 'l float_array ->
      ?howmanyo: coord list ->
      ?no: coord -> ?ofso: coord -> ?inco: coord -> 'l complex_array
      -> r2c plan

    val c2r : ?meas:measure ->
      ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi: coord list ->
      ?ni: coord -> ?ofsi: coord -> ?inci: coord -> 'l complex_array ->
      ?howmanyo: coord list ->
      ?no: coord -> ?ofso: coord -> ?inco: coord -> 'l float_array
      -> c2r plan

    val r2r : r2r_kind * r2r_kind -> ?meas:measure ->
      ?destroy_input:bool -> ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi: coord list ->
      ?ni: coord -> ?ofsi: coord -> ?inci: coord -> 'l float_array ->
      ?howmanyo: coord list ->
      ?no: coord -> ?ofso: coord -> ?inco: coord -> 'l float_array
      -> r2r plan
  end

  module Array3 :
  sig
    val create: ('a, 'b) kind -> 'c layout -> int -> int -> int
      -> ('a, 'b, 'c) Array3.t

    type 'l complex_array = (Complex.t, complex_elt, 'l) Array3.t
    type 'l float_array   = (float, float_elt, 'l) Array3.t
    type coord = int * int * int

    val dft : dir ->
      ?meas:measure ->
      ?destroy_input:bool -> ?unaligned:bool ->
      ?howmany_n: int array ->
      ?howmanyi: coord list ->
      ?ni: coord -> ?ofsi: coord -> ?inci: coord -> 'l complex_array ->
      ?howmanyo: coord list ->
      ?no: coord -> ?ofso: coord -> ?inco: coord -> 'l complex_array
      -> c2c plan

    val r2c : ?meas:measure ->
      ?destroy_input:bool -> ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi: coord list ->
      ?ni: coord -> ?ofsi: coord -> ?inci: coord -> 'l float_array ->
      ?howmanyo: coord list ->
      ?no: coord -> ?ofso: coord -> ?inco: coord -> 'l complex_array
      -> r2c plan

    val c2r : ?meas:measure ->
      ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi: coord list ->
      ?ni: coord -> ?ofsi: coord -> ?inci: coord -> 'l complex_array ->
      ?howmanyo: coord list ->
      ?no: coord -> ?ofso: coord -> ?inco: coord -> 'l float_array
      -> c2r plan

    val r2r : r2r_kind * r2r_kind * r2r_kind ->
      ?meas:measure ->
      ?destroy_input:bool -> ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi: coord list ->
      ?ni: coord -> ?ofsi: coord -> ?inci: coord -> 'l float_array ->
      ?howmanyo: coord list ->
      ?no: coord -> ?ofso: coord -> ?inco: coord -> 'l float_array
      -> r2r plan
  end
end



(** {2 Precision dependent modules}
 ***********************************************************************)

module D = Fftw3D

module S = Fftw3S


module Wisdom =
struct
  external to_file : string -> unit = "fftw3_ocaml_export_wisdom_to_file"

  external to_string : unit -> string = "fftw3_ocaml_export_wisdom_to_string"

  external export : unit -> unit = "fftw3_ocaml_export_wisdom"

  let export write =
    Callback.register "fftw3_wisdom_export" (write: char -> unit);
    export() (* FIXME: is the callback for each char way really efficient??
                It is probably better to export strings to ocaml and have
                an output function -- like Unix.output *)


  external from_system : unit -> unit = "fftw3_ocaml_import_system_wisdom"

  external from_file : string -> unit = "fftw3_ocaml_import_wisdom_from_file"

  external from_string : string -> unit
    = "fftw3_ocaml_import_wisdom_from_string"

  external import : unit -> unit = "fftw3_ocaml_import_wisdom"

  let import read =
    let read_char () = try Char.code(read()) with End_of_file -> -1 (* EOF *) in
    Callback.register "fftw3_wisdom_import" (read_char: unit -> int);
    import()


  external forget : unit -> unit = "fftw3_ocaml_forget_wisdom"

end
