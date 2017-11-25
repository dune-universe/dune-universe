(* File: fftw3SD.ml

   Copyright (C) 2006-

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: https://math.umons.ac.be/anum/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

(* FFTW3 interface for Single/Double precision *)

open Bigarray
open Fftw3_utils

type float_elt = floatXX_elt
type complex_elt = complexXX_elt
let float = floatXX
let complex = complexXX

type 'a fftw_plan (* single and double precision plans are different *)

(* Types of plans *)
type c2c
type r2c
type c2r
type r2r

type dir = Forward | Backward
type measure = Estimate | Measure | Patient | Exhaustive
type r2r_kind =
    (* Keep the order in sync with fftw3.h and the test in
       configure.ac (affects code in fftw3SD_stubs.c). *)
  | R2HC | HC2R | DHT
  | REDFT00 | REDFT01 | REDFT10 | REDFT11
  | RODFT00 | RODFT01 | RODFT10 | RODFT11
exception Failure of string             (* Localizing the Failure exn *)

let is_c_layout m =
  (Genarray.layout m = (Obj.magic c_layout : 'a layout))


(** External declarations
 ***********************************************************************)

(* The types for Array1,... can be converted to this at no cost. *)
type 'l complex_array = (Complex.t, complexXX_elt, 'l) Genarray.t
type 'l float_array   = (float, floatXX_elt, 'l) Genarray.t

(* Execution of plans
 ***********************************************************************)

external fftw_exec : 'a fftw_plan -> unit = "fftw_ocaml_execute" [@@noalloc]

external exec_dft : c2c fftw_plan -> 'l complex_array -> 'l complex_array
  -> unit = "fftw_ocaml_execute_dft" [@@noalloc]
external exec_split_dft : c2c fftw_plan -> 'l float_array -> 'l float_array ->
  'l float_array -> 'l float_array -> unit
  = "fftw_ocaml_execute_split_dft" [@@noalloc]

external exec_dft_r2c : r2c fftw_plan -> 'l float_array -> 'l complex_array
  -> unit
  = "fftw_ocaml_execute_dft_r2c" [@@noalloc]
external exec_split_dft_r2c : r2c fftw_plan -> 'l float_array ->
  'l float_array -> 'l float_array -> unit
  = "fftw_ocaml_execute_split_dft_r2c" [@@noalloc]

external exec_dft_c2r : c2r fftw_plan -> 'l complex_array -> 'l float_array
  -> unit
  = "fftw_ocaml_execute_dft_c2r" [@@noalloc]
external exec_split_dft_c2r : c2r fftw_plan -> 'l float_array -> 'l float_array
  -> 'l float_array -> unit
  = "fftw_ocaml_execute_split_dft_c2r" [@@noalloc]

external exec_r2r : r2r fftw_plan -> 'l float_array -> 'l float_array
  -> unit
  = "fftw_ocaml_execute_r2r" [@@noalloc]


(* Creating plans
 ***********************************************************************)

(* BEWARE: wrapper functions are just thin wrappers around their C
   counterpart.  In particular, their arguments must be thought for
   the C layout. *)
external guru_dft :
  (* in *)  'l complex_array ->
  (* out *) 'l complex_array ->
  (* sign (forward/backward) *) int ->
  (* flags (GOOD: they do not use the 32th bit) *) int ->
  (* input offset ([in] as 1D array, C layout) *) int ->
  (* output offset ([out] as 1D array, C layout) *) int ->
  (* n (transform dimensions; its length = transform rank) *) int array ->
  (* istride (same length as [n]) *) int array ->
  (* ostride (same length as [n]) *) int array ->
  (* howmany (multiplicity dimensions; its length=howmany_rank) *) int array ->
  (* howmany input strides (same length as [howmany]) *) int array ->
  (* howmany output strides (same length as [howmany]) *) int array
  -> c2c fftw_plan
  = "fftw_ocaml_guru_dft_bc" "fftw_ocaml_guru_dft"
  (* Wrapper of fftw_plan_guru_dft.  No coherence check is done in the
     C code.  @raise Failure if the plan cannot be created.

     The [istride] and [ostride] parameters can be longer than [n]
     without harm (but only the [Array.length n] first entries will be
     used).  The same applies for the "howmany" parameters. *)

external guru_r2c :
  (* in *) 'l float_array ->
  (* out *) 'l complex_array ->
  (* flags *) int ->
  (* input offset *) int ->
  (* output offset *) int ->
  (* n (transform dimensions) *) int array ->
  (* istride (same length as [n]) *) int array ->
  (* ostride (same length as [n]) *) int array ->
  (* howmany (multiplicity dimensions) *) int array ->
  (* howmany input strides (same length as [howmany]) *) int array ->
  (* howmany output strides (same length as [howmany]) *) int array
  -> r2c fftw_plan
  = "fftw_ocaml_guru_r2c_bc" "fftw_ocaml_guru_r2c"

external guru_c2r :
  (* in *) 'l complex_array ->
  (* out *) 'l float_array ->
  (* flags *) int ->
  (* input offset *) int ->
  (* output offset *) int ->
  (* n (transform LOGICAL dimensions) *) int array ->
  (* istride (same length as [n]) *) int array ->
  (* ostride (same length as [n]) *) int array ->
  (* howmany (multiplicity dimensions) *) int array ->
  (* howmany input strides (same length as [howmany]) *) int array ->
  (* howmany output strides (same length as [howmany]) *) int array
  -> c2r fftw_plan
  = "fftw_ocaml_guru_c2r_bc" "fftw_ocaml_guru_c2r"

external guru_r2r :
  (* in *) 'l float_array ->
  (* out *) 'l float_array ->
  (* kind (same length as [n]) *) r2r_kind array ->
  (* flags *) int ->
  (* input offset *) int ->
  (* output offset *) int ->
  (* n (transform dimensions) *) int array ->
  (* istride (same length as [n]) *) int array ->
  (* ostride (same length as [n]) *) int array ->
  (* howmany (multiplicity dimensions) *) int array ->
  (* howmany input strides (same length as [howmany]) *) int array ->
  (* howmany output strides (same length as [howmany]) *) int array
  -> r2r fftw_plan
  = "fftw_ocaml_guru_r2r_bc" "fftw_ocaml_guru_r2r"


(** Plans on the OCaml side
 ***********************************************************************)

type genarray
external genarray : (_,_,_) Genarray.t -> genarray = "%identity"
(* Since we want the FFT functions to be polymorphic in the layout of
   the arrays, some black magic is unavoidable.  This one way
   conversion is actually safe, it is the use of [genarray] by C
   functions that must be taken care of. *)

type 'a plan = {
  plan: 'a fftw_plan;
  i : genarray; (* hold input array => not freed by GC before the plan *)
  offseto : int; (* output offset; C-stubs *)
  strideo : int array; (* strides; C-stubs *)
  no : int array; (* dimensions *)
  o : genarray; (* output array *)
}

let sign_of_dir = function
  | Forward -> -1
  | Backward -> 1

(* WARNING: keep in sync with fftw3.h *)
let flags meas unaligned ~destroy_input : int =
  let f = match meas with
    | Measure -> 0 (* 0U *)
    | Exhaustive -> 8 (* 1U lsl 3 *)
    | Patient -> 32 (* 1U lsl 5 *)
    | Estimate -> 64 (* 1U lsl 6 *) in
  let f = if unaligned then f lor 2 (* 1U lsl 1 *) else f in
  if destroy_input then f lor 1 (* 1U lsl 0 *) else f lor 16 (* 1U lsl 4 *)


(** {2 Execution of plans}
 ***********************************************************************)

let exec p =
  fftw_exec p.plan

module Guru = struct

  let dft plan i o =
  (* how to check that the arrays conform to the plan specification? *)
  exec_dft plan i o

  let split_dft plan ri ii ro io =
    (* again, how to check conformance with the plan? *)
    exec_split_dft plan ri ii ro io

end

(** {2 Creating plans}
 ***********************************************************************)

module Genarray = struct
  external create: ('a, 'b) Bigarray.kind -> 'c Bigarray.layout ->
    int array -> ('a, 'b, 'c) Bigarray.Genarray.t
    = "fftw3_ocaml_ba_create"

  type 'l complex_array = (Complex.t, complexXX_elt, 'l) Genarray.t
  type 'l float_array   = (float, floatXX_elt, 'l) Genarray.t
  type coord = int array

  (* Layout independent function *)
  let apply name mk_plan hm_n  hmi ?ni ofsi inci i  hmo ?no ofso inco o
      ~logical_dims =
    let make offseti offseto n stridei strideo hm_ni hm_stridei hm_strideo =
      let p = (mk_plan offseti offseto n stridei strideo
                 hm_ni hm_stridei hm_strideo) in
      { plan = p;
        i = genarray i;
        offseto = offseto;
        strideo = strideo;
        no = n;                       (* LOGICAL dims FIXME: what we want? *)
        o = genarray o;
      } in
    (if is_c_layout i then Fftw3_geomC.apply else Fftw3_geomF.apply)
      name make hm_n  hmi ?ni ofsi inci i  hmo ?no ofso inco o ~logical_dims


  let dft_name =  "$FFTW.Genarray.dft"
  let dft dir ?(meas=Measure)
      ?(destroy_input=false) ?(unaligned=false) ?(howmany_n=[| |])
      ?(howmanyi=[]) ?ni ?ofsi ?inci (i: 'l complex_array)
      ?(howmanyo=[]) ?no ?ofso ?inco (o: 'l complex_array) =
    apply dft_name ~logical_dims:Geom.logical_c2c
      (guru_dft i o (sign_of_dir dir) (flags meas unaligned ~destroy_input))
      howmany_n  howmanyi ?ni ofsi inci i  howmanyo ?no ofso inco o

  (* At the moment, in place transforms are not possible but they may
     be if OCaml bug 0004333 is resolved. *)
  let r2c_name = "$FFTW.Genarray.r2c"
  let r2c ?(meas=Measure)
      ?(destroy_input=false) ?(unaligned=false) ?(howmany_n=[| |])
      ?(howmanyi=[]) ?ni ?ofsi ?inci (i: 'l float_array)
      ?(howmanyo=[]) ?no ?ofso ?inco (o: 'l complex_array) =
    apply r2c_name ~logical_dims:Geom.logical_r2c
      (guru_r2c i o (flags meas unaligned ~destroy_input))
      howmany_n  howmanyi ofsi ?ni inci i  howmanyo ?no ofso inco o

  let c2r_name = "$FFTW.Genarray.c2r"
  let c2r ?(meas=Measure)
      ?(destroy_input=true) ?(unaligned=false) ?(howmany_n=[| |])
      ?(howmanyi=[]) ?ni ?ofsi ?inci (i: 'l complex_array)
      ?(howmanyo=[]) ?no ?ofso ?inco (o: 'l float_array) =
    apply c2r_name ~logical_dims:Geom.logical_c2r
      (guru_c2r i o (flags meas unaligned ~destroy_input))
      howmany_n  howmanyi ?ni ofsi inci i  howmanyo ?no ofso inco o

  let r2r_name = "$FFTW.Genarray.r2r"
  let r2r kind ?(meas=Measure)
      ?(destroy_input=false) ?(unaligned=false) ?(howmany_n=[| |])
      ?(howmanyi=[]) ?ni ?ofsi ?inci (i: 'l float_array)
      ?(howmanyo=[]) ?no ?ofso ?inco (o: 'l float_array) =
    (* FIXME: must check [kind] has the right length/order?? *)
    apply r2r_name ~logical_dims:Geom.logical_r2r
      (guru_r2r i o kind (flags meas unaligned ~destroy_input))
      howmany_n  howmanyi ?ni ofsi inci i  howmanyo ?no ofso inco o
end


module Array1 = struct
  external array1_of_ba : ('a,'b,'c) Bigarray.Genarray.t -> ('a,'b,'c) Array1.t
    = "%identity"
    (* We know that the bigarray will have only 1D, convert without check *)

  let create kind layout dim =
    array1_of_ba(Genarray.create kind layout [|dim|])

  let of_array kind layout data =
    let ba = create kind layout (Array.length data) in
    let ofs = if layout = (Obj.magic c_layout : 'a layout) then 0 else 1 in
    for i = 0 to Array.length data - 1 do ba.{i + ofs} <- data.(i) done;
    ba


  type 'l complex_array = (Complex.t, complexXX_elt, 'l) Array1.t
  type 'l float_array   = (float, floatXX_elt, 'l) Array1.t


  let apply name make_plan hm_n  hmi ?ni ofsi inci i  hmo ?no ofso inco o
      ~logical_dims =
    let hmi = List.map (fun v -> [| v |]) hmi in
    let ni = option_map (fun n -> [| n |]) ni in
    let ofsi = option_map (fun n -> [| n |]) ofsi in
    let inci = Some [| inci |] in
    let hmo = List.map (fun v -> [| v |]) hmo in
    let no = option_map (fun n -> [| n |]) no in
    let ofso = option_map (fun n -> [| n |]) ofso in
    let inco = Some [| inco |] in
    Genarray.apply name make_plan
      hm_n  hmi ?ni ofsi inci i  hmo ?no ofso inco o  ~logical_dims

  let dft_name = "$FFTW.Array1.dft"
  let dft dir ?(meas=Measure)
      ?(destroy_input=false) ?(unaligned=false) ?(howmany_n=[| |])
      ?(howmanyi=[]) ?ni ?ofsi ?(inci=1) (i: 'l complex_array)
      ?(howmanyo=[]) ?no ?ofso ?(inco=1) (o: 'l complex_array) =
    let gi = genarray_of_array1 i
    and go = genarray_of_array1 o in
    apply dft_name ~logical_dims:Geom.logical_c2c
      (guru_dft gi go (sign_of_dir dir) (flags meas unaligned ~destroy_input))
      howmany_n  howmanyi ?ni ofsi inci gi howmanyo ?no ofso inco go

  let r2c_name = "$FFTW.Array1.r2c"
  let r2c ?(meas=Measure)
      ?(destroy_input=false) ?(unaligned=false) ?(howmany_n=[| |])
      ?(howmanyi=[]) ?ni ?ofsi ?(inci=1) (i: 'l float_array)
      ?(howmanyo=[]) ?no ?ofso ?(inco=1) (o: 'l complex_array) =
    let gi = genarray_of_array1 i
    and go = genarray_of_array1 o in
    apply r2c_name ~logical_dims:Geom.logical_r2c
      (guru_r2c gi go (flags meas unaligned ~destroy_input))
      howmany_n  howmanyi ?ni ofsi inci gi  howmanyo ?no ofso inco go

  let c2r_name = "$FFTW.Array1.c2r"
  let c2r ?(meas=Measure)
      ?(destroy_input=true) ?(unaligned=false) ?(howmany_n=[| |])
      ?(howmanyi=[]) ?ni ?ofsi ?(inci=1) (i: 'l complex_array)
      ?(howmanyo=[]) ?no ?ofso ?(inco=1) (o: 'l float_array) =
    let gi = genarray_of_array1 i
    and go = genarray_of_array1 o in
    apply c2r_name ~logical_dims:Geom.logical_c2r
      (guru_c2r gi go (flags meas unaligned ~destroy_input))
      howmany_n  howmanyi ?ni ofsi inci gi  howmanyo ?no ofso inco go

  let r2r_name = "$FFTW.Array1.r2r"
  let r2r kind ?(meas=Measure)
      ?(destroy_input=false) ?(unaligned=false) ?(howmany_n=[| |])
      ?(howmanyi=[]) ?ni ?ofsi ?(inci=1) (i: 'l float_array)
      ?(howmanyo=[]) ?no ?ofso ?(inco=1) (o: 'l float_array) =
    let gi = genarray_of_array1 i
    and go = genarray_of_array1 o in
    let kind = [| kind |] in
    apply r2r_name ~logical_dims:Geom.logical_r2r
      (guru_r2r gi go kind (flags meas unaligned ~destroy_input))
      howmany_n  howmanyi ?ni ofsi inci gi howmanyo ?no ofso inco go
end


module Array2 = struct
  external array2_of_ba : ('a,'b,'c) Bigarray.Genarray.t -> ('a,'b,'c) Array2.t
    = "%identity"
    (* BEWARE: only for bigarray with 2D, convert without check *)

  let create kind layout dim1 dim2 =
    array2_of_ba(Genarray.create kind layout [|dim1; dim2|])

  type 'l complex_array = (Complex.t, complexXX_elt, 'l) Array2.t
  type 'l float_array   = (float, floatXX_elt, 'l) Array2.t
  type coord = int * int

  let apply name make_plan hm_n  hmi ?ni ofsi (inci1,inci2) i
      hmo ?no ofso (inco1,inco2) o  ~logical_dims =
    let hmi = List.map (fun (d1,d2) -> [| d1; d2 |]) hmi in
    let ni = option_map (fun (n1,n2) -> [| n1; n2 |]) ni in
    let ofsi = option_map (fun (n1,n2) -> [| n1; n2 |]) ofsi in
    let inci = Some [| inci1; inci2 |] in
    let hmo = List.map (fun (d1,d2) -> [| d1; d2 |]) hmo in
    let no = option_map (fun (n1,n2) -> [| n1; n2 |]) no in
    let ofso = option_map (fun (n1,n2) -> [| n1; n2 |]) ofso in
    let inco = Some [| inco1; inco2 |] in
    Genarray.apply name make_plan
      hm_n  hmi ?ni ofsi inci i  hmo ?no ofso inco o  ~logical_dims

  let dft_name = "$FFTW.Array2.dft"
  let dft dir ?(meas=Measure)
      ?(destroy_input=false) ?(unaligned=false) ?(howmany_n=[| |])
      ?(howmanyi=[]) ?ni ?ofsi ?(inci=(1,1)) (i: 'l complex_array)
      ?(howmanyo=[]) ?no ?ofso ?(inco=(1,1)) (o: 'l complex_array) =
    let gi = genarray_of_array2 i
    and go = genarray_of_array2 o in
    apply dft_name ~logical_dims:Geom.logical_c2c
      (guru_dft gi go (sign_of_dir dir) (flags meas unaligned ~destroy_input))
      howmany_n  howmanyi ?ni ofsi inci gi howmanyo ?no ofso inco go

  let r2c_name = "$FFTW.Array2.r2c"
  let r2c ?(meas=Measure)
      ?(destroy_input=false) ?(unaligned=false) ?(howmany_n=[| |])
      ?(howmanyi=[]) ?ni ?ofsi ?(inci=(1,1)) (i: 'l float_array)
      ?(howmanyo=[]) ?no ?ofso ?(inco=(1,1)) (o: 'l complex_array) =
    let gi = genarray_of_array2 i
    and go = genarray_of_array2 o in
    apply r2c_name ~logical_dims:Geom.logical_r2c
      (guru_r2c gi go (flags meas unaligned ~destroy_input))
      howmany_n  howmanyi ?ni ofsi inci gi howmanyo ?no ofso inco go

  let c2r_name = "$FFTW.Array2.c2r"
  let c2r ?(meas=Measure)
      ?(unaligned=false) ?(howmany_n=[| |])
      ?(howmanyi=[]) ?ni ?ofsi ?(inci=(1,1)) (i: 'l complex_array)
      ?(howmanyo=[]) ?no ?ofso ?(inco=(1,1)) (o: 'l float_array) =
    let gi = genarray_of_array2 i
    and go = genarray_of_array2 o in
    apply c2r_name ~logical_dims:Geom.logical_c2r
      (guru_c2r gi go (flags meas unaligned ~destroy_input:true))
      howmany_n  howmanyi ?ni ofsi inci gi howmanyo ?no ofso inco go

  let r2r_name = "$FFTW.Array2.r2r"
  let r2r (kind1,kind2) ?(meas=Measure)
      ?(destroy_input=false) ?(unaligned=false) ?(howmany_n=[| |])
      ?(howmanyi=[]) ?ni ?ofsi ?(inci=(1,1)) (i: 'l float_array)
      ?(howmanyo=[]) ?no ?ofso ?(inco=(1,1)) (o: 'l float_array) =
    let gi = genarray_of_array2 i
    and go = genarray_of_array2 o in
    let kind = [| kind1; kind2 |] in
    apply r2r_name ~logical_dims:Geom.logical_r2r
      (guru_r2r gi go kind (flags meas unaligned ~destroy_input))
      howmany_n  howmanyi ?ni ofsi inci gi howmanyo ?no ofso inco go
end


module Array3 = struct
  external array3_of_ba : ('a,'b,'c) Bigarray.Genarray.t -> ('a,'b,'c) Array3.t
    = "%identity"
    (* BEWARE: only for bigarray with 3D, convert without check *)

  let create kind layout dim1 dim2 dim3 =
    array3_of_ba(Genarray.create kind layout [|dim1; dim2; dim3|])

  type 'l complex_array = (Complex.t, complexXX_elt, 'l) Array3.t
  type 'l float_array   = (float, floatXX_elt, 'l) Array3.t
  type coord = int * int * int

  let apply name make_plan hm_n  hmi ?ni ofsi (inci1,inci2,inci3) i
      hmo ?no ofso (inco1,inco2,inco3) o  ~logical_dims =
    let hmi = List.map (fun (d1,d2,d3) -> [| d1; d2; d3 |]) hmi in
    let ni = option_map (fun (n1,n2,n3) -> [| n1; n2; n3 |]) ni in
    let ofsi = option_map (fun (n1,n2,n3) -> [| n1; n2; n3 |]) ofsi in
    let inci = Some [| inci1; inci2; inci3 |] in
    let hmo = List.map (fun (d1,d2,d3) -> [| d1; d2; d3 |]) hmo in
    let no = option_map (fun (n1,n2,n3) -> [| n1; n2; n3 |]) no in
    let ofso = option_map (fun (n1,n2,n3) -> [| n1; n2; n3 |]) ofso in
    let inco = Some [| inco1; inco2; inco3 |] in
    Genarray.apply name make_plan
      hm_n  hmi ?ni ofsi inci i  hmo ?no ofso inco o  ~logical_dims

  let dft_name = "$FFTW.Array3.dft"
  let dft dir ?(meas=Measure)
      ?(destroy_input=false) ?(unaligned=false) ?(howmany_n=[| |])
      ?(howmanyi=[]) ?ni ?ofsi ?(inci=(1,1,1)) (i: 'l complex_array)
      ?(howmanyo=[]) ?no ?ofso ?(inco=(1,1,1)) (o: 'l complex_array) =
    let gi = genarray_of_array3 i
    and go = genarray_of_array3 o in
    apply dft_name ~logical_dims:Geom.logical_c2c
      (guru_dft gi go (sign_of_dir dir) (flags meas unaligned ~destroy_input))
      howmany_n  howmanyi ?ni ofsi inci gi howmanyo ?no ofso inco go

  let r2c_name = "$FFTW.Array3.r2c"
  let r2c ?(meas=Measure)
      ?(destroy_input=false) ?(unaligned=false) ?(howmany_n=[| |])
      ?(howmanyi=[]) ?ni ?ofsi ?(inci=(1,1,1)) (i: 'l float_array)
      ?(howmanyo=[]) ?no ?ofso ?(inco=(1,1,1)) (o: 'l complex_array) =
    let gi = genarray_of_array3 i
    and go = genarray_of_array3 o in
    apply r2c_name ~logical_dims:Geom.logical_r2c
      (guru_r2c gi go (flags meas unaligned ~destroy_input))
      howmany_n  howmanyi ?ni ofsi inci gi howmanyo ?no ofso inco go

  let c2r_name = "$FFTW.Array3.c2r"
  let c2r ?(meas=Measure)
      ?(unaligned=false) ?(howmany_n=[| |])
      ?(howmanyi=[]) ?ni ?ofsi ?(inci=(1,1,1)) (i: 'l complex_array)
      ?(howmanyo=[]) ?no ?ofso ?(inco=(1,1,1)) (o: 'l float_array) =
    let gi = genarray_of_array3 i
    and go = genarray_of_array3 o in
    apply c2r_name ~logical_dims:Geom.logical_c2r
      (guru_c2r gi go (flags meas unaligned ~destroy_input:true))
      howmany_n  howmanyi ?ni ofsi inci gi howmanyo ?no ofso inco go

  let r2r_name = "$FFTW.Array3.r2r"
  let r2r (kind1,kind2,kind3) ?(meas=Measure)
      ?(destroy_input=false) ?(unaligned=false) ?(howmany_n=[| |])
      ?(howmanyi=[]) ?ni ?ofsi ?(inci=(1,1,1)) (i: 'l float_array)
      ?(howmanyo=[]) ?no ?ofso ?(inco=(1,1,1)) (o: 'l float_array) =
    let gi = genarray_of_array3 i
    and go = genarray_of_array3 o in
    let kind = [| kind1; kind2; kind3 |] in
    apply r2r_name ~logical_dims:Geom.logical_r2r
      (guru_r2r gi go kind (flags meas unaligned ~destroy_input))
      howmany_n  howmanyi ?ni ofsi inci gi howmanyo ?no ofso inco go
end
