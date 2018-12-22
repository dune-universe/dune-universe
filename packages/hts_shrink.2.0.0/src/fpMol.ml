(* A fingerprint-encoded molecule *)

(* Copyright (C) 2018, Francois Berenger

   Yamanishi laboratory,
   Department of Bioscience and Bioinformatics,
   Faculty of Computer Science and Systems Engineering,
   Kyushu Institute of Technology,
   680-4 Kawazu, Iizuka, Fukuoka, 820-8502, Japan. *)

open Printf

module Fp = Fingerprint
module L = MyList

type t = { name: string; fp: Fp.t }

let create name bitstring =
  { name; fp = Fp.of_sfp_string bitstring }

(* get back to the format it came from *)
let to_string m =
  sprintf "%s,0.0,%s" m.name (Fp.to_string m.fp)

let to_out out m =
  fprintf out "%s\n" (to_string m)

let molecules_to_file fn format_header mols =
  Utls.with_out_file fn (fun out ->
      fprintf out "%s\n" format_header;
      L.iter (to_out out) mols
    )

let parse_one fptype line =
  let name, bitstring = Common.read_one_mol line in
  let fp = match fptype with
    | Fp.MACCSf -> Fp.of_maccs_string bitstring
    | Fp.ECFP4f -> Fp.of_ecfp4_string bitstring
    | Fp.PUBCHf -> Fp.of_pubch_string bitstring
    | Fp.MOP2Df -> Fp.of_sfp_string bitstring
    | Fp.SFPf -> Fp.of_sfp_string bitstring
  in
  { name; fp }

let dist m1 m2 =
  Fp.distance m1.fp m2.fp

let get_name x =
  x.name

let get_fp x =
  x.fp

let get_intmap x =
  Fp.get_intmap x.fp

let is_active x =
  Common.mol_is_active x.name
