(* Copyright (C) 2018, Francois Berenger

   Yamanishi laboratory,
   Department of Bioscience and Bioinformatics,
   Faculty of Computer Science and Systems Engineering,
   Kyushu Institute of Technology,
   680-4 Kawazu, Iizuka, Fukuoka, 820-8502, Japan. *)

module IntMap = MyIntMap
module L = MyList
module Log = Dolog.Log
module Fp = Fingerprint

let read_one input =
  FpMol.parse_one Fp.SFPf (input_line input)

let from_file fn =
  let count = ref 0 in
  let nb_features, mols =
    Utls.with_in_file fn (fun input ->
        let radius, index_fn = Mop2d_env.parse_comment input in
        let radius', mop2d_index = Mop2d_env.restore_mop2d_index index_fn in
        let nb_features = Hashtbl.length mop2d_index in
        assert(radius = radius');
        let res, exn =
          L.unfold_exc (fun () ->
              let res = read_one input in
              incr count;
              res
            ) in
        if exn <> End_of_file then raise exn;
        (nb_features, res)
      ) in
  Log.info "read %d from %s" !count fn;
  (nb_features, mols)
