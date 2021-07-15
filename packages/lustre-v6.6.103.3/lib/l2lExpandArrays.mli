(* Time-stamp: <modified the 18/08/2017 (at 11:04) by Erwan Jahier> *)


(** Expand structures and arrays. Necessary to generate ec code.

    Quite buggy :(

    Requires that nodes are expanded (should not be necessary, but it makes
    things simpler and this pass is only used for generating ec code anyway)
  *)

val doit : LicPrg.t -> LicPrg.t


