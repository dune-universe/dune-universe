open Core
open Util

module type UnfixedHashFunction = sig
  module Arg : Data
  module Key : Hashtbl.Key
  module Result : Data
  val f : (Arg.t -> Result.t) unfixed
  val extract_key : Arg.t -> Key.t
end

module FixHashMemoizerOf(F:UnfixedHashFunction) = struct
  let result_storage : (F.Key.t,F.Result.t) Hashtbl.t = Hashtbl.create (module F.Key)

  let rec evaluate
      (x:F.Arg.t)
    : F.Result.t =
    let key = F.extract_key x in
    begin match Hashtbl.find result_storage key with
      | None ->
        let y = F.f evaluate x in
        Hashtbl.add_exn ~key:key ~data:y result_storage;
        y
      | Some y -> y
    end
end

