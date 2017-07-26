open Core
open Typerep_extended.Std

module Mode = struct
  type sub_types = [
  | `p1_reads_p2
  | `p2_reads_p1
  ] [@@deriving sexp, enumerate]
  type t = [
  | sub_types
  | `both_reads_both
  ] [@@deriving sexp, enumerate]
  let to_string t =
    let s = Sexp.to_string (sexp_of_t t) in
    String.tr s ~target:'_' ~replacement:'-'
  let symbols = List.map all ~f:(fun t ->
    to_string t, t
  )
  let arg = Command.Spec.Arg_type.of_alist_exn symbols
  let doc = String.concat ~sep:"|" (List.map symbols ~f:fst)
end

exception Exn of Sexp.t [@@deriving sexp]

let ok_exn sexp_of_error = function
  | Ok () -> ()
  | Error error -> raise (Exn (sexp_of_error error))

let get_protocols prog =
  let channel = Unix.open_process_in prog in
  let stdout = In_channel.input_all channel in
  ok_exn Unix.Exit_or_signal.sexp_of_error (Unix.close_process_in channel);
  let sexp = Sexp.of_string stdout in
  Protocol_table.Protocols.t_of_sexp sexp

let skip_file string =
  match String.split ~on:'.' string with
  | _ :: "ml" :: tl -> String.concat ~sep:"." tl
  | _ -> string

module Diff_item = struct
  type diff = [
  | `only_defined_in of [ `p1 | `p2 ] * Type_struct.Versioned.t
  | `broken of Type_struct.Diff.t
  ] [@@deriving sexp_of]
  type t = string * diff [@@deriving sexp_of]
end

let main
    p1
    p2
    mode
    ()
    =
  let protocols = Array.map [| p1 ; p2 |] ~f:get_protocols in
  (* for this demo, we will skip the first part of the key, that is the name of the
     file where the type is actually defined. for other context, we might actually use the
     file as a meaningful info *)
  let protocols = Array.map protocols ~f:(fun protocols ->
    List.fold protocols ~init:String.Map.empty ~f:(fun set (key, data) ->
      let key = skip_file key in
      String.Map.add set ~key ~data
    )
  ) in
  (* now it is time to collect the keys defined by each protocol *)
  let iter_keys ~f =
    match mode with
    | `both_reads_both ->
      let set =
        Array.fold protocols ~init:String.Set.empty ~f:(fun acc map ->
          String.Map.fold map ~init:acc ~f:(fun ~key ~data:_ acc -> String.Set.add acc key)
        )
      in
      String.Set.iter ~f set
    | #Mode.sub_types as who_reads_who ->
      let index =
        match who_reads_who with
        | `p1_reads_p2 -> 1
        | `p2_reads_p1 -> 0
      in
      String.Map.iteri protocols.(index) ~f:(fun ~key ~data:_ -> f key)
  in
  let diff_item ty1 ty2 =
    match ty1, ty2 with
    | `Not_defined, `Not_defined -> None
    | `Not_defined, `Defined ty  ->
      if mode = `p2_reads_p1 then None else Some (`only_defined_in (`p2, ty))
    | `Defined ty,  `Not_defined ->
      if mode = `p1_reads_p2 then None else Some (`only_defined_in (`p1, ty))
    | `Defined ty1, `Defined ty2 ->
      let supertype, subtype =
        match mode with
        | `both_reads_both -> ty1, ty2
        | `p1_reads_p2     -> ty1, ty2
        | `p2_reads_p1     -> ty2, ty1
      in
      let diff = Type_struct.Versioned.Diff.compute subtype supertype in
      let diff =
        if mode = `both_reads_both
        then diff
        else Type_struct.Diff.incompatible_changes diff
      in
      if Type_struct.Diff.is_empty diff then None else Some (`broken diff)
  in
  let diffs = ref false in
  iter_keys ~f:(fun key ->
    let tys = Array.map protocols ~f:(fun tys ->
      Protocol_table.Elt.or_not_defined (String.Map.find tys key)
    ) in
    let diff_item = diff_item tys.(0) tys.(1) in
    Option.iter diff_item ~f:(fun diff_item ->
      diffs := true;
      print_endline (Sexp.to_string_hum (Diff_item.sexp_of_t (key, diff_item)))
    )
  );
  if not !diffs then print_endline (sprintf "(Ok %s)" (Mode.to_string mode))
;;

let summary =
  "Diff typerep metadata stored into two programs that wants to communicate via bin_io"
let readme () = "\
This is a simple toy command meant to be used as a demo.
It allows one to retrieve the typerep of types used as protocols via bin_io by 2 programs
and check whether:
 -both can read both
 -p1 can read p2
 -p2 can read p1
"

let command =
  let open Command.Spec in
  let p i = flag
    (sprintf "-p%d" i)
    (required string)
    ~doc:(sprintf "<path/to/prog1> specify the path to the program %d" i)
  in
  let mode () = flag
    "--mode"
    (optional_with_default `both_reads_both Mode.arg)
    ~doc:(sprintf "<%s> specify mode to use during check" Mode.doc)
  in
  Command.basic ~summary ~readme (
    empty
    +> p 1
    +> p 2
    +> mode ()
  ) main

let () = Exn.handle_uncaught ~exit:true (fun () -> Command.run command)
