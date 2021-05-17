open! Core_kernel

module U = Utils

(* Some of these we could define with some of the other functions
   directly, but to keep it consistent, all non [_exn] functions are
   defined in terms of their [exn] function. *)

type t = In_channel.t

let peek_char chan =
  match In_channel.input_char chan with
  | None -> None
  | Some c ->
      let pos = In_channel.pos chan in
      let () = assert (Int64.(pos > of_int 0)) in
      let () = In_channel.seek chan Int64.(pos - of_int 1) in
      Some c

let rewind ?(pos = Int64.of_int 0) chan = In_channel.seek chan pos

let equal c1 c2 = In_channel.equal c1 c2

exception Exn of string [@@deriving sexp]

let clean_sequence s =
  s |> String.strip |> String.substr_replace_all ~pattern:" " ~with_:""

let of_in_channel chan = chan
let to_in_channel chan = chan

let create_exn fname = In_channel.create fname
let create fname = U.try1 create_exn fname

let close_exn chan = In_channel.close chan
let close chan = U.try1 close_exn chan

let stdin_exn () = In_channel.stdin
let stdin () = U.try0 stdin_exn

let input_record_exn chan =
  let rec loop record =
    match (peek_char chan, record) with
    | None, None -> None
    | None, Some record' -> Some record'
    | Some '>', None ->
        let line = In_channel.input_line_exn ~fix_win_eol:true chan in
        loop (Some (Fasta_record.of_header_exn line))
    | Some '>', Some record' -> Some record'
    | Some _, None ->
        raise (Exn "Not at a header line, but not currently in a sequence")
    | Some _, Some record' ->
        let line = In_channel.input_line_exn ~fix_win_eol:true chan in
        let seq_part = clean_sequence line in
        let new_seq = Fasta_record.seq record' ^ seq_part in
        loop (Some (Fasta_record.with_seq new_seq record'))
  in
  loop None

let input_record chan = U.try1 input_record_exn chan

let fold_records_exn chan ~init ~f =
  let rec loop acc record =
    match record with
    | None -> acc
    | Some record' -> loop (f acc record') (input_record_exn chan)
  in
  loop init (input_record_exn chan)

let fold_records chan ~init ~f = U.try_fold fold_records_exn chan ~init ~f

let records_exn chan =
  List.rev
    (fold_records_exn chan ~init:[] ~f:(fun records record -> record :: records))

let records chan = U.try1 records_exn chan

let foldi_records_exn chan ~init ~f =
  snd
    (fold_records_exn chan ~init:(0, init) ~f:(fun (i, acc) record ->
         (i + 1, f i acc record)))

let foldi_records chan ~init ~f = U.try_fold foldi_records_exn chan ~init ~f

let iter_records_exn chan ~f =
  fold_records_exn chan ~init:() ~f:(fun () record -> f record)
let iter_records chan ~f =
  fold_records chan ~init:() ~f:(fun () record -> f record)

let iteri_records_exn chan ~f =
  foldi_records_exn chan ~init:() ~f:(fun i () record -> f i record)
let iteri_records chan ~f = U.try_map iteri_records_exn chan ~f

let with_file_exn fname ~f = In_channel.with_file fname ~f
let with_file fname ~f = U.try_map with_file_exn fname ~f

(* These are the with file versions of the above. *)

let with_file_records_exn fname = with_file_exn fname ~f:records_exn
let with_file_records fname = U.try1 with_file_records_exn fname

let with_file_fold_records_exn fname ~init ~f =
  with_file_exn fname ~f:(fun chan -> fold_records_exn chan ~init ~f)
let with_file_fold_records fname ~init ~f =
  U.try_fold with_file_fold_records_exn fname ~init ~f

let with_file_foldi_records_exn fname ~init ~f =
  with_file_exn fname ~f:(fun chan -> foldi_records_exn chan ~init ~f)
let with_file_foldi_records fname ~init ~f =
  U.try_fold with_file_foldi_records_exn fname ~init ~f

let with_file_iter_records_exn fname ~f =
  with_file_exn fname ~f:(fun chan -> iter_records_exn chan ~f)
let with_file_iter_records fname ~f =
  U.try_map with_file_iter_records_exn fname ~f

let with_file_iteri_records_exn fname ~f =
  with_file_exn fname ~f:(fun chan -> iteri_records_exn chan ~f)
let with_file_iteri_records fname ~f =
  U.try_map with_file_iteri_records_exn fname ~f

(* Sequence generating functions are a little bit different. *)

let record_sequence_exn chan =
  Sequence.unfold ~init:chan ~f:(fun ch ->
      Option.map (input_record_exn ch) ~f:(fun record -> (record, ch)))

let record_sequence chan =
  Sequence.unfold ~init:(Some chan) ~f:(fun chan' ->
      match chan' with
      (* None means the sequence is over. *)
      | None -> None
      | Some chan'' -> (
          match input_record chan'' with
          (* Some Error seems weird, but we need to yield an Error so
             the caller can handle it, then we need to trigger one
             more yield iteration to end the sequence next time with
             the None channel. *)
          | Error err -> Some (Error err, None)
          | Ok record -> (
              match record with
              (* None needed here to end the Sequence. *)
              | None -> None
              | Some record' -> Some (Or_error.return record', Some chan''))))
