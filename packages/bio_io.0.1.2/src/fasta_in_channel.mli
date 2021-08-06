(** An input channel for reading FASTA files.

    {1 Overview}

    {2 Quick start}

    - To get a list of records, use {!records} or {!with_file_records}.
    - To get a sequence of records, use: {!record_sequence} or
      {!with_file_record_sequence}.
    - To fold over records, use: {!fold_records} or {!with_file_fold_records}.
    - To iterate over records, use: {!iter_records} or
      {!with_file_iter_records}.

    For some more info, check out the {{!examples} usage examples}.

    {2 Function flavors}

    Most functions in this module come in a few different flavors.

    - Functions with [with_file_] prefix
    - Functions with the [_exn] suffix
    - Functions that return [Or_error.t]

    Functions that have the [with_file_] prefix in their name take a filename
    [string] rather than a [Fasta_in_channel.t]. They are a convenient way to
    avoid wrapping your all your code in {!with_file} function calls.

    Functions that the [_exn] suffix in their name may raise exceptions, either
    from logic specific to this module, or from calls to other [Core_kernel]
    functions.

    Functions that return [Or_error.t] catch {i all} exceptions, even those not
    originating in the logic of this module. These functions shouldn't raise in
    practice. In other words, if you aren't using a function with [_exn] in the
    name, and you get an exception, then you have found a bug.

    Here is an example of the different "flavors" of {!fold_records}:

    - [fold_records]: the normal [Fasta_in_channel.t] accepting, [Or_error.t]
      returning flavor
    - [fold_records_exn]: takes [Fasta_in_channel.t] but may raise exceptions
      instead of returning [Or_error.t]
    - [with_file_fold_records]: takes a file name ([string]) rather than a
      [Fasta_in_channel.t], and returns [Or_error.t]
    - [with_file_fold_records_exn]: takes a file name ([string]) rather than a
      [Fasta_in_channel.t], and may raise exceptions

    {1:examples Examples}

    {2 Return all records in a list}

    Simplest way. May raise exceptions.

    {[ let records = Fasta_in_channel.with_file_records_exn fname ]}

    A bit more involved, but you won't get exceptions. Instead, you have to
    handle the [Or_error.t]. One reason to do this is to give the user nicer
    error messages.

    {[
      let records =
        match Fasta_in_channel.with_file_records name with
        | Error err ->
            eprintf "Problem reading records: %s\n" (Error.to_string_hum err);
            exit 1
        | Ok records -> records
    ]}

    {2 Iterating over records}

    Use the [iter] functions when you need to go over each record and perform
    some side-effects with them.

    Print sequence IDs and sequence lengths

    {[
      let () =
        Fasta_in_channel.with_file_iter_records_exn "sequences.fasta"
          ~f:(fun record ->
            let open Fasta_record in
            printf "%s => %d\n" (id record) (String.length (seq record)))
    ]}

    Print sequence index, IDs, and sequence lengths.

    This is like the last example except that we also want to print the index.
    The first record is 0, the 2nd is 1, etc.

    {[
      let () =
        Fasta_in_channel.with_file_iteri_records_exn "sequences.fasta"
          ~f:(fun index record ->
            let open Fasta_record in
            printf "%d: %s => %d\n" (index + 1) (id record)
              (String.length (seq record)))
    ]}

    {2 Folding over records}

    If you need to reduce all the records down to a single value, use the [fold]
    functions.

    Get total length of all sequences in the file.

    Watch out as this may raise exceptions...see the [_exn] suffix.

    {[
      let total_length =
        Fasta_in_channel.with_file_fold_records_exn "sequences.fasta" ~init:0
          ~f:(fun length record ->
            let sequence = Fasta_record.seq record in
            length + String.length sequence)
    ]}

    Same thing, but this won't raise exceptions. You do have to handle
    [Or_error.t] to get the final value. Note that within the fold function, you
    get [Fasta_record.t] and not [Fasta_record.t Or_error.t].

    {[
      let total_length =
        match
          Fasta_in_channel.with_file_fold_records name ~init:0
            ~f:(fun length record ->
              length + String.length (Fasta_record.seq record))
        with
        | Error err ->
            eprintf "Problem reading records: %s\n" (Error.to_string_hum err);
            exit 1
        | Ok total_length -> total_length
    ]}

    {2:pipelines Pipelines with records}

    Sometimes you have a "pipeline" of computations that you need to do one
    after the other on records. In that case, use the [sequence] functions.

    You {i could} use [Lists] too, or just don't bother writing things in a
    pipeline, but with [Sequenecs] you can have your nice pipelines without all
    the extra intermediate allocations.

    Here's a silly example.

    In this case, I'm using the exception throwing versions.

    {[
      let () =
        Fasta_in_channel.with_file_exn name ~f:(fun chan ->
            Fasta_in_channel.record_sequence_exn chan
            (* Add sequence index to record description *)
            |> Sequence.mapi ~f:(fun i record ->
                   let new_desc =
                     match Fasta_record.desc record with
                     | None -> Some (sprintf "sequence %d" i)
                     | Some old_desc ->
                         Some (sprintf "%s -- sequence %d" old_desc i)
                   in
                   Fasta_record.with_desc new_desc record)
            (* Convert all sequence chars to lowercase *)
            |> Sequence.map ~f:(fun record ->
                   let new_seq = String.lowercase (Fasta_record.seq record) in
                   Fasta_record.with_seq new_seq record)
            (* Print sequences *)
            |> Sequence.iter ~f:(fun record ->
                   print_endline @@ Fasta_record.serialize record))
    ]}

    One thing to watch out for though...if you get an exception half way through
    and you are running side-effecting code like we are here then part of your
    side effects will have occured and part of them will {i not} have occured.

    There are also [Or_error.t] flavors of the [sequence] functions. Just watch
    out because these you actually {i do} have to deal with [Or_error.t] for
    each [Fasta_record.t] in the sequence.

    As an alternative, you could use the [record_sequence_exn] function, but
    wrap {i that} in the [with_file] function. That way you don't have to deal
    with the [Or_error.t] inside your pipeline. Instead you deal with it at the
    end.

    {[
      let total_length =
        match
          Fasta_in_channel.with_file name ~f:(fun chan ->
              Fasta_in_channel.record_sequence_exn chan
              (* Blow up pipeline on second sequence. *)
              |> Sequence.mapi ~f:(fun i record ->
                     if i = 1 then assert false;
                     record)
              |> Sequence.fold ~init:0 ~f:(fun length record ->
                     length + String.length (Fasta_record.seq record)))
        with
        | Error err ->
            eprintf "Problem in parsing pipeline: %s\n"
              (Error.to_string_hum err);
            exit 1
        | Ok total_length -> total_length
    ]}

    As you can see, if that fasta file has more than one sequence it will hit
    the [assert false] and blow up. *)

(** {1 API} *)

open! Core_kernel

type t

exception Exn of string
(** The only exception that logic introduced in this module will raise.
    Functions may raise other exceptions that occur "outside" of the module,
    like `Sys_error` when trying to open a file that doesn't exist. *)

(** {2 Open/Close} *)

val create_exn : string -> t
(** [create_exn filename] returns a [t] or raises an exception if the call
    fails. *)

val create : string -> t Or_error.t
(** [create filename] returns a [t] or an [Error.t] if the call fails *)

val stdin_exn : unit -> t
(** [stdin_exn ()] returns a [t] or raises an exception if the call fails. *)

val stdin : unit -> t Or_error.t
(** [stdin ()] is like [stdin_exn] except that it returns an [Or_error.t] rather
    than raising exceptions on failure. *)

val close_exn : t -> unit
(** [close_exn t] closes the channel or raises an exception if the call fails. *)

val close : t -> unit Or_error.t
(** [close t] is like [close_exnt t] except that it returns an [Or_error.t]
    rather than raising an exception on failure. *)

val with_file_exn : string -> f:(t -> 'a) -> 'a
(** [with_file_exn file_name ~f] executes [~f] on the channel created from
    [file_name] and closes it afterwards. May raise exceptions. *)

val with_file : string -> f:(t -> 'a) -> 'a Or_error.t
(** [with_file file_name ~f] is like [with_file_exn file_name ~f] except that it
    returns an [Or_error.t] rather than raising exceptions. *)

(** {2 Utilities} *)

val of_in_channel : In_channel.t -> t
(** [of_in_channel chan] converts an [In_channel.t] to a [t]. *)

val to_in_channel : t -> In_channel.t
(** [to_in_channel t] converts a [t] to an [In_channel.t]. *)

val equal : t -> t -> bool
(** [equal t1 t2] compares two [t] for equality using [In_channel.equal]. *)

val peek_char : t -> char option
(** [peek_char t] "peeks" the next [char] in the channel, then rewinds the
    channel back so the next read is not affected. *)

val rewind : ?pos:Int64.t -> t -> unit
(** [rewind ~pos t] seeks to the [pos] in the channel [t]. Default [~pos:0],
    which means go back to the beginning of the channel. *)

val input_record_exn : t -> Fasta_record.t Option.t
(** [input_record_exn t] returns [Some Fasta_record.t] if there is any more. If
    there are no more records, [None] is returned. [Exn] is raised on bad input. *)

val input_record : t -> Fasta_record.t Option.t Or_error.t
(** [input_record t] is like [input_record_exn t] except that it returns an
    [Or_error.t] rather than raising exceptions. *)

(** {2:getting_records Getting records} *)

(** {3:list_of_t As list} *)

val records_exn : t -> Fasta_record.t List.t
(** [records_exn t] returns a list of [Fasta_record.t]. May raise exceptions. *)

val records : t -> Fasta_record.t List.t Or_error.t
(** [records t] is like [records_exn t] except that it returns an [Or_error.t]
    and will not raise exceptions. *)

val with_file_records_exn : string -> Fasta_record.t List.t
(** [with_file_records_exn file_name] is like [records_exn t] except that it
    takes a file name directly rather than taking a [Fasta_in_channel.t]. You
    can use this to avoid manually creating and closing an [t], or to avoid
    wrapping the call in {!with_file}. *)

val with_file_records : string -> Fasta_record.t List.t Or_error.t
(** [with_file_records file_name] is like [with_file_records_exn
   file_name]
    except that it returns an [Or_error.t] rather than raising exceptions. *)

(** {3:sequence_of_t As sequence}

    These are a bit different:

    * There are no [with_file] versions as you would have to do some fiddly
    things to keep the channel open, making them not so nice to use.

    * Each [Fasta_record.t] that is yielded is wrapped in an [Or_error.t]. This
    is different from the [iter], [fold], and other non [_exn] functions in
    which case the entire result is wrapped in an [Or_error.t], letting you
    ignore errors in the passed in [~f] function and deal with failure once. *)

val record_sequence_exn : t -> Fasta_record.t Sequence.t
(** [record_sequence_exn t] returns a [Sequence.t] of [Fasta_record.t]. May
    raise exceptions. *)

val record_sequence : t -> Fasta_record.t Or_error.t Sequence.t
(** [record_sequence t] is like [record_sequence_exn t] except that instead of
    raising exceptions, each item of the sequence is a
    [Fasta_record.t Or_error.t] rather than an "unwrapped" [Fasta_record.t].
    This could make things annoying to deal with. If you don't want exceptions,
    you could instead wrap your entire sequence processing pipeline in a call to
    {!with_file} and handle the [Or_error.t] in that way. See the {{!pipelines}
    usage examples} for more info. *)

(** {2:folding Folding over records} *)

val fold_records_exn : t -> init:'a -> f:('a -> Fasta_record.t -> 'a) -> 'a
(** [fold_records_exn t ~init ~f] reduces all records from a [t] down to a
    single value of type ['a]. May raise exceptions. *)

val fold_records :
  t -> init:'a -> f:('a -> Fasta_record.t -> 'a) -> 'a Or_error.t
(** [fold_records t ~init ~f] is like [fold_records_exn t ~init ~f] except that
    it returns ['a Or_error.t] rather than ['a] and will not raise exceptions. *)

val with_file_fold_records_exn :
  string -> init:'a -> f:('a -> Fasta_record.t -> 'a) -> 'a
(** [with_file_fold_records_exn file_name ~init ~f] is like
    [fold_records_exn t ~init ~f] except that it takes the name of a fasta file
    directly rather than a [Fasta_in_channel.t]. *)

val with_file_fold_records :
  string -> init:'a -> f:('a -> Fasta_record.t -> 'a) -> 'a Or_error.t
(** [with_file_fold_records file_name ~init ~f] is like
    [fold_records
   t ~init ~f] except that it takes the name of a fasta file
    directly rather than a [Fasta_in_channel.t]. *)

(** {3 With index}

    These four functions are like their {{!folding} [fold]} counterparts except
    that the folding function [f] is also passed the index of the current
    record. *)

val foldi_records_exn :
  t -> init:'a -> f:(int -> 'a -> Fasta_record.t -> 'a) -> 'a
val foldi_records :
  t -> init:'a -> f:(int -> 'a -> Fasta_record.t -> 'a) -> 'a Or_error.t
val with_file_foldi_records_exn :
  string -> init:'a -> f:(int -> 'a -> Fasta_record.t -> 'a) -> 'a
val with_file_foldi_records :
  string -> init:'a -> f:(int -> 'a -> Fasta_record.t -> 'a) -> 'a Or_error.t

(** {2:iter Iterating over records}

    The [iter] functions are like the {{!folding} [fold]} functions except they
    do not take an [init] value and the [f] function returns [unit] insead of
    some other value ['a] (and thus return [unit] rather than a value ['a].

    They are mainly called for side effects. *)

val iter_records_exn : t -> f:(Fasta_record.t -> unit) -> unit
val iter_records : t -> f:(Fasta_record.t -> unit) -> unit Or_error.t
val with_file_iter_records_exn : string -> f:(Fasta_record.t -> unit) -> unit
val with_file_iter_records :
  string -> f:(Fasta_record.t -> unit) -> unit Or_error.t

(** {3 With index}

    The [iteri] variants are like the {{!iter} [iter]} functions except that the
    function [f] is passed in the index of the current record. *)

val iteri_records_exn : t -> f:(int -> Fasta_record.t -> unit) -> unit
val iteri_records : t -> f:(int -> Fasta_record.t -> unit) -> unit Or_error.t
val with_file_iteri_records_exn :
  string -> f:(int -> Fasta_record.t -> unit) -> unit
val with_file_iteri_records :
  string -> f:(int -> Fasta_record.t -> unit) -> unit Or_error.t
