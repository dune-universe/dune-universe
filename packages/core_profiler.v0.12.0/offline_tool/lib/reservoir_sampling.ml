(** This is a copy of resevoir sampling from [Jane.Order_stats_reservoir_sampling.Int]
    with several non-essential functions removed.

    We can kill this someday when the [Jane] is publicly released. *)
open Core

module Make (E : sig
    type t [@@deriving sexp, bin_io, compare]
    val make_array : len:int -> t array
    val set : t array -> int -> t -> unit
  end) = struct

  type element = E.t [@@deriving sexp, bin_io]

  type t = {
    mutable total_samples_seen : int;  (* total number of samples seen by [add] *)
    mutable samples_count : int;       (* number of samples retained in [samples] *)
    mutable samples : E.t array;       (* remembered samples (between the indices 0 and
                                          samples_count-1 inclusive; the other values are
                                          meaningless) *)
    mutable samples_are_sorted : bool; (* flag to avoid resorting *)
  } [@@deriving sexp, bin_io]

  let create ?(num_samples_to_keep=10_000) () =
    if num_samples_to_keep < 1
    then invalid_arg "num_samples_to_keep must be positive"
    else if num_samples_to_keep > 1_000_000_000
    then invalid_arg "num_samples_to_keep shouldn't be over a billion"
    else
      {
        total_samples_seen = 0;
        samples_count = 0;
        samples = E.make_array ~len:num_samples_to_keep;
        samples_are_sorted = true;
      }

  let add t sample =
    t.total_samples_seen <- t.total_samples_seen + 1;
    let index_to_replace = Random.int t.total_samples_seen in
    let len = Array.length t.samples in
    if index_to_replace < len
    then begin
      if t.samples_count < len
      then (* t.samples has unoccupied slots *)
        begin
          E.set t.samples t.samples_count sample;
          t.samples_count <- t.samples_count + 1;
        end
      else E.set t.samples index_to_replace sample;
      t.samples_are_sorted <- false;
    end

  let sort t =
    if not t.samples_are_sorted then begin
      Array.sort t.samples ~compare:E.compare ~pos:0 ~len:t.samples_count;
      t.samples_are_sorted <- true;
    end

  let percentile t p =
    if p < 0. || p > 1.
    then Or_error.error_string "Order_stats.percentile: must be between 0 and 1"
    else begin
      if t.samples_count = 0
      then Or_error.error_string "Order_stats.percentile: no samples yet"
      else begin
        sort t;
        let index = Float.iround_towards_zero_exn (p *. Float.of_int t.samples_count) in
        let index =
          if index >= t.samples_count      (* in case p=1 or rounding error *)
          then t.samples_count - 1
          else index
        in
        Result.Ok t.samples.(index)
      end
    end

  let percentile_exn t p = Or_error.ok_exn (percentile t p)

  let distribution t =
    sort t;
    (* looping in reverse to construct the list in sorted order *)
    let rec loop i ls =
      if i = -1
      then ls
      else loop (i-1) (t.samples.(i)::ls)
    in
    loop (t.samples_count-1) []
end

include  Make (struct
    include Int

    let make_array ~len = Array.create ~len 0
    let set (t:int array) i v = t.(i) <- v
  end)

