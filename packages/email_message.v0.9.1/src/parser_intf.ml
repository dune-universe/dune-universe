open Async
open Core
open Core_extended.Std

type 'a token = [`Token of 'a | `Eof]
type 'a continue = [`Continue of 'a | `End ]

(** Functions for the user parser to communicate with the library. *)
module Comm  = struct
  type ('a, 'b) t = ('a * (('b list, string) Result.t), string) Result.t

  let put_list t cookie_list = Result.Ok (t, Result.Ok cookie_list)
  let put t cookie = put_list t [cookie]
  let continue t = put_list t []
  let put_opt t = function
    | Some cookie -> put t cookie
    | None        -> continue t
  let warning t ~msg = Result.Ok (t, Result.Error msg)
  let fatal msg = Result.Error msg
end


module type Basic_S = sig
  type a
  type b
  type t
  val create : unit -> t
  val parse : t -> a token -> (t, b) Comm.t
end

module type S = sig
  include Basic_S
  (* Given a state, parses a token, returning the resulting state and
     a list of the result tokens produced (if any) *)
  val parse_exn : ?log:(string -> unit) -> t -> a token -> (t * (b list))

  val parse_seq : ?log:(string -> unit) -> a Lazy_sequence.t ->
    b Lazy_sequence.t
  val parse_lazy_list : ?log:(string -> unit) -> a Lazy_list.t ->
    b Lazy_list.t
  val parse_list : ?log:(string -> unit) -> a list -> b list
  val parse_pipe : ?log:(string -> unit) -> a Pipe.Reader.t -> b Pipe.Reader.t

end


module Make (T : Basic_S) = struct
  type t = T.t;;
  type a = T.a;;
  type b = T.b;;

  let create = T.create;;
  let parse = T.parse;;

  let eprintln s = eprintf "%s\n%!" s;;

  let parse_exn ?(log=eprintln) t dough =
    match Result.ok_or_failwith (parse t dough) with
    | (t, Result.Ok cookie_list) -> (t, cookie_list)
    | (t, Result.Error msg) -> log msg; (t, [])
  ;;

  let parse_seq ?log seq =
    Lazy_sequence.initialize (fun () ->
      let iter = Lazy_sequence.Iterator.create seq in
      let (==>>) = Lazy_sequence.(==>>) in
      let rec loop t =
        match Lazy_sequence.Iterator.get iter with
        | Some a ->
          let (t, b) = parse_exn ?log t (`Token a) in
          b ==>> (fun () -> loop t)
        | None      ->
          let (_, b) = parse_exn ?log t `Eof in
          b ==>> (fun () -> Lazy_sequence.empty)
      in
      loop (create ()))
  ;;

  let parse_lazy_list ?log l =
    let rec next' t l =
      match Lazy_list.decons l with
      | Some (a, rest_of_l) ->
        (* Continue parsing until we get something to give back *)
        begin
          match parse_exn ?log t (`Token a) with
          | (t, []) -> next' t rest_of_l
          | (t, b)  -> ((t, rest_of_l), b)
        end
      | None         ->
        let (t, b) = parse_exn ?log t `Eof in
        ((t, l), b)
    in
    let next ((t, l) as state, b) =
      match b with
      | [] | [_] -> next' t l
      | _ :: xs  -> (state, xs)
    in
    Lazy_list.of_iterator
      ~curr:(Fn.compose List.hd snd)
      ~init:(next' (create ()) l)
      ~next:next
  ;;

  let parse_list ?log l =
    Lazy_list.to_list
      (parse_lazy_list ?log
         (Lazy_list.of_list l))

  let parse_pipe ?log src =
    let output, dst = Pipe.create () in
    let to_dst l = Deferred.List.iter l
                     ~how:`Sequential
                     ~f:(fun x ->
                       if not (Pipe.is_closed dst) then
                         Pipe.write dst x
                       else begin
                         Pipe.close_read src;
                         Pipe.close dst;
                         return ()
                       end)
    in
    don't_wait_for (
      Pipe.fold_without_pushback src
        ~init:(return (create ()))
        ~f:(fun t a ->
          t >>= fun t ->
          let t, bs = parse_exn ?log t (`Token a) in
          to_dst bs >>| fun () ->
          t)
      >>= Fn.id >>= fun t ->
      let _, bs = parse_exn ?log t `Eof in
      to_dst bs >>| fun () ->
      Pipe.close dst
    );
    output
  ;;
end
