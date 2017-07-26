open! Core
open Core_extended.Std
open! Async

type 'a token = [`Token of 'a | `Eof]
type 'a continue = [`Continue of 'a | `End ]

(* API functions for parser creators *)
module Comm : sig
  type ('a, 'b) t

  (* Basic actions to produce tokens *)
  val put      : 'a -> 'b -> ('a, 'b) t
  val continue : 'a -> ('a, 'b) t

  (* Advance functions to produce tokens *)
  val put_opt  : 'a -> 'b option -> ('a, 'b) t
  val put_list : 'a -> 'b list -> ('a, 'b) t

  (* In-band signaling *)

  (* An error from which the parser can recover has ocurred *)
  val warning  : 'a -> msg:string -> ('a, 'b) t

  (* A fatal error from which the parser can't recover has ocurred *)
  val fatal    : string -> ('a, 'b) t

end

(* Mappings between languages of different fertility *)
module type Basic_S = sig
  type t
  type a
  type b
  val create : unit -> t
  val parse : t -> a token -> (t, b) Comm.t
end

(* Extended signature with automatically generated functions *)
module type S = sig
  include Basic_S
  val parse_exn : ?log:(string -> unit) -> t -> a token -> (t * (b list))

  val parse_seq : ?log:(string -> unit) -> a Lazy_sequence.t -> b Lazy_sequence.t
  val parse_lazy_list : ?log:(string -> unit) -> a Lazy_list.t -> b Lazy_list.t
  val parse_list : ?log:(string -> unit) -> a list -> b list
  val parse_pipe : ?log:(string -> unit) -> a Pipe.Reader.t -> b Pipe.Reader.t
end

module Make (S : Basic_S) :
  S with type t = S.t
     and type a = S.a
     and type b = S.b
