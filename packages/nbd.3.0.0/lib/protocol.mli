(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(** Types representing NBD protocol requests and responses. *)

open Result

module Error: sig
  (** Read and write requests can fail with an error response. *)

  type t = [
    | `EPERM  (** Operation not permitted *)
    | `EIO    (** Input/output error *)
    | `ENOMEM (** Cannot allocate memory *)
    | `EINVAL (** Invalid argument *)
    | `ENOSPC (** No space left on device *)
    | `Unknown of int32
  ] [@@deriving sexp]
  (** Defined error codes which can be returned in response to a request
      in the data-pushing phase. *)

  val to_string: t -> string
end

module Command: sig
  (** Once a connection has been established, the client can submit commands. *)

  type t =
    | Read  (** Read a block of data *)
    | Write (** Write a block of data *)
    | Disc  (** Disconnect: server must flush all outstanding commands and then
                will close the connection *)
    | Flush (** A flush request or write barrier. All requests received before
                this one will have completed before this command is acknowledged. *)
    | Trim  (** A hint that a data region is nolonger required and may be
                discarded. *)
    | Unknown of int32 (** A command which this protocol implementation doesn't
                           suport. *)
  [@@deriving sexp]

  val to_string: t -> string
end

module PerExportFlag: sig
  (** Every disk (or 'export') has a number of associated flags. This will
      be returned from the server when the negotiation is complete. *)

  type t =
    | Read_only   (** export is read/only. Writes will receive EPERM *)
    | Send_flush  (** server supports Command.Flush *)
    | Send_fua    (** server supports NBD_CMD_FLAG_FUA *)
    | Rotational  (** let the client schedule I/O for a rotational medium *)
    | Send_trim   (** server supports Command.Trim *)
  [@@deriving sexp]
  (** Per-export flags *)

  val to_string: t -> string
end

module GlobalFlag: sig
  (** During the protocol negotiation there are some defined flags used
      to choose protocol variants. These flags are sent by the server. *)

  type t =
    | Fixed_newstyle (** server supports the fixed newstyle protocol *)
    | No_zeroes      (** request to omit the 124 bytes of zeroes *)
  [@@deriving sexp]

  val to_string: t -> string
end

module ClientFlag: sig
  (** During the protocol negotiation there are some defined flags used
      to choose protocol variants. These flags are sent by the client. *)

  type t =
    | Fixed_newstyle (** client acknowledges use of fixed newstyle protocol *)
    | No_zeroes      (** client acknowledges omission of 124 bytes of zeroes *)
  [@@deriving sexp]

  val to_string: t -> string
end

module Option: sig
  (** In the 'newstyle' negotiation there is an opportunity for the client
      to negotiate options with the server. These are the known options. *)

  type t =
    | ExportName (** The client would like to connect to a given disk/export by
                     name *)
    | Abort      (** The client would like to quit. *)
    | List       (** The client would like to receive a list of known
                     disk/exports. *)
    | Unknown of int32 (** This option is unknown to this implementation *)
  [@@deriving sexp]

  val to_string: t -> string
end

module OptionResponse: sig
  (** When the client sends an option request, the server must reply. *)

  type t =
    | Ack (** Option acknowledged *)
    | Server (** A description of an export (in reponse to [List]) *)
    | Unsupported (** The option is unsupported *)
    | Policy (** The option is blocked by an admin policy *)
    | Invalid (** The option was invalid (i.e. the client is buggy) *)
    | Platform (** The option is not supported in this platform. *)
    | Unknown of int32 (** The response is unknown to this implementation. *)
  [@@deriving sexp]

  val to_string: t -> string
end

module Announcement: sig
  (** The server sends an initial greeting when the connectino is opened. It
      can be of two main types: the original [V1] and a 'newstyle' [V2]. *)

  type t = [ `V1 | `V2 ] [@@deriving sexp]

  val sizeof: int

  val marshal: Cstruct.t -> t -> unit
  val unmarshal: Cstruct.t -> (t, exn) result
end

module Negotiate: sig
  (** The initial greeting sent by the server *)

  type v1 = {
    size: int64; (** The size of the disk *)
    flags: PerExportFlag.t list; (** Flags associated with the disk *)
  } [@@deriving sexp]
  (** The original [V1] protocol supports only one disk. *)

  type v2 = GlobalFlag.t list [@@deriving sexp]
  (** The 'newstyle' [V2] protocol supports an option negotiation
      phase and a number of sub-options [GlobalFlag.t]s *)

  type t =
    | V1 of v1
    | V2 of v2
    (** The initial greeting sent by the server *)

  val to_string: t -> string

  val sizeof: Announcement.t -> int

  val marshal: Cstruct.t -> t -> unit
  val unmarshal: Cstruct.t -> Announcement.t -> (t, exn) result
end

module NegotiateResponse: sig
  (** The client's initial response to the server's greeting *)

  type t = ClientFlag.t list [@@deriving sexp]
  (** The client can send some flags, in response to flags set by the server. *)

  val sizeof: int

  val marshal: Cstruct.t -> t -> unit

  val unmarshal: Cstruct.t -> t
end

module OptionRequestHeader: sig
  (** Every option the client requests has the same header. *)

  type t = {
    ty: Option.t; (** The option type *)
    length: int32; (** The length of the option data *)
  } [@@deriving sexp]
  (** The header of an option request sent by the client *)

  val sizeof: int

  val marshal: Cstruct.t -> t -> unit
  val unmarshal: Cstruct.t -> (t, exn) result
end

module ExportName: sig
  (** An ExportName option payload *)

  type t = string [@@deriving sexp]
  (** The name of the export the client wishes to connect to *)

  val sizeof: t -> int

  val marshal: Cstruct.t -> t -> unit
end

module DiskInfo: sig
  (** Details about the export chosen by the client, sent in response
      to an [ExportName] option. *)

  type t = {
    size: int64; (** The size of the disk in bytes *)
    flags: PerExportFlag.t list; (** Flags associated with the disk *)
  } [@@deriving sexp]
  (** Details about the export chosen by the client. *)

  val sizeof: int

  val unmarshal: Cstruct.t -> (t, exn) result
  val marshal: Cstruct.t -> t -> unit
end

module OptionResponseHeader: sig
  (** The server sends a response to every option request sent by the
      client (except [ExportName] which is followed by a [DiskInfo].
      This is the header of the response. *)

  type t = {
    request_type: Option.t; (** The option type requested *)
    response_type: OptionResponse.t; (** The response code *)
    length: int32; (** The length of the payload associated with the response *)
  } [@@deriving sexp]
  (** The header of the response sent by the server in response to
      the client requesting an option. *)

  val sizeof: int

  val to_string: t -> string

  val unmarshal: Cstruct.t -> (t, exn) result
  val marshal: Cstruct.t -> t -> unit
end

module Server: sig
  (** In response to a [List] option, the server sends a number of
      [Server] responses and then finally an [Ack] *)

  type t = {
    name: string; (** The name of an available disk. *)
  } [@@deriving sexp]
  (** A reponse to a [List] option. Note this option is repeated, once
      per available disk. *)

  val sizeof: t -> int

  val unmarshal: Cstruct.t -> (t, exn) result
end

module Request: sig
  (** After the negotation phase, clients send I/O requests to the server. *)

  type t = {
    ty : Command.t; (** The command type *)
    handle : int64; (** A unique handle used to match requests with responses.*)
    from : int64;   (** The start of the data region *)
    len : int32;    (** The length of the data region *)
  } [@@deriving sexp]
  (** An I/O request sent by the client to the server. *)

  val to_string: t -> string

  val sizeof: int

  val marshal: Cstruct.t -> t -> unit
  val unmarshal: Cstruct.t -> (t, exn) result
end

module Reply: sig
  (** A reply sent from the server in response to a [Request]. Note
      these arrive out-of-order. *)

  type t = {
    error : (unit, Error.t) result; (** Success or failure of the request *)
    handle : int64; (** The unique id in the [Request] *)
  } [@@deriving sexp]

  val to_string: t -> string

  val sizeof: int

  val marshal: Cstruct.t -> t -> unit
  val unmarshal: Cstruct.t -> (t, exn) result
end
