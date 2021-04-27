(*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

module type PTR_TYP = sig
  type t

  val typ : t Ctypes.typ
end

module Type : sig
  val release : unit Ctypes.ptr -> unit

  val retain : unit Ctypes.ptr -> unit Ctypes.ptr
end

module String : sig
  module Encoding : sig
    type t =
      | MacRoman
      | WindowsLatin1
      | ISOLatin1
      | NextStepLatin
      | ASCII
      | Unicode
      | UTF8
      | NonLossyASCII
      | UTF16
      | UTF16BE
      | UTF16LE
      | UTF32
      | UTF32BE
      | UTF32LE

    val t : t Ctypes.typ
  end

  type t

  type cfstring = t

  module Bytes : sig
    type t = bytes

    val to_bytes : cfstring -> t

    val of_bytes : t -> cfstring

    include PTR_TYP with type t := t
  end

  module String : sig
    type t = string

    val to_string : cfstring -> t

    val of_string : t -> cfstring

    include PTR_TYP with type t := t
  end

  include PTR_TYP with type t := t
end

module Array : sig
  type t

  type cfarray = t

  module CArray : sig
    type t = unit Ctypes.ptr Ctypes.CArray.t

    val to_carray : cfarray -> t

    val of_carray : t -> cfarray

    module Make (T : PTR_TYP) : sig
      include PTR_TYP with type t = T.t Ctypes.CArray.t
    end

    include PTR_TYP with type t := t
  end

  module List : sig
    type t = unit Ctypes.ptr list

    val to_list : cfarray -> t

    val of_list : t -> cfarray

    module Make (T : PTR_TYP) : sig
      include PTR_TYP with type t = T.t list
    end

    include PTR_TYP with type t := t
  end
end

module Index : sig
  type t = int

  val typ : t Ctypes.typ
end

module Allocator : sig
  open Ctypes

  type retain_callback_t = unit ptr -> unit ptr

  type release_callback_t = unit ptr -> unit

  type copy_description_callback_t = unit ptr -> bytes

  val retain_callback_typ : retain_callback_t typ

  val release_callback_typ : release_callback_t typ

  val copy_description_callback_typ : copy_description_callback_t typ
end

module RunLoop : sig
  type t

  module Mode : sig
    type t = Default | CommonModes | Mode of string

    val typ : t Ctypes.typ
  end

  module Observer : sig
    module Activity : sig
      type t =
        | Entry
        | BeforeTimers
        | BeforeSources
        | BeforeWaiting
        | AfterWaiting
        | Exit

      type select = Only of t list | All

      val to_string : t -> string
    end

    module Callback : sig
      type t = Activity.t -> unit
    end

    type t

    val create :
      Activity.select -> ?repeats:bool -> ?order:int -> Callback.t -> t

    val invalidate : t -> unit
  end

  module RunResult : sig
    type t = Finished | Stopped | TimedOut | HandledSource

    val typ : t Ctypes.typ

    val to_string : t -> string
  end

  val typ : t Ctypes.typ

  val remove_observer : t -> Observer.t -> Mode.t -> unit

  val add_observer : t -> Observer.t -> Mode.t -> unit

  val run : unit -> unit

  val run_in_mode :
    ?return_after_source_handled:bool -> ?seconds:float -> Mode.t -> RunResult.t

  val get_current : unit -> t

  val stop : t -> unit

  val release : t -> unit
end

module TimeInterval : sig
  type t = float

  val typ : t Ctypes.typ
end

module Types = Types
module Types_detected = Types_detected
module Bindings = Bindings
module Generated = Generated
