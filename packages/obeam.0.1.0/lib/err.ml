(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type 'a t = {
  reason: 'a kind_t;
  backtrace: Source_code_position.t list;
  previous: 'a t option;
}
and 'a kind_t =
  | Not_supported_absform of string * 'a
[@@deriving sexp_of]

let create ~loc reason =
  {
    reason = reason;
    backtrace = [loc];
    previous = None;
  }

let record_backtrace ~loc err =
  {err with backtrace = loc :: err.backtrace}

let wrap ~loc reason previous =
  let err = create ~loc reason in
  {err with previous = Some previous}
