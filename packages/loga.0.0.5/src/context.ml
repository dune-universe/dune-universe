(*
 * Copyright yutopp 2017 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t = { severity : Severity.t; module_name : string; line : int }

let make ~severity ~module_name ~line = { severity; module_name; line }

let severity ctx = ctx.severity

let module_name ctx = ctx.module_name

let line ctx = ctx.line
