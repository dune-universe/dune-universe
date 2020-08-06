(*-------------------------------------------------------------------------
 * Copyright (c) 2019, 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * cookies v1.0.0
 *-------------------------------------------------------------------------*)

type t = [ `Default | `None | `Lax | `Strict ] [@@deriving sexp_of, eq, ord]

let default = `Default

let none = `None

let lax = `Lax

let strict = `Strict

let to_cookie_string = function
  | `Default -> ""
  | `None -> "None"
  | `Lax -> "Lax"
  | `Strict -> "Strict"
