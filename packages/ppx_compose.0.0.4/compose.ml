(* Copyright (C) 2017  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

(* The `();`-prefix, suggested by Gabriel Scherer in [1], forces the compiler to
 * produce a binary function, allowing an flambda-enabled compiler to inline the
 * typical usage.
 *
 * [1]: https://discuss.ocaml.org/t/ann-ppx-compose-0-0-3/345 *)
let (%)  g f = (); fun x -> g (f x)
let (%>) f g = (); fun x -> g (f x)
