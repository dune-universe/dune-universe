(*******************************************************************************
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2019 ONERA
 * Authors: Julien Brunel (ONERA), David Chemouil (ONERA)
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * 
 * SPDX-License-Identifier: MPL-2.0
 * License-Filename: LICENSE.md
 ******************************************************************************)

(** Specification of transformations. *)

type ('src, 'dst) t
(** A transformation conforms to the following signature.  *)

val make : string -> ('src -> 'dst) -> ('src, 'dst) t
(** Constructor. The first parameter is the name: it must be unique in the whole
    program.*)

val name : ('src, 'dst) t -> string
(** Retrieves the name... *)

val run : ('src, 'dst) t -> 'src -> 'dst
(** Runs a transformation *)

val fby : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
(** Returns the composition (left-to-right) of two transformations. *)

val identity : ('a, 'a) t
(** Identity transformation *)

type ('src, 'dst) tlist
(** A list of transformations with given source and target type. *)

val tlist : ('src, 'dst) t list -> ('src, 'dst) tlist
(** Creates a list of transformations with a default one .*)

val get_exn : ('src, 'dst) tlist -> string -> ('src, 'dst) t
(** Retrieves a transformation. May raise Not_found. *)
