(**************************************************************************)
(*                                                                        *)
(*                                 SCaml                                  *)
(*                                                                        *)
(*                       Jun Furuse, DaiLambda, Inc.                      *)
(*                                                                        *)
(*                   Copyright 2019,2020  DaiLambda, Inc.                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

val knormalize : IML.t -> IML.t
val beta       : bool ref -> IML.t -> IML.t
val assoc      : bool ref -> IML.t -> IML.t
val inline     : bool ref -> IML.t -> IML.t
val elim       : bool ref -> IML.t -> IML.t
val unknormalize : IML.t -> IML.t
val inline_pmatch : IML.t -> IML.t

val alpha_conv : (Ident.t * Ident.t) list -> IML.t -> IML.t
