(******************************************************************************)
(*                                                                            *)
(*                                  Monolith                                  *)
(*                                                                            *)
(*                              François Pottier                              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

module Gen = Gen
type 'a gen = 'a Gen.gen

module Print = Print
type 'a printer = 'a Print.printer

module Support = Support

include Code
include Spec
include BuiltinInt
include BuiltinBool
include BuiltinExn
include BuiltinAbstract
include BuiltinArrows
include BuiltinRot
include BuiltinSeq
include Ops
include DelayedOutput
include Engine
