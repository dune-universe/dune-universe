(***********************************************************************)
(*                                                                     *)
(*                             Active-DVI                              *)
(*                                                                     *)
(*                   Projet Cristal, INRIA Rocquencourt                *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License.          *)
(*                                                                     *)
(*  Jun Furuse, Didier Rémy and Pierre Weis.                           *)
(*  Contributions by Roberto Di Cosmo, Didier Le Botlan,               *)
(*  Xavier Leroy, and Alan Schmitt.                                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

(* $Id$ *)

(* Embedded applications *)
type app_mode =
   | Fake

      (* A [Fake] application is not launched. It is just used to
         reserve a given area on the screen for another application to
         display its material afterwards. *)

   | Raw

      (* A [Raw] application is launched at each invocation. It is
         never automatically killed, except at the end of show. As
         soon as launched, a [Raw] application remains visible until
         explicitly hidden. When hidden, [Raw] applications have to be
         remap manually when necessary. *)

   | Sticky

      (* A [Sticky] application is launched once and only once. It is
         not killed when a new slide is visualized. Hence, as soon as
         launched, a [Sticky] application remains visible throughout
         the show. *)

   | Persistent

      (* A [Persistent] application is not visible out of the slide
         that launched it. A [Persistent] application is launched once
         and only once and keeps running throughout the show. A
         [Persistent] application must be embeddable, since its window
         must be mapped and unmapped. *)

   | Ephemeral

      (* An [Ephemeral] is an application that is launched when the
         page it appears in is visualised. It is automatically killed
         when going to another slide. If the page becomes visible
         again, the application will be launched again as well. *)
;;

type signal = int;;

open Launch;;

val embed_app :
  app_command -> app_mode -> app_name -> int -> int -> int -> int -> unit;;
val map_embedded_app : app_name -> unit;;
val map_all_embedded_app : app_name -> unit;;
val unmap_embedded_app : app_name -> unit;;
val unmap_all_embedded_app : app_name -> unit;;
val kill_embedded_app : signal -> app_name -> unit;;
val kill_all_embedded_app : signal -> app_name -> unit;;
val kill_all_embedded_apps : unit -> unit;;
val kill_ephemeral_apps : unit -> unit;;
