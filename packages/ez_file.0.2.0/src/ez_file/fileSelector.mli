(**************************************************************************)
(*                                                                        *)
(*   Typerex Libraries                                                    *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type 'a t = {
  deep:bool;
  dft:[ `After | `Before ] option;
  filter:( bool -> string -> bool );
  follow_links:bool;
  error:(exn -> 'a -> unit);
  kinds : Unix.file_kind list option ;
}

val create :
  ?deep:bool ->
  ?dft:[ `After | `Before ] ->
  ?glob:string ->
  ?path:string ->
  ?ignore:string ->
  ?kinds: Unix.file_kind list ->
  ?filter:( bool -> string -> bool) ->
  ?follow_links:bool ->
  ?error:(exn -> 'a -> unit) -> unit -> 'a t

val globber :  ?pathname:bool -> string -> (string -> bool)


val make_select :
    (?select:'file t -> 'file -> 'res) ->
    ?deep:bool ->
    ?dft:[ `After | `Before ] ->
    ?glob:string -> (* check on basenames, not for recursion *)
    ?path:string -> (* check on paths, not for recursion *)
    ?ignore:string -> (* ignore these paths, always *)
    ?kinds: Unix.file_kind list ->
    ?filter:(bool -> string -> bool) ->
    ?follow_links:bool ->
    ?error:(exn -> 'file -> unit) ->
    'file ->
    'res
