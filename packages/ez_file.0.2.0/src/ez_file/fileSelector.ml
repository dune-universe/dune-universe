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
    filter:(bool -> string -> bool);
    follow_links:bool;
    error:(exn -> 'a -> unit);
    kinds : Unix.file_kind list option ;
}

let ok _for_rec _path = true
let no_error _exn _file = ()

let globber ?pathname glob =
  let regexp = Re.Glob.glob ~anchored:true ?pathname glob in
  let re = Re.compile regexp in
  Re.execp re

let create
    ?(deep=false)
    ?dft
    ?glob       (* keep these basenames, not used for recursion *)
    ?path       (* keep these paths, not used for recursion *)
    ?ignore     (* ignore these paths *)
    ?kinds (* keep only these kinds *)
    ?(filter=ok)
    ?(follow_links=false)
    ?(error=no_error)
    () =
  let filter = match glob with
    | None -> filter
    | Some glob ->
        let glob = globber glob in
        fun forrec path ->
          (forrec || glob (Filename.basename path)) && filter forrec path
  in
  let filter = match path with
    | None -> filter
    | Some glob ->
        let glob = globber glob in
        fun forrec path ->
          (forrec || glob path) && filter forrec path
  in
  let filter = match ignore with
    | None -> filter
    | Some glob ->
        let glob = globber glob in
        fun forrec path ->
          (forrec || not (glob path)) && filter forrec path
  in
  {
    deep;
    dft;
    filter;
    follow_links;
    error;
    kinds;
  }

let make_select
    f
    ?deep
    ?dft
    ?glob       (* keep these basenames, not used for recursion *)
    ?path       (* keep these paths, not used for recursion *)
    ?ignore     (* ignore these paths *)
    ?kinds (* keep only these kinds *)
    ?filter
    ?follow_links
    ?error
    file =
  let select =
    Some (create
            ?deep ?dft ?glob ?path ?ignore
            ?kinds ?filter ?follow_links
            ?error ()
         )
  in
  f ?select file
