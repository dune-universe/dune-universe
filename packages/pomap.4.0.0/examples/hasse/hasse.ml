(*
   hasse.ml - generation of Hasse-diagrams of partially ordered random lists

   Copyright (C) 2001-2002  Markus Mottl  (OEFAI)
   email: markus.mottl@gmail.com
   WWW:   http://www.ocaml.info

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place --- Suite 330, Boston, MA 02111-1307, USA.
*)

open Format
open Po_examples
open Pomap

(* Parse arguments *)
let n, len, choices =
  let n_ref = ref 20
  and l_ref = ref 2
  and c_ref = ref 100
  and help_ref = ref false in
  Random.self_init ();

  let help = "-help", Arg.Set help_ref, "\tdisplay this list of options"

  and n =
   "-n",
   Arg.Int ((:=) n_ref),
   (sprintf "\t\tnumber of random elements (default: %d)" !n_ref)

  and l =
   "-l",
   Arg.Int ((:=) l_ref),
   (sprintf "\t\tlength of element lists (default: %d)" !l_ref)

  and c =
   "-c",
   Arg.Int ((:=) c_ref),
   (sprintf "\t\tnumber of choices per list element (default: %d)" !c_ref)

  and s = "-s", Arg.Int Random.init, "\t\tinitial random seed (default: random)"

  and usage =
    "Usage: hasse [-help] [-n int] [-l int] [-c int] [-s int]\n\n\
     Generates Hasse-diagrams of partially ordered random lists.\n\
     Prints to standard output in a format recognized by AT&T's \
     \"dot\"-utility.\n" in

  let args = [help; n; l; c; s] in
  let others _ = Arg.usage args usage; exit 1 in
  Arg.parse args others usage;
  if !help_ref then begin Arg.usage args usage; exit 0 end;
  !n_ref, !l_ref, !c_ref

module PO = MakePONList (struct let len = len let choices = choices end)
module POMap = Pomap_impl.Make (PO)

open Display_hasse_impl

module DisplaySpec = struct
  include DefaultSpec

  type el = unit
  type 'a node = 'a POMap.node

  let pp_node_attr ppf node =
    fprintf ppf "label = \"%a\"" PO.pretty_print (POMap.get_key node)

  let rotation = 0.
end

module DisplayHasse = Display_hasse_impl.Make (POMap) (DisplaySpec)

let rec iterate f x n = if n <= 0 then x else iterate f (f x) (n - 1)
let ins_rand pm = POMap.add (PO.rand_el ()) () pm

let _ =
  let pm = iterate ins_rand POMap.empty n in
  DisplayHasse.printf pm
