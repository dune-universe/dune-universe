(* case insensitive, case perserving, unique lists based on hash
   tables

   Copyright (C) 2004 Eric Stokes, and The California State University
   at Northridge

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA
*)


type t = (string, string) Hashtbl.t;;

let create n = Hashtbl.create n;;
let mem lst item = Hashtbl.mem lst (String.lowercase item);;
let add lst item =
  let lcitem = String.lowercase item in
    if (Hashtbl.mem lst lcitem) = false then
      Hashtbl.add lst lcitem item; ();;
let addlst lst lst1 = List.iter (fun i -> add lst i) lst1;;
let remove lst item = Hashtbl.remove lst (String.lowercase item);;
let iter func lst = Hashtbl.iter (fun key valu -> func key) lst;;
let tolst lst = Hashtbl.fold (fun k v l -> v :: l) lst [];;
