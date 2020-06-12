(**************************************************************************)
(*  ocaml-gettext: a library to translate messages                        *)
(*                                                                        *)
(*  Copyright (C) 2003-2008 Sylvain Le Gall <sylvain@le-gall.net>         *)
(*                                                                        *)
(*  This library is free software; you can redistribute it and/or         *)
(*  modify it under the terms of the GNU Lesser General Public            *)
(*  License as published by the Free Software Foundation; either          *)
(*  version 2.1 of the License, or (at your option) any later version;    *)
(*  with the OCaml static compilation exception.                          *)
(*                                                                        *)
(*  This library is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  Lesser General Public License for more details.                       *)
(*                                                                        *)
(*  You should have received a copy of the GNU Lesser General Public      *)
(*  License along with this library; if not, write to the Free Software   *)
(*  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *)
(*  USA                                                                   *)
(**************************************************************************)

(** Signature of module for domain management.
    @author Sylvain Le Gall
  *)

open FilePath
open FileUtil
open GettextTypes
open GettextCategory

(* BUG : a mettre à jour *)
(** compute_path textdomain category t : return the path to the mo file
   corresponding to textdomain and category. Language is guessed from category
   binding. If the textdomain is not found, it tries to use the build default
   to find the file. The file returned exists and is readable. If such a file
   doesn't exists an exception DomainFileDoesntExist is thrown. If the function
   is unable to guess the current language an exception DomainLanguageNotSet is
   thrown.
*)

let make_filename dir language category textdomain =
  (* http://www.gnu.org/software/gettext/manual/html_mono/gettext.html#SEC148
    dir_name/locale/LC_category/domain_name.mo *)
  make_filename
    [
      (* BUG : should use add_extension *)
        dir;
      language;
      string_of_category category;
      textdomain ^ ".mo";
    ]

let find t languages category textdomain =
  let search_path =
    ( try
        match MapTextdomain.find textdomain t.textdomains with
        | _, Some dir -> [ dir ]
        | _, None -> []
      with Not_found -> [] )
    @ t.path
  in
  let ctest = test (And (Exists, Is_readable)) in
  let rec find_mo_file_aux dir languages =
    match languages with
    | language :: tl ->
        let current_filename =
          make_filename dir language category textdomain
        in
        if ctest current_filename then current_filename
        else find_mo_file_aux dir tl
    | [] -> raise Not_found
  in
  let rec find_mo_file path languages =
    match path with
    | dir :: tl -> (
        try find_mo_file_aux dir languages
        with Not_found -> find_mo_file tl languages )
    | [] -> raise Not_found
  in
  try find_mo_file search_path languages
  with Not_found ->
    raise
      (DomainFileDoesntExist
         (List.flatten
            (List.map
               (fun dir ->
                 List.map
                   (fun language ->
                     make_filename dir language category textdomain)
                   languages)
               search_path)))
