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

(** Ocaml-gettext tools.
    @author Sylvain Le Gall
  *)

(** Helper program to:
  - extract translatable strings from OCaml source,
  - compile PO file,
  - install MO file,
  - merge POT and PO file.
*)

open GettextTypes
open GettextCategory
open GettextUtils
open FilePath.DefaultPath
module OCamlGettextRealize = GettextDummy.Generic

module OcamlGettext =
  Gettext.Program
    (struct
      let textdomain = "ocaml-gettext"

      let codeset = None

      let dir = None

      let dependencies = Gettext.init
    end)
    (OCamlGettextRealize)

type action =
  | Extract
  | Compile
  | Install
  | Uninstall
  | Merge
  | Version
  | VersionShort

type t = {
  action_option : action option;
  extract_command : string;
  extract_default_option : string;
  extract_filename_options : (string * string) list;
  extract_pot : string;
  compile_output_file_option : string option;
  install_language_option : string option;
  install_category : GettextCategory.category;
  install_textdomain_option : string option;
  install_destdir : string;
  uninstall_language_option : string option;
  uninstall_category : GettextCategory.category;
  uninstall_textdomain_option : string option;
  uninstall_orgdir : string;
  merge_filename_pot : string;
  merge_backup_extension : string;
  input_files : string list;
  strict : bool;
}

exception ActionRequired

exception InstallUninstallTooManyFilename

exception CompileTooManyFilename

let string_of_exception exc =
  let s_ x = OcamlGettext.s_ x in
  match exc with
  | ActionRequired -> s_ "You must specify one action."
  | InstallUninstallTooManyFilename ->
      s_
        "You cannot specify at the same time a language, a textdomain\n\
         and provide more than one file to install/uninstall : all files\n\
         will have the same destination filename."
  | CompileTooManyFilename ->
      s_
        "You cannot specify a output filename and more than one\n\
         filename : all the compiled file will have the same output filename"
  | _ -> Gettext.string_of_exception exc

let do_extract t =
  let real_lst =
    let rec extract_potfiles accu lst =
      match lst with
      | str :: lst when str = "POTFILES" ->
          let chn = open_in str in
          let new_accu =
            let rec extract_potfiles_aux accu =
              try
                let new_filename = input_line chn in
                extract_potfiles_aux (new_filename :: accu)
              with End_of_file -> accu
            in
            extract_potfiles_aux accu
          in
          close_in chn;
          extract_potfiles new_accu lst
      | str :: lst -> extract_potfiles (str :: accu) lst
      | [] -> List.rev accu
    in
    extract_potfiles [] t.input_files
  in
  let map_filename_options =
    List.fold_left
      (fun map (fl, options) -> MapString.add fl options map)
      MapString.empty t.extract_filename_options
  in
  GettextCompile.extract t.extract_command t.extract_default_option
    map_filename_options real_lst t.extract_pot

let do_compile t =
  match (t.compile_output_file_option, t.input_files) with
  | Some fl_mo, [ fl_po ] -> GettextCompile.compile fl_po fl_mo
  | None, lst ->
      let fl_mo_of_fl_po fl_po =
        (* BUG: should use add_extension *)
        chop_extension fl_po ^ ".mo"
      in
      List.iter
        (fun fl_po -> GettextCompile.compile fl_po (fl_mo_of_fl_po fl_po))
        lst
  | Some _, [] -> ()
  | Some _, _ -> raise CompileTooManyFilename

let guess_language_textdomain (language_option, textdomain_option) lst =
  (* Rules for guessing language : language[.textdomain].mo *)
  match (language_option, textdomain_option, lst) with
  | Some language, Some textdomain, [ fl_mo ] ->
      [ (language, textdomain, fl_mo) ]
  | Some _, Some _, [] -> []
  | Some _, Some _, _ -> raise InstallUninstallTooManyFilename
  | Some language, None, lst ->
      List.map (fun fl_mo -> (language, chop_extension fl_mo, fl_mo)) lst
  | None, Some textdomain, lst ->
      List.map (fun fl_mo -> (chop_extension fl_mo, textdomain, fl_mo)) lst
  | None, None, lst ->
      List.map
        (fun _fl_mo ->
          (* BUG: should be able to have get_extension working *)
          (*
        let str_reduce =
          chop_extension fl_mo
        in
        * (((chop_extension str_reduce), (get_extension str_reduce)),fl_mo)*)
          raise
            (Failure
               "FilePath suffers from a default with the handling of\n\
                chop/get_extension. This bug should disappears with\n\
                newer version of ocaml-fileutils"))
        lst

let do_install t =
  let install (language, textdomain, fl_mo) =
    GettextCompile.install t.strict t.install_destdir language
      t.install_category textdomain fl_mo
  in
  List.iter install
    (guess_language_textdomain
       (t.install_language_option, t.install_textdomain_option)
       t.input_files)

let do_uninstall t =
  let uninstall (language, textdomain, _) =
    GettextCompile.uninstall t.uninstall_orgdir language t.uninstall_category
      textdomain
  in
  List.iter uninstall
    (guess_language_textdomain
       (t.uninstall_language_option, t.uninstall_textdomain_option)
       t.input_files)

let do_merge t =
  GettextCompile.merge t.merge_filename_pot t.input_files
    t.merge_backup_extension

let do_action t =
  match t.action_option with
  | Some Extract -> do_extract t
  | Some Compile -> do_compile t
  | Some Install -> do_install t
  | Some Uninstall -> do_uninstall t
  | Some Merge -> do_merge t
  | Some Version ->
      let _, gettext_copyright = OcamlGettext.init in
      print_string gettext_copyright;
      print_newline ()
  | Some VersionShort ->
      print_string GettextConfig.version;
      print_newline ()
  | None -> raise ActionRequired

let () =
  let spf x = Printf.sprintf x in
  let f_ x = OcamlGettext.f_ x in
  let s_ x = OcamlGettext.s_ x in
  let t =
    ref
      {
        action_option = None;
        extract_command = "ocaml-xgettext";
        extract_default_option = "";
        extract_filename_options = [];
        extract_pot = "messages.pot";
        compile_output_file_option = None;
        install_language_option = None;
        install_category = LC_MESSAGES;
        install_textdomain_option = None;
        install_destdir = GettextConfig.default_dir;
        uninstall_language_option = None;
        uninstall_category = LC_MESSAGES;
        uninstall_textdomain_option = None;
        uninstall_orgdir = GettextConfig.default_dir;
        merge_filename_pot = "messages.pot";
        merge_backup_extension = "bak";
        input_files = [];
        strict = false;
      }
  in
  let actions =
    [
      ("extract", Extract);
      ("compile", Compile);
      ("install", Install);
      ("uninstall", Uninstall);
      ("merge", Merge);
    ]
  in
  let gettext_args, gettext_copyright = OcamlGettext.init in
  let args =
    Arg.align
      ( [
          ( "--action",
            Arg.Symbol
              ( List.map fst actions,
                fun symbol ->
                  try
                    t :=
                      {
                        !t with
                        action_option = Some (List.assoc symbol actions);
                      }
                  with Not_found ->
                    raise (Arg.Bad (spf (f_ "Invalid action: %s.") symbol)) ),
            s_ "Action to execute. Default: none." );
          ( "--extract-command",
            Arg.String (fun cmd -> t := { !t with extract_command = cmd }),
            spf
              (f_
                 "cmd Command to extract translatable strings from an OCaml \
                  source file. Default: %s.")
              !t.extract_command );
          ( "--extract-default-option",
            Arg.String
              (fun default_option ->
                t := { !t with extract_default_option = default_option }),
            spf
              (f_
                 "options Default option used when extracting translatable \
                  strings. Default: %S.")
              !t.extract_default_option );
          ( "--extract-filename-option",
            Arg.Tuple
              (let filename = ref "" in
               [
                 Arg.String (fun str -> filename := str);
                 Arg.String
                   (fun options ->
                     t :=
                       {
                         !t with
                         extract_filename_options =
                           (!filename, options) :: !t.extract_filename_options;
                       });
               ]),
            spf
              (f_
                 "filename options Per filename option used when extracting \
                  strings from the specified filename. Default: %s.")
              (string_of_list
                 (List.map
                    (fun (str1, str2) -> spf "(%s,%s)" str1 str2)
                    !t.extract_filename_options)) );
          ( "--extract-pot",
            Arg.String (fun str -> t := { !t with extract_pot = str }),
            spf
              (f_
                 "filename POT file to write when extracting translatable \
                  strings. Default: %s.")
              !t.extract_pot );
          ( "--compile-output",
            Arg.String
              (fun str ->
                t := { !t with compile_output_file_option = Some str }),
            s_
              "filename MO file to write when compiling a PO file. Default: \
               name of the PO file with \".mo\" extension." );
          ( "--install-language",
            Arg.String
              (fun str -> t := { !t with install_language_option = Some str }),
            s_
              "language Language to use when installing a MO file. Default: \
               try to guess it from the name of the MO file." );
          ( "--install-category",
            Arg.String
              (fun str ->
                t :=
                  {
                    !t with
                    install_category = GettextCategory.category_of_string str;
                  }),
            spf
              (f_
                 "category Category to use when installing a MO file. \
                  Default: %s.")
              (GettextCategory.string_of_category !t.install_category) );
          ( "--install-textdomain",
            Arg.String
              (fun str ->
                t := { !t with install_textdomain_option = Some str }),
            s_
              "textdomain Textdomain to use when installing a MO file. \
               Default: try to guess it from the name of the MO file." );
          ( "--install-destdir",
            Arg.String (fun str -> t := { !t with install_destdir = str }),
            spf
              (f_
                 "dirname Base dir used when installing a MO file. Default: %s.")
              !t.install_destdir );
          ( "--strict",
            Arg.Unit (fun () -> t := { !t with strict = true }),
            spf
              (f_ " Additional check are errors during install. Default: %b.")
              !t.strict );
          ( "--uninstall-language",
            Arg.String
              (fun str ->
                t := { !t with uninstall_language_option = Some str }),
            s_
              "language Language to use when uninstalling a MO file. Default: \
               try to guess it from the name of the MO file." );
          ( "--uninstall-category",
            Arg.String
              (fun str ->
                t :=
                  {
                    !t with
                    uninstall_category = GettextCategory.category_of_string str;
                  }),
            spf
              (f_
                 "category Category to use when uninstalling a MO file.  \
                  Default: %s.")
              (GettextCategory.string_of_category !t.uninstall_category) );
          ( "--uninstall-textdomain",
            Arg.String
              (fun str ->
                t := { !t with uninstall_textdomain_option = Some str }),
            s_
              "textdomain Textdomain to use when uninstalling a MO file. \
               Default: try to guess it from the name of the MO file." );
          ( "--uninstall-orgdir",
            Arg.String (fun str -> t := { !t with uninstall_orgdir = str }),
            spf
              (f_
                 "dirname Base dir used when uninstalling a MO file. Default: \
                  %s.")
              !t.uninstall_orgdir );
          ( "--merge-pot",
            Arg.String (fun str -> t := { !t with merge_filename_pot = str }),
            spf
              (f_
                 "filename POT file to use as a master for merging PO file. \
                  Default: %s.")
              !t.merge_filename_pot );
          ( "--merge-backup-extension",
            Arg.String
              (fun str -> t := { !t with merge_backup_extension = str }),
            spf
              (f_
                 "extension Backup extension to use when moving PO file which \
                  have been merged. Default: %s.")
              !t.merge_backup_extension );
          ( "--version",
            Arg.Unit (fun () -> t := { !t with action_option = Some Version }),
            s_ " Returns version information on ocaml-gettext." );
          ( "--short-version",
            Arg.Unit
              (fun () -> t := { !t with action_option = Some VersionShort }),
            s_ " Returns only the version string of ocaml-gettext." );
        ]
      @ gettext_args )
  in
  let () =
    Arg.parse args
      (fun str -> t := { !t with input_files = str :: !t.input_files })
      (spf
         (f_
            "%s\n\n\
             Command: ocaml-gettext -action (%s) [options]\n\
             When trying to guess language and textdomain from a\n\
             MO file, the rules applied are: language.textdomain.mo\n\n\
             Options:")
         gettext_copyright
         (String.concat "|" (List.map fst actions)))
  in
  try do_action !t
  with exc ->
    prerr_string (string_of_exception exc);
    prerr_newline ();
    prerr_string (s_ "An error occurs while processing.");
    prerr_newline ();
    exit 1
