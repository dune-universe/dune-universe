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

open ProgramGettext.Gettext

let () =
  let my_name = ref "" in
  let spf x = Printf.sprintf x in
  let gettext_args, gettext_copyright = ProgramGettext.Gettext.init in
  let args =
    Arg.align
      ( [
          ( "--my-name",
            Arg.String (fun s -> my_name := s),
            spf (f_ "name Your name. Default : %S") !my_name );
        ]
      @ gettext_args )
  in
  let () =
    Arg.parse args
      (fun _ -> ())
      (spf
         (f_
            "\"Hello you\" program by Sylvain Le Gall\n\n\
             %s\n\n\
             Command: program [options]\n\n\
             Options:")
         gettext_copyright)
  in
  ExamplesLibrary.Library.hello_you !my_name;
  ExamplesGUI.Gui.hello_you !my_name
