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

open GuiGettext.Gettext

(* Give access to the init of GuiGettext *)
let init = Gettext.init

(* Build a simple window that display your name *)
let hello_you name =
  let spf x = Printf.sprintf x in
  let window =
    GWindow.window ~title:(s_ "Hello world !") ~border_width:12 ()
  in
  ignore (GMisc.label ~text:(spf (f_ "Hello %s") name) ~packing:window#add ());
  ignore (((window#event)#connect)#delete ~callback:(fun _ -> false));
  ignore ((window#connect)#destroy ~callback:(fun _ -> GMain.Main.quit ()));
  window#show ();
  GMain.Main.main ()
