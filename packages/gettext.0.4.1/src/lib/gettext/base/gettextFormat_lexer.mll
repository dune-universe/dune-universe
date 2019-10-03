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

{

  open GettextFormat_parser;;

}

let flags = ['-''0''+'' ']* ['0'-'9']* ('.'['0'-'9']*)?

rule
token = parse
 '%' flags    { format_char lexbuf }
| eof         { EOF }
| _           { token lexbuf }
and
format_char = parse
  "d"  as fc
| "i"  as fc
| "n"  as fc
| "N"  as fc
| "u"  as fc
| "x"  as fc
| "X"  as fc
| "o"  as fc
| "s"  as fc
| "S"  as fc
| "c"  as fc
| "C"  as fc
| "f"  as fc
| "F"  as fc
| "e"  as fc
| "E"  as fc
| "g"  as fc
| "G"  as fc
| "B"  as fc
| "b"  as fc
| "ld" as fc
| "li" as fc
| "lu" as fc
| "lx" as fc
| "lX" as fc
| "lo" as fc
| "nd" as fc
| "ni" as fc
| "nu" as fc
| "nx" as fc
| "nX" as fc
| "no" as fc
| "Ld" as fc
| "Li" as fc
| "Lu" as fc
| "Lx" as fc
| "LX" as fc
| "Lo" as fc
| "a"  as fc
| "t"  as fc  { FORMAT_CHAR fc }
| "!"
| "%"         { token lexbuf }

