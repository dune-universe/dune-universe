(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

{

  let buffer = Buffer.create 1024
}
(* scan the main LaTeX file to extract its prelude *)

rule scan = parse
  | "\\%" as s
      { Buffer.add_string buffer s; scan lexbuf }
  | "%" [^'\n']* '\n'
      { Buffer.add_char buffer '\n'; scan lexbuf }
  | _ as c
      { Buffer.add_char buffer c; scan lexbuf }
  | "\\begin{document}"
      { Buffer.contents buffer }
  | eof 
      { Buffer.contents buffer }

