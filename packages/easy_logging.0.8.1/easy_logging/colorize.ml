(*
    This file is part of easy_logging.

    easy_logging is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    easy_logging is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with easy_logging.  If not, see <https://www.gnu.org/licenses/>.
*)


(** Terminal styling function *)

(** Terminal colors *)
type color =
  | Default | Black | Red | Green | Yellow
  | Blue | Magenta | Cyan | Gray | White
  | LRed | LGreen | LYellow | LBlue
  | LMagenta | LCyan | LGray


(** Terminal styles*)
type format =
  | Bold | Underline | Invert
  | Fg of color | Bg of color


let to_fg_code c =
  match c with
  | Default -> 39 | Black -> 30  | Red -> 31
  | Green -> 32   | Yellow -> 33 | Blue -> 34
  | Magenta -> 35 | Cyan -> 36   | Gray -> 90
  | White -> 97   | LRed -> 91   | LGreen -> 92
  | LYellow -> 93 | LBlue -> 94  | LMagenta -> 95
  | LCyan -> 96   | LGray -> 37

let to_bg_code c =
  match c with
  | Default -> 49 | Black -> 40  | Red -> 41
  | Green -> 42   | Yellow -> 43 | Blue -> 44
  | Magenta -> 45 | Cyan -> 46   | Gray -> 100
  | White -> 107  | LRed -> 101  | LGreen -> 102
  | LYellow -> 103| LBlue -> 104  | LMagenta -> 105
  | LCyan -> 106   | LGray -> 47

let style_to_codes s =
  match s with
  | Bold -> 1, 21
  | Underline -> 4, 24
  | Invert -> 7, 27
  | Fg c -> to_fg_code c, to_fg_code Default
  | Bg c -> to_bg_code c, to_bg_code Default


(** [format styles str] formats [str] to the given [styles] *)
let rec format styles str =
  match styles with
  | _ as s :: styles' ->
    let set, reset = style_to_codes s in
    Printf.sprintf "\027[%dm%s\027[%dm" set (format styles' str) reset
  | [] -> str
