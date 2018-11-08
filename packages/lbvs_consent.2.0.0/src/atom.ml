
(* very small atom module to help compute atom environments (a la molprint2d) from MOL2 files *)

open Printf

type t = { idx: int;
           typ: string }

let create (idx: int) (typ: string): t =
  (* indexes start at 1 in MOL2 files *)
  { idx = idx - 1; typ }

let dummy = create (-1) ""

(* example line (output of OpenBabel):
   "      1 S          -0.0218    1.7554    0.0117 S.2     1  LIG1       -0.0637" *)
let of_mol2_line (l: string): t =
  try Scanf.sscanf l " %d %s %f %f %f %s@ %s@ %s@ %s"
        (fun idx _name _x _y _z typ _bs0 _bs1 _bs2 -> create idx typ)
  with _ -> failwith ("Atom.of_mol2_line: could not parse: " ^ l)

let to_string (a: t): string =
  sprintf "%d %s" a.idx a.typ
