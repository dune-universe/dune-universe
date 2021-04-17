
(* how to cut the input file into independant items *)

type t = Line (* default (e.g. for SMILES files) *)
       | Line_sep of string (* a line separator, at the end of each record
                               (MOL2, SDF, PDB, etc.) *)
       | Bytes of int (* a constant block size *)
       | Reg of Str.regexp (* a line-matching regexp *)

let of_string = function
  | "l" -> Line
  | demux_opt ->
    let head, tail = BatString.split demux_opt ~by:":" in
    match head with
    | "b" -> Bytes (int_of_string tail)
    | "s" -> Line_sep tail
    | "r" -> Reg (Str.regexp tail)
    | _ -> failwith ("Demux.of_string: unknown demux mode: " ^ demux_opt)
