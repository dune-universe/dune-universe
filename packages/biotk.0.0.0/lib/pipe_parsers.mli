open Biocaml_base
include module type of Biotk_pipes_unix.Pipe

val lines : unit -> (string, Line.t, unit) t
val lines_to_strings : unit -> (Line.t, string, unit) t
val bed_parser : unit -> (string, Biocaml_base.Bed.item, unit) t
val bed_unparser : unit -> (Biocaml_base.Bed.item, string, unit) t
val gff3_parser : unit -> (string, Gff.item, unit) t
val gff_unparser : [`two | `three] -> (Gff.item, string, unit) t
val table_parser : unit -> (string, string list, unit) t
val table_unparser : unit -> (string list, string, unit) t

val macs_xls_parser : (Line.t, Macs.Xls.item, unit) t
val macs_xls_unparser : (Macs.Xls.item, string, unit) t

val fasta_parser : unit -> (string, Biocaml_base.Fasta.item, unit) t
