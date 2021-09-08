open Datalog_AbstractSyntax
open AbstractSyntax

val parse_program : string -> (Proto_Program.t -> Proto_Program.t) option

val parse_edb : string -> ((Predicate.PredIdTable.table * ConstGen.Table.table * UtilsLib.IdGenerator.IntIdGen.t) -> (Rule.rule list * ConstGen.Table.table * UtilsLib.IdGenerator.IntIdGen.t)) option

val parse_query : string -> ((Predicate.PredIdTable.table * ConstGen.Table.table) -> (Predicate.predicate * Predicate.PredIdTable.table * Datalog_AbstractSyntax.ConstGen.Table.table)) option
