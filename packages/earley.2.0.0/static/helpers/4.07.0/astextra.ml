(** Extra types missing for old version *)

open Asttypes
open Docstrings
open Parsetree

type lid = Longident.t loc
type str = string loc
type loc = Location.t
type attrs = attribute list
