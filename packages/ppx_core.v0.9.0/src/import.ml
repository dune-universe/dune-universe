open Glue
include Base
include Stdio
include Ppx_ast

(* This is not re-exported by Base and we can't use [%here] in ppx_core *)
external __FILE__ : string = "%loc_FILE"

include Ast
