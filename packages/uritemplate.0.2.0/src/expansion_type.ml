type t =
  | Simple (* {var} *)
  | Reserved (* {+var} *)
  | Fragment (* {#var} *)
  | Dot (* {.var} *)
  | PathSegment (* {/var} *)
  | PathParameter (* {;var} *)
  | FormQuery (* {?var} *)
  | FormQueryContinuation (* {&var} *)

let expansion_type_of_string = function
  | "+" -> Reserved
  | "#" -> Fragment
  | "." -> Dot
  | "/" -> PathSegment
  | ";" -> PathParameter
  | "?" -> FormQuery
  | "&" -> FormQueryContinuation
  | _ -> Simple

let string_of_expansion_type = function
  | Reserved -> "+"
  | Fragment -> "#"
  | Dot -> "."
  | PathSegment -> "/"
  | PathParameter -> ";"
  | FormQuery -> "?"
  | FormQueryContinuation -> "&"
  | Simple -> ""

let separator_for_expansion_type = function
  | Simple | Reserved | Fragment -> ','
  | Dot -> '.'
  | PathSegment -> '/'
  | PathParameter -> ';'
  | FormQuery | FormQueryContinuation -> '&'
