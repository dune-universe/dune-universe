val zprintf : ('a, 'b, unit) format -> 'a
  (** Consumer of format string. Does nothing.

      Expression
        zprintf "complex %d format %s string" 10 "hello"
      does not perform any internal formatting of the format string
      and discards it and the corresponding arguments. 
  *)
      
val format_check :
  ('a, 'b, 'c, 'd, 'e, 'f) format6 
  -> string 
  -> ('a, 'b, 'c, 'd, 'e, 'f) format6
  (** runtime format type check. [format_check template string] checks
      [string] has the same type as [template] as format string. If it
      has, it converts [string] to the corresponding printf format
      string. Otherwise it raises Invalid_argument.
  *)

      
