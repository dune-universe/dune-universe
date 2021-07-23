(** If [dest] begins with /, returns a URL equivalent to [dest] relative to URL
    [src]. If [dest] does not begin with /, returns [dest]. *)
val relativize : src:string -> dest:string -> string
