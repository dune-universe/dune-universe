val split_extension : string -> string * string
(** [split_extension path] split the body and extension of [path].
    [split_extension "hello.world.txt" = "hello.world", ".txt"]
    [split_extension "hello_world" = "hello_world", ""]
*)

val change_extension : string -> ext:string -> string
(** [change_extension f ~ext] changes the extension part of [f].
    If [f] has no extension then [change_extension f ~ext] just attach
    [ext] to [f].

    [change_extension "hello_world.txt" ~ext:".obj" = "hello_world.obj"]
    [change_extension "hello.world.txt" ~ext:".obj" = "hello.world.obj"]
    [change_extension "hello_world"     ~ext:".obj" = "hello_world.obj"]
    [change_extension "hello_world"     ~ext:"obj"  = "hello_worldobj"]

    Note that [change_extension] does not check [ext] begins with ['.'] or not.
*)

val split_dir : string -> string list
(** [split_dir path] splits [path] recursively using [dirname] 
    and returns the list of directory and basename components 
    of [path].

  [split_dir "/a/b/c/d" = ["/"; "a"; "b"; "c"; "d"]]
  [split_dir "/a/b/c/d/" = ["/"; "a"; "b"; "c"; "d"]]
  [split_dir "a/b/c/d" = ["."; "a"; "b"; "c"; "d"]]

  [split_dir "/a/b/./c/d" = ["/"; "a"; "b"; "."; "c"; "d"]]
  [split_dir "/a/b/../c/d" = ["/"; "a"; "b"; ".."; "c"; "d"]]
  [split_dir "../a/b/c/d" = ["."; ".."; "a"; "b"; "c"; "d"]]

  In Windows,
  [split_dir "c:/a/b/c/d" = ["c:/"; "a"; "b"; "c"; "d"]]
  [split_dir "\\a\\b\\c\\d" = ["\\"; "a"; "b"; "c"; "d"]]

*)
  
val is_root : string -> bool

module Pervasives : sig
  val (^/) : string -> string -> string
  (** Filename concatenation. If the second argument is absolute,
      the first is ignored and the second is just returened.

      "hello" ^/ "world" = "hello/world"
      "hello" ^/ "/world" = "/world"
  *)
end


