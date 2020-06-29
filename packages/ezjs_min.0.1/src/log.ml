open Js_of_ocaml

let log o = Firebug.console##log o

let log2 o1 o2 = Firebug.console##log_2 o1 o2

let log3 o1 o2 o3 = Firebug.console##log_3 o1 o2 o3

let log_str s = Firebug.console##log (Js.string s)

let log_str2 s1 s2 = Firebug.console##log_2 (Js.string s1) (Js.string s2)

let log_str3 s1 s2 s3 =
  Firebug.console##log_3 (Js.string s1) (Js.string s2) (Js.string s3)

let debug o = Firebug.console##debug o

let debug2 o1 o2 = Firebug.console##debug_2 o1 o2

let debug3 o1 o2 o3 = Firebug.console##debug_3 o1 o2 o3

let debug_str s = Firebug.console##debug (Js.string s)

let debug_str2 s1 s2 = Firebug.console##debug_2 (Js.string s1) (Js.string s2)

let debug_str3 s1 s2 s3 =
  Firebug.console##debug_3 (Js.string s1) (Js.string s2) (Js.string s3)

let warn o = Firebug.console##warn o

let warn2 o1 o2 = Firebug.console##warn_2 o1 o2

let warn3 o1 o2 o3 = Firebug.console##warn_3 o1 o2 o3

let warn_str s = Firebug.console##warn (Js.string s)

let warn_str2 s1 s2 = Firebug.console##warn_2 (Js.string s1) (Js.string s2)

let warn_str3 s1 s2 s3 =
  Firebug.console##warn_3 (Js.string s1) (Js.string s2) (Js.string s3)

let error o = Firebug.console##error o

let error2 o1 o2 = Firebug.console##error_2 o1 o2

let error3 o1 o2 o3 = Firebug.console##error_3 o1 o2 o3

let error_str s = Firebug.console##error (Js.string s)

let error_str2 s1 s2 = Firebug.console##error_2 (Js.string s1) (Js.string s2)

let error_str3 s1 s2 s3 =
  Firebug.console##error_3 (Js.string s1) (Js.string s2) (Js.string s3)
