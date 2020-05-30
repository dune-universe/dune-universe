open Filename

let split_extension s = 
  try
    let body = chop_extension s in
    body, 
    String.sub s 
      (String.length body) 
      (String.length s - String.length body)
  with
  | Invalid_argument _ -> s, ""

let is_root d = not (is_relative d) && dirname d = d

let change_extension name ~ext =
  try Filename.chop_extension name ^ ext with
  | Invalid_argument _ (* chop_extension failed *) -> name ^ ext

[%%TEST
  change_extension "hello_world.txt" ~ext:".obj" = "hello_world.obj";;
  change_extension "hello.world.txt" ~ext:".obj" = "hello.world.obj";;
  change_extension "hello_world"     ~ext:".obj" = "hello_world.obj";;
  change_extension "hello_world"     ~ext:"obj"  = "hello_worldobj";;
]

let split_dir path =        
  let rec split_dir ds p =
    let d = dirname p in
    if d = p then d::ds
    else split_dir (basename p::ds) d 
  in
  split_dir [] path

[%%TEST
  split_dir "/a/b/c/d" = ["/"; "a"; "b"; "c"; "d"];;
  split_dir "/a/b/c/d/" = ["/"; "a"; "b"; "c"; "d"];;
  split_dir "a/b/c/d" = ["."; "a"; "b"; "c"; "d"];;

  if Sys.os_type <> "Win32" then true
  else split_dir "c:/a/b/c/d" = ["c:/"; "a"; "b"; "c"; "d"];;

  if Sys.os_type <> "Win32" then true
  else split_dir "\\a\\b\\c\\d" = ["\\"; "a"; "b"; "c"; "d"];;

  split_dir "/a/b/./c/d" = ["/"; "a"; "b"; "."; "c"; "d"];;
  split_dir "/a/b/../c/d" = ["/"; "a"; "b"; ".."; "c"; "d"];;
  split_dir "../a/b/c/d" = ["."; ".."; "a"; "b"; "c"; "d"];;
]

module Stdlib = struct
  let (^/) p1 p2 =
    if Filename.is_relative p2 then Filename.concat p1 p2 else p2
end

let concats = Xlist.fold_left1 Stdlib.(^/)
