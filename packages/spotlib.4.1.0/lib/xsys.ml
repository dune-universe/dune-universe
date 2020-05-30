let mkdir = Xunix.mkdir

let checkenv s = try ignore (Sys.getenv s); true with Not_found -> false

let command fmt = Printf.kprintf Sys.command fmt
let must fmt = Printf.kprintf (fun s -> if Sys.command s <> 0 then Exn.failwithf "command %s failed" s) fmt
let cp = must "/bin/cp %s %s"
let patch_p1 = must "/bin/patch -p1 < %s"

let with_chdir d f v =
  let cwd = Sys.getcwd () in
  Exn.protect 
    (fun () -> 
      Sys.chdir d;
      f v)
    ()
    ~finally:(fun () -> Sys.chdir cwd)

let with_chdir_ d f = with_chdir d f ()
