open Common




let split (sep:char) (path:string): (string * string) option =
  let len = String.length path
  in
  let len =
    if 2 <= len && path.[len-1] = sep then
      len - 1
    else
      len
  in
  let pos = String.find_bwd (fun c -> c = sep) len path
  in
  if pos = -1 || (pos = 0 && len = 1) then
    None
  else
    let basename = String.sub path (pos+1) (len-pos-1)
    and dirname =
      if pos = 0 then
        String.one sep
      else
        String.sub path 0 pos
    in
    Some (dirname, basename)






let normalize (sep:char) (path:string): string =
  (* remove duplicate separators and normalize "." and ".." *)
  let rec norm lst nlst =
    match lst with
    | [] ->
       String.concat (String.one sep) (List.rev nlst)
    | "" :: tl ->
       if nlst = [] || tl = [] then
         norm tl ("" :: nlst)
       else
         norm tl nlst
    | "." :: tl ->
       norm tl nlst
    | h :: _ :: ".." :: tl ->
       norm (h :: tl) nlst
    | _ :: ".." :: tl ->
       norm tl nlst
    | h :: tl ->
       norm tl (h :: nlst)
  in
  if String.length path = 0 then
    "."
  else
    norm (String.split_on_char sep path) []







let%test _ =
  split '/' "" = None

let%test _ =
  split '/' "/" = None

let%test _ =
  split '/' "/hello" = Some ("/", "hello")

let%test _ =
  split '/' "/User/name/xxx" = Some ("/User/name", "xxx")

let%test _ =
  split '/' "/User/name/xxx/" = Some ("/User/name", "xxx")






let%test _ =
  normalize '/' "/" = "/"

let%test _ =
  normalize '/' "//" = "/"

let%test _ =
  normalize '/' "" = "."

let%test _ =
  normalize '/' "abc/def/ghi" = "abc/def/ghi"

let%test _ =
  normalize '/' "abc/" = "abc/"

let%test _ =
  normalize '/' "abc/def///ghi" = "abc/def/ghi"

let%test _ =
  normalize '/' "abc/./def" = "abc/def"

let%test _ =
  normalize '/' "a/../b" = "b"

let%test _ =
  normalize '/' "../a" = "../a"

let%test _ =
  normalize '/' "a/b/../../d" = "d"


