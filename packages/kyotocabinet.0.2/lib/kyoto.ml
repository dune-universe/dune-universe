type db
type cursor

type db_type =
|  TYPEVOID
|  TYPEPHASH
|  TYPEPTREE
|  TYPESTASH
|  TYPECACHE
|  TYPEGRASS
|  TYPEHASH
|  TYPETREE
|  TYPEDIR
|  TYPEFOREST
|  TYPETEXT
|  TYPEMISC

type open_flag =
|  OREADER
|  OWRITER
|  OCREATE
|  OTRUNCATE
|  OAUTOTRAN
|  OAUTOSYNC
|  ONOLOCK
|  OTRYLOCK
|  ONOREPAIR

exception Error of string
let _ = Callback.register_exception "kyotocabinet.error" (Error "any string")

external opendb: string -> open_flag list -> db = "kc_open"
external close: db -> unit = "kc_close"

let with_db path flags f =
  let db = opendb path flags in
  try
    let res = f db in
    let () = close db in
    res
  with e ->
    let () = close db in
    raise e

external count: db -> int64 = "kc_count"
external size: db -> int64 = "kc_size"
external path: db -> string = "kc_path"
external status: db -> string = "kc_status"

external exists: db -> string -> bool = "kc_exists"
external get: db -> string -> string option = "kc_get"
external find: db -> string -> string = "kc_find"

external set: db -> string -> string -> unit = "kc_set"
external add: db -> string -> string -> unit = "kc_add"
external replace: db -> string -> string -> unit = "kc_replace"
external remove: db -> string -> unit = "kc_remove"

let update db init plus key value =
  match get db key with
  | None ->   set db key (init value)
  | Some v -> set db key (plus v value)

external cursor_open: db -> cursor = "kc_cursor_open"
external cursor_next: cursor -> (string*string) option = "kc_cursor_next"
external cursor_jump: cursor -> string -> unit = "kc_cursor_jump"
external cursor_close: cursor -> unit = "kc_cursor_close"

let with_cursor db f =
  let c = cursor_open db in
  try
    let res = f c in
    let () = cursor_close c in
    res
  with e ->
    let () = cursor_close c in
    raise e

let fold db f seed =
  let fold c =
    let rec loop acc =
      match cursor_next c with
      | None -> acc
      | Some kv -> loop (f acc kv)
    in
    loop seed
  in
  with_cursor db fold

let fold_while db init p f seed =
  let fold c =
    let () = init c in
    let rec loop acc =
      match cursor_next c with
      | None -> acc
      | Some kv ->
      let k = fst kv in
      if p k then loop (f acc kv) else acc
    in
    loop seed
  in
  with_cursor db fold

external is_prefix: string -> string -> bool = "kc_is_prefix"

let fold_prefix db prefix =
  fold_while db (fun c -> cursor_jump c prefix) (is_prefix prefix)

let fold_range db min max =
  fold_while db (fun c -> cursor_jump c min) (fun s -> String.compare s max < 0)

external begin_tran: db -> unit = "kc_begin_tran"
external begin_tran_sync: db -> unit = "kc_begin_tran_sync"
external commit_tran: db -> unit = "kc_commit_tran"
external abort_tran: db -> unit = "kc_abort_tran"

let with_transaction db f =
  try
    let () = begin_tran db in
    let res = f db in
    let () = commit_tran db in
    res
  with e ->
    let () = abort_tran db in
    raise e
  
let with_transaction_sync db f =
  try
    let () = begin_tran_sync db in
    let res = f db in
    let () = commit_tran db in
    res
  with e ->
    let () = abort_tran db in
    raise e
  
