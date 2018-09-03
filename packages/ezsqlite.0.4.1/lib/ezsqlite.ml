exception Sqlite_error of string

let _ =
    Callback.register_exception "sqlite error" (Sqlite_error "")

module Value = struct
    type value =
        | Null
        | Blob of Bytes.t
        | Text of string
        | Double of float
        | Integer of Int64.t

    type kind =
        | INTEGER
        | DOUBLE
        | TEXT
        | BLOB
        | NULL

    exception Invalid_type

    let is_null = function
        | Null -> true
        | _ -> false

    let to_string = function
        | Null -> "NULL"
        | Integer i -> Int64.to_string i
        | Double d -> string_of_float d
        | Text t -> "'" ^ Str.global_replace (Str.regexp "'") "''" t ^ "'"
        | Blob b ->
            let x = Bytes.to_string b |> Hex.of_string |> fun (`Hex h) -> String.uppercase_ascii h in
            "X'" ^ Str.global_replace (Str.regexp "'") "''" x ^ "'"

    let get_string = function
        | Null -> ""
        | Blob s -> Bytes.to_string s
        | Text s -> s
        | Integer i -> Int64.to_string i
        | Double d -> string_of_float d

    let get_bytes = function
        | Blob s -> s
        | _ -> raise Invalid_type

    let get_float = function
        | Integer i -> Int64.to_float i
        | Double d -> d
        | Text s -> begin try float_of_string s
            with _ -> raise Invalid_type end
        | _ -> raise Invalid_type

    let get_int = function
        | Integer i -> Int64.to_int i
        | Double d -> int_of_float d
        | Text s -> begin try int_of_string s
            with _ -> raise Invalid_type end
        | _ -> raise Invalid_type

    let get_int64 = function
        | Integer i -> i
        | Double d -> Int64.of_float d
        | Text s -> begin try Int64.of_string s
            with _ -> raise Invalid_type end
        | _ -> raise Invalid_type

    let get_bool = function
        | Integer 0L -> false
        | Integer _ -> true
        | Double 0. -> false
        | Double _ -> true
        | Text ("true"|"TRUE") -> true
        | Text ("false"|"FALSE") -> false
        | _ -> raise Invalid_type

    let kind_of_int = function
        | 1 -> INTEGER
        | 2 -> DOUBLE
        | 3 -> TEXT
        | 4 -> BLOB
        | _ -> NULL
end

open Value

(* DB *)

type t_handle
type t = {
    filename : string;
    mutable db : t_handle;
}

external _ezsqlite_db_load : string -> t_handle = "_ezsqlite_db_load"
external _ezsqlite_db_free : t_handle -> unit = "_ezsqlite_db_free"
external _ezsqlite_create_function :  t_handle -> string -> int -> unit = "_ezsqlite_db_create_function"

let load path =
    let db = {
        filename = path;
        db = _ezsqlite_db_load path;
    } in
    let _ = Gc.finalise (fun x ->
        _ezsqlite_db_free x.db) db in db

let auto_extension fn =
    Callback.register "auto extension" (fun x -> fn {filename = ""; db = x})

let commit_hook fn =
    Callback.register "commit hook" fn

let update_hook fn =
    Callback.register "update hook" fn

let create_function db name nargs fn =
    Callback.register name fn;
    _ezsqlite_create_function db.db name nargs

(* STMT *)

type stmt_handle
type stmt = {
    raw : string;
    mutable stmt : stmt_handle;
}

external _ezsqlite_stmt_prepare : t_handle -> string -> stmt_handle = "_ezsqlite_stmt_prepare"
external _ezsqlite_stmt_finalize : stmt_handle -> unit = "_ezsqlite_stmt_finalize"
external _ezsqlite_stmt_step : stmt_handle -> bool = "_ezsqlite_stmt_step"
external _ezsqlite_stmt_reset : stmt_handle -> unit = "_ezsqlite_stmt_reset"
external _ezsqlite_stmt_clear_bindings : stmt_handle -> unit = "_ezsqlite_stmt_clear_bindings"

let prepare db s =
    let stmt = {
        raw = s;
        stmt = _ezsqlite_stmt_prepare db.db s;
    } in
    let _ = Gc.finalise (fun x ->
        _ezsqlite_stmt_finalize x.stmt) stmt in stmt

let step stmt = _ezsqlite_stmt_step stmt.stmt
let reset stmt = _ezsqlite_stmt_reset stmt.stmt
let clear_bindings stmt = _ezsqlite_stmt_clear_bindings stmt.stmt

let clear stmt =
    reset stmt;
    clear_bindings stmt

external _ezsqlite_stmt_parameter_count : stmt_handle -> int = "_ezsqlite_stmt_parameter_count"
external _ezsqlite_stmt_parameter_index : stmt_handle -> string -> int  = "_ezsqlite_stmt_parameter_index"
let parameter_count stmt = _ezsqlite_stmt_parameter_count stmt.stmt
let parameter_index stmt = _ezsqlite_stmt_parameter_index stmt.stmt

(* BIND *)
external _ezsqlite_bind_null : stmt_handle -> int -> unit = "_ezsqlite_bind_null"
external _ezsqlite_bind_blob : stmt_handle -> int -> Bytes.t -> unit = "_ezsqlite_bind_blob"
external _ezsqlite_bind_text : stmt_handle -> int -> string -> unit = "_ezsqlite_bind_text"
external _ezsqlite_bind_double : stmt_handle -> int -> float -> unit = "_ezsqlite_bind_double"
external _ezsqlite_bind_int64 : stmt_handle -> int -> int64 -> unit = "_ezsqlite_bind_int64"

let bind stmt i = function
    | Null -> _ezsqlite_bind_null stmt.stmt i
    | Blob s -> _ezsqlite_bind_blob stmt.stmt i s
    | Text s -> _ezsqlite_bind_text stmt.stmt i s
    | Double d -> _ezsqlite_bind_double stmt.stmt i d
    | Integer d -> _ezsqlite_bind_int64 stmt.stmt i d

let bind_dict stmt dict =
    List.iter (fun (k, v) ->
        let i = parameter_index stmt k in
        if i > 0 then
            bind stmt i v) dict

let bind_list stmt list =
    let len = parameter_count stmt in
    try
        List.iteri (fun i x ->
            if i >= len then failwith "end"
            else bind stmt (i + 1) x) list
    with _ -> ()

(* COLUMN *)
external _ezsqlite_data_count : stmt_handle -> int = "_ezsqlite_data_count"
external _ezsqlite_column_type : stmt_handle -> int -> int = "_ezsqlite_column_type"
external _ezsqlite_column_text : stmt_handle -> int -> string = "_ezsqlite_column_text"
external _ezsqlite_column_blob : stmt_handle -> int -> Bytes.t = "_ezsqlite_column_blob"
external _ezsqlite_column_int64 : stmt_handle -> int -> int64 = "_ezsqlite_column_int64"
external _ezsqlite_column_int : stmt_handle -> int -> int = "_ezsqlite_column_int"
external _ezsqlite_column_double : stmt_handle -> int -> float = "_ezsqlite_column_double"
external _ezsqlite_column_name : stmt_handle -> int -> string = "_ezsqlite_column_name"
external _ezsqlite_database_name : stmt_handle -> int -> string = "_ezsqlite_database_name"
external _ezsqlite_table_name : stmt_handle -> int -> string = "_ezsqlite_table_name"
external _ezsqlite_origin_name : stmt_handle -> int -> string = "_ezsqlite_origin_name"

let data_count stmt =  _ezsqlite_data_count stmt.stmt

let text stmt i = if i < data_count stmt then _ezsqlite_column_text stmt.stmt i else raise Not_found

let blob stmt i = if i < data_count stmt then _ezsqlite_column_blob stmt.stmt i else raise Not_found

let int64 stmt i = if i < data_count stmt then _ezsqlite_column_int64 stmt.stmt i else raise Not_found

let int stmt i = if i < data_count stmt then _ezsqlite_column_int stmt.stmt i else raise Not_found

let double stmt i = if i < data_count stmt then _ezsqlite_column_double stmt.stmt i else raise Not_found

let column_type stmt i = if i > data_count stmt then raise Not_found else kind_of_int (_ezsqlite_column_type stmt.stmt i)

let column stmt i =
    match column_type stmt i with
        | INTEGER -> Integer (int64 stmt i)
        | DOUBLE -> Double (double stmt i)
        | TEXT -> Text (text stmt i)
        | BLOB -> Blob (blob stmt i)
        | NULL -> Null

let data stmt =
    let len = data_count stmt in
    let dst = Array.make len Null in
    for i = 0 to len - 1 do
        dst.(i) <- column stmt i
    done; dst

let column_name stmt n = if n < data_count stmt then _ezsqlite_column_name stmt.stmt n else raise Not_found
let database_name stmt n = if n < data_count stmt then _ezsqlite_database_name stmt.stmt n else raise Not_found
let table_name stmt n = if n < data_count stmt then _ezsqlite_table_name stmt.stmt n else raise Not_found
let origin_name stmt n = if n < data_count stmt then _ezsqlite_origin_name stmt.stmt n else raise Not_found

let dict stmt =
    data stmt |> Array.to_list |> List.mapi (fun i x ->
        column_name stmt i, x)

let exec stmt =
    while step stmt do () done

let iter stmt fn =
    while step stmt do fn stmt done

let map stmt fn =
    let dst = ref [] in
    while step stmt do
        dst := fn stmt::!dst
    done; List.rev !dst

let fold stmt fn acc =
    let dst = ref acc in
    while step stmt do
        dst := fn stmt !dst
    done; !dst

let run db s ?bind:(bind=[]) fn =
    let x = prepare db s in
    let () = bind_list x bind in
    map x fn

let run_ign db s ?bind () =
    ignore (run db s ?bind ignore)

let dump_sql db name =
    let b = Buffer.create 1024 in
    run db "SELECT sql FROM sqlite_master WHERE name=?" ~bind:[Text name] (fun stmt ->
        Buffer.add_string b (text stmt 0 ^ ";\n")) |> ignore;
    run db ("SELECT * FROM " ^ name) (fun stmt ->
        let s = dict stmt in
        let fields, values = List.split s in
        let fields = String.concat ", " fields in
        let values = String.concat ", " (List.map Value.to_string values) in
        Buffer.add_string b (Printf.sprintf "INSERT INTO %s (%s) VALUES (%s);\n" name fields values)) |> ignore;
    Buffer.contents b


module Backup = struct
    type backup_handle
    type backup = {
        backup : backup_handle;
    }

    external _ezsqlite_backup_init : t_handle -> string -> t_handle -> string -> backup_handle = "_ezsqlite_backup_init"
    external _ezsqlite_backup_finish : backup_handle -> unit = "_ezsqlite_backup_finish"
    external _ezsqlite_backup_step : backup_handle -> int ->  bool = "_ezsqlite_backup_step"
    external _ezsqlite_backup_pagecount : backup_handle -> int = "_ezsqlite_backup_pagecount"
    external _ezsqlite_backup_remaining : backup_handle -> int = "_ezsqlite_backup_remaining"

    let init dst dstName src srcName =
        let b = {
            backup = _ezsqlite_backup_init dst.db dstName src.db srcName
        } in
        let _ = Gc.finalise (fun x ->
            _ezsqlite_backup_finish(x.backup)) in b

    let step b n =
        _ezsqlite_backup_step b.backup n

    let remaining b =
        _ezsqlite_backup_remaining b.backup

    let pagecount b =
        _ezsqlite_backup_pagecount b.backup

end

module Blob = struct
    type blob_handle

    type blob = {
        blob : blob_handle;
        mutable closed : bool
    }

    external _ezsqlite_blob_open_ro : t_handle -> string -> string -> string -> int64 -> blob_handle = "_ezsqlite_blob_open_ro"
    external _ezsqlite_blob_open_rw : t_handle -> string -> string -> string -> int64 -> blob_handle = "_ezsqlite_blob_open_rw"
    external _ezsqlite_blob_close : blob_handle -> unit = "_ezsqlite_blob_close"
    external _ezsqlite_blob_reopen : blob_handle -> int64 -> unit = "_ezsqlite_blob_reopen"
    external _ezsqlite_blob_bytes : blob_handle -> int  = "_ezsqlite_blob_bytes"
    external _ezsqlite_blob_read : blob_handle -> Bytes.t -> int -> int -> unit = "_ezsqlite_blob_read"
    external _ezsqlite_blob_write : blob_handle -> Bytes.t -> int -> int-> unit = "_ezsqlite_blob_write"

    let close blob =
        _ezsqlite_blob_close(blob.blob);
        blob.closed <- true

    let open_blob db ?dbname:(dbname="main") ?rw:(rw=false) table col i =
        let b = {
            blob =
                if rw then _ezsqlite_blob_open_rw db.db dbname table col i
                else _ezsqlite_blob_open_ro db.db dbname table col i;
            closed = false;
        } in
        let _ = Gc.finalise (fun x ->
            if not x.closed then
                close x) in b

    let reopen blob i =
        _ezsqlite_blob_reopen blob.blob i

    let length blob =
        _ezsqlite_blob_bytes blob.blob

    let read blob ?offs:(offs=0)  buf n =
        _ezsqlite_blob_read blob.blob buf n offs

    let write blob ?offs:(offs=0) buf =
        _ezsqlite_blob_write blob.blob buf (Bytes.length buf) offs

end

module Infix = struct
    include Value
    let (|~) db s = run_ign db s (); db
    let (|-) db (s, bind) = run_ign db s ~bind (); db
    let (|+) db (s, fn) = run db s fn
    let (|$) db (s, bind, fn) = run db s ~bind fn
end
