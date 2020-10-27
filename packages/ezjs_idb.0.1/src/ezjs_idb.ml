open Ezjs_min

module Types = Ezjs_idb_types
open Types

type mode = READONLY | READWRITE | VERSIONCHANGE
type direction = NEXT | PREV | NEXTUNIQUE | PREVUNIQUE

type db_options = { key_path : string option; auto_increment : bool option }

type db_upgrade = { old_version : int; new_version : int }

type index_options = { unique : bool option; multi_entry : bool option; locale : bool option }

module type Tr_sig = sig
  type t
  type js
  val to_js : t -> js
  val of_js : js -> t
end

let str_of_mode = function
  | READONLY -> string "readonly"
  | READWRITE -> string "readwrite"
  | VERSIONCHANGE -> string "versionchange"

let str_of_direction = function
  | NEXT -> string "next"
  | PREV -> string "prev"
  | NEXTUNIQUE -> string "nextunique"
  | PREVUNIQUE -> string "prevunique"

let iDBKeyRange : Unsafe.any iDBKeyRange t = Unsafe.variable "IDBKeyRange"
let indexedDB : js_string t iDBFactory t = Unsafe.variable "window.indexedDB"

let result r = r##.result

let wrap ?error ?callback r =
  try
    let r = Lazy.force r in
    r##.onsuccess := AOpt.aopt (fun f -> wrap_meth_callback (fun r _e -> f (result r))) callback;
    r##.onerror := AOpt.aopt (fun f -> wrap_meth_callback (fun r _e -> f r)) error
  with exn ->
    match error with None -> raise exn | Some f -> catch_exn (fun e -> f (Unsafe.coerce e)) exn

let wrapf ?error ?callback g r =
  try
    let r = Lazy.force r in
    r##.onsuccess := AOpt.aopt (fun f -> wrap_meth_callback (fun r _e -> f (g @@ result r))) callback;
    r##.onerror := AOpt.aopt (fun f -> wrap_meth_callback (fun r _e -> f r)) error
  with exn ->
    match error with None -> raise exn | Some f -> catch_exn (fun e -> f (Unsafe.coerce e)) exn

let db_upgrade_event (e : iDBVersionChangeEvent t) = {
  old_version = e##.oldVersion;
  new_version = e##.newVersion;
}

let openDB ?upgrade ?error ?version name callback =
  let indexedDB : _ iDBFactory t = Unsafe.variable "window.indexedDB" in
  let r = lazy (indexedDB##_open (string name) (AOpt.option version)) in
  wrap ?error ~callback r;
  (Lazy.force r)##.onupgradeneeded :=
    AOpt.aopt (fun u -> wrap_meth_callback (fun r e ->
        let v = db_upgrade_event e in
        let db = result r in
        u db v)) upgrade

let transaction r : _ iDBTransaction t = r##.transaction

let create_db_options {key_path; auto_increment} : create_db_options t = object%js
  val keyPath = AOpt.aopt string key_path
  val autoIncrement = AOpt.aopt bool auto_increment
end

let create_store ?options (db : iDBDatabase t) name =
  Unsafe.coerce @@
  db##createObjectStore (string name) (AOpt.aopt create_db_options options)

let get_store ?mode (db : iDBDatabase t) name : (_, _) iDBObjectStore t =
  let tr = db##transaction (array [| string name |]) (AOpt.aopt str_of_mode mode) in
  Unsafe.coerce @@ tr##objectStore (string name)

module Store(K : Tr_sig)(D : Tr_sig) = struct

  type store = (K.js, D.js) iDBObjectStore t
  type keys = K of K.t | KR of K.js iDBKeyRange t

  let create ?options db name : store =
    create_store ?options db name

  let store ?mode db name : store =
    get_store ?mode db name

  let add ?callback ?error ?key (st : store) (x : D.t) =
    wrapf ?callback ?error K.of_js @@ lazy (st##add (D.to_js x) (AOpt.aopt K.to_js key))

  let put ?callback ?error ?key (st : store) (x : D.t) =
    wrap ?callback ?error @@ lazy (st##put (D.to_js x) (AOpt.aopt K.to_js key))

  let range ?olower ?oupper ?lower ?upper () =
    let iDBKeyRange : K.js iDBKeyRange t = Unsafe.variable "IDBKeyRange" in
    match lower, upper with
    | None, None -> assert false
    | Some lower, None ->
      KR (iDBKeyRange##lowerBound (K.to_js lower) (AOpt.aopt bool olower))
    | None, Some upper ->
      KR (iDBKeyRange##upperBound (K.to_js upper) (AOpt.aopt bool oupper))
    | Some lower, Some upper ->
      KR (iDBKeyRange##bound
            (K.to_js lower) (K.to_js upper)
            (AOpt.aopt bool olower) (AOpt.aopt bool oupper))

  let count ?error ?key (st : store) (callback : int -> unit) =
    match key with
    | None -> wrap ~callback ?error @@ lazy (st##count AOpt.undefined)
    | Some (K k) -> wrap ~callback ?error @@ lazy (st##count (AOpt.def (K.to_js k)))
    | Some (KR r) -> wrap ~callback ?error @@ lazy (st##count_range (AOpt.def r))

  let get ?error (st : (K.js, D.js) iDBObjectStore t) (callback : D.t -> unit) = function
    | K key -> wrapf ~callback ?error D.of_js @@ lazy (st##get (K.to_js key))
    | KR range -> wrapf ~callback ?error D.of_js @@ lazy (st##get_range range)

  let get_all ?error ?key ?count (st : (K.js, D.js) iDBObjectStore t) (callback : D.t list -> unit) =
    match key with
    | None -> wrapf ~callback ?error (to_listf D.of_js) @@ lazy (st##getAll AOpt.undefined (AOpt.option count))
    | Some (K k) -> wrapf ~callback ?error (to_listf D.of_js) @@ lazy (st##getAll (AOpt.def (K.to_js k)) (AOpt.option count))
    | Some (KR r) -> wrapf ~callback ?error (to_listf D.of_js) @@ lazy (st##getAll_range (AOpt.def r) (AOpt.option count))

  let get_key ?error (st : (K.js, _) iDBObjectStore t) (callback : K.t -> unit) = function
    | K key -> wrapf ~callback ?error K.of_js @@ lazy (st##getKey (K.to_js key))
    | KR range -> wrapf ~callback ?error K.of_js @@ lazy (st##getKey_range range)

  let get_all_keys ?error ?key ?count (st : (K.js, _) iDBObjectStore t) (callback : K.t list -> unit) =
    match key with
    | None -> wrapf ~callback ?error (to_listf K.of_js) @@ lazy (st##getAllKeys AOpt.undefined (AOpt.option count))
    | Some (K k) -> wrapf ~callback ?error (to_listf K.of_js) @@ lazy (st##getAllKeys (AOpt.def (K.to_js k)) (AOpt.option count))
    | Some (KR r) -> wrapf ~callback ?error (to_listf K.of_js) @@ lazy (st##getAllKeys_range (AOpt.def r) (AOpt.option count))

  let delete ?callback ?error (st : (K.js, _) iDBObjectStore t) = function
    | K key -> wrap ?error ?callback @@ lazy (st##delete (K.to_js key))
    | KR range -> wrap ?error ?callback @@ lazy (st##delete_range range)

  let iter ?error ?key ?direction (st : (K.js, D.js) iDBObjectStore t)
      (f : K.t -> D.t -> unit) =
    let callback c =
      match AOpt.to_option c with
      | None -> ()
      | Some c ->
        match AOpt.to_aopt K.of_js c##.key with
        | Some k ->
          f k (D.of_js c##.value);
          c##continue AOpt.undefined
        | None -> c##continue AOpt.undefined in
    match key with
    | None -> wrap ~callback ?error @@
      lazy (st##openCursor AOpt.undefined (AOpt.aopt str_of_direction direction))
    | Some (K k) -> wrap ~callback ?error @@
      lazy (st##openCursor (AOpt.def (K.to_js k)) (AOpt.aopt str_of_direction direction))
    | Some (KR r) -> wrap ~callback ?error @@
      lazy (st##openCursor_range (AOpt.def r) (AOpt.aopt str_of_direction direction))

  let fold ?error ?key ?direction (st : (K.js, D.js) iDBObjectStore t)
      (f : K.t -> D.t -> 'a -> 'a) (start : 'a) (callback : 'a -> unit) =
    let acc = ref start in
    let callback c =
      match AOpt.to_option c with
      | None -> callback !acc
      | Some c ->
        match AOpt.to_aopt K.of_js c##.key with
        | Some k ->
          acc := f k (D.of_js c##.value) !acc;
          c##continue AOpt.undefined
        | None -> c##continue AOpt.undefined in
    match key with
    | None -> wrap ~callback ?error @@
      lazy (st##openCursor AOpt.undefined (AOpt.aopt str_of_direction direction))
    | Some (K k) -> wrap ~callback ?error @@
      lazy (st##openCursor (AOpt.def (K.to_js k)) (AOpt.aopt str_of_direction direction))
    | Some (KR r) -> wrap ~callback ?error @@
      lazy (st##openCursor_range (AOpt.def r) (AOpt.aopt str_of_direction direction))

  let iter_keys ?error ?key ?direction (st : (K.js, D.js) iDBObjectStore t)
      (f : K.t -> unit) =
    let callback c =
      match AOpt.to_option c with
      | None -> ()
      | Some c ->
        match AOpt.to_aopt K.of_js c##.key with
        | Some k ->
          f k;
          c##continue AOpt.undefined
        | None -> c##continue AOpt.undefined in
    match key with
    | None -> wrap ~callback ?error @@
      lazy (st##openKeyCursor AOpt.undefined (AOpt.aopt str_of_direction direction))
    | Some (K k) -> wrap ~callback ?error @@
      lazy (st##openKeyCursor (AOpt.def (K.to_js k)) (AOpt.aopt str_of_direction direction))
    | Some (KR r) -> wrap ~callback ?error @@
      lazy (st##openKeyCursor_range (AOpt.def r) (AOpt.aopt str_of_direction direction))

  let fold_keys ?error ?key ?direction (st : (K.js, D.js) iDBObjectStore t)
      (f : K.t -> 'a -> 'a) (start : 'a) (callback : 'a -> unit) =
    let acc = ref start in
    let callback c =
      match AOpt.to_option c with
      | None -> callback !acc
      | Some c ->
        match AOpt.to_aopt K.of_js c##.key with
        | Some k ->
          acc := f k !acc;
          c##continue AOpt.undefined
        | None -> c##continue AOpt.undefined in
    match key with
    | None -> wrap ~callback ?error @@
      lazy (st##openKeyCursor AOpt.undefined (AOpt.aopt str_of_direction direction))
    | Some (K k) -> wrap ~callback ?error @@
      lazy (st##openKeyCursor (AOpt.def (K.to_js k)) (AOpt.aopt str_of_direction direction))
    | Some (KR r) -> wrap ~callback ?error @@
      lazy (st##openKeyCursor_range (AOpt.def r) (AOpt.aopt str_of_direction direction))

  let clear ?error ?callback (st : (K.js, D.js) iDBObjectStore t) =
    wrap ?callback ?error @@ lazy st##clear

  let create_index_options {unique; multi_entry; locale} : create_index_options t = object%js
    val unique = AOpt.aopt bool unique
    val multiEntry = AOpt.aopt bool multi_entry
    val locale = AOpt.aopt bool locale
  end

  let create_index ?options (st : (K.js, D.js) iDBObjectStore t) name key_path =
    let options = AOpt.aopt create_index_options options in
    st##createIndex (string name) (string key_path) options

  let delete_index (st : (K.js, D.js) iDBObjectStore t) name =
    ignore @@ st##deleteIndex (string name)

  let get_index (st : (K.js, D.js) iDBObjectStore t) name =
    st##index (string name)

end

module NoTr(S : sig type t end) : Tr_sig with type t = S.t and type js = S.t = struct
  type t = S.t
  type js = S.t
  let to_js x = x
  let of_js x = x
end

module IntTr : (Tr_sig with type js = int and type t = int) = NoTr(struct type t = int end)

module StringTr : Tr_sig with type js = js_string t and type t = string = struct
  type js = js_string t
  type t = string
  let to_js x = string x
  let of_js x = to_string x
end

module NumberTr : Tr_sig with type js = number t and type t = float = struct
  type js = number t
  type t = float
  let to_js x = number_of_float x
  let of_js x = float_of_number x
end

module DateTr : Tr_sig with type js = date t and type t = string = struct
  type js = date t
  type t = string
  let to_js x = new%js date_fromTimeValue (date##parse (string x))
  let of_js x = to_string x##toLocaleString
end
