open Ezjs_min

type ('a, 'b) listener = ('a, 'b -> unit) meth_callback aopt

class type domException = object
  method code : int aopt readonly_prop
  method name : js_string t readonly_prop
  method message : js_string t readonly_prop
end

class type domStringList = object
  method length : int readonly_prop
  method item : int -> js_string t meth
  method contains : js_string t -> bool t meth
end

class type iDBVersionChangeEvent = object
  inherit Dom_html.event
  method oldVersion : int readonly_prop
  method newVersion : int readonly_prop
end

class type ['key] iDBKeyRange = object
  method lower : 'key readonly_prop
  method upper : 'key readonly_prop
  method lowerOpen : bool t readonly_prop
  method upperOpen : bool t readonly_prop
  method bound : 'key -> 'key -> bool t aopt -> bool t aopt -> 'key iDBKeyRange t meth
  method only : 'key -> 'key iDBKeyRange t meth
  method lowerBound : 'key -> bool t aopt -> 'key iDBKeyRange t meth
  method upperBound : 'key -> bool t aopt -> 'key iDBKeyRange t meth
  method includes : 'key -> bool t meth
end

class type create_index_options = object
  method unique : bool t aopt readonly_prop
  method multiEntry : bool t aopt readonly_prop
  method locale : bool t aopt readonly_prop
end

class type database_dict = object
  method name : js_string t readonly_prop
  method version : int readonly_prop
end

class type create_db_options = object
  method keyPath : js_string t aopt readonly_prop
  method autoIncrement : bool t aopt readonly_prop
end

class type ['objectStore] iDBTransaction = object
  inherit Dom_html.eventTarget
  method db : 'db readonly_prop
  method error : domException t aopt readonly_prop
  method mode : js_string t readonly_prop
  method objectStoreNames : domStringList t readonly_prop
  method abort : unit meth
  method objectStore : js_string t -> 'objectStore meth
  method commit : unit meth
  method onabort : ('objectStore iDBTransaction, Dom_html.event t) listener prop
  method oncomplete : ('objectStore iDBTransaction, Dom_html.event t) listener prop
  method onerror : ('objectStore iDBTransaction, Dom_html.event t) listener prop
end

class type ['source, 'result] iDBRequest = object
  inherit Dom_html.eventTarget
  method error : domException t aopt readonly_prop
  method result : 'result readonly_prop
  method source : 'source readonly_prop
  method readyState : js_string t readonly_prop
  method transaction : 'result iDBTransaction t aopt readonly_prop
  method onerror : (('source, 'result) iDBRequest t, Dom_html.event t) listener prop
  method onsuccess : (('source, 'result) iDBRequest t, Dom_html.event t) listener prop
end

class type ['key, 'source, 'result] iDBCursor = object
  method source : 'source readonly_prop
  method direction : js_string t readonly_prop
  method key : 'key aopt readonly_prop
  method primaryKey : 'key aopt readonly_prop
  method request : ('source, 'result) iDBRequest t readonly_prop
  method advance : int -> unit meth
  method continue : 'key aopt -> unit meth
  method continuePrimaryKey : 'key -> 'key -> unit meth
  method delete : ('source, unit aopt) iDBRequest meth
  method update : 'result -> ('source, 'result) iDBRequest meth
end

class type ['key, 'source, 'result] iDBCursorWithValue = object
  inherit ['key, 'source, 'result] iDBCursor
  method value : 'result readonly_prop
end

class type ['key, 'data] iDBIndex = object
  inherit Dom_html.eventTarget
  method isAutoLocale : bool t readonly_prop
  method locale : js_string t readonly_prop
  method name : js_string t prop
  method objectStore : ('key, 'data) iDBObjectStore t readonly_prop
  method keyPath : js_string t aopt readonly_prop
  method multiEntry : bool t readonly_prop
  method unique : bool t readonly_prop
  method count : js_string t aopt -> (('key, 'data) iDBIndex t, int) iDBRequest t meth
  method get : 'key aopt -> (('key, 'data) iDBIndex t, 'data) iDBRequest t meth
  method get_range : 'key iDBKeyRange t aopt -> (('key, 'data) iDBIndex t, 'data) iDBRequest t meth
  method getKey : 'key aopt -> (('key, 'data) iDBIndex t, 'key) iDBRequest t meth
  method getKey_range : 'key iDBKeyRange t aopt -> (('key, 'data) iDBIndex t, 'key) iDBRequest t meth
  method getAll : 'key aopt -> int aopt -> (('key, 'data) iDBIndex t, 'data js_array t) iDBRequest t meth
  method getAll_range : 'key iDBKeyRange t aopt -> int aopt -> (('key, 'data) iDBIndex t, 'data js_array t) iDBRequest t meth
  method getAllKeys : 'key aopt -> int aopt -> (('key, 'data) iDBIndex t, 'key js_array t) iDBRequest t meth
  method getAllKeys_range : 'key iDBKeyRange t aopt -> int aopt -> (('key, 'data) iDBIndex t, 'key js_array t) iDBRequest t meth
  method openCursor : 'key aopt -> js_string t aopt -> (('key, 'data) iDBIndex t, ('key, ('key, 'data) iDBIndex, 'data) iDBCursorWithValue t aopt) iDBRequest t meth
  method openCursor_range : 'key iDBKeyRange t aopt -> js_string t aopt -> (('key, 'data) iDBIndex t, ('key, ('key, 'data) iDBIndex, 'data) iDBCursorWithValue t aopt) iDBRequest t meth
  method openKeyCursor : 'key aopt -> js_string t aopt -> (('key, 'data) iDBIndex t, ('key, ('key, 'data) iDBIndex, 'key) iDBCursor t aopt) iDBRequest t meth
  method openKeyCursor_range : 'key iDBKeyRange t aopt -> js_string t aopt -> (('key, 'data) iDBIndex t, ('key, ('key, 'data) iDBIndex, 'key) iDBCursor t aopt) iDBRequest t meth
end

and ['key, 'data] iDBObjectStore = object
  method indexNames : domStringList t readonly_prop
  method keyPath : js_string t aopt readonly_prop
  method name : js_string t prop
  method transaction : ('key, 'data) iDBObjectStore t iDBTransaction t readonly_prop
  method autoIncrement : bool t readonly_prop
  method add : 'data -> 'key aopt -> (('key, 'data) iDBObjectStore t, 'key) iDBRequest t meth
  method clear : (('key, 'data) iDBObjectStore t, unit aopt) iDBRequest t meth
  method count : 'key aopt -> (('key, 'data) iDBObjectStore t, int) iDBRequest t meth
  method count_range : 'key iDBKeyRange t aopt -> (('key, 'data) iDBObjectStore t, int) iDBRequest t meth
  method createIndex : js_string t -> js_string t -> create_index_options t aopt -> ('key, 'data) iDBIndex t meth
  method delete : 'key -> (('key, 'data) iDBObjectStore t, unit aopt) iDBRequest t meth
  method delete_range : 'key iDBKeyRange t -> (('key, 'data) iDBObjectStore t, unit aopt) iDBRequest t meth
  method deleteIndex : js_string t -> unit aopt meth
  method get : 'key -> (('key, 'data) iDBObjectStore t, 'data) iDBRequest t meth
  method get_range : 'key iDBKeyRange t -> (('key, 'data) iDBObjectStore t, 'data) iDBRequest t meth
  method getKey : 'key -> (('key, 'data) iDBObjectStore t, 'key) iDBRequest t meth
  method getKey_range : 'key iDBKeyRange t -> (('key, 'data) iDBObjectStore t, 'key) iDBRequest t meth
  method getAll : 'key aopt -> int aopt -> (('key, 'data) iDBObjectStore t, 'data js_array t) iDBRequest t meth
  method getAll_range : 'key iDBKeyRange t aopt -> int aopt -> (('key, 'data) iDBObjectStore t, 'data js_array t) iDBRequest t meth
  method getAllKeys : 'key aopt -> int aopt -> (('key, 'data) iDBObjectStore t, 'key js_array t) iDBRequest t meth
  method getAllKeys_range : 'key iDBKeyRange t aopt -> int aopt -> (('key, 'data) iDBObjectStore t, 'key js_array t) iDBRequest t meth
  method index : js_string t -> ('key, 'data) iDBIndex t meth
  method openCursor : 'key aopt -> js_string t aopt -> (('key, 'data) iDBObjectStore t, ('key, ('key, 'data) iDBObjectStore, 'data) iDBCursorWithValue t aopt) iDBRequest t meth
  method openCursor_range : 'key iDBKeyRange t aopt -> js_string t aopt -> (('key, 'data) iDBObjectStore t, ('key, ('key, 'data) iDBObjectStore, 'data) iDBCursorWithValue t aopt) iDBRequest t meth
  method openKeyCursor : 'key aopt -> js_string t aopt -> (('key, 'data) iDBObjectStore t, ('key, ('key, 'data) iDBObjectStore, 'key) iDBCursor t aopt) iDBRequest t meth
  method openKeyCursor_range : 'key iDBKeyRange t aopt -> js_string t aopt -> (('key, 'data) iDBObjectStore t,  ('key, ('key, 'data) iDBObjectStore, 'key) iDBCursor t aopt) iDBRequest t meth
  method put : 'data -> 'key aopt -> (('key, 'data) iDBObjectStore t, 'data) iDBRequest t meth
end

class type iDBDatabase = object
  inherit Dom_html.eventTarget
  method name : js_string t readonly_prop
  method version : int readonly_prop
  method objectStoreNames : domStringList t readonly_prop
  method close : unit meth
  method createMutableFile : js_string t -> File.file t meth
  method createObjectStore : js_string t -> create_db_options t aopt -> (Unsafe.any, _) iDBObjectStore t meth
  method deleteObjectStore : js_string t -> unit meth
  method transaction : js_string t js_array t -> js_string t aopt -> (Unsafe.any, _) iDBObjectStore t iDBTransaction t meth
  method onabort : (iDBDatabase t, Dom_html.event t) listener prop
  method onclose : (iDBDatabase t, Dom_html.event t) listener prop
  method onerror : (iDBDatabase t, Dom_html.event t) listener prop
  method onversionchange : (iDBDatabase t, iDBVersionChangeEvent t) listener prop
end

class type ['source, 'result] iDBOpenDBRequest = object
  inherit ['source, 'result] iDBRequest
  method onblocked : (('source, 'result) iDBOpenDBRequest t, Dom_html.event t) listener prop
  method onupgradeneeded : (('source, 'result) iDBOpenDBRequest t, iDBVersionChangeEvent t) listener prop
end

class type ['key] iDBFactory = object
  method _open : js_string t -> int aopt -> (unit, iDBDatabase t) iDBOpenDBRequest t meth
  method deleteDatabase : js_string t -> int aopt -> (unit, unit aopt) iDBOpenDBRequest t meth
  method cmp : 'key -> 'key -> int meth
  method databases : database_dict t meth
end

class type ['key] iDBEnvironment = object
  method indexedDB : 'key iDBFactory t readonly_prop
end

class type ['key] iDBLocaleAwareKeyRange = object (* firefox only *)
  inherit ['key] iDBKeyRange
end
