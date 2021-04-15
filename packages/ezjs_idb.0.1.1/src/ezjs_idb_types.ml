open Ezjs_min

type 'a listener = ('a -> unit) callback aopt

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

class type iDBTransaction = object
  inherit Dom_html.eventTarget
  method db : 'db readonly_prop
  method error : domException t aopt readonly_prop
  method mode : js_string t readonly_prop
  method objectStoreNames : domStringList t readonly_prop
  method abort : unit meth
  method objectStore : js_string t -> 'objectStore meth
  method commit : unit meth
  method onabort : Dom_html.event t listener prop
  method oncomplete : Dom_html.event t listener prop
  method onerror : Dom_html.event t listener prop
end

class type ['result] iDBRequest = object
  inherit Dom_html.eventTarget
  method error : domException t aopt readonly_prop
  method result : 'result readonly_prop
  method source : 'source readonly_prop
  method readyState : js_string t readonly_prop
  method transaction : iDBTransaction t aopt readonly_prop
  method onerror : Dom_html.event t listener prop
  method onsuccess : Dom_html.event t listener prop
end

class type ['key, 'result] iDBCursor = object
  method source : 'source readonly_prop
  method direction : js_string t readonly_prop
  method key : 'key aopt readonly_prop
  method primaryKey : 'key aopt readonly_prop
  method request : 'result iDBRequest t readonly_prop
  method advance : int -> unit meth
  method continue : 'key aopt -> unit meth
  method continuePrimaryKey : 'key -> 'key -> unit meth
  method delete : unit aopt iDBRequest meth
  method update : 'result -> 'result iDBRequest meth
end

class type ['key, 'result] iDBCursorWithValue = object
  inherit ['key, 'result] iDBCursor
  method value : 'result readonly_prop
end

class type ['key, 'data] iDBIndex = object
  inherit Dom_html.eventTarget
  method isAutoLocale : bool t readonly_prop
  method locale : js_string t readonly_prop
  method name : js_string t prop
  method objectStore : 'objectStore readonly_prop
  method keyPath : js_string t aopt readonly_prop
  method multiEntry : bool t readonly_prop
  method unique : bool t readonly_prop
  method count : js_string t aopt -> int iDBRequest t meth
  method get : 'key aopt -> 'data aopt iDBRequest t meth
  method get_range : 'key iDBKeyRange t aopt -> 'data aopt iDBRequest t meth
  method getKey : 'key aopt -> 'key aopt iDBRequest t meth
  method getKey_range : 'key iDBKeyRange t aopt -> 'key aopt iDBRequest t meth
  method getAll : 'key aopt -> int aopt -> 'data js_array t iDBRequest t meth
  method getAll_range : 'key iDBKeyRange t aopt -> int aopt -> 'data js_array t iDBRequest t meth
  method getAllKeys : 'key aopt -> int aopt -> 'key js_array t iDBRequest t meth
  method getAllKeys_range : 'key iDBKeyRange t aopt -> int aopt -> 'key js_array t iDBRequest t meth
  method openCursor : 'key aopt -> js_string t aopt -> ('key, 'data) iDBCursorWithValue t aopt iDBRequest t meth
  method openCursor_range : 'key iDBKeyRange t aopt -> js_string t aopt -> ('key, 'data) iDBCursorWithValue t aopt iDBRequest t meth
  method openKeyCursor : 'key aopt -> js_string t aopt -> ('key, 'key) iDBCursor t aopt iDBRequest t meth
  method openKeyCursor_range : 'key iDBKeyRange t aopt -> js_string t aopt -> ('key, 'key) iDBCursor t aopt iDBRequest t meth
end

class type ['key, 'data] iDBObjectStore = object
  method indexNames : domStringList t readonly_prop
  method keyPath : js_string t aopt readonly_prop
  method name : js_string t prop
  method transaction : iDBTransaction t readonly_prop
  method autoIncrement : bool t readonly_prop
  method add : 'data -> 'key aopt -> 'key iDBRequest t meth
  method clear : unit aopt iDBRequest t meth
  method count : 'key aopt -> int iDBRequest t meth
  method count_range : 'key iDBKeyRange t aopt -> int iDBRequest t meth
  method createIndex : js_string t -> js_string t -> create_index_options t aopt -> ('key, 'data) iDBIndex t meth
  method delete : 'key -> unit aopt iDBRequest t meth
  method delete_range : 'key iDBKeyRange t -> unit aopt iDBRequest t meth
  method deleteIndex : js_string t -> unit aopt meth
  method get : 'key -> 'data aopt iDBRequest t meth
  method get_range : 'key iDBKeyRange t -> 'data aopt iDBRequest t meth
  method getKey : 'key -> 'key aopt iDBRequest t meth
  method getKey_range : 'key iDBKeyRange t -> 'key aopt iDBRequest t meth
  method getAll : 'key aopt -> int aopt -> 'data js_array t iDBRequest t meth
  method getAll_range : 'key iDBKeyRange t aopt -> int aopt -> 'data js_array t iDBRequest t meth
  method getAllKeys : 'key aopt -> int aopt -> 'key js_array t iDBRequest t meth
  method getAllKeys_range : 'key iDBKeyRange t aopt -> int aopt -> 'key js_array t iDBRequest t meth
  method index : js_string t -> ('key, 'data) iDBIndex t meth
  method openCursor : 'key aopt -> js_string t aopt -> ('key, 'data) iDBCursorWithValue t aopt iDBRequest t meth
  method openCursor_range : 'key iDBKeyRange t aopt -> js_string t aopt -> ('key, 'data) iDBCursorWithValue t aopt iDBRequest t meth
  method openKeyCursor : 'key aopt -> js_string t aopt -> ('key, 'key) iDBCursor t aopt iDBRequest t meth
  method openKeyCursor_range : 'key iDBKeyRange t aopt -> js_string t aopt -> ('key, 'key) iDBCursor t aopt iDBRequest t meth
  method put : 'data -> 'key aopt -> 'data iDBRequest t meth
end

class type iDBDatabase = object
  inherit Dom_html.eventTarget
  method name : js_string t readonly_prop
  method version : int readonly_prop
  method objectStoreNames : domStringList t readonly_prop
  method close : unit meth
  method createMutableFile : js_string t -> File.file t meth
  method createObjectStore : js_string t -> create_db_options t aopt -> (Unsafe.any, Unsafe.any) iDBObjectStore t meth
  method deleteObjectStore : js_string t -> unit meth
  method transaction : js_string t js_array t -> js_string t aopt -> iDBTransaction t meth
  method onabort : Dom_html.event t listener prop
  method onclose : Dom_html.event t listener prop
  method onerror : Dom_html.event t listener prop
  method onversionchange : iDBVersionChangeEvent t listener prop
end

class type ['result] iDBOpenDBRequest = object
  inherit ['result] iDBRequest
  method onblocked : Dom_html.event t listener prop
  method onupgradeneeded : iDBVersionChangeEvent t listener prop
end

class type ['key] iDBFactory = object
  method _open : js_string t -> int aopt -> iDBDatabase t iDBOpenDBRequest t meth
  method deleteDatabase : js_string t -> int aopt -> unit aopt iDBOpenDBRequest t meth
  method cmp : 'key -> 'key -> int meth
  method databases : database_dict t meth
end

class type ['key] iDBEnvironment = object
  method indexedDB : 'key iDBFactory t readonly_prop
end

class type ['key] iDBLocaleAwareKeyRange = object (* firefox only *)
  inherit ['key] iDBKeyRange
end
