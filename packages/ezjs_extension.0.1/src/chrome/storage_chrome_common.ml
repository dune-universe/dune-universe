open Ezjs_min

class type storageChange = object
  method oldValue : 'a prop
  method newValue : 'a prop
end

class type storageArea = object
  method get : js_string t opt -> ('a -> unit) callback -> unit meth
  method get_arr : js_string t js_array t opt -> ('a -> unit) callback -> unit meth
  method get_o : 'a opt -> ('a -> unit) callback -> unit meth
  method getBytesInUse : js_string t opt -> (int -> unit) callback -> unit meth
  method getBytesInUse_arr : js_string t js_array t opt -> (int -> unit) callback -> unit meth
  method set : 'a -> (unit -> unit) callback optdef -> unit meth
  method remove : js_string t -> (unit -> unit) callback optdef -> unit meth
  method clear : (unit -> unit) callback optdef -> unit meth
end

class type storage = object
  method sync : storageArea t prop
  method local : storageArea t prop
  method managed : storageArea t prop
end

let storage : storage t = Unsafe.variable "chrome.storage"
let local = storage##.local
let sync = storage##.sync
let managed = storage##.managed
