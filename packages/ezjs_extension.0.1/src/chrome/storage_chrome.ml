open Ezjs_min
include Chrome_common.Storage

let get ?key (st:storageArea t) f = st##get (opt string key) (wrap_callback f)
let get_arr ?keys (st:storageArea t) f =
  let keys = opt (of_listf string) keys in
  st##get_arr keys (wrap_callback f)
let get_o ?obj (st:storageArea t) f = st##get (Opt.option obj) (wrap_callback f)
let getBytesInUse ?key (st:storageArea t) f =
  st##getBytesInUse (opt string key) (wrap_callback f)
let getBytesInUse_list ?keys (st:storageArea t) f =
  let keys = opt (of_listf string) keys in
  st##getBytesInUse_arr keys (wrap_callback f)
let set ?callback (st:storageArea t) o = st##set o (optdef wrap_callback callback)
let remove ?callback (st:storageArea t) s = st##remove (string s) (optdef wrap_callback callback)
let clear ?callback (st:storageArea t) = st##clear (optdef wrap_callback callback)
