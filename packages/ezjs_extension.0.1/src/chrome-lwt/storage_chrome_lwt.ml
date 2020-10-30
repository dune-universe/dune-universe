open Ezjs_min_lwt
open Promise
include Chrome_common.Storage

let get ?key (st:storageArea t) =
  to_lwt_cb (fun cb -> st##get (opt string key) cb)
let get_arr ?keys (st:storageArea t) =
  let keys = opt (of_listf string) keys in
  to_lwt_cb (fun cb -> st##get_arr keys cb)
let get_o ?obj (st:storageArea t) =
  to_lwt_cb (fun cb -> st##get (Opt.option obj) cb)
let getBytesInUse ?key (st:storageArea t) =
  to_lwt_cb (fun cb -> st##getBytesInUse (opt string key) cb)
let getBytesInUse_list ?keys (st:storageArea t) =
  let keys = opt (of_listf string) keys in
  to_lwt_cb (fun cb -> st##getBytesInUse_arr keys cb)
let set ?callback (st:storageArea t) o =
  to_lwt_cb_opt callback (fun cb -> st##set o cb)
let remove ?callback (st:storageArea t) s =
  to_lwt_cb_opt callback (fun cb -> st##remove (string s) cb)
let clear ?callback (st:storageArea t) =
  to_lwt_cb_opt callback (fun cb -> st##clear cb)
