open Ezjs_min
include Chrome_common.Windows

let get ?info id f = windows##get id (Optdef.option info) (wrap_callback f)
let getCurrent ?info f = windows##getCurrent (Optdef.option info) (wrap_callback f)
let getLastFocused ?info f = windows##getLastFocused (Optdef.option info) (wrap_callback f)
let getAll ?info f =
  windows##getAll (Optdef.option info) (wrap_callback (fun a -> f (to_list a)))
let create ?info ?callback () =
  windows##create (Optdef.option info) (optdef wrap_callback callback)
let update ?callback id info =
  windows##update id info (optdef wrap_callback callback)
let remove ?callback id = windows##remove id (optdef wrap_callback callback)
