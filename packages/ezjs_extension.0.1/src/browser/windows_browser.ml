open Ezjs_min
open Promise
include Browser_common.Windows

let get ?info id f = jthen (windows##get id (Optdef.option info)) f
let getCurrent ?info f = jthen (windows##getCurrent (Optdef.option info)) f
let getLastFocused ?info f = jthen (windows##getLastFocused (Optdef.option info)) f
let getAll ?info f =
  jthen (windows##getAll (Optdef.option info)) (fun a -> f (to_list a))
let create ?info ?callback () =
  jthen_opt (windows##create (Optdef.option info)) callback
let update ?callback id info =
  jthen_opt (windows##update id info) callback
let remove ?callback id = jthen_opt (windows##remove id) callback
