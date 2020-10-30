open Ezjs_min
include Chrome_common.Tabs

let captureVisibleTab ?id ?options f =
  tabs##captureVisibleTab (Optdef.option id) (Optdef.option options)
    (wrap_callback (fun s -> f (to_string s)))
let create ?callback tab = tabs##create tab (optdef wrap_callback callback)
let detectLanguage ?id f =
  tabs##detectLanguage (Optdef.option id) (wrap_callback (fun s -> f (to_string s)))
let discard ?id ?callback () = tabs##discard (Optdef.option id) (optdef wrap_callback callback)
let duplicate ?callback id = tabs##duplicate id (optdef wrap_callback callback)
let executeScript ?id ?callback details =
  tabs##executeScript (Optdef.option id) details (optdef wrap_callback callback)
let get id f = tabs##get id (wrap_callback f)
let getCurrent f = tabs##getCurrent (wrap_callback f)
let getZoom ?id f = tabs##getZoom (Optdef.option id) (wrap_callback (fun x -> f (float_of_number x)))
let getZoomSettings ?id f = tabs##getZoomSettings (Optdef.option id) (wrap_callback f)
let goBack ?id ?callback () = tabs##goBack (Optdef.option id) (optdef wrap_callback callback)
let goForward ?id ?callback () = tabs##goForward (Optdef.option id) (optdef wrap_callback callback)
let highlight ?callback info = tabs##highlight info (optdef wrap_callback callback)
let move ?callback id props = tabs##move id props (optdef wrap_callback callback)
let move_list ?callback ids props = tabs##move_arr (of_list ids) props (optdef wrap_callback callback)
let query info f = tabs##query info (wrap_callback f)
let reload ?id ?props ?callback () =
  tabs##reload (Optdef.option id) (Optdef.option props) (optdef wrap_callback callback)
let remove ?callback id = tabs##remove id (optdef wrap_callback callback)
let remove_list ?callback id = tabs##remove_arr (of_list id) (optdef wrap_callback callback)
let sendMessage ?details ?callback id message =
  tabs##sendMessage id message (Optdef.option details) (optdef wrap_callback callback)
let setZoom ?id ?callback factor =
  tabs##setZoom (Optdef.option id) (number_of_float factor) (optdef wrap_callback callback)
let setZoomSettings ?id props f =
  tabs##setZoomSettings (Optdef.option id) props (wrap_callback f)
let update ?id ?callback props =
  tabs##update (Optdef.option id) props (optdef wrap_callback callback)
