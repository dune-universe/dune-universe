open Ezjs_min
open Promise
include Browser_common.Tabs

let captureTab ?id ?options f =
  jthen (tabs##captureTab (Optdef.option id) (Optdef.option options))
    (fun s -> f (to_string s))
let captureVisibleTab ?id ?options f =
  jthen (tabs##captureVisibleTab (Optdef.option id) (Optdef.option options))
    (fun s -> f (to_string s))
let create ?callback tab = jthen_opt (tabs##create tab) callback
let detectLanguage ?id f =
  jthen (tabs##detectLanguage (Optdef.option id) (wrap_callback (fun s -> f (to_string s))))
    (fun s -> f (to_string s))
let discard ?callback id = jthen_opt (tabs##discard id) callback
let discard_list ?callback ids =
  jthen_opt (tabs##discard_arr (of_list ids)) callback
let duplicate ?callback id = jthen_opt (tabs##duplicate id) callback
let executeScript ?id ?callback details =
  jthen_opt (tabs##executeScript (Optdef.option id) details) callback
let get id f = jthen (tabs##get id) f
let getCurrent f = jthen tabs##getCurrent f
let getZoom ?id f = jthen (tabs##getZoom (Optdef.option id)) (fun x -> f (float_of_number x))
let getZoomSettings ?id f = jthen (tabs##getZoomSettings (Optdef.option id)) f
let highlight ?callback info = jthen_opt (tabs##highlight info) callback
let move ?callback id props = jthen_opt (tabs##move id props) callback
let move_list ?callback ids props =
  jthen_opt (tabs##move_arr (of_list ids) props) callback
let query info f = jthen (tabs##query info) f
let reload ?id ?props ?callback () =
  jthen_opt (tabs##reload (Optdef.option id) (Optdef.option props)) callback
let remove ?callback id = jthen_opt (tabs##remove id) callback
let saveAsPDF settings f = jthen (tabs##saveAsPDF settings) (fun s -> f (to_string s))
let remove_list ?callback id = jthen_opt (tabs##remove_arr (of_list id)) callback
let sendMessage ?details ?callback id message =
  jthen_opt (tabs##sendMessage id message (Optdef.option details)) callback
let setZoom ?id ?callback factor =
  jthen_opt (tabs##setZoom (Optdef.option id) (number_of_float factor)) callback
let setZoomSettings ?id props f =
  jthen (tabs##setZoomSettings (Optdef.option id) props) f
let update ?id ?callback props =
  jthen_opt (tabs##update (Optdef.option id) props) callback
