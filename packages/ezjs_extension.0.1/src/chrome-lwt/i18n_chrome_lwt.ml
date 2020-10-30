open Ezjs_min_lwt
open Promise
include Chrome_common.I18n

let getAcceptLanguages () =
  to_lwt_cb_tr (to_listf to_string) i18n##getAcceptLanguages
let detectLanguage text =
  to_lwt_cb (fun cb -> i18n##detectLanguage (string text) cb)
