open Ezjs_min_lwt
open Promise
include Browser_common.I18n

let getAcceptLanguages () =
  to_lwt i18n##getAcceptLanguages >>= function
  | Error e -> return (Error e)
  | Ok a -> return (Ok (to_listf to_string a))
let detectLanguage text = to_lwt (i18n##detectLanguage (string text))
