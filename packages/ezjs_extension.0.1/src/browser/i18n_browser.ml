open Ezjs_min
open Promise
include Browser_common.I18n

let getAcceptLanguages f =
  jthen i18n##getAcceptLanguages (fun a -> f (of_listf string a))
let detectLanguage text = i18n##detectLanguage (string text)
