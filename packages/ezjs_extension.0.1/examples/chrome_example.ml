open Ezjs_min

let color = Ezjs_dom.strings_to_object [ ("color", Ezjs_dom.encapse "#3aa757") ]
let () =
  js_log color
