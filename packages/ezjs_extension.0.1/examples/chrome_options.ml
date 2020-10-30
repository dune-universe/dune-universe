open Ezjs_min
open Ezjs_dom

let page = by_id "buttonDiv"

let kButtonColors = [ "#3aa757"; "#e8453c"; "#f9bb2d"; "#4688f1" ]

class type color = object
  method color : js_string t prop
end

let () =
  List.iter (fun color ->
      let color_obj = strings_to_object [ ("color", encapse color) ] in
      let callback _ = log_str ("color is " ^ color) in
      let onclick _ = Chrome.Storage.set ~callback Chrome.sync color_obj; true in
      let b = El.button ~styles:["background-color", color] ~listen:["click", onclick] [] in
      appendChild page b
    ) kButtonColors
