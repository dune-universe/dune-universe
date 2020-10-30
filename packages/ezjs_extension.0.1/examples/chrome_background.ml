open Ezjs_min
open Ezjs_dom
open Chrome
module Content = Declarative_content

let () =
  Runtime.onInstalled (fun _details ->
      let color = strings_to_object [ ("color", encapse "#3aa757") ] in
      Storage.set ~callback:(fun () -> log_str "color is green") sync color ;
      Content.removeRules (fun _ ->
          let condition = Content.make_condition ~hostEquals:"localhost" () in
          let action = Content.showPageAction () in
          let rule = Content.make_rule [condition] [action] in
          Content.addRules [rule] (fun _ -> ())
        )
    )
