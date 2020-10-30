open Ezjs_min
open Ezjs_dom
open Chrome

let changeColor = by_id "changeColor"

let () =
  Storage.get ~key:"color" sync (fun data ->
      changeColor##.style##.backgroundColor := data##.color;
      changeColor##setAttribute (string "value") data##.color
    );

  addListener changeColor "click" (fun ev ->
      match Opt.to_option ev##.target with
      | None -> true
      | Some target ->
        let color = (Unsafe.coerce target)##.value in
        let color = to_string color in
        let query  = Utils.Tabs.make_query ~active:true ~currentWindow:true () in
        Tabs.query query (fun tbs ->
            match Optdef.to_option (array_get tbs 0) with
            | None -> ()
            | Some tab ->
              let details = Utils.Tabs.make_script_details
                  ~code:("document.body.style.backgroundColor = \"" ^ color ^ "\";") () in
              Tabs.executeScript ?id:(Optdef.to_option tab##.id) details
          );
        true
    )
