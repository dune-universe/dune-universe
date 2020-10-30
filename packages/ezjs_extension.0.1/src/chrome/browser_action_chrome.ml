open Ezjs_min
include Chrome_common.Browser_action

let set_title ?id ?title ?callback () =
  let details : titleDetails t = Unsafe.obj [||] in
  details##.title := opt string title;
  details##.tabId := Optdef.option id;
  browserAction##setTitle details (optdef wrap_callback callback)

let get_title ?id f =
  let details : tabDetails t = Unsafe.obj [||] in
  details##.tabId := Optdef.option id;
  browserAction##getTitle details (wrap_callback (fun s -> f (to_string s)))

let set_icon ?data ?path ?id ?callback () =
  let details : iconDetails t = Unsafe.obj [||] in
  (match path, data with
   | Some s, _ -> details##.path := def (string s)
   | _, Some d -> details##.imageData := def d
   | _ -> ());
  details##.tabId := Optdef.option id;
  browserAction##setIcon details (optdef wrap_callback callback)

let set_popup ?id ?popup ?callback () =
  let details : popupDetails t = Unsafe.obj [||] in
  details##.popup := opt string popup;
  details##.tabId := Optdef.option id;
  browserAction##setPopup details (optdef wrap_callback callback)

let get_popup ?id f =
  let details : tabDetails t = Unsafe.obj [||] in
  details##.tabId := Optdef.option id;
  browserAction##getPopup details (wrap_callback (fun s -> f (to_string s)))

let set_badge ?id ?text ?callback () =
  let details : badgeDetails t = Unsafe.obj [||] in
  details##.text := opt string text;
  details##.tabId := Optdef.option id;
  browserAction##setBadgeText details (optdef wrap_callback callback)

let get_badge ?id f =
  let details : tabDetails t = Unsafe.obj [||] in
  details##.tabId := Optdef.option id;
  browserAction##getBadgeText details (wrap_callback (fun s -> f (to_string s)))

let set_badge_bg ?id ?color ?callback () =
  let details : badgeColorDetails t = Unsafe.obj [||] in
  details##.color := opt string color;
  details##.tabId := Optdef.option id;
  browserAction##setBadgeBackgroundColor details (optdef wrap_callback callback)

let get_badge_bg ?id f =
  let details : tabDetails t = Unsafe.obj [||] in
  details##.tabId := Optdef.option id;
  browserAction##getBadgeBackgroundColor details (wrap_callback f)

let enable ?id ?callback () =
  browserAction##enable (Optdef.option id) (optdef wrap_callback callback)

let disable ?id ?callback () =
  browserAction##disable (Optdef.option id) (optdef wrap_callback callback)
