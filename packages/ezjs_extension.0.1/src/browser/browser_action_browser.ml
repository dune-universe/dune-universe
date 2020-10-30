open Ezjs_min
open Promise
include Browser_common.Browser_action

let get_title ?tabId ?windowId f =
  let details : tabDetails t = Unsafe.obj [||] in
  details##.tabId := Optdef.option tabId;
  details##.windowId := Optdef.option windowId;
  jthen (browserAction##getTitle details) (fun s -> f (to_string s))

let get_popup ?tabId ?windowId f =
  let details : tabDetails t = Unsafe.obj [||] in
  details##.tabId := Optdef.option tabId;
  details##.windowId := Optdef.option windowId;
  jthen (browserAction##getPopup details) (fun s -> f (to_string s))

let get_badge ?tabId ?windowId f =
  let details : tabDetails t = Unsafe.obj [||] in
  details##.tabId := Optdef.option tabId;
  details##.windowId := Optdef.option windowId;
  jthen (browserAction##getBadgeText details) (fun s -> f (to_string s))

let get_badge_bg ?tabId ?windowId f =
  let details : tabDetails t = Unsafe.obj [||] in
  details##.tabId := Optdef.option tabId;
  details##.windowId := Optdef.option windowId;
  jthen (browserAction##getBadgeBackgroundColor details) f

let get_badge_color ?tabId ?windowId f =
  let details : tabDetails t = Unsafe.obj [||] in
  details##.tabId := Optdef.option tabId;
  details##.windowId := Optdef.option windowId;
  jthen (browserAction##getBadgeTextColor details) f

let isEnabled ?tabId ?windowId f =
  let details : tabDetails t = Unsafe.obj [||] in
  details##.tabId := Optdef.option tabId;
  details##.windowId := Optdef.option windowId;
  jthen (browserAction##isEnabled details) (fun b -> f (to_bool b))
