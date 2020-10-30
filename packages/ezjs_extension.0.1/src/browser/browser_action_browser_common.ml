open Ezjs_min
open Promise
open Extension_utils

type uint8Array = Typed_array.uint8Array

class type imageData = object
  method data : uint8Array t prop
  method height : int prop
  method width : int prop
end

class type tabDetails = object
  method tabId : int t optdef prop
  method windowId : int t optdef prop
end

class type titleDetails = object
  inherit tabDetails
  method title : js_string t opt prop
end

class type iconDetails = object
  inherit tabDetails
  method imageData : imageData t optdef prop
  method path : js_string t optdef prop
end

class type popupDetails = object
  inherit tabDetails
  method popup : js_string t opt prop
end

class type badgeDetails = object
  inherit tabDetails
  method text : js_string t opt prop
end

class type badgeColorDetails = object
  inherit tabDetails
  method color : js_string t opt prop
end


class type browserAction = object
  method setTitle : titleDetails t -> unit t meth
  method getTitle : tabDetails t -> js_string t promise t meth
  method setIcon : iconDetails t -> unit promise t meth
  method setPopup : popupDetails t -> unit meth
  method getPopup : tabDetails t -> js_string t promise t meth
  method setBadgeText : badgeDetails t -> unit meth
  method getBadgeText : tabDetails t -> js_string t promise t meth
  method setBadgeBackgroundColor : badgeColorDetails t -> unit meth
  method getBadgeBackgroundColor : tabDetails t -> uint8Array t promise t meth
  method setBadgeTextColor : badgeColorDetails t -> unit meth
  method getBadgeTextColor : tabDetails t -> uint8Array t promise t meth
  method enable : tabDetails t -> unit meth
  method disable : tabDetails t -> unit meth
  method isEnabled : tabDetails t -> bool t promise t meth
  method onClicked : Tabs.tab Browser.event t prop
end

let browserAction : browserAction t = Unsafe.variable "chrome.browserAction"

let set_title ?tabId ?windowId ?title () =
  let details : titleDetails t = Unsafe.obj [||] in
  details##.title := opt string title;
  details##.tabId := Optdef.option tabId;
  details##.windowId := Optdef.option windowId;
  browserAction##setTitle details

let set_icon ?data ?path ?tabId ?windowId () =
  let details : iconDetails t = Unsafe.obj [||] in
  (match path, data with
   | Some s, _ -> details##.path := def (string s)
   | _, Some d -> details##.imageData := def d
   | _ -> ());
  details##.tabId := Optdef.option tabId;
  details##.windowId := Optdef.option windowId;
  browserAction##setIcon details

let set_popup ?tabId ?windowId ?popup () =
  let details : popupDetails t = Unsafe.obj [||] in
  details##.popup := opt string popup;
  details##.tabId := Optdef.option tabId;
  details##.windowId := Optdef.option windowId;
  browserAction##setPopup details

let set_badge ?tabId ?windowId ?text () =
  let details : badgeDetails t = Unsafe.obj [||] in
  details##.text := opt string text;
  details##.tabId := Optdef.option tabId;
  details##.windowId := Optdef.option windowId;
  browserAction##setBadgeText details

let set_badge_bg ?tabId ?windowId ?color () =
  let details : badgeColorDetails t = Unsafe.obj [||] in
  details##.color := opt string color;
  details##.tabId := Optdef.option tabId;
  details##.windowId := Optdef.option windowId;
  browserAction##setBadgeBackgroundColor details

let set_badge_color ?tabId ?windowId ?color () =
  let details : badgeColorDetails t = Unsafe.obj [||] in
  details##.color := opt string color;
  details##.tabId := Optdef.option tabId;
  details##.windowId := Optdef.option windowId;
  browserAction##setBadgeTextColor details

let enable ?tabId () =
  let details : tabDetails t = Unsafe.obj [||] in
  details##.tabId := Optdef.option tabId;
  browserAction##enable details

let disable ?tabId () =
  let details : tabDetails t = Unsafe.obj [||] in
  details##.tabId := Optdef.option tabId;
  browserAction##disable details

let onClicked f =
  Browser.addListener1 browserAction##.onClicked f
