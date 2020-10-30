open Ezjs_min
open Extension_utils

type uint8Array = Typed_array.uint8Array

class type imageData = object
  method data : uint8Array t prop
  method height : int prop
  method width : int prop
end

class type tabDetails = object
  method tabId : int optdef prop
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
  method setTitle : titleDetails t -> (unit -> unit) callback optdef -> unit meth
  method getTitle : tabDetails t -> (js_string t -> unit) callback -> unit meth
  method setIcon : iconDetails t -> (unit -> unit) callback optdef -> unit meth
  method setPopup : popupDetails t -> (unit -> unit) callback optdef -> unit meth
  method getPopup : tabDetails t -> (js_string t -> unit) callback -> unit meth
  method setBadgeText : badgeDetails t -> (unit -> unit) callback optdef -> unit meth
  method getBadgeText : tabDetails t -> (js_string t -> unit) callback -> unit meth
  method setBadgeBackgroundColor : badgeColorDetails t -> (unit -> unit) callback optdef -> unit meth
  method getBadgeBackgroundColor : tabDetails t -> (uint8Array t -> unit) callback -> unit meth
  method enable : int optdef -> (unit -> unit) callback optdef -> unit meth
  method disable : int optdef -> (unit -> unit) callback optdef -> unit meth
  method onClicked : Tabs.tab Browser.event t prop
end

let browserAction : browserAction t = Unsafe.variable "chrome.browserAction"

let onClicked f =
  Browser.addListener1 browserAction##.onClicked f
