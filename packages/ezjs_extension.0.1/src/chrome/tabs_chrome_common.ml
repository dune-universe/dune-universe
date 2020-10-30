open Ezjs_min
open Extension_utils
open Browser
open Runtime
open Tabs

let make_create ?active ?index ?openerTabId ?pinned ?selected ?url ?windowId () =
  let tab : createProperties t = Unsafe.obj [||] in
  tab##.active := optdef bool active;
  tab##.index := Optdef.option index;
  tab##.openerTabId := Optdef.option openerTabId;
  tab##.pinned := optdef bool pinned;
  tab##.selected := optdef bool selected;
  tab##.url := optdef string url;
  tab##.windowId := Optdef.option windowId;
  tab

class type tabs = object
  method _TAB_ID_NONE : js_string t prop
  method captureVisibleTab : int optdef -> imageDetails t optdef -> (js_string t -> unit) callback -> unit meth
  method connect : int -> 'a t optdef -> port meth
  method create : createProperties t -> (tab t -> unit) callback optdef -> unit meth
  method detectLanguage : int optdef -> (js_string t -> unit) callback -> unit meth
  method discard : int optdef -> (tab t -> unit) callback optdef -> unit meth
  method duplicate : int -> (tab t -> unit) callback optdef -> unit meth
  method executeScript : int optdef -> details t -> ('a t js_array t -> unit) callback optdef -> unit meth
  method get : int -> (tab t -> unit) callback -> unit meth
  method getCurrent : (tab t -> unit) callback -> unit meth
  method getZoom : int optdef -> (number t -> unit) callback -> unit meth
  method getZoomSettings : int optdef -> (zoomSettings t -> unit) callback -> unit meth
  method goBack : int optdef -> (unit -> unit) callback optdef -> unit meth
  method goForward : int optdef -> (unit -> unit) callback optdef -> unit meth
  method highlight : highlightInfo t -> (window t -> unit) callback optdef -> unit meth
  method insertCSS : int optdef -> details t meth
  method move : int -> moveProperties t -> (tab t -> unit) callback optdef -> unit meth
  method move_arr : int js_array t -> moveProperties -> (tab t js_array t -> unit) callback optdef -> unit meth
  method query : queryInfo t -> (tab t js_array t -> unit) callback -> unit meth
  method reload : int optdef -> reloadProperties t optdef -> (unit -> unit) callback optdef -> unit meth
  method remove : int -> (unit -> unit) callback optdef -> unit meth
  method remove_arr : int js_array t -> (unit -> unit) callback optdef -> unit meth
  method sendMessage : int -> 'a t -> details t optdef -> ('b t -> unit) callback optdef -> unit meth
  method setZoom : int optdef -> number t -> (unit -> unit) callback optdef -> unit meth
  method setZoomSettings : int optdef -> zoomSettings t -> (unit -> unit) callback -> unit meth
  method update : int optdef -> updateProperties t -> (tab t -> unit) callback optdef -> unit meth
  method onActivated : 'a t event t prop
  method onAttached : (int, 'a t) event2 t prop
  method onCreated : tab t event t prop
  method onDetached : (int, 'a t) event2 t prop
  method onHighlighted : 'a t event t prop
  method onMoved : (int, 'a t) event2 t prop
  method onRemoved : (int, 'a t) event2 t prop
  method onReplaced : (int, int) event2 t prop
  method onUpdated : (int, 'a t, tab t) event3 t prop
  method onZoomChanged : 'a t event t prop
end

let tabs : tabs t = Unsafe.variable "chrome.tabs"
let tab_id_none () = tabs##._TAB_ID_NONE

let connect ?info id = tabs##connect id (Optdef.option info)
let insertCSS ?id () = tabs##insertCSS (Optdef.option id)

let onActivated f = addListener1 tabs##.onActivated f
let onAttached f = addListener2 tabs##.onAttached f
let onCreated f = addListener1 tabs##.onCreated f
let onDetached f = addListener2 tabs##.onDetached f
let onHighLighted f = addListener1 tabs##.onHighlighted f
let onMoved f = addListener2 tabs##.onMoved f
let onRemoved f = addListener2 tabs##.onRemoved f
let onReplaced f= addListener2 tabs##.onReplaced f
let onUpdated f = addListener3 tabs##.onUpdated f
let onZoomChanged f = addListener1 tabs##.onZoomChanged f
