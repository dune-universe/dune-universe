open Ezjs_min
open Extension_utils
open Browser


class type window = object
  method id : int optdef prop
  method focused : bool t prop
  method top : int optdef prop
  method left : int optdef prop
  method width : int optdef prop
  method height : int optdef prop
  method tabs : Tabs.tab t js_array t optdef prop
  method incognito : bool t prop
  method _type : js_string t optdef prop
  method state : js_string t optdef prop
  method alwaysOnTop : bool t prop
  method sessionId : js_string t optdef prop
end

class type getInfo = object
  method populate : bool optdef prop
  method windowTypes : js_string t js_array t optdef prop
end

class type createData = object
  method url : js_string t optdef prop
  method url_arr : js_string t js_array t optdef prop
  method tabId : int optdef prop
  method left : int optdef prop
  method top : int optdef prop
  method width : int optdef prop
  method height : int optdef prop
  method focused : bool t optdef prop
  method incognito : bool t optdef prop
  method _type : js_string t optdef prop
  method state : js_string t optdef prop
  method setSelfAsOpener : bool t optdef prop
end

class type updateInfo = object
  method left : int optdef prop
  method top : int optdef prop
  method width : int optdef prop
  method height : int optdef prop
  method focused : bool t optdef prop
  method drawAttention : bool t optdef prop
  method state : js_string t optdef prop
end

class type windows = object
  method get : int -> getInfo t optdef -> (window t -> unit) callback -> unit meth
  method getCurrent : getInfo t optdef -> (window t -> unit) callback -> unit meth
  method getLastFocused : getInfo t optdef -> (window t -> unit) callback -> unit meth
  method getAll : getInfo t optdef -> (window t js_array t -> unit) callback -> unit meth
  method create : createData t optdef -> (window t -> unit) callback optdef -> unit meth
  method update : int -> updateInfo t -> (window t -> unit) callback optdef -> unit meth
  method remove : int -> (unit -> unit) callback optdef -> unit meth
  method onCreated : window t event t prop
  method onRemoved : int event t prop
  method onFocusChanged : int event t prop
end

let make_createData ?url ?url_l ?tabId ?left ?top ?width ?height ?focused ?typ
    ?state ?selfOpener () =
  let data : createData t = Unsafe.obj [||] in
  (match url, url_l with
   | Some _, None -> data##.url := optdef string url
   | None, Some _ ->   data##.url_arr := optdef (of_listf string) url_l
   | None, None -> ()
   | _ -> log_str "cannot define both url and url_l for window creation");
  data##.tabId := Optdef.option tabId;
  data##.left := Optdef.option left;
  data##.top := Optdef.option top;
  data##.width := Optdef.option width;
  data##.height := Optdef.option height;
  data##.focused := optdef bool focused;
  data##._type := optdef string typ;
  data##.state := optdef string state;
  data##.setSelfAsOpener := optdef bool selfOpener;
  data

let make_updateInfo ?left ?top ?width ?height ?focused ?drawAttention ?state () =
  let data : updateInfo t = Unsafe.obj [||] in
  data##.left := Optdef.option left;
  data##.top := Optdef.option top;
  data##.width := Optdef.option width;
  data##.height := Optdef.option height;
  data##.focused := optdef bool focused;
  data##.drawAttention := optdef bool drawAttention;
  data##.state := optdef string state;
  data

let windows : windows t = Unsafe.variable "chrome.windows"

let onCreated handler =
  addListener1 windows##.onCreated handler
let onRemoved handler =
  addListener1 windows##.onRemoved handler
let onFocusChanged handler =
  addListener1 windows##.onFocusChanged handler
