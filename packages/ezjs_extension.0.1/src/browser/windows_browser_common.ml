open Ezjs_min
open Promise
open Extension_utils
open Browser

class type window = object
  method alwaysOnTop : bool t prop
  method focused : bool t prop
  method height : int optdef prop
  method id : int optdef prop
  method incognito : bool t prop
  method left : int optdef prop
  method sessionId : js_string t optdef prop
  method state : js_string t optdef prop
  method tabs : Tabs.tab t js_array t optdef prop
  method title : js_string t optdef prop
  method top : int optdef prop
  method _type : js_string t optdef prop
  method width : int optdef prop
end

class type getInfo = object
  method populate : bool optdef prop
  method windowTypes : js_string t js_array t optdef prop
end

class type createData = object
  method allowScriptsToClose : bool t optdef prop
  method cookieStoreId : int optdef prop
  method focused : bool t optdef prop
  method height : int optdef prop
  method incognito : bool t optdef prop
  method left : int optdef prop
  method state : js_string t optdef prop
  method tabId : int optdef prop
  method titlePreface : js_string t optdef prop
  method top : int optdef prop
  method _type : js_string t optdef prop
  method url : js_string t optdef prop
  method url_arr : js_string t js_array t optdef prop
  method width : int optdef prop
end

class type updateInfo = object
  method drawAttention : bool t optdef prop
  method focused : bool t optdef prop
  method height : int optdef prop
  method left : int optdef prop
  method state : js_string t optdef prop
  method titlePreface : js_string t optdef prop
  method top : int optdef prop
  method width : int optdef prop
end

class type windows = object
  method _WINDOW_ID_NONE : int prop
  method _WINDOW_ID_CURRENT : int prop
  method get : int -> getInfo t optdef -> window t promise t meth
  method getCurrent : getInfo t optdef -> window t promise t meth
  method getLastFocused : getInfo t optdef -> window t promise t meth
  method getAll : getInfo t optdef -> window t js_array t promise t meth
  method create : createData t optdef -> window t promise t meth
  method update : int -> updateInfo t -> window t promise t meth
  method remove : int -> unit promise t meth
  method onCreated : window t event t prop
  method onRemoved : int event t prop
  method onFocusChanged : int event t prop
end

let make_createData ?url ?url_l ?tabId ?left ?top ?width ?height ?focused ?typ
    ?state ?allowScriptsToClose ?cookieStoreId ?titlePreface () =
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
  begin match focused with
    | None -> ()
    | Some _ -> data##.focused := optdef bool focused
  end;
  data##._type := optdef string typ;
  data##.state := optdef string state;
  data##.allowScriptsToClose := optdef bool allowScriptsToClose;
  data##.cookieStoreId := Optdef.option cookieStoreId;
  data##.titlePreface := optdef string titlePreface;
  data

let make_updateInfo ?left ?top ?width ?height ?focused ?drawAttention ?state
    ?titlePreface () =
  let data : updateInfo t = Unsafe.obj [||] in
  data##.left := Optdef.option left;
  data##.top := Optdef.option top;
  data##.width := Optdef.option width;
  data##.height := Optdef.option height;
  data##.focused := optdef bool focused;
  data##.drawAttention := optdef bool drawAttention;
  data##.state := optdef string state;
  data##.titlePreface := optdef string titlePreface;
  data

let windows : windows t = Unsafe.variable "browser.windows"

let onCreated handler =
  addListener1 windows##.onCreated handler
let onRemoved handler =
  addListener1 windows##.onRemoved handler
let onFocusChanged handler =
  addListener1 windows##.onFocusChanged handler
