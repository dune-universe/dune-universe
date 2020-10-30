open Ezjs_min
open Extension_utils.Browser
open Extension_utils.Runtime

class type onInstalledEvent = object
  method id : js_string t optdef prop
  method previousVersion : js_string t optdef prop
  method reason : js_string t prop
end

class type runtime = object
  method lastError : error t prop
  method id : js_string t prop
  method getBackgroundPage : (window t -> unit) callback -> unit meth
  method openOptionsPage : (unit -> unit) callback optdef -> unit meth
  method getManifest : 'a t meth
  method getURL : js_string t -> js_string t meth
  method setUninstallURL : js_string t -> (unit -> unit) callback optdef -> unit meth
  method reload : unit meth
  method requestUpdateCheck : (requestCheckResponse t -> unit) callback -> unit meth
  method restart : unit meth
  method restartAfterDelay : int -> (unit -> unit) callback optdef -> unit meth
  method connect : js_string t optdef -> connectInfo t optdef -> port t meth
  method connectNative : js_string t -> port t meth
  method sendMessage : js_string t optdef -> 'a t -> connectInfo t optdef -> ('b t -> unit) optdef -> unit meth
  method sendNativeMessage : js_string t -> 'a t -> ('b t -> unit) optdef -> unit meth
  method getPlatformInfo : (platformInfo t -> unit) callback -> unit meth
  method getPackageDirectoryEntry : ('a t -> unit) callback -> unit meth
  method onStartup : unit event t prop
  method onInstalled : onInstalledEvent t event t prop
  method onSuspend : unit event t prop
  method onSuspendCanceled : unit event t prop
  method onUpdateAvailable : 'a t event t prop
  method onConnect : port t event t prop
  method onConnectExternal : port t event t prop
  method onConnectNative : port t event t prop
  method onMessage : ('a t,  messageSender t, ('b t -> bool t)) event3 t prop
  method onMessageExternal : ('a t,  messageSender t, ('a t -> bool t)) event3 t prop
  method onRestartRequired : js_string t event t prop
end

let runtime : runtime t = Unsafe.variable "chrome.runtime"

let last_error () = runtime##.lastError
let id () = runtime##.id

let getManifest () = runtime##getManifest
let getURL s = to_string (runtime##getURL (string s))
let reload () = runtime##reload
let restart () = runtime##restart
let connect ?id ?info () = runtime##connect (Optdef.option id) (Optdef.option info)
let connectNative application = runtime##connectNative (string application)

let onStartup f = addListener1 runtime##.onStartup f
let onInstalled f = addListener1 runtime##.onInstalled f
let onSuspend f = addListener1 runtime##.onSuspend f
let onSuspendCanceled f = addListener1 runtime##.onSuspendCanceled f
let onUpdateAvailabale f = addListener1 runtime##.onUpdateAvailable f
let onConnect f = addListener1 runtime##.onConnect f
let onConnectExternal f = addListener1 runtime##.onConnectExternal f
let onConnectNative f = addListener1 runtime##.onConnectNative f
let onMessage f = addListener3 runtime##.onMessage f
let onMessageExternal f = addListener3 runtime##.onMessageExternal f
let onRestartRequired f = addListener1 runtime##.onRestartRequired f
