open Ezjs_min
open Promise
open Extension_utils
open Browser
open Runtime

class type onInstalledEvent = object
  method id : js_string t optdef prop
  method previousVersion : js_string t optdef prop
  method reason : js_string t prop
  method temporary : bool t prop
end

class type runtime = object
  method lastError : error t prop
  method id : js_string t prop
  method getBackgroundPage : window t promise t meth
  method openOptionsPage : unit promise t meth
  method getManifest : 'a t meth
  method getURL : js_string t -> js_string t meth
  method setUninstallURL : js_string t -> unit promise t meth
  method reload : unit meth
  method requestUpdateCheck : requestCheckResponse t prop promise t meth
  method connect : js_string t opt -> 'a t opt -> port t meth
  method sendMessage : js_string t opt -> 'a t -> connectInfo t opt -> 'b t promise t meth
  method sendNativeMessage : js_string t -> 'a t -> 'b t promise t meth
  method getPlatformInfo : platformInfo t promise t meth
  method getBrowserInfo : browserInfo t promise t meth
  method getPackageDirectoryEntry : 'a t meth
  method onStartup : unit event t prop
  method onInstalled : onInstalledEvent t event t prop
  method onSuspend : unit event t prop
  method onSuspendCanceled : unit event t prop
  method onUpdateAvailable : 'a t event t prop
  method onBrowserUpdateAvailable : unit event t prop
  method onConnect : port t event t prop
  method onConnectExternal : port t event t prop
  method onMessage : ('a t,  js_string t, ('a t -> bool t) callback) event3 t prop
  method onMessageExternal : ('a t,  js_string t, ('a t -> bool t) callback) event3 t prop
  method onRestartRequired : js_string t event t prop
end

let runtime : runtime t = Unsafe.variable "browser.runtime"

let last_error () = runtime##.lastError
let id () = runtime##.id
let getManifest () = runtime##getManifest
let getURL s = to_string (runtime##getURL (string s))
let reload () = runtime##reload
let connect ?id ?info () =
  let id = Opt.option id in
  let info = Opt.option info in
  runtime##connect id info

let onStartup f = addListener1 runtime##.onStartup f
let onInstalled f = addListener1 runtime##.onInstalled f
let onSuspend f = addListener1 runtime##.onSuspend f
let onSuspendCanceled f = addListener1 runtime##.onSuspendCanceled f
let onUpdateAvailabale f = addListener1 runtime##.onUpdateAvailable f
let onConnect f = addListener1 runtime##.onConnect f
let onConnectExternal f = addListener1 runtime##.onConnectExternal f
let onMessage f = addListener3 runtime##.onMessage f
let onMessageExternal f = addListener3 runtime##.onMessageExternal f
let onRestartRequired f = addListener1 runtime##.onRestartRequired f
