open Ezjs_min
open Extension_utils.Runtime
include Chrome_common.Runtime

let getBackgroundPage f = runtime##getBackgroundPage (wrap_callback f)
let openOptionsPage ?callback () = runtime##openOptionsPage (optdef wrap_callback callback)
let setUninstallURL ?callback s = runtime##setUninstallURL s (optdef wrap_callback callback)
let requestUpdateCheck f = runtime##requestUpdateCheck (wrap_callback f)
let restartAfterDelay ?callback i = runtime##restartAfterDelay i (optdef wrap_callback callback)
let sendMessage ?id ?options ?callback message =
  runtime##sendMessage (Optdef.option id) message (Optdef.option options)
    (Optdef.option callback)
let sendNativeMessage ?callback application message =
  runtime##sendNativeMessage (string application) message (Optdef.option callback)
let getPlatformInfo f =
  runtime##getPlatformInfo (wrap_callback (fun o -> f (to_platform_info o)))
let getPackageDirectoryEntry f =
  runtime##getPackageDirectoryEntry (wrap_callback f)
