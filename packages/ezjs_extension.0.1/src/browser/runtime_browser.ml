open Ezjs_min
open Promise
include Browser_common.Runtime

let getBackgroundPage f = jthen runtime##getBackgroundPage f
let openOptionsPage ?callback () =
  jthen runtime##openOptionsPage (match callback with None -> (fun _ -> ()) | Some cb -> cb)
let setUninstallURL ?callback s =
  jthen (runtime##setUninstallURL s) (match callback with None -> (fun _ -> ()) | Some cb -> cb)
let requestUpdateCheck f = jthen runtime##requestUpdateCheck f
let sendMessage ?id ?options ?callback message =
  jthen (runtime##sendMessage (Opt.option id) message (Opt.option options))
    (match callback with None -> (fun _ -> ()) | Some cb -> cb)
let sendNativeMessage ?callback application message =
  jthen (runtime##sendNativeMessage (string application) message)
    (match callback with None -> (fun _ -> ()) | Some cb -> cb)
let getPlatformInfo f =
  jthen runtime##getPlatformInfo (fun o -> f (Extension_utils.Runtime.to_platform_info o))
let getPackageDirectoryEntry f =
  jthen runtime##getPackageDirectoryEntry f
