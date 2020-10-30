open Ezjs_min_lwt
open Promise
include Browser_common.Runtime

let getBackgroundPage () = to_lwt_exn runtime##getBackgroundPage
let openOptionsPage ?callback () =
  to_lwt_exn_opt callback runtime##openOptionsPage
let setUninstallURL ?callback s =
  to_lwt_exn_opt callback (runtime##setUninstallURL s)
let requestUpdateCheck () = to_lwt_exn runtime##requestUpdateCheck
let sendMessage ?id ?options ?callback message =
  to_lwt_exn_opt callback @@
  runtime##sendMessage (Opt.option id) message (Opt.option options)
let sendNativeMessage ?callback application message =
  to_lwt_exn_opt callback @@ runtime##sendNativeMessage (string application) message
let getPlatformInfo () =
  to_lwt_exn_tr Extension_utils.Runtime.to_platform_info runtime##getPlatformInfo
let getPackageDirectoryEntry () =
  to_lwt_exn runtime##getPackageDirectoryEntry
