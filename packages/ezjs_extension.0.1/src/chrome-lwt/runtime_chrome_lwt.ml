open Ezjs_min_lwt
open Promise
include Chrome_common.Runtime

let getBackgroundPage () =
  to_lwt_cb (fun cb -> runtime##getBackgroundPage cb)
let openOptionsPage ?callback () =
  to_lwt_cb_opt callback (fun cb -> runtime##openOptionsPage cb)
let setUninstallURL ?callback s =
  to_lwt_cb_opt callback (fun cb -> runtime##setUninstallURL s cb)
let requestUpdateCheck () =
  to_lwt_cb (fun cb -> runtime##requestUpdateCheck cb)
let restartAfterDelay ?callback i =
  to_lwt_cb_opt callback (fun cb -> runtime##restartAfterDelay i cb)
let sendMessage ?id ?options ?callback message =
  match callback with
  | Some callback ->
    let waiter, notifier = Lwt.wait () in
    runtime##sendMessage (Optdef.option id) message (Optdef.option options) (def (Lwt.wakeup notifier));
    waiter >>= fun x -> return (Some (callback x))
  | None -> runtime##sendMessage (Optdef.option id) message (Optdef.option options) undefined; Lwt.return_none
let sendNativeMessage ?callback application message =
  match callback with
  | Some callback ->
    let waiter, notifier = Lwt.wait () in
    runtime##sendNativeMessage (string application) message (def (Lwt.wakeup notifier));
    waiter >>= fun x -> return (Some (callback x))
  | None -> runtime##sendNativeMessage (string application) message undefined; Lwt.return_none
let getPlatformInfo () =
  to_lwt_cb_tr Extension_utils.Runtime.to_platform_info (fun cb -> runtime##getPlatformInfo cb)
let getPackageDirectoryEntry () =
  to_lwt_cb (fun cb -> runtime##getPackageDirectoryEntry cb)
