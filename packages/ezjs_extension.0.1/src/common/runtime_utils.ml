open Ezjs_min
open Tabs_utils

class type requestCheckResponse = object
  method status : js_string t prop
  method details : 'a t prop
end

class type messageSender = object
  method tab : tab t optdef prop
  method frameId : int optdef prop
  method id : js_string t optdef prop
  method url : js_string t optdef prop
  method nativeApplication : js_string optdef prop
  method tlsChannelId : js_string t optdef prop
end

class type port = object
  method name : js_string t prop
  method disconnect : unit meth
  method onDisconnect : port t Browser_utils.event t prop
  method onMessage : 'a t Browser_utils.event t prop
  method postMessage : 'a t -> unit meth
  method sender : messageSender t optdef prop
end

class type platformInfo = object
  method os : js_string t prop
  method arch : js_string t prop
  method nacl_arch : js_string t prop
end

class type browserInfo = object
  method name : js_string t prop
  method vendor : js_string t prop
  method version : js_string t prop
  method buildId : js_string t prop
end

class type connectInfo = object
  method name : js_string t prop
  method includeTlsChannelId : bool t prop
end

type platform_info = {
  platform_os : string;
  platform_arch : string;
  platform_nacl_arch : string
}

let of_platform_info {platform_os; platform_arch; platform_nacl_arch} =
  let r : platformInfo t = Unsafe.obj [||] in
  r##.os := string platform_os;
  r##.arch := string platform_arch;
  r##.nacl_arch := string platform_nacl_arch;
  r

let to_platform_info (o:platformInfo t) = {
  platform_os = to_string o##.os;
  platform_arch = to_string o##.arch;
  platform_nacl_arch = to_string o##.nacl_arch
}

let mk_connection_info name =
  let i : connectInfo t = Unsafe.obj [||] in
  i##.name := string name ;
  i
