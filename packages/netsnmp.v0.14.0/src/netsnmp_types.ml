open Netsnmp_raw
module Snmp_sec_auth_proto = Session.Snmp_sec_auth_proto

module Snmp_v1_2c_auth_data = struct
  type t = {
    community             : string
  }
end

module Snmp_v3_auth_data = struct
  type t = {
    securityName          : string
  ; securityAuthProto     : Snmp_sec_auth_proto.t
  ; securityAuthPassword  : string
  }
end

module Snmp_version_auth = struct
  type t =
      Version_1  of Snmp_v1_2c_auth_data.t
    | Version_2c of Snmp_v1_2c_auth_data.t
    | Version_3  of Snmp_v3_auth_data.t

  let create_v1 community = Version_1 { Snmp_v1_2c_auth_data.community }
  let create_v2c community = Version_2c { Snmp_v1_2c_auth_data.community }

  let create_v3 ~securityName ~securityAuthProto ~securityAuthPassword =
    Version_3 {
      Snmp_v3_auth_data.securityName
    ; securityAuthProto
    ; securityAuthPassword
  }
end


module Connection_info = struct
  type t = {
    version_auth          : Snmp_version_auth.t
  ; peername              : string
  ; localname             : string option
  ; local_port            : int option
  ; retries               : int option
  ; timeout               : int option
  }

  let create ~version_auth ~peername ?localname ?local_port ?retries ?timeout () =
    {
      version_auth
    ; peername
    ; localname
    ; local_port
    ; retries
    ; timeout
    }
end
