module Def (S : Cstubs.Types.TYPE) = struct
  open S

  type socket = int

  type socket_status =
    [ `Init
    | `Opened
    | `Listening
    | `Connecting
    | `Connected
    | `Broken
    | `Closing
    | `Closed
    | `Nonexist ]

  type socket_opt =
    [ `Messageapi
    | `Payloadsize
    | `Transtype
    | `Rcvsyn
    | `Sndsyn
    | `Reuseaddr
    | `Rcvbuf
    | `Sndbuf
    | `Udp_rcvbuf
    | `Udp_sndbuf
    | `Enforced_encryption ]

  type transtype = [ `Live | `File | `Invalid ]
  type poll_flag = [ `Read | `Write | `Error ]

  type errno =
    [ `Easyncfail
    | `Easyncrcv
    | `Easyncsnd
    | `Eboundsock
    | `Econgest
    | `Econnfail
    | `Econnlost
    | `Econnrej
    | `Econnsetup
    | `Econnsock
    | `Eduplisten
    | `Efile
    | `Einvalbufferapi
    | `Einvalmsgapi
    | `Einvop
    | `Einvparam
    | `Einvpollid
    | `Einvrdoff
    | `Einvsock
    | `Einvwroff
    | `Elargemsg
    | `Enobuf
    | `Enoconn
    | `Enolisten
    | `Enoserver
    | `Epeererr
    | `Erdperm
    | `Erdvnoserv
    | `Erdvunbound
    | `Eresource
    | `Esecfail
    | `Esockfail
    | `Ethread
    | `Etimeout
    | `Eunboundsock
    | `Eunknown
    | `Ewrperm
    | `Success ]

  let socket_status : socket_status typ =
    enum "SRT_SOCKSTATUS"
      [
        (`Init, constant "SRTS_INIT" int64_t);
        (`Opened, constant "SRTS_OPENED" int64_t);
        (`Listening, constant "SRTS_LISTENING" int64_t);
        (`Connecting, constant "SRTS_CONNECTING" int64_t);
        (`Connected, constant "SRTS_CONNECTED" int64_t);
        (`Broken, constant "SRTS_BROKEN" int64_t);
        (`Closing, constant "SRTS_CLOSING" int64_t);
        (`Closed, constant "SRTS_CLOSED" int64_t);
        (`Nonexist, constant "SRTS_NONEXIST" int64_t);
      ]

  let socket_opt : socket_opt typ =
    enum "SRT_SOCKOPT"
      [
        (`Messageapi, constant "SRTO_MESSAGEAPI" int64_t);
        (`Payloadsize, constant "SRTO_PAYLOADSIZE" int64_t);
        (`Transtype, constant "SRTO_TRANSTYPE" int64_t);
        (`Rcvsyn, constant "SRTO_RCVSYN" int64_t);
        (`Sndsyn, constant "SRTO_SNDSYN" int64_t);
        (`Reuseaddr, constant "SRTO_REUSEADDR" int64_t);
        (`Rcvbuf, constant "SRTO_RCVBUF" int64_t);
        (`Sndbuf, constant "SRTO_SNDBUF" int64_t);
        (`Udp_rcvbuf, constant "SRTO_UDP_RCVBUF" int64_t);
        (`Udp_sndbuf, constant "SRTO_UDP_SNDBUF" int64_t);
        (`Enforced_encryption, constant "SRTO_ENFORCEDENCRYPTION" int64_t);
      ]

  let srtt_live = constant "SRTT_LIVE" int64_t
  let srtt_file = constant "SRTT_FILE" int64_t
  let srtt_invalid = constant "SRTT_INVALID" int64_t

  let transtype : transtype typ =
    enum "SRT_TRANSTYPE"
      [(`Live, srtt_live); (`File, srtt_file); (`Invalid, srtt_invalid)]

  let log_crit = constant "LOG_CRIT" int
  let log_err = constant "LOG_ERR" int
  let log_warning = constant "LOG_WARNING" int
  let log_notice = constant "LOG_NOTICE" int
  let log_debug = constant "LOG_DEBUG" int

  let errno : errno typ =
    enum "SRT_ERRNO"
      [
        (`Eunknown, constant "SRT_EUNKNOWN" int64_t);
        (`Success, constant "SRT_SUCCESS" int64_t);
        (`Econnsetup, constant "SRT_ECONNSETUP" int64_t);
        (`Enoserver, constant "SRT_ENOSERVER" int64_t);
        (`Econnrej, constant "SRT_ECONNREJ" int64_t);
        (`Esockfail, constant "SRT_ESOCKFAIL" int64_t);
        (`Esecfail, constant "SRT_ESECFAIL" int64_t);
        (`Econnfail, constant "SRT_ECONNFAIL" int64_t);
        (`Econnlost, constant "SRT_ECONNLOST" int64_t);
        (`Enoconn, constant "SRT_ENOCONN" int64_t);
        (`Eresource, constant "SRT_ERESOURCE" int64_t);
        (`Ethread, constant "SRT_ETHREAD" int64_t);
        (`Enobuf, constant "SRT_ENOBUF" int64_t);
        (`Efile, constant "SRT_EFILE" int64_t);
        (`Einvrdoff, constant "SRT_EINVRDOFF" int64_t);
        (`Erdperm, constant "SRT_ERDPERM" int64_t);
        (`Einvwroff, constant "SRT_EINVWROFF" int64_t);
        (`Ewrperm, constant "SRT_EWRPERM" int64_t);
        (`Einvop, constant "SRT_EINVOP" int64_t);
        (`Eboundsock, constant "SRT_EBOUNDSOCK" int64_t);
        (`Econnsock, constant "SRT_ECONNSOCK" int64_t);
        (`Einvparam, constant "SRT_EINVPARAM" int64_t);
        (`Einvsock, constant "SRT_EINVSOCK" int64_t);
        (`Eunboundsock, constant "SRT_EUNBOUNDSOCK" int64_t);
        (`Enolisten, constant "SRT_ENOLISTEN" int64_t);
        (`Erdvnoserv, constant "SRT_ERDVNOSERV" int64_t);
        (`Erdvunbound, constant "SRT_ERDVUNBOUND" int64_t);
        (`Einvalmsgapi, constant "SRT_EINVALMSGAPI" int64_t);
        (`Einvalbufferapi, constant "SRT_EINVALBUFFERAPI" int64_t);
        (`Eduplisten, constant "SRT_EDUPLISTEN" int64_t);
        (`Elargemsg, constant "SRT_ELARGEMSG" int64_t);
        (`Einvpollid, constant "SRT_EINVPOLLID" int64_t);
        (`Easyncfail, constant "SRT_EASYNCFAIL" int64_t);
        (`Easyncsnd, constant "SRT_EASYNCSND" int64_t);
        (`Easyncrcv, constant "SRT_EASYNCRCV" int64_t);
        (`Etimeout, constant "SRT_ETIMEOUT" int64_t);
        (`Econgest, constant "SRT_ECONGEST" int64_t);
        (`Epeererr, constant "SRT_EPEERERR" int64_t);
      ]

  let poll_flag : poll_flag typ =
    enum "SRT_EPOLL_OPT"
      [
        (`Read, constant "SRT_EPOLL_IN" int64_t);
        (`Write, constant "SRT_EPOLL_OUT" int64_t);
        (`Error, constant "SRT_EPOLL_ERR" int64_t);
      ]

  let poll_flag = typedef poll_flag "const int"
  let const_string = typedef string "const char*"
end
