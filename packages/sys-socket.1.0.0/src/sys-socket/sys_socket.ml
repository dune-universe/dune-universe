open Ctypes

include Sys_socket_types.SaFamily

include Sys_socket_stubs.Def(Sys_socket_generated_stubs)

type socket_type = int
let socket_type_t = int

let sock_dgram = Types.sock_dgram
let sock_stream = Types.sock_stream
let sock_seqpacket = Types.sock_seqpacket

let sa_family_t = Types.sa_family_t

let af_inet = Types.af_inet
let af_inet6 = Types.af_inet6
let af_unspec = Types.af_unspec

type socklen = Types.socklen
let socklen_t = Types.socklen_t
let int_of_socklen = Types.int_of_socklen
let socklen_of_int = Types.socklen_of_int

let from_sockaddr_storage t ptr =
  from_voidp t (to_voidp ptr)

module SockaddrStorage = Types.SockaddrStorage

type sockaddr_storage = SockaddrStorage.t structure
let sockaddr_storage_t = SockaddrStorage.t

module Sockaddr = struct
  include Types.Sockaddr
  let from_sockaddr_storage = from_sockaddr_storage t
  let sa_data_len = Types.sa_data_len
end

type sockaddr = Sockaddr.t structure
let sockaddr_t = Sockaddr.t

type in_port = Unsigned.uint16
let in_port_t = uint16_t

module SockaddrInet = struct
  include Types.SockaddrInet
  let from_sockaddr_storage = from_sockaddr_storage t
end

type sockaddr_in = SockaddrInet.t structure
let sockaddr_in_t = SockaddrInet.t

module SockaddrInet6 = struct
  include Types.SockaddrInet6
  let from_sockaddr_storage = from_sockaddr_storage t
end

type sockaddr_in6 = SockaddrInet6.t structure
let sockaddr_in6_t = SockaddrInet6.t

let getnameinfo sockaddr_ptr =
  let maxhost = Types.ni_maxhost in
  let s = allocate_n char ~count:maxhost in
  let maxserv = Types.ni_maxserv in
  let p = allocate_n char ~count:maxserv in
  match getnameinfo sockaddr_ptr (socklen_of_int (sizeof sockaddr_t))
                    s (socklen_of_int maxhost) 
                    p (socklen_of_int maxserv)
                    (Types.ni_numerichost lor
                     Types.ni_numericserv)  with
    | 0 ->
      let host =
        let length =
          Unsigned.Size_t.to_int
            (strnlen s (Unsigned.Size_t.of_int maxhost))
        in
        string_from_ptr s ~length
      in
      let port =
        let length =
          Unsigned.Size_t.to_int
            (strnlen p (Unsigned.Size_t.of_int maxserv))
        in
        let port =
          string_from_ptr p ~length
        in
        try
          int_of_string port
        with _ ->
          match getservbyname p null with
            | ptr when is_null ptr -> failwith "getnameinfo"
            | ptr ->
               Unsigned.UInt16.to_int
                 (ntohs (!@ (ptr |-> Types.Servent.s_port)))
      in
      host, port
    | _ -> failwith "getnameinfo"

let getaddrinfo host port =
  let port = string_of_int port in
  let hints =
    allocate_n Types.Addrinfo.t ~count:1
  in
  (hints |-> Types.Addrinfo.ai_flags) <-@
     (Types.ni_numerichost lor
      Types.ni_numericserv);
  let ret =
    allocate_n (ptr Types.Addrinfo.t) ~count:1
  in
  match getaddrinfo host port hints ret with
    | 0 ->
      let addrinfo =
         allocate Sockaddr.t
           (!@ (!@ ((!@ ret) |-> Types.Addrinfo.ai_addr)))
       in
       freeaddrinfo (!@ ret);
       addrinfo
    | _ -> failwith "getaddrinfo"

let to_unix_sockaddr s =
  match !@ (s |-> Sockaddr.sa_family) with
    | id when id = af_inet || id = af_inet6 ->
       let inet_addr, port = getnameinfo s in
       let inet_addr =
         Unix.inet_addr_of_string inet_addr
       in
       Unix.ADDR_INET (inet_addr, port)
    | _ -> failwith "Not implemented"

let from_unix_sockaddr = function
  | Unix.ADDR_UNIX _ ->
      failwith "Not implemented"
  | Unix.ADDR_INET (inet_addr,port) ->
      let inet_addr =
        Unix.string_of_inet_addr inet_addr
      in
      getaddrinfo inet_addr port
