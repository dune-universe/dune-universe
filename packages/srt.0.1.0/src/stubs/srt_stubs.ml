open Ctypes
open Sys_socket

type socket = int

module Def (F : Cstubs.FOREIGN) = struct
  include Srt_types.Def(Srt_generated_types)

  open F

  let startup = foreign "srt_startup" (void @-> returning void)

  let cleanup = foreign "srt_cleanup" (void @-> returning void)

  let getlasterror = foreign "srt_getlasterror" (ptr int @-> (returning errno))

  let getlasterror_str = foreign "srt_getlasterror_str" (void @-> (returning const_string))

  let clearlasterror = foreign "srt_clearlasterror" (void @-> returning void)

  let create_socket = foreign "srt_create_socket" (void @-> returning int)

  let socket = foreign "srt_socket" (sa_family_t @-> socket_type_t @-> int @-> returning int)

  let bind = foreign "srt_bind" (int @-> ptr sockaddr_t @-> int @-> returning int) 

  let listen = foreign "srt_listen" (int @-> int @-> returning int)

  let accept = foreign "srt_accept" (int @-> ptr sockaddr_t @-> ptr int @-> returning int)

  let connect = foreign "srt_connect" (int @-> ptr sockaddr_t @-> int @-> returning int)

  let rendez_vous = foreign "srt_rendezvous" (int @-> ptr sockaddr_t @-> int @-> ptr sockaddr_t @-> int @-> returning int)

  let send = foreign "srt_send" (int @-> string @-> int @-> returning int)

  let recv = foreign "srt_recv" (int @-> ptr char @-> int @-> returning int)

  let recvmsg = foreign "srt_recvmsg" (int @-> ptr char @-> int @-> returning int)

  let sendmsg = foreign "srt_sendmsg" (int @-> string @-> int @-> int @-> bool @-> returning int)

  let setsockflag = foreign "srt_setsockflag" (int @-> socket_opt @-> ptr void @-> int @-> returning int)

  let getsockflag = foreign "srt_getsockflag" (int @-> socket_opt @-> ptr void @-> ptr int @-> returning int)

  let getsockstate = foreign "srt_getsockstate" (int @-> (returning socket_status))

  let setloglevel = foreign "srt_setloglevel" (int @-> returning void)

  let close = foreign "srt_close" (int @-> returning int)

  let epoll_create = foreign "srt_epoll_create" (void @-> returning int)

  let epoll_add_usock = foreign "srt_epoll_add_usock" (int @-> int @-> ptr poll_flag @-> returning int)

  let epoll_remove_usock = foreign "srt_epoll_remove_usock" (int @-> int @-> returning int)  

  let epoll_update_usock = foreign "srt_epoll_update_usock" (int @-> int @-> ptr poll_flag @-> returning int)

  let epoll_wait = foreign "srt_epoll_wait" (int @-> ptr int @-> ptr int @-> ptr int @-> ptr int @-> int64_t @-> ptr void @-> ptr void @-> ptr void @-> ptr void @-> returning int)

  let epoll_release = foreign "srt_epoll_release" (int @-> returning int)
end
