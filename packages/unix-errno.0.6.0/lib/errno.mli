(*
 * Copyright (c) 2014-2015 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

type t =
  | E2BIG
  | EACCES
  | EADDRINUSE
  | EADDRNOTAVAIL
  | EAFNOSUPPORT
  | EAGAIN
  | EALREADY
  | EBADF
  | EBADMSG
  | EBUSY
  | ECANCELED
  | ECHILD
  | ECONNABORTED
  | ECONNREFUSED
  | ECONNRESET
  | EDEADLK
  | EDESTADDRREQ
  | EDOM
  | EDQUOT
  | EEXIST
  | EFAULT
  | EFBIG
  | EHOSTDOWN
  | EHOSTUNREACH
  | EIDRM
  | EILSEQ
  | EINPROGRESS
  | EINTR
  | EINVAL
  | EIO
  | EISCONN
  | EISDIR
  | ELOOP
  | EMFILE
  | EMLINK
  | EMSGSIZE
  | EMULTIHOP
  | ENAMETOOLONG
  | ENETDOWN
  | ENETRESET
  | ENETUNREACH
  | ENFILE
  | ENOBUFS
  | ENODEV
  | ENOENT
  | ENOEXEC
  | ENOLCK
  | ENOLINK
  | ENOMEM
  | ENOMSG
  | ENOPROTOOPT
  | ENOSPC
  | ENOSYS
  | ENOTBLK
  | ENOTCONN
  | ENOTDIR
  | ENOTEMPTY
  | ENOTRECOVERABLE
  | ENOTSOCK
  | ENOTSUP
  | ENOTTY
  | ENXIO
  | EOPNOTSUPP
  | EOVERFLOW
  | EOWNERDEAD
  | EPERM
  | EPFNOSUPPORT
  | EPIPE
  | EPROTO
  | EPROTONOSUPPORT
  | EPROTOTYPE
  | ERANGE
  | EREMOTE
  | EROFS
  | ESHUTDOWN
  | ESOCKTNOSUPPORT
  | ESPIPE
  | ESRCH
  | ESTALE
  | ETIMEDOUT
  | ETOOMANYREFS
  | ETXTBSY
  | EUSERS
  | EWOULDBLOCK
  | EXDEV
  | ECHRNG
  | EL2NSYNC
  | EL3HLT
  | EL3RST
  | ELNRNG
  | EUNATCH
  | ENOCSI
  | EL2HLT
  | EBADE
  | EBADR
  | EXFULL
  | ENOANO
  | EBADRQC
  | EBADSLT
  | EBFONT
  | ENONET
  | ENOPKG
  | EADV
  | ESRMNT
  | ECOMM
  | EDOTDOT
  | ENOTUNIQ
  | EBADFD
  | EREMCHG
  | ELIBACC
  | ELIBBAD
  | ELIBSCN
  | ELIBMAX
  | ELIBEXEC
  | ERESTART
  | ESTRPIPE
  | EUCLEAN
  | ENOTNAM
  | ENAVAIL
  | EISNAM
  | EREMOTEIO
  | ENOMEDIUM
  | EMEDIUMTYPE
  | ENOKEY
  | EKEYEXPIRED
  | EKEYREVOKED
  | EKEYREJECTED
  | ERFKILL
  | EHWPOISON
  | EPWROFF
  | EDEVERR
  | EBADEXEC
  | EBADARCH
  | ESHLIBVERS
  | EBADMACHO
  | ENOPOLICY
  | EQFULL
  | EDOOFUS
  | ENOTCAPABLE
  | ECAPMODE
  | EPROCLIM
  | EBADRPC
  | ERPCMISMATCH
  | EPROGUNAVAIL
  | EPROGMISMATCH
  | EPROCUNAVAIL
  | EFTYPE
  | EAUTH
  | ENEEDAUTH
  | ENOATTR
  | ENOSTR
  | ENODATA
  | ETIME
  | ENOSR
  | EUNKNOWNERR of Signed.sint

type error = {
  errno : t list;
  call  : string;
  label : string;
}

exception Error of error

(** NB: This module registers a printer for the [Error] exception. *)

type defns = {
  e2big : Signed.sint option;
  eacces : Signed.sint option;
  eaddrinuse : Signed.sint option;
  eaddrnotavail : Signed.sint option;
  eafnosupport : Signed.sint option;
  eagain : Signed.sint option;
  ealready : Signed.sint option;
  ebadf : Signed.sint option;
  ebadmsg : Signed.sint option;
  ebusy : Signed.sint option;
  ecanceled : Signed.sint option;
  echild : Signed.sint option;
  econnaborted : Signed.sint option;
  econnrefused : Signed.sint option;
  econnreset : Signed.sint option;
  edeadlk : Signed.sint option;
  edestaddrreq : Signed.sint option;
  edom : Signed.sint option;
  edquot : Signed.sint option;
  eexist : Signed.sint option;
  efault : Signed.sint option;
  efbig : Signed.sint option;
  ehostdown : Signed.sint option;
  ehostunreach : Signed.sint option;
  eidrm : Signed.sint option;
  eilseq : Signed.sint option;
  einprogress : Signed.sint option;
  eintr : Signed.sint option;
  einval : Signed.sint option;
  eio : Signed.sint option;
  eisconn : Signed.sint option;
  eisdir : Signed.sint option;
  eloop : Signed.sint option;
  emfile : Signed.sint option;
  emlink : Signed.sint option;
  emsgsize : Signed.sint option;
  emultihop : Signed.sint option;
  enametoolong : Signed.sint option;
  enetdown : Signed.sint option;
  enetreset : Signed.sint option;
  enetunreach : Signed.sint option;
  enfile : Signed.sint option;
  enobufs : Signed.sint option;
  enodev : Signed.sint option;
  enoent : Signed.sint option;
  enoexec : Signed.sint option;
  enolck : Signed.sint option;
  enolink : Signed.sint option;
  enomem : Signed.sint option;
  enomsg : Signed.sint option;
  enoprotoopt : Signed.sint option;
  enospc : Signed.sint option;
  enosys : Signed.sint option;
  enotblk : Signed.sint option;
  enotconn : Signed.sint option;
  enotdir : Signed.sint option;
  enotempty : Signed.sint option;
  enotrecoverable : Signed.sint option;
  enotsock : Signed.sint option;
  enotsup : Signed.sint option;
  enotty : Signed.sint option;
  enxio : Signed.sint option;
  eopnotsupp : Signed.sint option;
  eoverflow : Signed.sint option;
  eownerdead : Signed.sint option;
  eperm : Signed.sint option;
  epfnosupport : Signed.sint option;
  epipe : Signed.sint option;
  eproto : Signed.sint option;
  eprotonosupport : Signed.sint option;
  eprototype : Signed.sint option;
  erange : Signed.sint option;
  eremote : Signed.sint option;
  erofs : Signed.sint option;
  eshutdown : Signed.sint option;
  esocktnosupport : Signed.sint option;
  espipe : Signed.sint option;
  esrch : Signed.sint option;
  estale : Signed.sint option;
  etimedout : Signed.sint option;
  etoomanyrefs : Signed.sint option;
  etxtbsy : Signed.sint option;
  eusers : Signed.sint option;
  ewouldblock : Signed.sint option;
  exdev : Signed.sint option;
  echrng : Signed.sint option;
  el2nsync : Signed.sint option;
  el3hlt : Signed.sint option;
  el3rst : Signed.sint option;
  elnrng : Signed.sint option;
  eunatch : Signed.sint option;
  enocsi : Signed.sint option;
  el2hlt : Signed.sint option;
  ebade : Signed.sint option;
  ebadr : Signed.sint option;
  exfull : Signed.sint option;
  enoano : Signed.sint option;
  ebadrqc : Signed.sint option;
  ebadslt : Signed.sint option;
  ebfont : Signed.sint option;
  enonet : Signed.sint option;
  enopkg : Signed.sint option;
  eadv : Signed.sint option;
  esrmnt : Signed.sint option;
  ecomm : Signed.sint option;
  edotdot : Signed.sint option;
  enotuniq : Signed.sint option;
  ebadfd : Signed.sint option;
  eremchg : Signed.sint option;
  elibacc : Signed.sint option;
  elibbad : Signed.sint option;
  elibscn : Signed.sint option;
  elibmax : Signed.sint option;
  elibexec : Signed.sint option;
  erestart : Signed.sint option;
  estrpipe : Signed.sint option;
  euclean : Signed.sint option;
  enotnam : Signed.sint option;
  enavail : Signed.sint option;
  eisnam : Signed.sint option;
  eremoteio : Signed.sint option;
  enomedium : Signed.sint option;
  emediumtype : Signed.sint option;
  enokey : Signed.sint option;
  ekeyexpired : Signed.sint option;
  ekeyrevoked : Signed.sint option;
  ekeyrejected : Signed.sint option;
  erfkill : Signed.sint option;
  ehwpoison : Signed.sint option;
  epwroff : Signed.sint option;
  edeverr : Signed.sint option;
  ebadexec : Signed.sint option;
  ebadarch : Signed.sint option;
  eshlibvers : Signed.sint option;
  ebadmacho : Signed.sint option;
  enopolicy : Signed.sint option;
  eqfull : Signed.sint option;
  edoofus : Signed.sint option;
  enotcapable : Signed.sint option;
  ecapmode : Signed.sint option;
  eproclim : Signed.sint option;
  ebadrpc : Signed.sint option;
  erpcmismatch : Signed.sint option;
  eprogunavail : Signed.sint option;
  eprogmismatch : Signed.sint option;
  eprocunavail : Signed.sint option;
  eftype : Signed.sint option;
  eauth : Signed.sint option;
  eneedauth : Signed.sint option;
  enoattr : Signed.sint option;
  enostr : Signed.sint option;
  enodata : Signed.sint option;
  etime : Signed.sint option;
  enosr : Signed.sint option;
}

module Host : sig
  type t

  val of_defns : defns -> t

  val to_defns : t -> defns

end

val to_code : host:Host.t -> t -> Signed.sint option

val of_code : host:Host.t -> Signed.sint -> t list

val to_string : t -> string

val iter_defns : defns -> (Signed.sint -> t -> unit) -> (t -> unit) -> unit

val string_of_defns : defns -> string

val defns_of_string : string -> defns

val check_errno : (unit -> 'a) -> ('a, error) Result.result

val string_of_error : error -> string
