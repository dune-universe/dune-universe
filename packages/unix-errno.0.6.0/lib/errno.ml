(*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
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
  | EHOSTDOWN (* Linux: Host is down *)
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
  | EPFNOSUPPORT (* Linux: Protocol family not supported *)
  | EPIPE
  | EPROTO
  | EPROTONOSUPPORT
  | EPROTOTYPE
  | ERANGE
  | EREMOTE
  | EROFS
  | ESHUTDOWN (* Linux: Cannot send after transport endpoint shutdown *)
  | ESOCKTNOSUPPORT (* Linux: Socket type not supported *)
  | ESPIPE
  | ESRCH
  | ESTALE
  | ETIMEDOUT
  | ETOOMANYREFS (* Linux: Too many references: cannot splice *)
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

type index = (Signed.sint, t) Hashtbl.t

let empty_defns = {
  e2big = None;
  eacces = None;
  eaddrinuse = None;
  eaddrnotavail = None;
  eafnosupport = None;
  eagain = None;
  ealready = None;
  ebadf = None;
  ebadmsg = None;
  ebusy = None;
  ecanceled = None;
  echild = None;
  econnaborted = None;
  econnrefused = None;
  econnreset = None;
  edeadlk = None;
  edestaddrreq = None;
  edom = None;
  edquot = None;
  eexist = None;
  efault = None;
  efbig = None;
  ehostdown = None;
  ehostunreach = None;
  eidrm = None;
  eilseq = None;
  einprogress = None;
  eintr = None;
  einval = None;
  eio = None;
  eisconn = None;
  eisdir = None;
  eloop = None;
  emfile = None;
  emlink = None;
  emsgsize = None;
  emultihop = None;
  enametoolong = None;
  enetdown = None;
  enetreset = None;
  enetunreach = None;
  enfile = None;
  enobufs = None;
  enodev = None;
  enoent = None;
  enoexec = None;
  enolck = None;
  enolink = None;
  enomem = None;
  enomsg = None;
  enoprotoopt = None;
  enospc = None;
  enosys = None;
  enotblk = None;
  enotconn = None;
  enotdir = None;
  enotempty = None;
  enotrecoverable = None;
  enotsock = None;
  enotsup = None;
  enotty = None;
  enxio = None;
  eopnotsupp = None;
  eoverflow = None;
  eownerdead = None;
  eperm = None;
  epfnosupport = None;
  epipe = None;
  eproto = None;
  eprotonosupport = None;
  eprototype = None;
  erange = None;
  eremote = None;
  erofs = None;
  eshutdown = None;
  esocktnosupport = None;
  espipe = None;
  esrch = None;
  estale = None;
  etimedout = None;
  etoomanyrefs = None;
  etxtbsy = None;
  eusers = None;
  ewouldblock = None;
  exdev = None;
  echrng = None;
  el2nsync = None;
  el3hlt = None;
  el3rst = None;
  elnrng = None;
  eunatch = None;
  enocsi = None;
  el2hlt = None;
  ebade = None;
  ebadr = None;
  exfull = None;
  enoano = None;
  ebadrqc = None;
  ebadslt = None;
  ebfont = None;
  enonet = None;
  enopkg = None;
  eadv = None;
  esrmnt = None;
  ecomm = None;
  edotdot = None;
  enotuniq = None;
  ebadfd = None;
  eremchg = None;
  elibacc = None;
  elibbad = None;
  elibscn = None;
  elibmax = None;
  elibexec = None;
  erestart = None;
  estrpipe = None;
  euclean = None;
  enotnam = None;
  enavail = None;
  eisnam = None;
  eremoteio = None;
  enomedium = None;
  emediumtype = None;
  enokey = None;
  ekeyexpired = None;
  ekeyrevoked = None;
  ekeyrejected = None;
  erfkill = None;
  ehwpoison = None;
  epwroff = None;
  edeverr = None;
  ebadexec = None;
  ebadarch = None;
  eshlibvers = None;
  ebadmacho = None;
  enopolicy = None;
  eqfull = None;
  edoofus = None;
  enotcapable = None;
  ecapmode = None;
  eproclim = None;
  ebadrpc = None;
  erpcmismatch = None;
  eprogunavail = None;
  eprogmismatch = None;
  eprocunavail = None;
  eftype = None;
  eauth = None;
  eneedauth = None;
  enoattr = None;
  enostr = None;
  enodata = None;
  etime = None;
  enosr = None;
}

let to_code ~host = let (defns,_) = host in function
  | E2BIG -> defns.e2big
  | EACCES -> defns.eacces
  | EADDRINUSE -> defns.eaddrinuse
  | EADDRNOTAVAIL -> defns.eaddrnotavail
  | EAFNOSUPPORT -> defns.eafnosupport
  | EAGAIN -> defns.eagain
  | EALREADY -> defns.ealready
  | EBADF -> defns.ebadf
  | EBADMSG -> defns.ebadmsg
  | EBUSY -> defns.ebusy
  | ECANCELED -> defns.ecanceled
  | ECHILD -> defns.echild
  | ECONNABORTED -> defns.econnaborted
  | ECONNREFUSED -> defns.econnrefused
  | ECONNRESET -> defns.econnreset
  | EDEADLK -> defns.edeadlk
  | EDESTADDRREQ -> defns.edestaddrreq
  | EDOM -> defns.edom
  | EDQUOT -> defns.edquot
  | EEXIST -> defns.eexist
  | EFAULT -> defns.efault
  | EFBIG -> defns.efbig
  | EHOSTDOWN -> defns.ehostdown
  | EHOSTUNREACH -> defns.ehostunreach
  | EIDRM -> defns.eidrm
  | EILSEQ -> defns.eilseq
  | EINPROGRESS -> defns.einprogress
  | EINTR -> defns.eintr
  | EINVAL -> defns.einval
  | EIO -> defns.eio
  | EISCONN -> defns.eisconn
  | EISDIR -> defns.eisdir
  | ELOOP -> defns.eloop
  | EMFILE -> defns.emfile
  | EMLINK -> defns.emlink
  | EMSGSIZE -> defns.emsgsize
  | EMULTIHOP -> defns.emultihop
  | ENAMETOOLONG -> defns.enametoolong
  | ENETDOWN -> defns.enetdown
  | ENETRESET -> defns.enetreset
  | ENETUNREACH -> defns.enetunreach
  | ENFILE -> defns.enfile
  | ENOBUFS -> defns.enobufs
  | ENODEV -> defns.enodev
  | ENOENT -> defns.enoent
  | ENOEXEC -> defns.enoexec
  | ENOLCK -> defns.enolck
  | ENOLINK -> defns.enolink
  | ENOMEM -> defns.enomem
  | ENOMSG -> defns.enomsg
  | ENOPROTOOPT -> defns.enoprotoopt
  | ENOSPC -> defns.enospc
  | ENOSYS -> defns.enosys
  | ENOTBLK -> defns.enotblk
  | ENOTCONN -> defns.enotconn
  | ENOTDIR -> defns.enotdir
  | ENOTEMPTY -> defns.enotempty
  | ENOTRECOVERABLE -> defns.enotrecoverable
  | ENOTSOCK -> defns.enotsock
  | ENOTSUP -> defns.enotsup
  | ENOTTY -> defns.enotty
  | ENXIO -> defns.enxio
  | EOPNOTSUPP -> defns.eopnotsupp
  | EOVERFLOW -> defns.eoverflow
  | EOWNERDEAD -> defns.eownerdead
  | EPERM -> defns.eperm
  | EPFNOSUPPORT -> defns.epfnosupport
  | EPIPE -> defns.epipe
  | EPROTO -> defns.eproto
  | EPROTONOSUPPORT -> defns.eprotonosupport
  | EPROTOTYPE -> defns.eprototype
  | ERANGE -> defns.erange
  | EREMOTE -> defns.eremote
  | EROFS -> defns.erofs
  | ESHUTDOWN -> defns.eshutdown
  | ESOCKTNOSUPPORT -> defns.esocktnosupport
  | ESPIPE -> defns.espipe
  | ESRCH -> defns.esrch
  | ESTALE -> defns.estale
  | ETIMEDOUT -> defns.etimedout
  | ETOOMANYREFS -> defns.etoomanyrefs
  | ETXTBSY -> defns.etxtbsy
  | EUSERS -> defns.eusers
  | EWOULDBLOCK -> defns.ewouldblock
  | EXDEV -> defns.exdev
  | ECHRNG -> defns.echrng
  | EL2NSYNC -> defns.el2nsync
  | EL3HLT -> defns.el3hlt
  | EL3RST -> defns.el3rst
  | ELNRNG -> defns.elnrng
  | EUNATCH -> defns.eunatch
  | ENOCSI -> defns.enocsi
  | EL2HLT -> defns.el2hlt
  | EBADE -> defns.ebade
  | EBADR -> defns.ebadr
  | EXFULL -> defns.exfull
  | ENOANO -> defns.enoano
  | EBADRQC -> defns.ebadrqc
  | EBADSLT -> defns.ebadslt
  | EBFONT -> defns.ebfont
  | ENONET -> defns.enonet
  | ENOPKG -> defns.enopkg
  | EADV -> defns.eadv
  | ESRMNT -> defns.esrmnt
  | ECOMM -> defns.ecomm
  | EDOTDOT -> defns.edotdot
  | ENOTUNIQ -> defns.enotuniq
  | EBADFD -> defns.ebadfd
  | EREMCHG -> defns.eremchg
  | ELIBACC -> defns.elibacc
  | ELIBBAD -> defns.elibbad
  | ELIBSCN -> defns.elibscn
  | ELIBMAX -> defns.elibmax
  | ELIBEXEC -> defns.elibexec
  | ERESTART -> defns.erestart
  | ESTRPIPE -> defns.estrpipe
  | EUCLEAN -> defns.euclean
  | ENOTNAM -> defns.enotnam
  | ENAVAIL -> defns.enavail
  | EISNAM -> defns.eisnam
  | EREMOTEIO -> defns.eremoteio
  | ENOMEDIUM -> defns.enomedium
  | EMEDIUMTYPE -> defns.emediumtype
  | ENOKEY -> defns.enokey
  | EKEYEXPIRED -> defns.ekeyexpired
  | EKEYREVOKED -> defns.ekeyrevoked
  | EKEYREJECTED -> defns.ekeyrejected
  | ERFKILL -> defns.erfkill
  | EHWPOISON -> defns.ehwpoison
  | EPWROFF -> defns.epwroff
  | EDEVERR -> defns.edeverr
  | EBADEXEC -> defns.ebadexec
  | EBADARCH -> defns.ebadarch
  | ESHLIBVERS -> defns.eshlibvers
  | EBADMACHO -> defns.ebadmacho
  | ENOPOLICY -> defns.enopolicy
  | EQFULL -> defns.eqfull
  | EDOOFUS -> defns.edoofus
  | ENOTCAPABLE -> defns.enotcapable
  | ECAPMODE -> defns.ecapmode
  | EPROCLIM -> defns.eproclim
  | EBADRPC -> defns.ebadrpc
  | ERPCMISMATCH -> defns.erpcmismatch
  | EPROGUNAVAIL -> defns.eprogunavail
  | EPROGMISMATCH -> defns.eprogmismatch
  | EPROCUNAVAIL -> defns.eprocunavail
  | EFTYPE -> defns.eftype
  | EAUTH -> defns.eauth
  | ENEEDAUTH -> defns.eneedauth
  | ENOATTR -> defns.enoattr
  | ENOSTR -> defns.enostr
  | ENODATA -> defns.enodata
  | ETIME -> defns.etime
  | ENOSR -> defns.enosr
  | EUNKNOWNERR x   -> Some x

let with_code defns symbol code = match symbol with
  | E2BIG -> { defns with e2big = code }
  | EACCES -> { defns with eacces = code }
  | EADDRINUSE -> { defns with eaddrinuse = code }
  | EADDRNOTAVAIL -> { defns with eaddrnotavail = code }
  | EAFNOSUPPORT -> { defns with eafnosupport = code }
  | EAGAIN -> { defns with eagain = code }
  | EALREADY -> { defns with ealready = code }
  | EBADF -> { defns with ebadf = code }
  | EBADMSG -> { defns with ebadmsg = code }
  | EBUSY -> { defns with ebusy = code }
  | ECANCELED -> { defns with ecanceled = code }
  | ECHILD -> { defns with echild = code }
  | ECONNABORTED -> { defns with econnaborted = code }
  | ECONNREFUSED -> { defns with econnrefused = code }
  | ECONNRESET -> { defns with econnreset = code }
  | EDEADLK -> { defns with edeadlk = code }
  | EDESTADDRREQ -> { defns with edestaddrreq = code }
  | EDOM -> { defns with edom = code }
  | EDQUOT -> { defns with edquot = code }
  | EEXIST -> { defns with eexist = code }
  | EFAULT -> { defns with efault = code }
  | EFBIG -> { defns with efbig = code }
  | EHOSTDOWN -> { defns with ehostdown = code }
  | EHOSTUNREACH -> { defns with ehostunreach = code }
  | EIDRM -> { defns with eidrm = code }
  | EILSEQ -> { defns with eilseq = code }
  | EINPROGRESS -> { defns with einprogress = code }
  | EINTR -> { defns with eintr = code }
  | EINVAL -> { defns with einval = code }
  | EIO -> { defns with eio = code }
  | EISCONN -> { defns with eisconn = code }
  | EISDIR -> { defns with eisdir = code }
  | ELOOP -> { defns with eloop = code }
  | EMFILE -> { defns with emfile = code }
  | EMLINK -> { defns with emlink = code }
  | EMSGSIZE -> { defns with emsgsize = code }
  | EMULTIHOP -> { defns with emultihop = code }
  | ENAMETOOLONG -> { defns with enametoolong = code }
  | ENETDOWN -> { defns with enetdown = code }
  | ENETRESET -> { defns with enetreset = code }
  | ENETUNREACH -> { defns with enetunreach = code }
  | ENFILE -> { defns with enfile = code }
  | ENOBUFS -> { defns with enobufs = code }
  | ENODEV -> { defns with enodev = code }
  | ENOENT -> { defns with enoent = code }
  | ENOEXEC -> { defns with enoexec = code }
  | ENOLCK -> { defns with enolck = code }
  | ENOLINK -> { defns with enolink = code }
  | ENOMEM -> { defns with enomem = code }
  | ENOMSG -> { defns with enomsg = code }
  | ENOPROTOOPT -> { defns with enoprotoopt = code }
  | ENOSPC -> { defns with enospc = code }
  | ENOSYS -> { defns with enosys = code }
  | ENOTBLK -> { defns with enotblk = code }
  | ENOTCONN -> { defns with enotconn = code }
  | ENOTDIR -> { defns with enotdir = code }
  | ENOTEMPTY -> { defns with enotempty = code }
  | ENOTRECOVERABLE -> { defns with enotrecoverable = code }
  | ENOTSOCK -> { defns with enotsock = code }
  | ENOTSUP -> { defns with enotsup = code }
  | ENOTTY -> { defns with enotty = code }
  | ENXIO -> { defns with enxio = code }
  | EOPNOTSUPP -> { defns with eopnotsupp = code }
  | EOVERFLOW -> { defns with eoverflow = code }
  | EOWNERDEAD -> { defns with eownerdead = code }
  | EPERM -> { defns with eperm = code }
  | EPFNOSUPPORT -> { defns with epfnosupport = code }
  | EPIPE -> { defns with epipe = code }
  | EPROTO -> { defns with eproto = code }
  | EPROTONOSUPPORT -> { defns with eprotonosupport = code }
  | EPROTOTYPE -> { defns with eprototype = code }
  | ERANGE -> { defns with erange = code }
  | EREMOTE -> { defns with eremote = code }
  | EROFS -> { defns with erofs = code }
  | ESHUTDOWN -> { defns with eshutdown = code }
  | ESOCKTNOSUPPORT -> { defns with esocktnosupport = code }
  | ESPIPE -> { defns with espipe = code }
  | ESRCH -> { defns with esrch = code }
  | ESTALE -> { defns with estale = code }
  | ETIMEDOUT -> { defns with etimedout = code }
  | ETOOMANYREFS -> { defns with etoomanyrefs = code }
  | ETXTBSY -> { defns with etxtbsy = code }
  | EUSERS -> { defns with eusers = code }
  | EWOULDBLOCK -> { defns with ewouldblock = code }
  | EXDEV -> { defns with exdev = code }
  | ECHRNG -> { defns with echrng = code }
  | EL2NSYNC -> { defns with el2nsync = code }
  | EL3HLT -> { defns with el3hlt = code }
  | EL3RST -> { defns with el3rst = code }
  | ELNRNG -> { defns with elnrng = code }
  | EUNATCH -> { defns with eunatch = code }
  | ENOCSI -> { defns with enocsi = code }
  | EL2HLT -> { defns with el2hlt = code }
  | EBADE -> { defns with ebade = code }
  | EBADR -> { defns with ebadr = code }
  | EXFULL -> { defns with exfull = code }
  | ENOANO -> { defns with enoano = code }
  | EBADRQC -> { defns with ebadrqc = code }
  | EBADSLT -> { defns with ebadslt = code }
  | EBFONT -> { defns with ebfont = code }
  | ENONET -> { defns with enonet = code }
  | ENOPKG -> { defns with enopkg = code }
  | EADV -> { defns with eadv = code }
  | ESRMNT -> { defns with esrmnt = code }
  | ECOMM -> { defns with ecomm = code }
  | EDOTDOT -> { defns with edotdot = code }
  | ENOTUNIQ -> { defns with enotuniq = code }
  | EBADFD -> { defns with ebadfd = code }
  | EREMCHG -> { defns with eremchg = code }
  | ELIBACC -> { defns with elibacc = code }
  | ELIBBAD -> { defns with elibbad = code }
  | ELIBSCN -> { defns with elibscn = code }
  | ELIBMAX -> { defns with elibmax = code }
  | ELIBEXEC -> { defns with elibexec = code }
  | ERESTART -> { defns with erestart = code }
  | ESTRPIPE -> { defns with estrpipe = code }
  | EUCLEAN -> { defns with euclean = code }
  | ENOTNAM -> { defns with enotnam = code }
  | ENAVAIL -> { defns with enavail = code }
  | EISNAM -> { defns with eisnam = code }
  | EREMOTEIO -> { defns with eremoteio = code }
  | ENOMEDIUM -> { defns with enomedium = code }
  | EMEDIUMTYPE -> { defns with emediumtype = code }
  | ENOKEY -> { defns with enokey = code }
  | EKEYEXPIRED -> { defns with ekeyexpired = code }
  | EKEYREVOKED -> { defns with ekeyrevoked = code }
  | EKEYREJECTED -> { defns with ekeyrejected = code }
  | ERFKILL -> { defns with erfkill = code }
  | EHWPOISON -> { defns with ehwpoison = code }
  | EPWROFF -> { defns with epwroff = code }
  | EDEVERR -> { defns with edeverr = code }
  | EBADEXEC -> { defns with ebadexec = code }
  | EBADARCH -> { defns with ebadarch = code }
  | ESHLIBVERS -> { defns with eshlibvers = code }
  | EBADMACHO -> { defns with ebadmacho = code }
  | ENOPOLICY -> { defns with enopolicy = code }
  | EQFULL -> { defns with eqfull = code }
  | EDOOFUS -> { defns with edoofus = code }
  | ENOTCAPABLE -> { defns with enotcapable = code }
  | ECAPMODE -> { defns with ecapmode = code }
  | EPROCLIM -> { defns with eproclim = code }
  | EBADRPC -> { defns with ebadrpc = code }
  | ERPCMISMATCH -> { defns with erpcmismatch = code }
  | EPROGUNAVAIL -> { defns with eprogunavail = code }
  | EPROGMISMATCH -> { defns with eprogmismatch = code }
  | EPROCUNAVAIL -> { defns with eprocunavail = code }
  | EFTYPE -> { defns with eftype = code }
  | EAUTH -> { defns with eauth = code }
  | ENEEDAUTH -> { defns with eneedauth = code }
  | ENOATTR -> { defns with enoattr = code }
  | ENOSTR -> { defns with enostr = code }
  | ENODATA -> { defns with enodata = code }
  | ETIME -> { defns with etime = code }
  | ENOSR -> { defns with enosr = code }
  | EUNKNOWNERR _ -> defns

let of_code ~host code =
  let (_,index) = host in
  match Hashtbl.find_all index code with
  | [] -> [EUNKNOWNERR code]
  | errnos -> errnos

let to_string = function
  | E2BIG -> "E2BIG"
  | EACCES -> "EACCES"
  | EADDRINUSE -> "EADDRINUSE"
  | EADDRNOTAVAIL -> "EADDRNOTAVAIL"
  | EAFNOSUPPORT -> "EAFNOSUPPORT"
  | EAGAIN -> "EAGAIN"
  | EALREADY -> "EALREADY"
  | EBADF -> "EBADF"
  | EBADMSG -> "EBADMSG"
  | EBUSY -> "EBUSY"
  | ECANCELED -> "ECANCELED"
  | ECHILD -> "ECHILD"
  | ECONNABORTED -> "ECONNABORTED"
  | ECONNREFUSED -> "ECONNREFUSED"
  | ECONNRESET -> "ECONNRESET"
  | EDEADLK -> "EDEADLK"
  | EDESTADDRREQ -> "EDESTADDRREQ"
  | EDOM -> "EDOM"
  | EDQUOT -> "EDQUOT"
  | EEXIST -> "EEXIST"
  | EFAULT -> "EFAULT"
  | EFBIG -> "EFBIG"
  | EHOSTDOWN -> "EHOSTDOWN"
  | EHOSTUNREACH -> "EHOSTUNREACH"
  | EIDRM -> "EIDRM"
  | EILSEQ -> "EILSEQ"
  | EINPROGRESS -> "EINPROGRESS"
  | EINTR -> "EINTR"
  | EINVAL -> "EINVAL"
  | EIO -> "EIO"
  | EISCONN -> "EISCONN"
  | EISDIR -> "EISDIR"
  | ELOOP -> "ELOOP"
  | EMFILE -> "EMFILE"
  | EMLINK -> "EMLINK"
  | EMSGSIZE -> "EMSGSIZE"
  | EMULTIHOP -> "EMULTIHOP"
  | ENAMETOOLONG -> "ENAMETOOLONG"
  | ENETDOWN -> "ENETDOWN"
  | ENETRESET -> "ENETRESET"
  | ENETUNREACH -> "ENETUNREACH"
  | ENFILE -> "ENFILE"
  | ENOBUFS -> "ENOBUFS"
  | ENODEV -> "ENODEV"
  | ENOENT -> "ENOENT"
  | ENOEXEC -> "ENOEXEC"
  | ENOLCK -> "ENOLCK"
  | ENOLINK -> "ENOLINK"
  | ENOMEM -> "ENOMEM"
  | ENOMSG -> "ENOMSG"
  | ENOPROTOOPT -> "ENOPROTOOPT"
  | ENOSPC -> "ENOSPC"
  | ENOSYS -> "ENOSYS"
  | ENOTBLK -> "ENOTBLK"
  | ENOTCONN -> "ENOTCONN"
  | ENOTDIR -> "ENOTDIR"
  | ENOTEMPTY -> "ENOTEMPTY"
  | ENOTRECOVERABLE -> "ENOTRECOVERABLE"
  | ENOTSOCK -> "ENOTSOCK"
  | ENOTSUP -> "ENOTSUP"
  | ENOTTY -> "ENOTTY"
  | ENXIO -> "ENXIO"
  | EOPNOTSUPP -> "EOPNOTSUPP"
  | EOVERFLOW -> "EOVERFLOW"
  | EOWNERDEAD -> "EOWNERDEAD"
  | EPERM -> "EPERM"
  | EPFNOSUPPORT -> "EPFNOSUPPORT"
  | EPIPE -> "EPIPE"
  | EPROTO -> "EPROTO"
  | EPROTONOSUPPORT -> "EPROTONOSUPPORT"
  | EPROTOTYPE -> "EPROTOTYPE"
  | ERANGE -> "ERANGE"
  | EREMOTE -> "EREMOTE"
  | EROFS -> "EROFS"
  | ESHUTDOWN -> "ESHUTDOWN"
  | ESOCKTNOSUPPORT -> "ESOCKTNOSUPPORT"
  | ESPIPE -> "ESPIPE"
  | ESRCH -> "ESRCH"
  | ESTALE -> "ESTALE"
  | ETIMEDOUT -> "ETIMEDOUT"
  | ETOOMANYREFS -> "ETOOMANYREFS"
  | ETXTBSY -> "ETXTBSY"
  | EUSERS -> "EUSERS"
  | EWOULDBLOCK -> "EWOULDBLOCK"
  | EXDEV -> "EXDEV"
  | ECHRNG -> "ECHRNG"
  | EL2NSYNC -> "EL2NSYNC"
  | EL3HLT -> "EL3HLT"
  | EL3RST -> "EL3RST"
  | ELNRNG -> "ELNRNG"
  | EUNATCH -> "EUNATCH"
  | ENOCSI -> "ENOCSI"
  | EL2HLT -> "EL2HLT"
  | EBADE -> "EBADE"
  | EBADR -> "EBADR"
  | EXFULL -> "EXFULL"
  | ENOANO -> "ENOANO"
  | EBADRQC -> "EBADRQC"
  | EBADSLT -> "EBADSLT"
  | EBFONT -> "EBFONT"
  | ENONET -> "ENONET"
  | ENOPKG -> "ENOPKG"
  | EADV -> "EADV"
  | ESRMNT -> "ESRMNT"
  | ECOMM -> "ECOMM"
  | EDOTDOT -> "EDOTDOT"
  | ENOTUNIQ -> "ENOTUNIQ"
  | EBADFD -> "EBADFD"
  | EREMCHG -> "EREMCHG"
  | ELIBACC -> "ELIBACC"
  | ELIBBAD -> "ELIBBAD"
  | ELIBSCN -> "ELIBSCN"
  | ELIBMAX -> "ELIBMAX"
  | ELIBEXEC -> "ELIBEXEC"
  | ERESTART -> "ERESTART"
  | ESTRPIPE -> "ESTRPIPE"
  | EUCLEAN -> "EUCLEAN"
  | ENOTNAM -> "ENOTNAM"
  | ENAVAIL -> "ENAVAIL"
  | EISNAM -> "EISNAM"
  | EREMOTEIO -> "EREMOTEIO"
  | ENOMEDIUM -> "ENOMEDIUM"
  | EMEDIUMTYPE -> "EMEDIUMTYPE"
  | ENOKEY -> "ENOKEY"
  | EKEYEXPIRED -> "EKEYEXPIRED"
  | EKEYREVOKED -> "EKEYREVOKED"
  | EKEYREJECTED -> "EKEYREJECTED"
  | ERFKILL -> "ERFKILL"
  | EHWPOISON -> "EHWPOISON"
  | EPWROFF -> "EPWROFF"
  | EDEVERR -> "EDEVERR"
  | EBADEXEC -> "EBADEXEC"
  | EBADARCH -> "EBADARCH"
  | ESHLIBVERS -> "ESHLIBVERS"
  | EBADMACHO -> "EBADMACHO"
  | ENOPOLICY -> "ENOPOLICY"
  | EQFULL -> "EQFULL"
  | EDOOFUS -> "EDOOFUS"
  | ENOTCAPABLE -> "ENOTCAPABLE"
  | ECAPMODE -> "ECAPMODE"
  | EPROCLIM -> "EPROCLIM"
  | EBADRPC -> "EBADRPC"
  | ERPCMISMATCH -> "ERPCMISMATCH"
  | EPROGUNAVAIL -> "EPROGUNAVAIL"
  | EPROGMISMATCH -> "EPROGMISMATCH"
  | EPROCUNAVAIL -> "EPROCUNAVAIL"
  | EFTYPE -> "EFTYPE"
  | EAUTH -> "EAUTH"
  | ENEEDAUTH -> "ENEEDAUTH"
  | ENOATTR -> "ENOATTR"
  | ENOSTR -> "ENOSTR"
  | ENODATA -> "ENODATA"
  | ETIME -> "ETIME"
  | ENOSR -> "ENOSR"
  | EUNKNOWNERR x   -> "EUNKNOWNERR_"^(Signed.SInt.to_string x)

let of_string = function
  | "E2BIG" -> Some E2BIG
  | "EACCES" -> Some EACCES
  | "EADDRINUSE" -> Some EADDRINUSE
  | "EADDRNOTAVAIL" -> Some EADDRNOTAVAIL
  | "EAFNOSUPPORT" -> Some EAFNOSUPPORT
  | "EAGAIN" -> Some EAGAIN
  | "EALREADY" -> Some EALREADY
  | "EBADF" -> Some EBADF
  | "EBADMSG" -> Some EBADMSG
  | "EBUSY" -> Some EBUSY
  | "ECANCELED" -> Some ECANCELED
  | "ECHILD" -> Some ECHILD
  | "ECONNABORTED" -> Some ECONNABORTED
  | "ECONNREFUSED" -> Some ECONNREFUSED
  | "ECONNRESET" -> Some ECONNRESET
  | "EDEADLK" -> Some EDEADLK
  | "EDESTADDRREQ" -> Some EDESTADDRREQ
  | "EDOM" -> Some EDOM
  | "EDQUOT" -> Some EDQUOT
  | "EEXIST" -> Some EEXIST
  | "EFAULT" -> Some EFAULT
  | "EFBIG" -> Some EFBIG
  | "EHOSTDOWN" -> Some EHOSTDOWN
  | "EHOSTUNREACH" -> Some EHOSTUNREACH
  | "EIDRM" -> Some EIDRM
  | "EILSEQ" -> Some EILSEQ
  | "EINPROGRESS" -> Some EINPROGRESS
  | "EINTR" -> Some EINTR
  | "EINVAL" -> Some EINVAL
  | "EIO" -> Some EIO
  | "EISCONN" -> Some EISCONN
  | "EISDIR" -> Some EISDIR
  | "ELOOP" -> Some ELOOP
  | "EMFILE" -> Some EMFILE
  | "EMLINK" -> Some EMLINK
  | "EMSGSIZE" -> Some EMSGSIZE
  | "EMULTIHOP" -> Some EMULTIHOP
  | "ENAMETOOLONG" -> Some ENAMETOOLONG
  | "ENETDOWN" -> Some ENETDOWN
  | "ENETRESET" -> Some ENETRESET
  | "ENETUNREACH" -> Some ENETUNREACH
  | "ENFILE" -> Some ENFILE
  | "ENOBUFS" -> Some ENOBUFS
  | "ENODEV" -> Some ENODEV
  | "ENOENT" -> Some ENOENT
  | "ENOEXEC" -> Some ENOEXEC
  | "ENOLCK" -> Some ENOLCK
  | "ENOLINK" -> Some ENOLINK
  | "ENOMEM" -> Some ENOMEM
  | "ENOMSG" -> Some ENOMSG
  | "ENOPROTOOPT" -> Some ENOPROTOOPT
  | "ENOSPC" -> Some ENOSPC
  | "ENOSYS" -> Some ENOSYS
  | "ENOTBLK" -> Some ENOTBLK
  | "ENOTCONN" -> Some ENOTCONN
  | "ENOTDIR" -> Some ENOTDIR
  | "ENOTEMPTY" -> Some ENOTEMPTY
  | "ENOTRECOVERABLE" -> Some ENOTRECOVERABLE
  | "ENOTSOCK" -> Some ENOTSOCK
  | "ENOTSUP" -> Some ENOTSUP
  | "ENOTTY" -> Some ENOTTY
  | "ENXIO" -> Some ENXIO
  | "EOPNOTSUPP" -> Some EOPNOTSUPP
  | "EOVERFLOW" -> Some EOVERFLOW
  | "EOWNERDEAD" -> Some EOWNERDEAD
  | "EPERM" -> Some EPERM
  | "EPFNOSUPPORT" -> Some EPFNOSUPPORT
  | "EPIPE" -> Some EPIPE
  | "EPROTO" -> Some EPROTO
  | "EPROTONOSUPPORT" -> Some EPROTONOSUPPORT
  | "EPROTOTYPE" -> Some EPROTOTYPE
  | "ERANGE" -> Some ERANGE
  | "EREMOTE" -> Some EREMOTE
  | "EROFS" -> Some EROFS
  | "ESHUTDOWN" -> Some ESHUTDOWN
  | "ESOCKTNOSUPPORT" -> Some ESOCKTNOSUPPORT
  | "ESPIPE" -> Some ESPIPE
  | "ESRCH" -> Some ESRCH
  | "ESTALE" -> Some ESTALE
  | "ETIMEDOUT" -> Some ETIMEDOUT
  | "ETOOMANYREFS" -> Some ETOOMANYREFS
  | "ETXTBSY" -> Some ETXTBSY
  | "EUSERS" -> Some EUSERS
  | "EWOULDBLOCK" -> Some EWOULDBLOCK
  | "EXDEV" -> Some EXDEV
  | "ECHRNG" -> Some ECHRNG
  | "EL2NSYNC" -> Some EL2NSYNC
  | "EL3HLT" -> Some EL3HLT
  | "EL3RST" -> Some EL3RST
  | "ELNRNG" -> Some ELNRNG
  | "EUNATCH" -> Some EUNATCH
  | "ENOCSI" -> Some ENOCSI
  | "EL2HLT" -> Some EL2HLT
  | "EBADE" -> Some EBADE
  | "EBADR" -> Some EBADR
  | "EXFULL" -> Some EXFULL
  | "ENOANO" -> Some ENOANO
  | "EBADRQC" -> Some EBADRQC
  | "EBADSLT" -> Some EBADSLT
  | "EBFONT" -> Some EBFONT
  | "ENONET" -> Some ENONET
  | "ENOPKG" -> Some ENOPKG
  | "EADV" -> Some EADV
  | "ESRMNT" -> Some ESRMNT
  | "ECOMM" -> Some ECOMM
  | "EDOTDOT" -> Some EDOTDOT
  | "ENOTUNIQ" -> Some ENOTUNIQ
  | "EBADFD" -> Some EBADFD
  | "EREMCHG" -> Some EREMCHG
  | "ELIBACC" -> Some ELIBACC
  | "ELIBBAD" -> Some ELIBBAD
  | "ELIBSCN" -> Some ELIBSCN
  | "ELIBMAX" -> Some ELIBMAX
  | "ELIBEXEC" -> Some ELIBEXEC
  | "ERESTART" -> Some ERESTART
  | "ESTRPIPE" -> Some ESTRPIPE
  | "EUCLEAN" -> Some EUCLEAN
  | "ENOTNAM" -> Some ENOTNAM
  | "ENAVAIL" -> Some ENAVAIL
  | "EISNAM" -> Some EISNAM
  | "EREMOTEIO" -> Some EREMOTEIO
  | "ENOMEDIUM" -> Some ENOMEDIUM
  | "EMEDIUMTYPE" -> Some EMEDIUMTYPE
  | "ENOKEY" -> Some ENOKEY
  | "EKEYEXPIRED" -> Some EKEYEXPIRED
  | "EKEYREVOKED" -> Some EKEYREVOKED
  | "EKEYREJECTED" -> Some EKEYREJECTED
  | "ERFKILL" -> Some ERFKILL
  | "EHWPOISON" -> Some EHWPOISON
  | "EPWROFF" -> Some EPWROFF
  | "EDEVERR" -> Some EDEVERR
  | "EBADEXEC" -> Some EBADEXEC
  | "EBADARCH" -> Some EBADARCH
  | "ESHLIBVERS" -> Some ESHLIBVERS
  | "EBADMACHO" -> Some EBADMACHO
  | "ENOPOLICY" -> Some ENOPOLICY
  | "EQFULL" -> Some EQFULL
  | "EDOOFUS" -> Some EDOOFUS
  | "ENOTCAPABLE" -> Some ENOTCAPABLE
  | "ECAPMODE" -> Some ECAPMODE
  | "EPROCLIM" -> Some EPROCLIM
  | "EBADRPC" -> Some EBADRPC
  | "ERPCMISMATCH" -> Some ERPCMISMATCH
  | "EPROGUNAVAIL" -> Some EPROGUNAVAIL
  | "EPROGMISMATCH" -> Some EPROGMISMATCH
  | "EPROCUNAVAIL" -> Some EPROCUNAVAIL
  | "EFTYPE" -> Some EFTYPE
  | "EAUTH" -> Some EAUTH
  | "ENEEDAUTH" -> Some ENEEDAUTH
  | "ENOATTR" -> Some ENOATTR
  | "ENOSTR" -> Some ENOSTR
  | "ENODATA" -> Some ENODATA
  | "ETIME" -> Some ETIME
  | "ENOSR" -> Some ENOSR
  | _ -> None

let iter_defns defns f_exist f_missing =
  (match defns.e2big with
   | Some x -> f_exist x E2BIG | None -> f_missing E2BIG);
  (match defns.eacces with
   | Some x -> f_exist x EACCES | None -> f_missing EACCES);
  (match defns.eaddrinuse with
   | Some x -> f_exist x EADDRINUSE | None -> f_missing EADDRINUSE);
  (match defns.eaddrnotavail with
   | Some x -> f_exist x EADDRNOTAVAIL | None -> f_missing EADDRNOTAVAIL);
  (match defns.eafnosupport with
   | Some x -> f_exist x EAFNOSUPPORT | None -> f_missing EAFNOSUPPORT);
  (match defns.eagain with
   | Some x -> f_exist x EAGAIN | None -> f_missing EAGAIN);
  (match defns.ealready with
   | Some x -> f_exist x EALREADY | None -> f_missing EALREADY);
  (match defns.ebadf with
   | Some x -> f_exist x EBADF | None -> f_missing EBADF);
  (match defns.ebadmsg with
   | Some x -> f_exist x EBADMSG | None -> f_missing EBADMSG);
  (match defns.ebusy with
   | Some x -> f_exist x EBUSY | None -> f_missing EBUSY);
  (match defns.ecanceled with
   | Some x -> f_exist x ECANCELED | None -> f_missing ECANCELED);
  (match defns.echild with
   | Some x -> f_exist x ECHILD | None -> f_missing ECHILD);
  (match defns.econnaborted with
   | Some x -> f_exist x ECONNABORTED | None -> f_missing ECONNABORTED);
  (match defns.econnrefused with
   | Some x -> f_exist x ECONNREFUSED | None -> f_missing ECONNREFUSED);
  (match defns.econnreset with
   | Some x -> f_exist x ECONNRESET | None -> f_missing ECONNRESET);
  (match defns.edeadlk with
   | Some x -> f_exist x EDEADLK | None -> f_missing EDEADLK);
  (match defns.edestaddrreq with
   | Some x -> f_exist x EDESTADDRREQ | None -> f_missing EDESTADDRREQ);
  (match defns.edom with
   | Some x -> f_exist x EDOM | None -> f_missing EDOM);
  (match defns.edquot with
   | Some x -> f_exist x EDQUOT | None -> f_missing EDQUOT);
  (match defns.eexist with
   | Some x -> f_exist x EEXIST | None -> f_missing EEXIST);
  (match defns.efault with
   | Some x -> f_exist x EFAULT | None -> f_missing EFAULT);
  (match defns.efbig with
   | Some x -> f_exist x EFBIG | None -> f_missing EFBIG);
  (match defns.ehostdown with
   | Some x -> f_exist x EHOSTDOWN | None -> f_missing EHOSTDOWN);
  (match defns.ehostunreach with
   | Some x -> f_exist x EHOSTUNREACH | None -> f_missing EHOSTUNREACH);
  (match defns.eidrm with
   | Some x -> f_exist x EIDRM | None -> f_missing EIDRM);
  (match defns.eilseq with
   | Some x -> f_exist x EILSEQ | None -> f_missing EILSEQ);
  (match defns.einprogress with
   | Some x -> f_exist x EINPROGRESS | None -> f_missing EINPROGRESS);
  (match defns.eintr with
   | Some x -> f_exist x EINTR | None -> f_missing EINTR);
  (match defns.einval with
   | Some x -> f_exist x EINVAL | None -> f_missing EINVAL);
  (match defns.eio with
   | Some x -> f_exist x EIO | None -> f_missing EIO);
  (match defns.eisconn with
   | Some x -> f_exist x EISCONN | None -> f_missing EISCONN);
  (match defns.eisdir with
   | Some x -> f_exist x EISDIR | None -> f_missing EISDIR);
  (match defns.eloop with
   | Some x -> f_exist x ELOOP | None -> f_missing ELOOP);
  (match defns.emfile with
   | Some x -> f_exist x EMFILE | None -> f_missing EMFILE);
  (match defns.emlink with
   | Some x -> f_exist x EMLINK | None -> f_missing EMLINK);
  (match defns.emsgsize with
   | Some x -> f_exist x EMSGSIZE | None -> f_missing EMSGSIZE);
  (match defns.emultihop with
   | Some x -> f_exist x EMULTIHOP | None -> f_missing EMULTIHOP);
  (match defns.enametoolong with
   | Some x -> f_exist x ENAMETOOLONG | None -> f_missing ENAMETOOLONG);
  (match defns.enetdown with
   | Some x -> f_exist x ENETDOWN | None -> f_missing ENETDOWN);
  (match defns.enetreset with
   | Some x -> f_exist x ENETRESET | None -> f_missing ENETRESET);
  (match defns.enetunreach with
   | Some x -> f_exist x ENETUNREACH | None -> f_missing ENETUNREACH);
  (match defns.enfile with
   | Some x -> f_exist x ENFILE | None -> f_missing ENFILE);
  (match defns.enobufs with
   | Some x -> f_exist x ENOBUFS | None -> f_missing ENOBUFS);
  (match defns.enodev with
   | Some x -> f_exist x ENODEV | None -> f_missing ENODEV);
  (match defns.enoent with
   | Some x -> f_exist x ENOENT | None -> f_missing ENOENT);
  (match defns.enoexec with
   | Some x -> f_exist x ENOEXEC | None -> f_missing ENOEXEC);
  (match defns.enolck with
   | Some x -> f_exist x ENOLCK | None -> f_missing ENOLCK);
  (match defns.enolink with
   | Some x -> f_exist x ENOLINK | None -> f_missing ENOLINK);
  (match defns.enomem with
   | Some x -> f_exist x ENOMEM | None -> f_missing ENOMEM);
  (match defns.enomsg with
   | Some x -> f_exist x ENOMSG | None -> f_missing ENOMSG);
  (match defns.enoprotoopt with
   | Some x -> f_exist x ENOPROTOOPT | None -> f_missing ENOPROTOOPT);
  (match defns.enospc with
   | Some x -> f_exist x ENOSPC | None -> f_missing ENOSPC);
  (match defns.enosys with
   | Some x -> f_exist x ENOSYS | None -> f_missing ENOSYS);
  (match defns.enotblk with
   | Some x -> f_exist x ENOTBLK | None -> f_missing ENOTBLK);
  (match defns.enotconn with
   | Some x -> f_exist x ENOTCONN | None -> f_missing ENOTCONN);
  (match defns.enotdir with
   | Some x -> f_exist x ENOTDIR | None -> f_missing ENOTDIR);
  (match defns.enotempty with
   | Some x -> f_exist x ENOTEMPTY | None -> f_missing ENOTEMPTY);
  (match defns.enotrecoverable with
   | Some x -> f_exist x ENOTRECOVERABLE | None -> f_missing ENOTRECOVERABLE);
  (match defns.enotsock with
   | Some x -> f_exist x ENOTSOCK | None -> f_missing ENOTSOCK);
  (match defns.enotsup with
   | Some x -> f_exist x ENOTSUP | None -> f_missing ENOTSUP);
  (match defns.enotty with
   | Some x -> f_exist x ENOTTY | None -> f_missing ENOTTY);
  (match defns.enxio with
   | Some x -> f_exist x ENXIO | None -> f_missing ENXIO);
  (match defns.eopnotsupp with
   | Some x -> f_exist x EOPNOTSUPP | None -> f_missing EOPNOTSUPP);
  (match defns.eoverflow with
   | Some x -> f_exist x EOVERFLOW | None -> f_missing EOVERFLOW);
  (match defns.eownerdead with
   | Some x -> f_exist x EOWNERDEAD | None -> f_missing EOWNERDEAD);
  (match defns.eperm with
   | Some x -> f_exist x EPERM | None -> f_missing EPERM);
  (match defns.epfnosupport with
   | Some x -> f_exist x EPFNOSUPPORT | None -> f_missing EPFNOSUPPORT);
  (match defns.epipe with
   | Some x -> f_exist x EPIPE | None -> f_missing EPIPE);
  (match defns.eproto with
   | Some x -> f_exist x EPROTO | None -> f_missing EPROTO);
  (match defns.eprotonosupport with
   | Some x -> f_exist x EPROTONOSUPPORT | None -> f_missing EPROTONOSUPPORT);
  (match defns.eprototype with
   | Some x -> f_exist x EPROTOTYPE | None -> f_missing EPROTOTYPE);
  (match defns.erange with
   | Some x -> f_exist x ERANGE | None -> f_missing ERANGE);
  (match defns.eremote with
   | Some x -> f_exist x EREMOTE | None -> f_missing EREMOTE);
  (match defns.erofs with
   | Some x -> f_exist x EROFS | None -> f_missing EROFS);
  (match defns.eshutdown with
   | Some x -> f_exist x ESHUTDOWN | None -> f_missing ESHUTDOWN);
  (match defns.esocktnosupport with
   | Some x -> f_exist x ESOCKTNOSUPPORT | None -> f_missing ESOCKTNOSUPPORT);
  (match defns.espipe with
   | Some x -> f_exist x ESPIPE | None -> f_missing ESPIPE);
  (match defns.esrch with
   | Some x -> f_exist x ESRCH | None -> f_missing ESRCH);
  (match defns.estale with
   | Some x -> f_exist x ESTALE | None -> f_missing ESTALE);
  (match defns.etimedout with
   | Some x -> f_exist x ETIMEDOUT | None -> f_missing ETIMEDOUT);
  (match defns.etoomanyrefs with
   | Some x -> f_exist x ETOOMANYREFS | None -> f_missing ETOOMANYREFS);
  (match defns.etxtbsy with
   | Some x -> f_exist x ETXTBSY | None -> f_missing ETXTBSY);
  (match defns.eusers with
   | Some x -> f_exist x EUSERS | None -> f_missing EUSERS);
  (match defns.ewouldblock with
   | Some x -> f_exist x EWOULDBLOCK | None -> f_missing EWOULDBLOCK);
  (match defns.exdev with
   | Some x -> f_exist x EXDEV | None -> f_missing EXDEV);
  (match defns.echrng with
   | Some x -> f_exist x ECHRNG | None -> f_missing ECHRNG);
  (match defns.el2nsync with
   | Some x -> f_exist x EL2NSYNC | None -> f_missing EL2NSYNC);
  (match defns.el3hlt with
   | Some x -> f_exist x EL3HLT | None -> f_missing EL3HLT);
  (match defns.el3rst with
   | Some x -> f_exist x EL3RST | None -> f_missing EL3RST);
  (match defns.elnrng with
   | Some x -> f_exist x ELNRNG | None -> f_missing ELNRNG);
  (match defns.eunatch with
   | Some x -> f_exist x EUNATCH | None -> f_missing EUNATCH);
  (match defns.enocsi with
   | Some x -> f_exist x ENOCSI | None -> f_missing ENOCSI);
  (match defns.el2hlt with
   | Some x -> f_exist x EL2HLT | None -> f_missing EL2HLT);
  (match defns.ebade with
   | Some x -> f_exist x EBADE | None -> f_missing EBADE);
  (match defns.ebadr with
   | Some x -> f_exist x EBADR | None -> f_missing EBADR);
  (match defns.exfull with
   | Some x -> f_exist x EXFULL | None -> f_missing EXFULL);
  (match defns.enoano with
   | Some x -> f_exist x ENOANO | None -> f_missing ENOANO);
  (match defns.ebadrqc with
   | Some x -> f_exist x EBADRQC | None -> f_missing EBADRQC);
  (match defns.ebadslt with
   | Some x -> f_exist x EBADSLT | None -> f_missing EBADSLT);
  (match defns.ebfont with
   | Some x -> f_exist x EBFONT | None -> f_missing EBFONT);
  (match defns.enonet with
   | Some x -> f_exist x ENONET | None -> f_missing ENONET);
  (match defns.enopkg with
   | Some x -> f_exist x ENOPKG | None -> f_missing ENOPKG);
  (match defns.eadv with
   | Some x -> f_exist x EADV | None -> f_missing EADV);
  (match defns.esrmnt with
   | Some x -> f_exist x ESRMNT | None -> f_missing ESRMNT);
  (match defns.ecomm with
   | Some x -> f_exist x ECOMM | None -> f_missing ECOMM);
  (match defns.edotdot with
   | Some x -> f_exist x EDOTDOT | None -> f_missing EDOTDOT);
  (match defns.enotuniq with
   | Some x -> f_exist x ENOTUNIQ | None -> f_missing ENOTUNIQ);
  (match defns.ebadfd with
   | Some x -> f_exist x EBADFD | None -> f_missing EBADFD);
  (match defns.eremchg with
   | Some x -> f_exist x EREMCHG | None -> f_missing EREMCHG);
  (match defns.elibacc with
   | Some x -> f_exist x ELIBACC | None -> f_missing ELIBACC);
  (match defns.elibbad with
   | Some x -> f_exist x ELIBBAD | None -> f_missing ELIBBAD);
  (match defns.elibscn with
   | Some x -> f_exist x ELIBSCN | None -> f_missing ELIBSCN);
  (match defns.elibmax with
   | Some x -> f_exist x ELIBMAX | None -> f_missing ELIBMAX);
  (match defns.elibexec with
   | Some x -> f_exist x ELIBEXEC | None -> f_missing ELIBEXEC);
  (match defns.erestart with
   | Some x -> f_exist x ERESTART | None -> f_missing ERESTART);
  (match defns.estrpipe with
   | Some x -> f_exist x ESTRPIPE | None -> f_missing ESTRPIPE);
  (match defns.euclean with
   | Some x -> f_exist x EUCLEAN | None -> f_missing EUCLEAN);
  (match defns.enotnam with
   | Some x -> f_exist x ENOTNAM | None -> f_missing ENOTNAM);
  (match defns.enavail with
   | Some x -> f_exist x ENAVAIL | None -> f_missing ENAVAIL);
  (match defns.eisnam with
   | Some x -> f_exist x EISNAM | None -> f_missing EISNAM);
  (match defns.eremoteio with
   | Some x -> f_exist x EREMOTEIO | None -> f_missing EREMOTEIO);
  (match defns.enomedium with
   | Some x -> f_exist x ENOMEDIUM | None -> f_missing ENOMEDIUM);
  (match defns.emediumtype with
   | Some x -> f_exist x EMEDIUMTYPE | None -> f_missing EMEDIUMTYPE);
  (match defns.enokey with
   | Some x -> f_exist x ENOKEY | None -> f_missing ENOKEY);
  (match defns.ekeyexpired with
   | Some x -> f_exist x EKEYEXPIRED | None -> f_missing EKEYEXPIRED);
  (match defns.ekeyrevoked with
   | Some x -> f_exist x EKEYREVOKED | None -> f_missing EKEYREVOKED);
  (match defns.ekeyrejected with
   | Some x -> f_exist x EKEYREJECTED | None -> f_missing EKEYREJECTED);
  (match defns.erfkill with
   | Some x -> f_exist x ERFKILL | None -> f_missing ERFKILL);
  (match defns.ehwpoison with
   | Some x -> f_exist x EHWPOISON | None -> f_missing EHWPOISON);
  (match defns.epwroff with
   | Some x -> f_exist x EPWROFF | None -> f_missing EPWROFF);
  (match defns.edeverr with
   | Some x -> f_exist x EDEVERR | None -> f_missing EDEVERR);
  (match defns.ebadexec with
   | Some x -> f_exist x EBADEXEC | None -> f_missing EBADEXEC);
  (match defns.ebadarch with
   | Some x -> f_exist x EBADARCH | None -> f_missing EBADARCH);
  (match defns.eshlibvers with
   | Some x -> f_exist x ESHLIBVERS | None -> f_missing ESHLIBVERS);
  (match defns.ebadmacho with
   | Some x -> f_exist x EBADMACHO | None -> f_missing EBADMACHO);
  (match defns.enopolicy with
   | Some x -> f_exist x ENOPOLICY | None -> f_missing ENOPOLICY);
  (match defns.eqfull with
   | Some x -> f_exist x EQFULL | None -> f_missing EQFULL);
  (match defns.edoofus with
   | Some x -> f_exist x EDOOFUS | None -> f_missing EDOOFUS);
  (match defns.enotcapable with
   | Some x -> f_exist x ENOTCAPABLE | None -> f_missing ENOTCAPABLE);
  (match defns.ecapmode with
   | Some x -> f_exist x ECAPMODE | None -> f_missing ECAPMODE);
  (match defns.eproclim with
   | Some x -> f_exist x EPROCLIM | None -> f_missing EPROCLIM);
  (match defns.ebadrpc with
   | Some x -> f_exist x EBADRPC | None -> f_missing EBADRPC);
  (match defns.erpcmismatch with
   | Some x -> f_exist x ERPCMISMATCH | None -> f_missing ERPCMISMATCH);
  (match defns.eprogunavail with
   | Some x -> f_exist x EPROGUNAVAIL | None -> f_missing EPROGUNAVAIL);
  (match defns.eprogmismatch with
   | Some x -> f_exist x EPROGMISMATCH | None -> f_missing EPROGMISMATCH);
  (match defns.eprocunavail with
   | Some x -> f_exist x EPROCUNAVAIL | None -> f_missing EPROCUNAVAIL);
  (match defns.eftype with
   | Some x -> f_exist x EFTYPE | None -> f_missing EFTYPE);
  (match defns.eauth with
   | Some x -> f_exist x EAUTH | None -> f_missing EAUTH);
  (match defns.eneedauth with
   | Some x -> f_exist x ENEEDAUTH | None -> f_missing ENEEDAUTH);
  (match defns.enoattr with
   | Some x -> f_exist x ENOATTR | None -> f_missing ENOATTR);
  (match defns.enostr with
   | Some x -> f_exist x ENOSTR | None -> f_missing ENOSTR);
  (match defns.enodata with
   | Some x -> f_exist x ENODATA | None -> f_missing ENODATA);
  (match defns.etime with
   | Some x -> f_exist x ETIME | None -> f_missing ETIME);
  (match defns.enosr with
   | Some x -> f_exist x ENOSR | None -> f_missing ENOSR);
  ()

module Host = struct
  type t = defns * index

  let index_of_defns defns =
    let h = Hashtbl.create 100 in
    iter_defns defns (Hashtbl.add h) (fun _ -> ());
    h

  let of_defns defns = (defns, index_of_defns defns)

  let to_defns (defns, _) = defns

end

let string_of_defns defns =
  let buf = Buffer.create 1024 in
  iter_defns defns
    (fun code symbol ->
       Buffer.add_string buf (Printf.sprintf "%s\t%s\n" (to_string symbol)
                                (Signed.SInt.to_string code))
    )
    (fun symbol ->
       Buffer.add_string buf (Printf.sprintf "%s\t\n" (to_string symbol))
    );
  Buffer.contents buf

let defns_of_string s =
  let rec read_lines defns s =
    try
      let symbol, code, off = Scanf.sscanf s "%s\t%s\n" (fun symbol_s code_s ->
        of_string symbol_s,
        (if code_s = "" then None else Some (Signed.SInt.of_string code_s)),
        String.(length symbol_s + 1 + length code_s + 1)
      ) in
      let defns = match symbol with
        | Some symbol -> with_code defns symbol code
        | None -> defns
      in
      read_lines defns String.(sub s off (length s - off))
    with End_of_file -> defns
  in
  read_lines empty_defns s

let check_errno fn =
  try Result.Ok (fn ())
  with Error e -> Result.Error e

let string_of_error { errno; call; label } =
   Printf.sprintf "{ errno = [%s]; call = %s; label = %s }"
     (String.concat "; " (List.map to_string errno))
     call label

let () = Printexc.register_printer
   (function Error e -> Some (string_of_error e)
           | _ -> None)
