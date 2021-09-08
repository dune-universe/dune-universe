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

module Type = Unix_errno_types.C(Unix_errno_types_detected)
module C = Unix_errno_bindings.C(Unix_errno_generated)

let host =
  let option i = Some i in
  let platform i = Signed.SInt.(if i = minus_one then None else Some i) in
  let defns = Errno.(Type.({
    e2big = option e2big;
    eacces = option eacces;
    eaddrinuse = option eaddrinuse;
    eaddrnotavail = option eaddrnotavail;
    eafnosupport = option eafnosupport;
    eagain = option eagain;
    ealready = option ealready;
    ebadf = option ebadf;
    ebadmsg = option ebadmsg;
    ebusy = option ebusy;
    ecanceled = option ecanceled;
    echild = option echild;
    econnaborted = option econnaborted;
    econnrefused = option econnrefused;
    econnreset = option econnreset;
    edeadlk = option edeadlk;
    edestaddrreq = option edestaddrreq;
    edom = option edom;
    edquot = option edquot;
    eexist = option eexist;
    efault = option efault;
    efbig = option efbig;
    ehostdown = option ehostdown;
    ehostunreach = option ehostunreach;
    eidrm = option eidrm;
    eilseq = option eilseq;
    einprogress = option einprogress;
    eintr = option eintr;
    einval = option einval;
    eio = option eio;
    eisconn = option eisconn;
    eisdir = option eisdir;
    eloop = option eloop;
    emfile = option emfile;
    emlink = option emlink;
    emsgsize = option emsgsize;
    emultihop = option emultihop;
    enametoolong = option enametoolong;
    enetdown = option enetdown;
    enetreset = option enetreset;
    enetunreach = option enetunreach;
    enfile = option enfile;
    enobufs = option enobufs;
    enodev = option enodev;
    enoent = option enoent;
    enoexec = option enoexec;
    enolck = option enolck;
    enolink = option enolink;
    enomem = option enomem;
    enomsg = option enomsg;
    enoprotoopt = option enoprotoopt;
    enospc = option enospc;
    enosys = option enosys;
    enotblk = option enotblk;
    enotconn = option enotconn;
    enotdir = option enotdir;
    enotempty = option enotempty;
    enotrecoverable = option enotrecoverable;
    enotsock = option enotsock;
    enotsup = option enotsup;
    enotty = option enotty;
    enxio = option enxio;
    eopnotsupp = option eopnotsupp;
    eoverflow = option eoverflow;
    eownerdead = option eownerdead;
    eperm = option eperm;
    epfnosupport = option epfnosupport;
    epipe = option epipe;
    eproto = option eproto;
    eprotonosupport = option eprotonosupport;
    eprototype = option eprototype;
    erange = option erange;
    eremote = option eremote;
    erofs = option erofs;
    eshutdown = option eshutdown;
    esocktnosupport = option esocktnosupport;
    espipe = option espipe;
    esrch = option esrch;
    estale = option estale;
    etimedout = option etimedout;
    etoomanyrefs = option etoomanyrefs;
    etxtbsy = option etxtbsy;
    eusers = option eusers;
    ewouldblock = option ewouldblock;
    exdev = option exdev;
    echrng = platform (C.echrng ());
    el2nsync = platform (C.el2nsync ());
    el3hlt = platform (C.el3hlt ());
    el3rst = platform (C.el3rst ());
    elnrng = platform (C.elnrng ());
    eunatch = platform (C.eunatch ());
    enocsi = platform (C.enocsi ());
    el2hlt = platform (C.el2hlt ());
    ebade = platform (C.ebade ());
    ebadr = platform (C.ebadr ());
    exfull = platform (C.exfull ());
    enoano = platform (C.enoano ());
    ebadrqc = platform (C.ebadrqc ());
    ebadslt = platform (C.ebadslt ());
    ebfont = platform (C.ebfont ());
    enonet = platform (C.enonet ());
    enopkg = platform (C.enopkg ());
    eadv = platform (C.eadv ());
    esrmnt = platform (C.esrmnt ());
    ecomm = platform (C.ecomm ());
    edotdot = platform (C.edotdot ());
    enotuniq = platform (C.enotuniq ());
    ebadfd = platform (C.ebadfd ());
    eremchg = platform (C.eremchg ());
    elibacc = platform (C.elibacc ());
    elibbad = platform (C.elibbad ());
    elibscn = platform (C.elibscn ());
    elibmax = platform (C.elibmax ());
    elibexec = platform (C.elibexec ());
    erestart = platform (C.erestart ());
    estrpipe = platform (C.estrpipe ());
    euclean = platform (C.euclean ());
    enotnam = platform (C.enotnam ());
    enavail = platform (C.enavail ());
    eisnam = platform (C.eisnam ());
    eremoteio = platform (C.eremoteio ());
    enomedium = platform (C.enomedium ());
    emediumtype = platform (C.emediumtype ());
    enokey = platform (C.enokey ());
    ekeyexpired = platform (C.ekeyexpired ());
    ekeyrevoked = platform (C.ekeyrevoked ());
    ekeyrejected = platform (C.ekeyrejected ());
    erfkill = platform (C.erfkill ());
    ehwpoison = platform (C.ehwpoison ());
    epwroff = platform (C.epwroff ());
    edeverr = platform (C.edeverr ());
    ebadexec = platform (C.ebadexec ());
    ebadarch = platform (C.ebadarch ());
    eshlibvers = platform (C.eshlibvers ());
    ebadmacho = platform (C.ebadmacho ());
    enopolicy = platform (C.enopolicy ());
    eqfull = platform (C.eqfull ());
    edoofus = platform (C.edoofus ());
    enotcapable = platform (C.enotcapable ());
    ecapmode = platform (C.ecapmode ());
    eproclim = platform (C.eproclim ());
    ebadrpc = platform (C.ebadrpc ());
    erpcmismatch = platform (C.erpcmismatch ());
    eprogunavail = platform (C.eprogunavail ());
    eprogmismatch = platform (C.eprogmismatch ());
    eprocunavail = platform (C.eprocunavail ());
    eftype = platform (C.eftype ());
    eauth = platform (C.eauth ());
    eneedauth = platform (C.eneedauth ());
    enoattr = platform (C.enoattr ());
    enostr = platform (C.enostr ());
    enodata = platform (C.enodata ());
    etime = platform (C.etime ());
    enosr = platform (C.enosr ());
  })) in
  Errno.Host.of_defns defns

let optional_unknown ~host errno = match Errno.to_code ~host errno with
  | Some i -> Some (Unix.EUNKNOWNERR (Signed.SInt.to_int i))
  | None -> None

let to_unix ?(host=host) = Errno.(function
  | E2BIG -> Some Unix.E2BIG
  | EACCES -> Some Unix.EACCES
  | EADDRINUSE -> Some Unix.EADDRINUSE
  | EADDRNOTAVAIL -> Some Unix.EADDRNOTAVAIL
  | EAFNOSUPPORT -> Some Unix.EAFNOSUPPORT
  | EAGAIN -> Some Unix.EAGAIN
  | EALREADY -> Some Unix.EALREADY
  | EBADF -> Some Unix.EBADF
  | EBADMSG -> optional_unknown ~host EBADMSG
  | EBUSY -> Some Unix.EBUSY
  | ECANCELED -> optional_unknown ~host ECANCELED
  | ECHILD -> Some Unix.ECHILD
  | ECONNABORTED -> Some Unix.ECONNABORTED
  | ECONNREFUSED -> Some Unix.ECONNREFUSED
  | ECONNRESET -> Some Unix.ECONNRESET
  | EDEADLK -> Some Unix.EDEADLK
  | EDESTADDRREQ -> Some Unix.EDESTADDRREQ
  | EDOM -> Some Unix.EDOM
  | EDQUOT -> optional_unknown ~host EDQUOT
  | EEXIST -> Some Unix.EEXIST
  | EFAULT -> Some Unix.EFAULT
  | EFBIG -> Some Unix.EFBIG
  | EHOSTDOWN -> Some Unix.EHOSTDOWN
  | EHOSTUNREACH -> Some Unix.EHOSTUNREACH
  | EIDRM -> optional_unknown ~host EIDRM
  | EILSEQ -> optional_unknown ~host EILSEQ
  | EINPROGRESS -> Some Unix.EINPROGRESS
  | EINTR -> Some Unix.EINTR
  | EINVAL -> Some Unix.EINVAL
  | EIO -> Some Unix.EIO
  | EISCONN -> Some Unix.EISCONN
  | EISDIR -> Some Unix.EISDIR
  | ELOOP -> Some Unix.ELOOP
  | EMFILE -> Some Unix.EMFILE
  | EMLINK -> Some Unix.EMLINK
  | EMSGSIZE -> Some Unix.EMSGSIZE
  | EMULTIHOP -> optional_unknown ~host EMULTIHOP
  | ENAMETOOLONG -> Some Unix.ENAMETOOLONG
  | ENETDOWN -> Some Unix.ENETDOWN
  | ENETRESET -> Some Unix.ENETRESET
  | ENETUNREACH -> Some Unix.ENETUNREACH
  | ENFILE -> Some Unix.ENFILE
  | ENOBUFS -> Some Unix.ENOBUFS
  | ENODEV -> Some Unix.ENODEV
  | ENOENT -> Some Unix.ENOENT
  | ENOEXEC -> Some Unix.ENOEXEC
  | ENOLCK -> Some Unix.ENOLCK
  | ENOLINK -> optional_unknown ~host ENOLINK
  | ENOMEM -> Some Unix.ENOMEM
  | ENOMSG -> optional_unknown ~host ENOMSG
  | ENOPROTOOPT -> Some Unix.ENOPROTOOPT
  | ENOSPC -> Some Unix.ENOSPC
  | ENOSYS -> Some Unix.ENOSYS
  | ENOTBLK -> optional_unknown ~host ENOTBLK
  | ENOTCONN -> Some Unix.ENOTCONN
  | ENOTDIR -> Some Unix.ENOTDIR
  | ENOTEMPTY -> Some Unix.ENOTEMPTY
  | ENOTRECOVERABLE -> optional_unknown ~host ENOTRECOVERABLE
  | ENOTSOCK -> Some Unix.ENOTSOCK
  | ENOTSUP -> optional_unknown ~host ENOTSUP
  | ENOTTY -> Some Unix.ENOTTY
  | ENXIO -> Some Unix.ENXIO
  | EOPNOTSUPP -> Some Unix.EOPNOTSUPP
  | EOVERFLOW -> Some Unix.EOVERFLOW
  | EOWNERDEAD -> optional_unknown ~host EOWNERDEAD
  | EPERM -> Some Unix.EPERM
  | EPFNOSUPPORT -> Some Unix.EPFNOSUPPORT
  | EPIPE -> Some Unix.EPIPE
  | EPROTO -> optional_unknown ~host EPROTO
  | EPROTONOSUPPORT -> Some Unix.EPROTONOSUPPORT
  | EPROTOTYPE -> Some Unix.EPROTOTYPE
  | ERANGE -> Some Unix.ERANGE
  | EREMOTE -> optional_unknown ~host EREMOTE
  | EROFS -> Some Unix.EROFS
  | ESHUTDOWN -> Some Unix.ESHUTDOWN
  | ESOCKTNOSUPPORT -> Some Unix.ESOCKTNOSUPPORT
  | ESPIPE -> Some Unix.ESPIPE
  | ESRCH -> Some Unix.ESRCH
  | ESTALE -> optional_unknown ~host ESTALE
  | ETIMEDOUT -> Some Unix.ETIMEDOUT
  | ETOOMANYREFS -> Some Unix.ETOOMANYREFS
  | ETXTBSY -> optional_unknown ~host ETXTBSY
  | EUSERS -> optional_unknown ~host EUSERS
  | EWOULDBLOCK -> Some Unix.EWOULDBLOCK
  | EXDEV -> Some Unix.EXDEV
  | ECHRNG -> optional_unknown ~host ECHRNG
  | EL2NSYNC -> optional_unknown ~host EL2NSYNC
  | EL3HLT -> optional_unknown ~host EL3HLT
  | EL3RST -> optional_unknown ~host EL3RST
  | ELNRNG -> optional_unknown ~host ELNRNG
  | EUNATCH -> optional_unknown ~host EUNATCH
  | ENOCSI -> optional_unknown ~host ENOCSI
  | EL2HLT -> optional_unknown ~host EL2HLT
  | EBADE -> optional_unknown ~host EBADE
  | EBADR -> optional_unknown ~host EBADR
  | EXFULL -> optional_unknown ~host EXFULL
  | ENOANO -> optional_unknown ~host ENOANO
  | EBADRQC -> optional_unknown ~host EBADRQC
  | EBADSLT -> optional_unknown ~host EBADSLT
  | EBFONT -> optional_unknown ~host EBFONT
  | ENONET -> optional_unknown ~host ENONET
  | ENOPKG -> optional_unknown ~host ENOPKG
  | EADV -> optional_unknown ~host EADV
  | ESRMNT -> optional_unknown ~host ESRMNT
  | ECOMM -> optional_unknown ~host ECOMM
  | EDOTDOT -> optional_unknown ~host EDOTDOT
  | ENOTUNIQ -> optional_unknown ~host ENOTUNIQ
  | EBADFD -> optional_unknown ~host EBADFD
  | EREMCHG -> optional_unknown ~host EREMCHG
  | ELIBACC -> optional_unknown ~host ELIBACC
  | ELIBBAD -> optional_unknown ~host ELIBBAD
  | ELIBSCN -> optional_unknown ~host ELIBSCN
  | ELIBMAX -> optional_unknown ~host ELIBMAX
  | ELIBEXEC -> optional_unknown ~host ELIBEXEC
  | ERESTART -> optional_unknown ~host ERESTART
  | ESTRPIPE -> optional_unknown ~host ESTRPIPE
  | EUCLEAN -> optional_unknown ~host EUCLEAN
  | ENOTNAM -> optional_unknown ~host ENOTNAM
  | ENAVAIL -> optional_unknown ~host ENAVAIL
  | EISNAM -> optional_unknown ~host EISNAM
  | EREMOTEIO -> optional_unknown ~host EREMOTEIO
  | ENOMEDIUM -> optional_unknown ~host ENOMEDIUM
  | EMEDIUMTYPE -> optional_unknown ~host EMEDIUMTYPE
  | ENOKEY -> optional_unknown ~host ENOKEY
  | EKEYEXPIRED -> optional_unknown ~host EKEYEXPIRED
  | EKEYREVOKED -> optional_unknown ~host EKEYREVOKED
  | EKEYREJECTED -> optional_unknown ~host EKEYREJECTED
  | ERFKILL -> optional_unknown ~host ERFKILL
  | EHWPOISON -> optional_unknown ~host EHWPOISON
  | EPWROFF -> optional_unknown ~host EPWROFF
  | EDEVERR -> optional_unknown ~host EDEVERR
  | EBADEXEC -> optional_unknown ~host EBADEXEC
  | EBADARCH -> optional_unknown ~host EBADARCH
  | ESHLIBVERS -> optional_unknown ~host ESHLIBVERS
  | EBADMACHO -> optional_unknown ~host EBADMACHO
  | ENOPOLICY -> optional_unknown ~host ENOPOLICY
  | EQFULL -> optional_unknown ~host EQFULL
  | EDOOFUS -> optional_unknown ~host EDOOFUS
  | ENOTCAPABLE -> optional_unknown ~host ENOTCAPABLE
  | ECAPMODE -> optional_unknown ~host ECAPMODE
  | EPROCLIM -> optional_unknown ~host EPROCLIM
  | EBADRPC -> optional_unknown ~host EBADRPC
  | ERPCMISMATCH -> optional_unknown ~host ERPCMISMATCH
  | EPROGUNAVAIL -> optional_unknown ~host EPROGUNAVAIL
  | EPROGMISMATCH -> optional_unknown ~host EPROGMISMATCH
  | EPROCUNAVAIL -> optional_unknown ~host EPROCUNAVAIL
  | EFTYPE -> optional_unknown ~host EFTYPE
  | EAUTH -> optional_unknown ~host EAUTH
  | ENEEDAUTH -> optional_unknown ~host ENEEDAUTH
  | ENOATTR -> optional_unknown ~host ENOATTR
  | ENOSTR -> optional_unknown ~host ENOSTR
  | ENODATA -> optional_unknown ~host ENODATA
  | ETIME -> optional_unknown ~host ETIME
  | ENOSR -> optional_unknown ~host ENOSR
  | EUNKNOWNERR x -> Some (Unix.EUNKNOWNERR (Signed.SInt.to_int x))
)

let of_unix ?(host=host) = Unix.(function
  | E2BIG -> [Errno.E2BIG]
  | EACCES -> [Errno.EACCES]
  | EADDRINUSE -> [Errno.EADDRINUSE]
  | EADDRNOTAVAIL -> [Errno.EADDRNOTAVAIL]
  | EAFNOSUPPORT -> [Errno.EAFNOSUPPORT]
  | EAGAIN -> [Errno.EAGAIN]
  | EALREADY -> [Errno.EALREADY]
  | EBADF -> [Errno.EBADF]
  | EBUSY -> [Errno.EBUSY]
  | ECHILD -> [Errno.ECHILD]
  | ECONNABORTED -> [Errno.ECONNABORTED]
  | ECONNREFUSED -> [Errno.ECONNREFUSED]
  | ECONNRESET -> [Errno.ECONNRESET]
  | EDEADLK -> [Errno.EDEADLK]
  | EDESTADDRREQ -> [Errno.EDESTADDRREQ]
  | EDOM -> [Errno.EDOM]
  | EEXIST -> [Errno.EEXIST]
  | EFAULT -> [Errno.EFAULT]
  | EFBIG -> [Errno.EFBIG]
  | EHOSTDOWN -> [Errno.EHOSTDOWN]
  | EHOSTUNREACH -> [Errno.EHOSTUNREACH]
  | EINPROGRESS -> [Errno.EINPROGRESS]
  | EINTR -> [Errno.EINTR]
  | EINVAL -> [Errno.EINVAL]
  | EIO -> [Errno.EIO]
  | EISCONN -> [Errno.EISCONN]
  | EISDIR -> [Errno.EISDIR]
  | ELOOP -> [Errno.ELOOP]
  | EMFILE -> [Errno.EMFILE]
  | EMLINK -> [Errno.EMLINK]
  | EMSGSIZE -> [Errno.EMSGSIZE]
  | ENAMETOOLONG -> [Errno.ENAMETOOLONG]
  | ENETDOWN -> [Errno.ENETDOWN]
  | ENETRESET -> [Errno.ENETRESET]
  | ENETUNREACH -> [Errno.ENETUNREACH]
  | ENFILE -> [Errno.ENFILE]
  | ENOBUFS -> [Errno.ENOBUFS]
  | ENODEV -> [Errno.ENODEV]
  | ENOENT -> [Errno.ENOENT]
  | ENOEXEC -> [Errno.ENOEXEC]
  | ENOLCK -> [Errno.ENOLCK]
  | ENOMEM -> [Errno.ENOMEM]
  | ENOPROTOOPT -> [Errno.ENOPROTOOPT]
  | ENOSPC -> [Errno.ENOSPC]
  | ENOSYS -> [Errno.ENOSYS]
  | ENOTCONN -> [Errno.ENOTCONN]
  | ENOTDIR -> [Errno.ENOTDIR]
  | ENOTEMPTY -> [Errno.ENOTEMPTY]
  | ENOTSOCK -> [Errno.ENOTSOCK]
  | ENOTTY -> [Errno.ENOTTY]
  | ENXIO -> [Errno.ENXIO]
  | EOPNOTSUPP -> [Errno.EOPNOTSUPP]
  | EOVERFLOW -> [Errno.EOVERFLOW]
  | EPERM -> [Errno.EPERM]
  | EPFNOSUPPORT -> [Errno.EPFNOSUPPORT]
  | EPIPE -> [Errno.EPIPE]
  | EPROTONOSUPPORT -> [Errno.EPROTONOSUPPORT]
  | EPROTOTYPE -> [Errno.EPROTOTYPE]
  | ERANGE -> [Errno.ERANGE]
  | EROFS -> [Errno.EROFS]
  | ESHUTDOWN -> [Errno.ESHUTDOWN]
  | ESOCKTNOSUPPORT -> [Errno.ESOCKTNOSUPPORT]
  | ESPIPE -> [Errno.ESPIPE]
  | ESRCH -> [Errno.ESRCH]
  | ETIMEDOUT -> [Errno.ETIMEDOUT]
  | ETOOMANYREFS -> [Errno.ETOOMANYREFS]
  | EWOULDBLOCK -> [Errno.EWOULDBLOCK]
  | EXDEV -> [Errno.EXDEV]
  | EUNKNOWNERR x -> Errno.of_code ~host (Signed.SInt.of_int x)
)

let get_errno = C.get_errno

let reset_errno = C.reset_errno

let raise_errno ?(call="") ?(label="") code =
  raise Errno.(Error { errno = of_code ~host code; call; label; })

let raise_on_errno ?(call="") ?(label="") fn =
  reset_errno ();
  match fn () with
  | Some r -> r
  | None -> raise_errno ~call ~label (get_errno ())

let to_errno_exn = function
  | Unix.Unix_error (err, call, label) ->
    let errno = of_unix err in
    Errno.Error { Errno.errno; call; label }
  | exn -> exn

let with_errno_exn fn = try fn () with e -> raise (to_errno_exn e)

let rec unix_error_of_errno = function
  | [] -> None
  | err::rest -> match to_unix err with
    | Some err -> Some err
    | None -> unix_error_of_errno rest

let to_unix_exn = function
  | Errno.Error { Errno.errno; call; label } as e ->
    begin match unix_error_of_errno errno with
      | Some err -> Unix.Unix_error (err, call, label)
      | None -> e
    end
  | exn -> exn

let with_unix_exn fn = try fn () with e -> raise (to_unix_exn e)
