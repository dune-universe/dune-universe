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

open Ctypes

module C(F: Cstubs.FOREIGN) = struct

  let reset_errno = F.(foreign "unix_errno_reset" (void @-> returning void))

  let get_errno = F.(foreign "unix_errno_get" (void @-> returning sint))

  (* OS X and FreeBSD don't have these *)
  let echrng       = F.(foreign "unix_errno_echrng" (void @-> returning sint))
  let el2nsync     = F.(foreign "unix_errno_el2nsync" (void @-> returning sint))
  let el3hlt       = F.(foreign "unix_errno_el3hlt" (void @-> returning sint))
  let el3rst       = F.(foreign "unix_errno_el3rst" (void @-> returning sint))
  let elnrng       = F.(foreign "unix_errno_elnrng" (void @-> returning sint))
  let eunatch      = F.(foreign "unix_errno_eunatch" (void @-> returning sint))
  let enocsi       = F.(foreign "unix_errno_enocsi" (void @-> returning sint))
  let el2hlt       = F.(foreign "unix_errno_el2hlt" (void @-> returning sint))
  let ebade        = F.(foreign "unix_errno_ebade" (void @-> returning sint))
  let ebadr        = F.(foreign "unix_errno_ebadr" (void @-> returning sint))
  let exfull       = F.(foreign "unix_errno_exfull" (void @-> returning sint))
  let enoano       = F.(foreign "unix_errno_enoano" (void @-> returning sint))
  let ebadrqc      = F.(foreign "unix_errno_ebadrqc" (void @-> returning sint))
  let ebadslt      = F.(foreign "unix_errno_ebadslt" (void @-> returning sint))
  let ebfont       = F.(foreign "unix_errno_ebfont" (void @-> returning sint))
  let enonet       = F.(foreign "unix_errno_enonet" (void @-> returning sint))
  let enopkg       = F.(foreign "unix_errno_enopkg" (void @-> returning sint))
  let eadv         = F.(foreign "unix_errno_eadv" (void @-> returning sint))
  let esrmnt       = F.(foreign "unix_errno_esrmnt" (void @-> returning sint))
  let ecomm        = F.(foreign "unix_errno_ecomm" (void @-> returning sint))
  let edotdot      = F.(foreign "unix_errno_edotdot" (void @-> returning sint))
  let enotuniq     = F.(foreign "unix_errno_enotuniq" (void @-> returning sint))
  let ebadfd       = F.(foreign "unix_errno_ebadfd" (void @-> returning sint))
  let eremchg      = F.(foreign "unix_errno_eremchg" (void @-> returning sint))
  let elibacc      = F.(foreign "unix_errno_elibacc" (void @-> returning sint))
  let elibbad      = F.(foreign "unix_errno_elibbad" (void @-> returning sint))
  let elibscn      = F.(foreign "unix_errno_elibscn" (void @-> returning sint))
  let elibmax      = F.(foreign "unix_errno_elibmax" (void @-> returning sint))
  let elibexec     = F.(foreign "unix_errno_elibexec" (void @-> returning sint))
  let erestart     = F.(foreign "unix_errno_erestart" (void @-> returning sint))
  let estrpipe     = F.(foreign "unix_errno_estrpipe" (void @-> returning sint))
  let euclean      = F.(foreign "unix_errno_euclean" (void @-> returning sint))
  let enotnam      = F.(foreign "unix_errno_enotnam" (void @-> returning sint))
  let enavail      = F.(foreign "unix_errno_enavail" (void @-> returning sint))
  let eisnam       = F.(foreign "unix_errno_eisnam" (void @-> returning sint))
  let eremoteio    = F.(foreign "unix_errno_eremoteio" (void @-> returning sint))
  let enomedium    = F.(foreign "unix_errno_enomedium" (void @-> returning sint))
  let emediumtype  = F.(foreign "unix_errno_emediumtype" (void @-> returning sint))
  let enokey       = F.(foreign "unix_errno_enokey" (void @-> returning sint))
  let ekeyexpired  = F.(foreign "unix_errno_ekeyexpired" (void @-> returning sint))
  let ekeyrevoked  = F.(foreign "unix_errno_ekeyrevoked" (void @-> returning sint))
  let ekeyrejected = F.(foreign "unix_errno_ekeyrejected" (void @-> returning sint))
  let erfkill      = F.(foreign "unix_errno_erfkill" (void @-> returning sint))
  let ehwpoison    = F.(foreign "unix_errno_ehwpoison" (void @-> returning sint))

  (* Linux and FreeBSD don't have these *)
  let epwroff    = F.(foreign "unix_errno_epwroff" (void @-> returning sint))
  let edeverr    = F.(foreign "unix_errno_edeverr" (void @-> returning sint))
  let ebadexec   = F.(foreign "unix_errno_ebadexec" (void @-> returning sint))
  let ebadarch   = F.(foreign "unix_errno_ebadarch" (void @-> returning sint))
  let eshlibvers = F.(foreign "unix_errno_eshlibvers" (void @-> returning sint))
  let ebadmacho  = F.(foreign "unix_errno_ebadmacho" (void @-> returning sint))
  let enopolicy  = F.(foreign "unix_errno_enopolicy" (void @-> returning sint))
  let eqfull     = F.(foreign "unix_errno_eqfull" (void @-> returning sint))

  (* Linux and OS X don't have these *)
  let edoofus     = F.(foreign "unix_errno_edoofus" (void @-> returning sint))
  let enotcapable = F.(foreign "unix_errno_enotcapable" (void @-> returning sint))
  let ecapmode    = F.(foreign "unix_errno_ecapmode" (void @-> returning sint))

  (* Linux doesn't have these *)
  let eproclim      = F.(foreign "unix_errno_eproclim" (void @-> returning sint))
  let ebadrpc       = F.(foreign "unix_errno_ebadrpc" (void @-> returning sint))
  let erpcmismatch  = F.(foreign "unix_errno_erpcmismatch" (void @-> returning sint))
  let eprogunavail  = F.(foreign "unix_errno_eprogunavail" (void @-> returning sint))
  let eprogmismatch = F.(foreign "unix_errno_eprogmismatch" (void @-> returning sint))
  let eprocunavail  = F.(foreign "unix_errno_eprocunavail" (void @-> returning sint))
  let eftype        = F.(foreign "unix_errno_eftype" (void @-> returning sint))
  let eauth         = F.(foreign "unix_errno_eauth" (void @-> returning sint))
  let eneedauth     = F.(foreign "unix_errno_eneedauth" (void @-> returning sint))
  let enoattr       = F.(foreign "unix_errno_enoattr" (void @-> returning sint))

  (* FreeBSD doesn't have these *)
  let enostr = F.(foreign "unix_errno_enostr" (void @-> returning sint))
  let enodata = F.(foreign "unix_errno_enodata" (void @-> returning sint))
  let etime = F.(foreign "unix_errno_etime" (void @-> returning sint))
  let enosr = F.(foreign "unix_errno_enosr" (void @-> returning sint))

end
