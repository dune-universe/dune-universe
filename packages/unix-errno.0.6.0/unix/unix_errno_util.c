#ifndef __FreeBSD__
#  define _GNU_SOURCE
#  define _POSIX_C_SOURCE 200809L
#  define _DARWIN_C_SOURCE
#endif

#include <errno.h>

#define FORCE_CHECK 0

#define LINUX defined(__linux__)
#define APPLE defined(__APPLE__)
#define FREEBSD defined(__FreeBSD__)

void unix_errno_reset() { errno = 0; }

int unix_errno_get() { return errno; }

// NOT OS X or FreeBSD

int unix_errno_echrng() {
#define ECHRNG_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && ECHRNG_UNAVAILABLE && !defined(ECHRNG)
  return -1;
#else
  return ECHRNG;
#endif
}

int unix_errno_el2nsync() {
#define EL2NSYNC_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && EL2NSYNC_UNAVAILABLE && !defined(EL2NSYNC)
  return -1;
#else
  return EL2NSYNC;
#endif
}

int unix_errno_el3hlt() {
#define EL3HLT_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && EL3HLT_UNAVAILABLE && !defined(EL3HLT)
  return -1;
#else
  return EL3HLT;
#endif
}

int unix_errno_el3rst() {
#define EL3RST_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && EL3RST_UNAVAILABLE && !defined(EL3RST)
  return -1;
#else
  return EL3RST;
#endif
}

int unix_errno_elnrng() {
#define ELNRNG_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && ELNRNG_UNAVAILABLE && !defined(ELNRNG)
  return -1;
#else
  return ELNRNG;
#endif
}

int unix_errno_eunatch() {
#define EUNATCH_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && EUNATCH_UNAVAILABLE && !defined(EUNATCH)
  return -1;
#else
  return EUNATCH;
#endif
}

int unix_errno_enocsi() {
#define ENOCSI_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && ENOCSI_UNAVAILABLE && !defined(ENOCSI)
  return -1;
#else
  return ENOCSI;
#endif
}

int unix_errno_el2hlt() {
#define EL2HLT_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && EL2HLT_UNAVAILABLE && !defined(EL2HLT)
  return -1;
#else
  return EL2HLT;
#endif
}

int unix_errno_ebade() {
#define EBADE_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && EBADE_UNAVAILABLE && !defined(EBADE)
  return -1;
#else
  return EBADE;
#endif
}

int unix_errno_ebadr() {
#define EBADR_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && EBADR_UNAVAILABLE && !defined(EBADR)
  return -1;
#else
  return EBADR;
#endif
}

int unix_errno_exfull() {
#define EXFULL_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && EXFULL_UNAVAILABLE && !defined(EXFULL)
  return -1;
#else
  return EXFULL;
#endif
}

int unix_errno_enoano() {
#define ENOANO_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && ENOANO_UNAVAILABLE && !defined(ENOANO)
  return -1;
#else
  return ENOANO;
#endif
}

int unix_errno_ebadrqc() {
#define EBADRQC_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && EBADRQC_UNAVAILABLE && !defined(EBADRQC)
  return -1;
#else
  return EBADRQC;
#endif
}

int unix_errno_ebadslt() {
#define EBADSLT_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && EBADSLT_UNAVAILABLE && !defined(EBADSLT)
  return -1;
#else
  return EBADSLT;
#endif
}

int unix_errno_ebfont() {
#define EBFONT_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && EBFONT_UNAVAILABLE && !defined(EBFONT)
  return -1;
#else
  return EBFONT;
#endif
}

int unix_errno_enonet() {
#define ENONET_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && ENONET_UNAVAILABLE && !defined(ENONET)
  return -1;
#else
  return ENONET;
#endif
}

int unix_errno_enopkg() {
#define ENOPKG_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && ENOPKG_UNAVAILABLE && !defined(ENOPKG)
  return -1;
#else
  return ENOPKG;
#endif
}

int unix_errno_eadv() {
#define EADV_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && EADV_UNAVAILABLE && !defined(EADV)
  return -1;
#else
  return EADV;
#endif
}

int unix_errno_esrmnt() {
#define ESRMNT_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && ESRMNT_UNAVAILABLE && !defined(ESRMNT)
  return -1;
#else
  return ESRMNT;
#endif
}

int unix_errno_ecomm() {
#define ECOMM_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && ECOMM_UNAVAILABLE && !defined(ECOMM)
  return -1;
#else
  return ECOMM;
#endif
}

int unix_errno_edotdot() {
#define EDOTDOT_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && EDOTDOT_UNAVAILABLE && !defined(EDOTDOT)
  return -1;
#else
  return EDOTDOT;
#endif
}

int unix_errno_enotuniq() {
#define ENOTUNIQ_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && ENOTUNIQ_UNAVAILABLE && !defined(ENOTUNIQ)
  return -1;
#else
  return ENOTUNIQ;
#endif
}

int unix_errno_ebadfd() {
#define EBADFD_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && EBADFD_UNAVAILABLE && !defined(EBADFD)
  return -1;
#else
  return EBADFD;
#endif
}

int unix_errno_eremchg() {
#define EREMCHG_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && EREMCHG_UNAVAILABLE && !defined(EREMCHG)
  return -1;
#else
  return EREMCHG;
#endif
}

int unix_errno_elibacc() {
#define ELIBACC_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && ELIBACC_UNAVAILABLE && !defined(ELIBACC)
  return -1;
#else
  return ELIBACC;
#endif
}

int unix_errno_elibbad() {
#define ELIBBAD_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && ELIBBAD_UNAVAILABLE && !defined(ELIBBAD)
  return -1;
#else
  return ELIBBAD;
#endif
}

int unix_errno_elibscn() {
#define ELIBSCN_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && ELIBSCN_UNAVAILABLE && !defined(ELIBSCN)
  return -1;
#else
  return ELIBSCN;
#endif
}

int unix_errno_elibmax() {
#define ELIBMAX_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && ELIBMAX_UNAVAILABLE && !defined(ELIBMAX)
  return -1;
#else
  return ELIBMAX;
#endif
}

int unix_errno_elibexec() {
#define ELIBEXEC_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && ELIBEXEC_UNAVAILABLE && !defined(ELIBEXEC)
  return -1;
#else
  return ELIBEXEC;
#endif
}

int unix_errno_erestart() {
#define ERESTART_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && ERESTART_UNAVAILABLE && !defined(ERESTART)
  return -1;
#else
  return ERESTART;
#endif
}

int unix_errno_estrpipe() {
#define ESTRPIPE_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && ESTRPIPE_UNAVAILABLE && !defined(ESTRPIPE)
  return -1;
#else
  return ESTRPIPE;
#endif
}

int unix_errno_euclean() {
#define EUCLEAN_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && EUCLEAN_UNAVAILABLE && !defined(EUCLEAN)
  return -1;
#else
  return EUCLEAN;
#endif
}

int unix_errno_enotnam() {
#define ENOTNAM_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && ENOTNAM_UNAVAILABLE && !defined(ENOTNAM)
  return -1;
#else
  return ENOTNAM;
#endif
}

int unix_errno_enavail() {
#define ENAVAIL_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && ENAVAIL_UNAVAILABLE && !defined(ENAVAIL)
  return -1;
#else
  return ENAVAIL;
#endif
}

int unix_errno_eisnam() {
#define EISNAM_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && EISNAM_UNAVAILABLE && !defined(EISNAM)
  return -1;
#else
  return EISNAM;
#endif
}

int unix_errno_eremoteio() {
#define EREMOTEIO_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && EREMOTEIO_UNAVAILABLE && !defined(EREMOTEIO)
  return -1;
#else
  return EREMOTEIO;
#endif
}

int unix_errno_enomedium() {
#define ENOMEDIUM_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && ENOMEDIUM_UNAVAILABLE && !defined(ENOMEDIUM)
  return -1;
#else
  return ENOMEDIUM;
#endif
}

int unix_errno_emediumtype() {
#define EMEDIUMTYPE_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && EMEDIUMTYPE_UNAVAILABLE && !defined(EMEDIUMTYPE)
  return -1;
#else
  return EMEDIUMTYPE;
#endif
}

int unix_errno_enokey() {
#define ENOKEY_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && ENOKEY_UNAVAILABLE && !defined(ENOKEY)
  return -1;
#else
  return ENOKEY;
#endif
}

int unix_errno_ekeyexpired() {
#define EKEYEXPIRED_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && EKEYEXPIRED_UNAVAILABLE && !defined(EKEYEXPIRED)
  return -1;
#else
  return EKEYEXPIRED;
#endif
}

int unix_errno_ekeyrevoked() {
#define EKEYREVOKED_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && EKEYREVOKED_UNAVAILABLE && !defined(EKEYREVOKED)
  return -1;
#else
  return EKEYREVOKED;
#endif
}

int unix_errno_ekeyrejected() {
#define EKEYREJECTED_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && EKEYREJECTED_UNAVAILABLE && !defined(EKEYREJECTED)
  return -1;
#else
  return EKEYREJECTED;
#endif
}

int unix_errno_erfkill() {
#define ERFKILL_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && ERFKILL_UNAVAILABLE && !defined(ERFKILL)
  return -1;
#else
  return ERFKILL;
#endif
}

int unix_errno_ehwpoison() {
#define EHWPOISON_UNAVAILABLE (APPLE || FREEBSD)
#if !FORCE_CHECK && EHWPOISON_UNAVAILABLE && !defined(EHWPOISON)
  return -1;
#else
  return EHWPOISON;
#endif
}

// NOT FreeBSD or Linux

int unix_errno_epwroff() {
#define EPWROFF_UNAVAILABLE (LINUX || FREEBSD)
#if !FORCE_CHECK && EPWROFF_UNAVAILABLE && !defined(EPWROFF)
  return -1;
#else
  return EPWROFF;
#endif
}

int unix_errno_edeverr() {
#define EDEVERR_UNAVAILABLE (LINUX || FREEBSD)
#if !FORCE_CHECK && EDEVERR_UNAVAILABLE && !defined(EDEVERR)
  return -1;
#else
  return EDEVERR;
#endif
}

int unix_errno_ebadexec() {
#define EBADEXEC_UNAVAILABLE (LINUX || FREEBSD)
#if !FORCE_CHECK && EBADEXEC_UNAVAILABLE && !defined(EBADEXEC)
  return -1;
#else
  return EBADEXEC;
#endif
}

int unix_errno_ebadarch() {
#define EBADARCH_UNAVAILABLE (LINUX || FREEBSD)
#if !FORCE_CHECK && EBADARCH_UNAVAILABLE && !defined(EBADARCH)
  return -1;
#else
  return EBADARCH;
#endif
}

int unix_errno_eshlibvers() {
#define ESHLIBVERS_UNAVAILABLE (LINUX || FREEBSD)
#if !FORCE_CHECK && ESHLIBVERS_UNAVAILABLE && !defined(ESHLIBVERS)
  return -1;
#else
  return ESHLIBVERS;
#endif
}

int unix_errno_ebadmacho() {
#define EBADMACHO_UNAVAILABLE (LINUX || FREEBSD)
#if !FORCE_CHECK && EBADMACHO_UNAVAILABLE && !defined(EBADMACHO)
  return -1;
#else
  return EBADMACHO;
#endif
}

int unix_errno_enopolicy() {
#define ENOPOLICY_UNAVAILABLE (LINUX || FREEBSD)
#if !FORCE_CHECK && ENOPOLICY_UNAVAILABLE && !defined(ENOPOLICY)
  return -1;
#else
  return ENOPOLICY;
#endif
}

int unix_errno_eqfull() {
#define EQFULL_UNAVAILABLE (LINUX || FREEBSD)
#if !FORCE_CHECK && EQFULL_UNAVAILABLE && !defined(EQFULL)
  return -1;
#else
  return EQFULL;
#endif
}

// NOT OS X or Linux

int unix_errno_edoofus() {
#define EDOOFUS_UNAVAILABLE (LINUX || APPLE)
#if !FORCE_CHECK && EDOOFUS_UNAVAILABLE && !defined(EDOOFUS)
  return -1;
#else
  return EDOOFUS;
#endif
}

int unix_errno_enotcapable() {
#define ENOTCAPABLE_UNAVAILABLE (LINUX || APPLE)
#if !FORCE_CHECK && ENOTCAPABLE_UNAVAILABLE && !defined(ENOTCAPABLE)
  return -1;
#else
  return ENOTCAPABLE;
#endif
}

int unix_errno_ecapmode() {
#define ECAPMODE_UNAVAILABLE (LINUX || APPLE)
#if !FORCE_CHECK && ECAPMODE_UNAVAILABLE && !defined(ECAPMODE)
  return -1;
#else
  return ECAPMODE;
#endif
}

// NOT Linux

int unix_errno_eproclim() {
#define EPROCLIM_UNAVAILABLE (LINUX)
#if !FORCE_CHECK && EPROCLIM_UNAVAILABLE && !defined(EPROCLIM)
  return -1;
#else
  return EPROCLIM;
#endif
}

int unix_errno_ebadrpc() {
#define EBADRPC_UNAVAILABLE (LINUX)
#if !FORCE_CHECK && EBADRPC_UNAVAILABLE && !defined(EBADRPC)
  return -1;
#else
  return EBADRPC;
#endif
}

int unix_errno_erpcmismatch() {
#define ERPCMISMATCH_UNAVAILABLE (LINUX)
#if !FORCE_CHECK && ERPCMISMATCH_UNAVAILABLE && !defined(ERPCMISMATCH)
  return -1;
#else
  return ERPCMISMATCH;
#endif
}

int unix_errno_eprogunavail() {
#define EPROGUNAVAIL_UNAVAILABLE (LINUX)
#if !FORCE_CHECK && EPROGUNAVAIL_UNAVAILABLE && !defined(EPROGUNAVAIL)
  return -1;
#else
  return EPROGUNAVAIL;
#endif
}

int unix_errno_eprogmismatch() {
#define EPROGMISMATCH_UNAVAILABLE (LINUX)
#if !FORCE_CHECK && EPROGMISMATCH_UNAVAILABLE && !defined(EPROGMISMATCH)
  return -1;
#else
  return EPROGMISMATCH;
#endif
}

int unix_errno_eprocunavail() {
#define EPROCUNAVAIL_UNAVAILABLE (LINUX)
#if !FORCE_CHECK && EPROCUNAVAIL_UNAVAILABLE && !defined(EPROCUNAVAIL)
  return -1;
#else
  return EPROCUNAVAIL;
#endif
}

int unix_errno_eftype() {
#define EFTYPE_UNAVAILABLE (LINUX)
#if !FORCE_CHECK && EFTYPE_UNAVAILABLE && !defined(EFTYPE)
  return -1;
#else
  return EFTYPE;
#endif
}

int unix_errno_eauth() {
#define EAUTH_UNAVAILABLE (LINUX)
#if !FORCE_CHECK && EAUTH_UNAVAILABLE && !defined(EAUTH)
  return -1;
#else
  return EAUTH;
#endif
}

int unix_errno_eneedauth() {
#define ENEEDAUTH_UNAVAILABLE (LINUX)
#if !FORCE_CHECK && ENEEDAUTH_UNAVAILABLE && !defined(ENEEDAUTH)
  return -1;
#else
  return ENEEDAUTH;
#endif
}

int unix_errno_enoattr() {
#define ENOATTR_UNAVAILABLE (LINUX)
#if !FORCE_CHECK && ENOATTR_UNAVAILABLE && !defined(ENOATTR)
  return -1;
#else
  return ENOATTR;
#endif
}

// NOT FreeBSD

int unix_errno_enostr() {
#define ENOSTR_UNAVAILABLE (FREEBSD)
#if !FORCE_CHECK && ENOSTR_UNAVAILABLE && !defined(ENOSTR)
  return -1;
#else
  return ENOSTR;
#endif
}

int unix_errno_enodata() {
#define ENODATA_UNAVAILABLE (FREEBSD)
#if !FORCE_CHECK && ENODATA_UNAVAILABLE && !defined(ENODATA)
  return -1;
#else
  return ENODATA;
#endif
}

int unix_errno_etime() {
#define ETIME_UNAVAILABLE (FREEBSD)
#if !FORCE_CHECK && ETIME_UNAVAILABLE && !defined(ETIME)
  return -1;
#else
  return ETIME;
#endif
}

int unix_errno_enosr() {
#define ENOSR_UNAVAILABLE (FREEBSD)
#if !FORCE_CHECK && ENOSR_UNAVAILABLE && !defined(ENOSR)
  return -1;
#else
  return ENOSR;
#endif
}
