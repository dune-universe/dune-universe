void unix_errno_reset();
int unix_errno_get();

// linux

int unix_errno_echrng();
int unix_errno_el2nsync();
int unix_errno_el3hlt();
int unix_errno_el3rst();
int unix_errno_elnrng();
int unix_errno_eunatch();
int unix_errno_enocsi();
int unix_errno_el2hlt();
int unix_errno_ebade();
int unix_errno_ebadr();
int unix_errno_exfull();
int unix_errno_enoano();
int unix_errno_ebadrqc();
int unix_errno_ebadslt();
int unix_errno_ebfont();
int unix_errno_enonet();
int unix_errno_enopkg();
int unix_errno_eadv();
int unix_errno_esrmnt();
int unix_errno_ecomm();
int unix_errno_edotdot();
int unix_errno_enotuniq();
int unix_errno_ebadfd();
int unix_errno_eremchg();
int unix_errno_elibacc();
int unix_errno_elibbad();
int unix_errno_elibscn();
int unix_errno_elibmax();
int unix_errno_elibexec();
int unix_errno_erestart();
int unix_errno_estrpipe();
int unix_errno_euclean();
int unix_errno_enotnam();
int unix_errno_enavail();
int unix_errno_eisnam();
int unix_errno_eremoteio();
int unix_errno_enomedium();
int unix_errno_emediumtype();
int unix_errno_enokey();
int unix_errno_ekeyexpired();
int unix_errno_ekeyrevoked();
int unix_errno_ekeyrejected();
int unix_errno_erfkill();
int unix_errno_ehwpoison();

// osx

int unix_errno_epwroff();
int unix_errno_edeverr();
int unix_errno_ebadexec();
int unix_errno_ebadarch();
int unix_errno_eshlibvers();
int unix_errno_ebadmacho();
int unix_errno_enopolicy();
int unix_errno_eqfull();

// freebsd

int unix_errno_edoofus();
int unix_errno_enotcapable();
int unix_errno_ecapmode();

// osx + freebsd

int unix_errno_eproclim();
int unix_errno_ebadrpc();
int unix_errno_erpcmismatch();
int unix_errno_eprogunavail();
int unix_errno_eprogmismatch();
int unix_errno_eprocunavail();
int unix_errno_eftype();
int unix_errno_eauth();
int unix_errno_eneedauth();
int unix_errno_enoattr();

// osx + linux

int unix_errno_enostr();
int unix_errno_enodata();
int unix_errno_etime();
int unix_errno_enosr();
