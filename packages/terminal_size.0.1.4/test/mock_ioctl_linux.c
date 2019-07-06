#define _GNU_SOURCE

#include <dlfcn.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/ioctl.h>

/* Work around differing ioctl signatures between muscl (alpine), */
/* glibc (debian) and libsystem (mac)                             */
#if defined(__GLIBC__) || defined(__APPLE__)
typedef unsigned long request_t;
#else
typedef int request_t;
#endif

typedef int (*ioctl_t)(int, request_t, char*);

static ioctl_t real_ioctl = NULL;

int ioctl(int fd, request_t request, ...)
{
	va_list ap;
	va_start(ap, request);
	char *argp = va_arg(ap, char *);
	int ret = 0;
	if (real_ioctl == NULL) {
		real_ioctl = (ioctl_t) dlsym(RTLD_NEXT, "ioctl");
	}
	if (request == TIOCGWINSZ) {
		char* p_rows = getenv("FAKE_ROWS");
		char* p_cols = getenv("FAKE_COLS");
		if (*p_rows == '\0' || *p_cols == '\0') {
			ret = -1;
		} else {
			struct winsize *pws = (struct winsize *) argp;
			pws->ws_row = atoi(p_rows);
			pws->ws_col = atoi(p_cols);
			ret = 0;
		}
	} else {
		ret = real_ioctl(fd, request, argp);
	}
	va_end(ap);
	return ret;
}
