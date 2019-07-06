#define _GNU_SOURCE

#include <dlfcn.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/ioctl.h>

#define DYLD_INTERPOSE(_replacment,_replacee) \
     __attribute__((used)) static struct{ const void* replacment; const void* replacee; } _interpose_##_replacee \
                 __attribute__ ((section ("__DATA,__interpose"))) = { (const void*)(unsigned long)&_replacment, (const void*)(unsigned long)&_replacee };


int interposed_ioctl(int fd, unsigned long request, ...)
{
	va_list ap;
	va_start(ap, request);
	char *argp = va_arg(ap, char *);
	int ret = 0;
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
		ret = ioctl(fd, request, argp);
	}
	va_end(ap);
	return ret;
}

DYLD_INTERPOSE(interposed_ioctl, ioctl);
