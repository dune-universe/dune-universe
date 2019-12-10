#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <sys/ioctl.h>
#include <unistd.h>

CAMLprim value ocaml_term_width(value unit) {
	CAMLparam1(unit);
	struct winsize ws;
	int z = ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws);
  int width = 0;
	if(z == 0) {
    width = ws.ws_col;
	}
	CAMLreturn (Val_int(width));
}
