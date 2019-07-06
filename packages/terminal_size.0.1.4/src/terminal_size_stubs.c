#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <sys/ioctl.h>
#include <unistd.h>

CAMLprim value ocaml_terminal_size_get(value unit) {
	CAMLparam1(unit);
	CAMLlocal2(result, pair);
	struct winsize ws;
	int z = ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws);
	if(z == 0) {
		result = caml_alloc(1, 0);
		pair = caml_alloc(2, 0);
		Store_field(result, 0, pair);
		Store_field(pair, 0, Val_int(ws.ws_row));
		Store_field(pair, 1, Val_int(ws.ws_col));
	} else {
		result = Val_int(0);
	}
	CAMLreturn (result);
}
