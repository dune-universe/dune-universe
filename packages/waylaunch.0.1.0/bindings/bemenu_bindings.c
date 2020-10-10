#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include "bemenu/common.h"

static struct client client = {
    .filter_mode = BM_FILTER_MODE_DMENU,
    .title = "waylaunch",
};

static const char* result = "";

static void item_cb(const struct client *client, struct bm_item *item) {
    (void)client;
    result = bm_item_get_text(item);
}

int argc = 3;
char* argv_aux[3] = {
    "waylaunch",
    "-l",
    "5",
};
char** argv = (char**)argv_aux;

value bmenu_binding(value l) {
    CAMLparam1(l);
    CAMLlocal1(r);

    bm_init();

    parse_args(&client, &argc, &argv);

    struct bm_menu *menu = menu_with_options(&client);

    while (l != Val_emptylist) {
        struct bm_item *item = bm_item_new(String_val(Field(l, 0)));
        bm_menu_add_item(menu, item);
        l = Field(l, 1);
    }

    run_menu(&client, menu, item_cb);
    r = caml_copy_string(result);
    bm_menu_free(menu);

    CAMLreturn (r);
}
