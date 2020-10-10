#include "internal.h"

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <dirent.h>
#include <stdio.h>
#include <assert.h>

static struct bm_renderer renderer;

bool
bm_renderer_activate(struct bm_renderer *renderer, struct bm_menu *menu)
{
    assert(renderer);

    menu->renderer = renderer;

    if (!renderer->api.constructor(menu))
        goto fail;

    return true;

fail:
    menu->renderer = NULL;
    return false;
}

bool
bm_init(void)
{
    register_renderer(&renderer.api);
}

struct bm_renderer*
bm_get_renderer(void)
{
    return &renderer;
}

const char*
bm_version(void)
{
    return BM_VERSION;
}

/* vim: set ts=8 sw=4 tw=0 :*/
