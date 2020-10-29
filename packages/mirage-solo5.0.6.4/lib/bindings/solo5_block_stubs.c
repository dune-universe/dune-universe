/* Copyright (c) 2015, IBM
 * Author(s): Dan Williams <djwillia@us.ibm.com>
 * Copyright 2019 Martin Lucina <martin@lucina.net>
 *
 * Permission to use, copy, modify, and/or distribute this software
 * for any purpose with or without fee is hereby granted, provided
 * that the above copyright notice and this permission notice appear
 * in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
 * WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
 * AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
 * OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include "solo5.h"

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/bigarray.h>

CAMLprim value
mirage_solo5_block_acquire(value v_name)
{
    CAMLparam1(v_name);
    CAMLlocal2(v_info, v_result);
    solo5_result_t result;
    solo5_handle_t handle;
    struct solo5_block_info bi;

    result = solo5_block_acquire(String_val(v_name), &handle, &bi);
    v_info = caml_alloc(2, 0);
    if (result != SOLO5_R_OK) {
        /*
         * On error (*bi) is not valid, so fake an empty structure to return.
         */
        Store_field(v_info, 0, caml_copy_int64(0));
        Store_field(v_info, 1, caml_copy_int64(0));
    }
    else {
        Store_field(v_info, 0, caml_copy_int64(bi.capacity));
        Store_field(v_info, 1, caml_copy_int64(bi.block_size));
    }

    v_result = caml_alloc_tuple(3);
    Store_field(v_result, 0, Val_int(result));
    Store_field(v_result, 1, caml_copy_int64(handle));
    Store_field(v_result, 2, v_info);
    CAMLreturn(v_result);
}

CAMLprim value
mirage_solo5_block_read_3(value v_handle, value v_offset, value v_buf,
        value v_buf_offset, value v_size)
{
    CAMLparam5(v_handle, v_offset, v_buf, v_buf_offset, v_size);
    solo5_handle_t handle = Int64_val(v_handle);
    solo5_off_t offset = Int64_val(v_offset);
    long buf_offset = Long_val(v_buf_offset);
    uint8_t *buf = (uint8_t *)Caml_ba_data_val(v_buf) + buf_offset;
    size_t size = Long_val(v_size);
    solo5_result_t result;

    result = solo5_block_read(handle, offset, buf, size);
    CAMLreturn(Val_int(result));
}

CAMLprim value
mirage_solo5_block_write_3(value v_handle, value v_offset, value v_buf,
        value v_buf_offset, value v_size)
{
    CAMLparam5(v_handle, v_offset, v_buf, v_buf_offset, v_size);
    solo5_handle_t handle = Int64_val(v_handle);
    solo5_off_t offset = Int64_val(v_offset);
    long buf_offset = Long_val(v_buf_offset);
    const uint8_t *buf = (uint8_t *)Caml_ba_data_val(v_buf) + buf_offset;
    size_t size = Long_val(v_size);
    solo5_result_t result;

    result = solo5_block_write(handle, offset, buf, size);
    CAMLreturn(Val_int(result));
}
