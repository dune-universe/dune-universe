/*
 * Copyright (c) 2010-2011 Anil Madhavapeddy <anil@recoil.org>
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
 */

#include <stdint.h>
#include <assert.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/bigarray.h>

#include "console.h"
#include "xenstore.h"
#include "barrier.h"

typedef unsigned int RING_IDX; /* from ring.h */

extern void *memset(void *s, int c, size_t n);

#define PAGE_SIZE 4096

/* Raw ring operations
   These have no request/response structs, just byte strings
 */

#define DEFINE_RAW_RING_OPS(xname,xtype,xin,xout) \
CAMLprim value \
caml_##xname##_ring_init(value v_ptr) \
{ \
   memset((void *)Caml_ba_data_val(v_ptr), 0, PAGE_SIZE);	\
   return Val_unit; \
} \
CAMLprim value \
caml_##xname##_ring_write(value v_ptr, value v_str, value v_len) \
{ \
   struct xtype *intf = (struct xtype *)Caml_ba_data_val(v_ptr);	\
   int sent = 0, len = Int_val(v_len); \
   char *data = String_val(v_str); \
   XENCONS_RING_IDX cons, prod; \
   cons = intf->xout##_cons; \
   prod = intf->xout##_prod; \
   xen_mb(); \
   /* BUG_ON((prod - cons) > sizeof(intf->xout));*/				\
   while ((sent < len) && ((prod - cons) < sizeof(intf->xout))) \
     intf->xout[MASK_XENCONS_IDX(prod++, intf->xout)] = data[sent++]; \
   xen_wmb(); \
   intf->xout##_prod = prod; \
   return Val_int(sent); \
} \
CAMLprim value \
caml_##xname##_ring_read(value v_ptr, value v_str, value v_len) \
{ \
   struct xtype *intf = (struct xtype *)Caml_ba_data_val(v_ptr);	\
   int pos=0, len = Int_val(v_len); \
   char *data = String_val(v_str); \
   XENCONS_RING_IDX cons, prod; \
   cons = intf->xin##_cons; \
   prod = intf->xin##_prod; \
   xen_mb(); \
   /* BUG_ON((prod - cons) > sizeof(intf->xin));*/	\
   while (cons != prod && pos < len) \
     data[pos++] = intf->xin[MASK_XENCONS_IDX(cons++, intf->xin)]; \
   xen_mb(); \
   intf->xin##_cons = cons; \
   return Val_int(pos); \
}

DEFINE_RAW_RING_OPS(console,xencons_interface,in,out);
DEFINE_RAW_RING_OPS(console_back,xencons_interface,out,in);
DEFINE_RAW_RING_OPS(xenstore,xenstore_domain_interface,rsp,req);
DEFINE_RAW_RING_OPS(xenstore_back,xenstore_domain_interface,req,rsp);
