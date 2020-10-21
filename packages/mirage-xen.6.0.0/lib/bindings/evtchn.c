/*
 * Copyright (c) 2020 Martin Lucina <martin@lucina.net>
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

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include "solo5.h"
#include "bindings.h"
#include "hypercall.h"
#include "xen/xen.h"
#include "xen/event_channel.h"

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#undef EVTCHN_DEBUG
#ifdef EVTCHN_DEBUG
#include <stdio.h>
#define DPRINTF(level, format, ...) \
    if (EVTCHN_DEBUG >= level) printf("%s(): " format, __func__, __VA_ARGS__)
#else
#define DPRINTF(level, format, ...)
#endif

/*
 * Find first bit set in (word). Undefined if (word) is zero.
 */
static inline unsigned long ffs(unsigned long word)
{
    return __builtin_ffsl(word) - 1;
}

_Static_assert((sizeof (xen_ulong_t) == sizeof (unsigned long)),
        "xen_ulong_t not an unsigned long");

/*
 * OCaml-side array of pending event channels. If evtchn_callback_ml[port] = 1
 * then the port has events pending (demuxed from Xen) but not yet processed by
 * the OCaml Activations code.
 *
 * TODO: This mirrors the original Mirage/Xen implementation, but is a
 * needless waste of space (and cache). Could be re-worked to use bitstrings
 * just like the shared_info fields that it is essentially copying, which would
 * probably also enable further optimizations.
 */
static uint8_t evtchn_callback_ml[EVTCHN_2L_NR_CHANNELS];

/*
 * Overrides default Solo5 event channel upcall handler via strong linkage.
 *
 * Called in interrupt context.
 */
int solo5__xen_evtchn_vector_handler(void *arg __attribute__((unused)))
{
    /*
     * All work is done outside of interrupt context by evtchn_demux_pending(),
     * so there is nothing we need to do here except returning 1 to mark the
     * IRQ as handled and ACK it with the APIC.
     */
    return 1;
}

static bool evtchn_demux_pending(void)
{
    struct shared_info *s = SHARED_INFO();
    struct vcpu_info *vi = VCPU0_INFO();
    bool some_pending = false;

    vi->evtchn_upcall_pending = 0;

    /*
     * Demux events received from Xen.
     *
     * pending_l1 is the "outer" per-VCPU selector (evtchn_pending_sel).
     * pending_l2 is the "inner" system-wide word (evtchn_pending[l1i]).
     */
    xen_ulong_t pending_l1, pending_l2;
    atomic_sync_xchg(&vi->evtchn_pending_sel, 0, &pending_l1);
    while (pending_l1 != 0) {
        xen_ulong_t l1i = ffs(pending_l1);
        pending_l1 &= ~(1UL << l1i);

        /*
         * Masking pending_l2 with ~evtchn_mask[l1i] is necessary to get the
         * *current* masked events; otherwise races like the following
         * can occur:
         *
         *     1. X is generated, upcall scheduled by Xen.
         *     2. X is masked.
         *     3. Upcall is delivered.
         *     4. X fires despite now being masked.
         */
        while ((pending_l2 =
                    (s->evtchn_pending[l1i] & ~s->evtchn_mask[l1i])) != 0) {
            xen_ulong_t l2i = ffs(pending_l2);
            pending_l2 &= ~(1UL << l2i);

            evtchn_port_t port = (l1i * (sizeof(xen_ulong_t) * 8)) + l2i;
            /*
             * Mark as pending on the OCaml side and mask the event until
             * just before OCaml gets around to handling it. Also, clear
             * the pending bit on the Xen side.
             */
            evtchn_callback_ml[port] = 1;
            atomic_sync_bts(l2i, &s->evtchn_mask[l1i]);
            atomic_sync_btc(l2i, &s->evtchn_pending[l1i]);
            some_pending = true;
        }
    }
    return some_pending;
}

/*
 * Demux events from Xen, returning true if any were pending.
 *
 * Caller: OS.Main.run, @@noalloc.
 */
CAMLprim value
mirage_xen_evtchn_demux_pending(value v_unit)
{
    bool events_pending = evtchn_demux_pending();
    return Val_bool(events_pending);
}

static void evtchn_unmask(evtchn_port_t port)
{
    struct shared_info *s = SHARED_INFO();
    int pending = 0;

    atomic_sync_btc(port, &s->evtchn_mask[0]);
    pending = sync_bt(port, &s->evtchn_pending[0]);
    if (pending) {
        /*
         * Slow path:
         *
         * If pending is set here, then there was a race, and we lost the
         * upcall.  Mask the port again and force an upcall via a call to
         * hyperspace.
         *
         * This should be sufficient for HVM/PVHv2 based on my understanding of
         * Linux drivers/xen/events/events_2l.c.
         */
        atomic_sync_bts(port, &s->evtchn_mask[0]);
        hypercall_evtchn_unmask(port);
    }
}

/*
 * "Test if an event on (port) is pending and clear".
 *
 * Called by OCaml (via stub below) immediately before the event would be
 * handled. If there is an event pending on (port), clear it and unmask the
 * event on the Xen side.
 */
static inline bool evtchn_test_and_clear(evtchn_port_t port)
{
    assert(port < EVTCHN_2L_NR_CHANNELS);
    if (evtchn_callback_ml[port] > 0) {
        evtchn_callback_ml[port] = 0;
        evtchn_unmask(port);
        return true;
    }
    else {
        return false;
    }
}

/*
 * Caller: OS.Activations, @@noalloc.
 */
CAMLprim value
mirage_xen_evtchn_test_and_clear(value v_port)
{
    bool pending = evtchn_test_and_clear(Int_val(v_port));
    return Val_bool(pending);
}

/*
 * Caller: OS.Activations, @@noalloc.
 */
CAMLprim value
mirage_xen_evtchn_get_nr_events(value v_unit)
{
    return Val_int(EVTCHN_2L_NR_CHANNELS);
}

/*
 * Caller: OS.Eventchn.bind_unbound_port.
 */
CAMLprim value
mirage_xen_evtchn_alloc_unbound(value v_unused_ctx, value v_remote_domid)
{
    CAMLparam2(v_unused_ctx, v_remote_domid);
    domid_t remote = Int_val(v_remote_domid);
    int rc;
    evtchn_port_t port;

    rc = hypercall_evtchn_alloc_unbound(remote, &port);
    if (rc)
        caml_failwith("evtchn_alloc_unbound");
    else {
        DPRINTF(1, "(remote = 0x%x) = 0x%x\n", remote, port);
        CAMLreturn(Val_int(port));
    }
}

/*
 * Caller: OS.Eventchn.bind_interdomain.
 */
CAMLprim value
mirage_xen_evtchn_bind_interdomain(value v_unused_ctx, value v_remote_domid,
        value v_remote_port)
{
    CAMLparam3(v_unused_ctx, v_remote_domid, v_remote_port);
    domid_t remote = Int_val(v_remote_domid);
    evtchn_port_t remote_port = Int_val(v_remote_port);
    evtchn_port_t local_port;
    int rc;

    rc = hypercall_evtchn_bind_interdomain(remote, remote_port, &local_port);
    if (rc)
        caml_failwith("evtchn_bind_interdomain");
    else {
        DPRINTF(1, "(remote = 0x%x, remote_port = 0x%x) = 0x%x\n", remote,
                remote_port, local_port);
        CAMLreturn(Val_int(local_port));
    }
}

/*
 * Caller: OS.Eventchn.bind_dom_exc_virq.
 */
CAMLprim value
mirage_xen_evtchn_bind_virq(value v_unused_ctx, value v_virq)
{
    CAMLparam2(v_unused_ctx, v_virq);
    uint32_t virq = Int_val(v_virq);
    evtchn_port_t port;
    int rc;

    rc = hypercall_evtchn_bind_virq(virq, 0, &port);
    if (rc)
       caml_failwith("evtchn_bind_virq");
    else {
        DPRINTF(1, "(virq = 0x%x) = 0x%x\n", virq, port);
        CAMLreturn(Val_int(port));
    }
}

/*
 * Caller: OS.Eventchn.notify, @@noalloc.
 */
CAMLprim value
mirage_xen_evtchn_notify(value v_unused_ctx, value v_port)
{
    evtchn_port_t port = Int_val(v_port);

    int rc = hypercall_evtchn_send(port);
    assert(rc == 0);
    return Val_unit;
}

/*
 * Caller: OS.Eventchn.unmask.
 */
CAMLprim value
mirage_xen_evtchn_unmask(value v_unused_ctx, value v_port)
{
    CAMLparam2(v_unused_ctx, v_port);
    evtchn_unmask(Int_val(v_port));
    CAMLreturn(Val_unit);
}

/*
 * Caller: OS.Eventchn.unbind.
 */
CAMLprim value
mirage_xen_evtchn_unbind(value v_unused_ctx, value v_port)
{
    CAMLparam2(v_unused_ctx, v_port);
    evtchn_port_t port = Int_val(v_port);
    int rc;

    rc = hypercall_evtchn_close(port);
    if (rc)
       caml_failwith("evtchn_unbind");
    else
       CAMLreturn(Val_unit);
}

/*
 * Caller: OS.Eventchn.bind_dom_exc_virq.
 */
CAMLprim value
mirage_xen_evtchn_virq_dom_exc(value v_unit)
{
    CAMLparam1(v_unit);
    CAMLreturn(Val_int(VIRQ_DOM_EXC));
}

/*
 * Block domain until monotonic time reaches (v_deadline) or some event
 * channels have triggered.
 *
 * Caller: OS.Main.run, @@noalloc.
 */
CAMLprim value
mirage_xen_evtchn_block_domain(value v_deadline)
{
    struct vcpu_info *vi = VCPU0_INFO();
    solo5_time_t deadline = Int64_val(v_deadline);

    if (solo5_clock_monotonic() < deadline) {
        cpu_cli();
        cc_barrier();
        if (vi->evtchn_upcall_pending) {
            cpu_sti();
        }
        else {
            hypercall_set_timer_op(deadline);
            cpu_wfi();
        }
    }
    return Val_unit;
}
