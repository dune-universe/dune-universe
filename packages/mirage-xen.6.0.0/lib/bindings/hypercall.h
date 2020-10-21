/*
 * Copyright (c) 2015-2020 Contributors as noted in the AUTHORS file
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

#ifndef __XEN_HYPERCALL_H__
#define __XEN_HYPERCALL_H__

#include "bindings.h"

#if defined(__x86_64__)
#include "hypercall-x86_64.h"
#define HYPERCALL1 _hypercall64_1
#define HYPERCALL2 _hypercall64_2
#define HYPERCALL3 _hypercall64_3
#define HYPERCALL4 _hypercall64_4
#else
#error Not implemented
#endif

#include "xen/event_channel.h"
#include "xen/grant_table.h"
#include "xen/hvm/hvm_op.h"
#include "xen/memory.h"
#include "xen/sched.h"
#include "xen/vcpu.h"
#include "xen/xen.h"

/*
 * Hypercall primitives.
 */
static inline long hypercall__memory_op(unsigned int cmd, void *arg)
{
    return HYPERCALL2(long, __HYPERVISOR_memory_op, cmd, arg);
}

static inline long hypercall__sched_op(unsigned int cmd, void *arg)
{
    return HYPERCALL2(long, __HYPERVISOR_sched_op, cmd, arg);
}

static inline long hypercall__event_channel_op(unsigned int cmd, void *arg)
{
    return HYPERCALL2(long, __HYPERVISOR_event_channel_op, cmd, arg);
}

static inline long hypercall__hvm_op(unsigned int cmd, void *arg)
{
    return HYPERCALL2(long, __HYPERVISOR_hvm_op, cmd, arg);
}

static inline long hypercall__grant_table_op(unsigned int cmd, void *args,
        unsigned int count /* # of entries in args[] */)
{
    return HYPERCALL3(long, __HYPERVISOR_grant_table_op, cmd, args, count);
}

/*
 * Higher level hypercall helpers, type-safe.
 */
static inline int hypercall_evtchn_alloc_unbound(domid_t remote,
        evtchn_port_t *port)
{
    evtchn_alloc_unbound_t op = {
        .dom = DOMID_SELF,
        .remote_dom = remote
    };

    int rc = hypercall__event_channel_op(EVTCHNOP_alloc_unbound, &op);

    if (rc == 0)
        *port = op.port;
    return rc;
}

static inline int hypercall_evtchn_bind_interdomain(domid_t remote,
        evtchn_port_t remote_port, evtchn_port_t *local_port)
{
    evtchn_bind_interdomain_t op = {
        .remote_dom = remote,
        .remote_port = remote_port
    };

    int rc = hypercall__event_channel_op(EVTCHNOP_bind_interdomain, &op);

    if (rc == 0)
        *local_port = op.local_port;
    return rc;
}

static inline int hypercall_evtchn_bind_virq(uint32_t virq, uint32_t vcpu,
        evtchn_port_t *port)
{
    evtchn_bind_virq_t op = {
        .virq = virq,
        .vcpu = vcpu
    };
    int rc = hypercall__event_channel_op(EVTCHNOP_bind_virq, &op);

    if (rc == 0)
        *port = op.port;
    return rc;
}

static inline int hypercall_evtchn_close(evtchn_port_t port)
{
    evtchn_close_t op = {
        .port = port
    };
    return hypercall__event_channel_op(EVTCHNOP_close, &op);
}

static inline int hypercall_evtchn_send(evtchn_port_t port)
{
    return hypercall__event_channel_op(EVTCHNOP_send, &port);
}

static inline void hypercall_evtchn_unmask(evtchn_port_t port)
{
    evtchn_unmask_t op = {
        .port = port
    };

    (void)hypercall__event_channel_op(EVTCHNOP_unmask, &op);
}

static inline int hypercall_gnttab_map_grant_ref(uint64_t host_addr,
        uint32_t flags, grant_ref_t ref, domid_t dom,
        grant_handle_t *out_handle, uint64_t *out_dev_bus_addr)
{
    gnttab_map_grant_ref_t op = {
        .host_addr = host_addr,
        .flags = flags,
        .ref = ref,
        .dom = dom,
    };
    int rc = hypercall__grant_table_op(GNTTABOP_map_grant_ref, &op, 1);
    /*
     * Always set these, otherwise GCC warns that handle may be used
     * unitialised in caller.
     */
    *out_handle = op.handle;
    *out_dev_bus_addr = op.dev_bus_addr;
    if (rc == 0 && op.status < 0)
        rc = -op.status;
    return rc;
}

static inline int hypercall_gnttab_unmap_grant_ref(uint64_t host_addr,
        uint64_t dev_bus_addr, grant_handle_t handle)
{
    gnttab_unmap_grant_ref_t op = {
        .host_addr = host_addr,
        .dev_bus_addr = dev_bus_addr,
        .handle = handle
    };
    int rc = hypercall__grant_table_op(GNTTABOP_unmap_grant_ref, &op, 1);
    if (rc == 0 && op.status < 0)
        rc = -op.status;
    return rc;
}

static inline int hypercall_hvm_get_param(unsigned int idx, uint64_t *value)
{
    xen_hvm_param_t p = {
        .domid = DOMID_SELF,
        .index = idx
    };
    int rc = hypercall__hvm_op(HVMOP_get_param, &p);

    if (rc == 0)
        *value = p.value;
    return rc;
}

static inline int hypercall_hvm_set_param(unsigned int idx, uint64_t value)
{
    xen_hvm_param_t p = {
        .domid = DOMID_SELF,
        .index = idx,
        .value = value
    };

    return hypercall__hvm_op(HVMOP_set_param, &p);
}

static inline int hypercall_physmap_add_shared_info(unsigned int idx,
        uint64_t gpfn)
{
    xen_add_to_physmap_t add = {
        .domid = DOMID_SELF,
        .space = XENMAPSPACE_shared_info,
        .idx = idx,
        .gpfn = gpfn
    };

    return hypercall__memory_op(XENMEM_add_to_physmap, &add);
}

static inline int hypercall_physmap_add_grant_table(unsigned int idx,
        uint64_t gpfn)
{
    xen_add_to_physmap_t add = {
        .domid = DOMID_SELF,
        .space = XENMAPSPACE_grant_table,
        .idx = idx,
        .gpfn = gpfn
    };

    return hypercall__memory_op(XENMEM_add_to_physmap, &add);
}

static inline int hypercall_set_timer_op(uint64_t deadline)
{
    return HYPERCALL1(long, __HYPERVISOR_set_timer_op, deadline);
}

static inline void hypercall_shutdown(unsigned int reason)
{
    (void)hypercall__sched_op(SCHEDOP_shutdown, &reason);
}

static inline void hypercall_yield(void)
{
    (void)hypercall__sched_op(SCHEDOP_yield, NULL);
}

#endif /* __XEN_HYPERCALL_H__ */
