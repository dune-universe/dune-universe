#ifndef __XEN_BINDINGS_H__
#define __XEN_BINDINGS_H__

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include "xen/xen.h"

/*
 * Accessors for Xen shared_info and VCPU 0 info structures shared with
 * hypervisor.
 */
extern uint8_t solo5__xen_HYPERVISOR_SHARED_INFO[];

inline struct shared_info *SHARED_INFO(void)
{
    return (struct shared_info *)&solo5__xen_HYPERVISOR_SHARED_INFO;
}

inline struct vcpu_info *VCPU0_INFO(void)
{
    return &(SHARED_INFO()->vcpu_info[0]);
}

/*
 * Initialise grant table functionality; called from solo5_app_main().
 */
void gnttab_init(void);

/*
 * Low-level definitions specific to the x86_64 architecture.
 */
#if defined(__x86_64__)

#define PAGE_SIZE  4096
#define PAGE_SHIFT 12

#define cpu_cli() __asm__ __volatile__("cli")
#define cpu_sti() __asm__ __volatile__("sti")

/*
 * "wait for interrupt". On x86, must be called with interrupts disabled for
 * this operation to be race-free.
 */
#define cpu_wfi() __asm__ __volatile__("sti; hlt")

/* compiler-only memory "barrier" */
#define cc_barrier() __asm__ __volatile__("" : : : "memory")

#define cpu_barrier() __asm__ __volatile__("mfence" : : : "memory")
#define cpu_read_barrier() __asm__ __volatile__("lfence" : : : "memory")
#define cpu_write_barrier() __asm__ __volatile__("sfence" : : : "memory")

/*
 * Atomically test-and-set bit (nr) in (*bits). Fully synchronous.
 */
static inline bool atomic_sync_bts(int nr, volatile void *bits)
{
    uint8_t *byte = ((uint8_t *)bits) + (nr >> 3);
    uint8_t bit = 1 << (nr & 7);
    uint8_t orig;

    orig = __atomic_fetch_or(byte, bit, __ATOMIC_SEQ_CST);
    return (orig & bit) != 0;
}

/*
 * Atomically test-and-clear bit (nr) in (*bits). Fully synchronous.
 */
static inline bool atomic_sync_btc(int nr, volatile void *bits)
{
    uint8_t *byte = ((uint8_t *)bits) + (nr >> 3);
    uint8_t bit = 1 << (nr & 7);
    uint8_t orig;

    orig = __atomic_fetch_and(byte, ~bit, __ATOMIC_SEQ_CST);
    return (orig & bit) != 0;
}

/*
 * Test if bit (nr) in (*bits) is set. Fully synchronous.
 */
static inline bool sync_bt(int nr, const volatile void *bits)
{
    const volatile uint8_t *byte = ((const volatile uint8_t *)bits) + (nr >> 3);
    uint8_t bit = 1 << (nr & 7);
    uint8_t result;

    result = ((bit & *byte) != 0);
    cc_barrier();
    return result;
}

/*
 * Atomically exchange (*ptr) with val, storing the result in (*result).
 * Fully synchronous.
 */
static inline void atomic_sync_xchg(volatile unsigned long *ptr,
        unsigned long val, unsigned long *result)
{
    __atomic_exchange(ptr, &val, result, __ATOMIC_SEQ_CST);
}

/*
 * Atomically compare the 16-bit word (*ptr) with (expected) and set (*ptr) to
 * (desired) if the comparison succeeds. Returns previous value. Fully
 * synchronous.
 */
static inline uint16_t atomic_sync_cmpxchgw(volatile uint16_t *ptr,
        uint16_t expected, uint16_t desired)
{
    uint16_t prev = expected;
    (void)__atomic_compare_exchange_n(ptr, &prev, desired,
            false, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
    return prev;
}

#else /* __x86_64__ */
#error Not implemented
#endif /* __x86_64__ */

/*
 * Accessor for retrieving guest-virtual memory range suitable for importing
 * grant mappings from Solo5.
 */
void solo5__xen_get_gntmap_area(uint64_t *addr, size_t *size);

/*
 * Bitmap allocator for virtual memory used for importing grant mappings.
 */
struct bmap_allocator;
typedef struct bmap_allocator bmap_allocator_t;

bmap_allocator_t *bmap_init(uint64_t start_addr, size_t n_pages);
void *bmap_alloc(bmap_allocator_t *alloc, size_t n);
void bmap_free(bmap_allocator_t *alloc, void *addr, size_t n);

#endif /* __XEN_BINDINGS_H__ */
