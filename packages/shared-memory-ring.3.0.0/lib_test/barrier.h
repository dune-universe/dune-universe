/* Taken from include/minios/x86/os.h
 *
 * random collection of macros and definition
 */

#ifndef _BARRIER_H_
#define _BARRIER_H_

#define xen_barrier() asm volatile ( "" : : : "memory")

#if defined(__i386__)
#define xen_mb()  asm volatile ( "lock; addl $0,0(%%esp)" : : : "memory" )
#define xen_rmb() xen_barrier()
#define xen_wmb() xen_barrier()
#elif defined(__x86_64__)
#define xen_mb()  asm volatile ( "mfence" : : : "memory")
#define xen_rmb() xen_barrier()
#define xen_wmb() xen_barrier()
#elif defined(__arm__)
# ifndef _M_ARM
#define xen_mb()   {}
#define xen_rmb()  {}
#define xen_wmb()  {}
# elif _M_ARM > 6
#define xen_mb()   asm volatile ("dmb" : : : "memory")
#define xen_rmb()  asm volatile ("dmb" : : : "memory")
#define xen_wmb()  asm volatile ("dmb" : : : "memory")
# else
#error This ARM variant needs testing
# endif /* ifdef _M_ARM */
#elif defined(__aarch64__)
#define xen_mb()   asm volatile ("dmb sy" : : : "memory")
#define xen_rmb()  asm volatile ("dmb sy" : : : "memory")
#define xen_wmb()  asm volatile ("dmb sy" : : : "memory")
#else
#error "Define barriers"
#endif

#endif /* _BARRIER_H_ */
