AC_INIT(setcore, 1.0.0, unixjunkie@sdf.org)

AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
 AC_MSG_ERROR([You must install the OCaml compiler])
fi

AC_HEADER_STDC
AC_CHECK_HEADERS([sched.h])
AC_CHECK_DECLS([sched_setaffinity], [], [], [[
#define _GNU_SOURCE 1
#include <sched.h>
]])

AC_CHECK_HEADERS([mach/thread_policy.h])
AC_CHECK_DECLS([thread_policy_set], [], [], [[
#include <mach/mach_init.h>
#include <mach/thread_policy.h>
]])

AC_CONFIG_HEADERS([src/config.h])
AC_OUTPUT
