/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>
#include <caml/socketaddr.h>

#include <rdma/rsocket.h>
#include "ordma_debug.h"
//#include <errno.h>
CAMLprim value ordma_raccept(value sock)
{
  ORDMA_LOG("ordma_raccept: begin");
  
  int retcode;
  value res;
  value a;
  union sock_addr_union addr;
  socklen_param_type addr_len;

  addr_len = sizeof(addr);
  int sock_ = Int_val(sock);
  ORDMA_LOG("ordma_raccept: sock_=%i addr_len=%i", sock_, addr_len);
  enter_blocking_section();
  //errno = 0;
  retcode = raccept(sock_, &addr.s_gen, &addr_len);
  leave_blocking_section();
  if (retcode == -1) {
    uerror("raccept", Nothing);
  }
  a = alloc_sockaddr(&addr, addr_len, retcode);
  Begin_root (a);
    res = alloc_small(2, 0);
    Field(res, 0) = Val_int(retcode);
    Field(res, 1) = a;
  End_roots();
  ORDMA_LOG("ordma_raccept: end");
  return res;
}


