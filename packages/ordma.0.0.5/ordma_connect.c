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

#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>
#include <caml/socketaddr.h>

#include <rdma/rsocket.h>
#include "ordma_debug.h"

CAMLprim value ordma_rconnect(value socket, value address)
{
  ORDMA_LOG("ordma_rconnect: begin");
  int retcode;
  union sock_addr_union addr;
  socklen_param_type addr_len;

  get_sockaddr(address, &addr, &addr_len);
  enter_blocking_section();
  retcode = rconnect(Int_val(socket), &addr.s_gen, addr_len);
  leave_blocking_section();
  if (retcode == -1) {
    uerror("connect", Nothing);
  }
  ORDMA_LOG("ordma_rconnect: end");
  return Val_unit;
}


