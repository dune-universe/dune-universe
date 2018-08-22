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
#include <caml/signals.h>
#include <caml/unixsupport.h>

#include <rdma/rsocket.h>
#include "ordma_debug.h"

CAMLprim value ordma_rclose(value fd)
{
  
  int ret;
  int fd_ = Int_val(fd);
  ORDMA_LOG("ordma_rclose(%i)", fd_);
  caml_enter_blocking_section();
  ret = rclose(fd_);
  caml_leave_blocking_section();
  ORDMA_LOG("ordma_rclose(%i),ret=%i",fd_,ret);
  if (ret == -1) {
      uerror("close", Nothing);
  }
  return Val_unit;
}
