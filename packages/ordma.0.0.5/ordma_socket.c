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
#include <caml/unixsupport.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <rdma/rsocket.h>
#include <fcntl.h>

int ordma_socket_domain_table[] = {
  PF_UNIX, PF_INET,
  PF_INET6
};

int ordma_socket_type_table[] = {
  SOCK_STREAM, SOCK_DGRAM, SOCK_RAW, SOCK_SEQPACKET
};

CAMLprim value ordma_rsocket(value domain, value type, value proto)
{
  int retcode;
  retcode = rsocket(ordma_socket_domain_table[Int_val(domain)],
                    ordma_socket_type_table[Int_val(type)],
                    Int_val(proto));
  if (retcode == -1) uerror("socket", Nothing);
  return Val_int(retcode);

}

CAMLprim value ordma_set_nonblock(value fd)
{
  int retcode;
  retcode = rfcntl(Int_val(fd), F_GETFL, 0);
  if (retcode == -1 ||
      rfcntl(Int_val(fd), F_SETFL, retcode | O_NONBLOCK) == -1){
    uerror("set_nonblock", Nothing);
  }
  return Val_unit;
}
