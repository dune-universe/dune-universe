/*---------------------------------------------------------------------------
   Copyright (c) 2016 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   --------------------------------------------------------------------------*/

#include <string.h>
#include <errno.h>

#include <net/if.h>
#include <netinet/ip.h>

#include <caml/mlvalues.h>
#include <caml/bigarray.h>

#ifdef __APPLE__
#define IPV6_ADD_MEMBERSHIP IPV6_JOIN_GROUP
#define IPV6_DROP_MEMBERSHIP IPV6_LEAVE_GROUP
#endif

CAMLprim value sizeof_sockaddr_storage(value unit) {
    return Val_int(sizeof(struct sockaddr_storage));
}

CAMLprim value ml_strerror_r(value buf) {
  if (strerror_r(errno, Bytes_val(buf), caml_string_length(buf)) != 0)
      return Val_int(0);
  else return Val_long(strlen(Bytes_val(buf)));
}

CAMLprim value ml_if_nametoindex(value ifname)
{
  return Val_int(if_nametoindex(String_val(ifname)));
}

CAMLprim value ml_send(value sockfd, value buf, value flags)
{
  return Val_int(send(Int_val(sockfd),
                      Caml_ba_data_val(buf),
                      Caml_ba_array_val(buf)->dim[0],
                      Int32_val(flags)));
}

CAMLprim value ml_send_bytes(value sockfd, value buf, value pos, value len, value flags)
{
  return Val_int(send(Int_val(sockfd),
                      Bytes_val(buf)+Int_val(pos),
                      Int_val(len),
                      Int32_val(flags)));
}

CAMLprim value ml_sendto(value sockfd, value buf, value flags, value saddr)
{
  return Val_int(sendto(Int_val(sockfd),
                        Caml_ba_data_val(buf),
                        Caml_ba_array_val(buf)->dim[0],
                        Int32_val(flags),
                        Caml_ba_data_val(saddr),
                        Caml_ba_array_val(saddr)->dim[0]));
}

CAMLprim value ml_sendto_bytes(value sockfd, value buf, value pos,
                               value len, value flags, value saddr)
{
  return Val_int(sendto(Int_val(sockfd),
                        Bytes_val(buf)+Int_val(pos),
                        Int_val(len),
                        Int32_val(flags),
                        Caml_ba_data_val(saddr),
                        Caml_ba_array_val(saddr)->dim[0]));
}

CAMLprim value ml_sendto_bytes_bytecode(value *argv, int argn) {
    return ml_sendto_bytes(argv[0], argv[1], argv[2],
                           argv[3], argv[4], argv[5]);
}

CAMLprim value ml_recv(value sockfd, value buf, value flags)
{
    return Val_int(recv(Int_val(sockfd),
                        Caml_ba_data_val(buf),
                        Caml_ba_array_val(buf)->dim[0],
                        Int32_val(flags)));
}

CAMLprim value ml_recv_bytes(value sockfd, value buf, value pos, value len, value flags)
{
    return Val_int(recv(Int_val(sockfd),
                        Bytes_val(buf)+Int_val(pos),
                        Int_val(len),
                        Int32_val(flags)));
}

CAMLprim value ml_recvfrom(value sockfd, value buf, value flags, value saddr, value sa_len)
{
    return Val_int(recvfrom(Int_val(sockfd),
                            Caml_ba_data_val(buf),
                            Caml_ba_array_val(buf)->dim[0],
                            Int32_val(flags),
                            Caml_ba_data_val(saddr),
                            Caml_ba_data_val(sa_len)));
}

CAMLprim value ml_recvfrom_bytes(value sockfd, value buf, value pos,
                                 value len, value flags, value saddr, value sa_len)
{
    return Val_int(recvfrom(Int_val(sockfd),
                            Bytes_val(buf)+Int_val(pos),
                            Int_val(len),
                            Int32_val(flags),
                            Caml_ba_data_val(saddr),
                            Caml_ba_data_val(sa_len)));
}

CAMLprim value ml_recvfrom_bytes_bytecode(value *argv, int argn) {
    return ml_recvfrom_bytes(argv[0], argv[1], argv[2], argv[3],
                             argv[4], argv[5], argv[6]);
}

CAMLprim value ml_bind(value sockfd, value saddr)
{
  return Val_int(bind(Int_val(sockfd),
                      Caml_ba_data_val(saddr),
                      Caml_ba_array_val(saddr)->dim[0]));
}

CAMLprim value ml_connect(value sockfd, value ifname, value saddr)
{
  return Val_int(connect(Int_val(sockfd),
                         Caml_ba_data_val(saddr),
                         Caml_ba_array_val(saddr)->dim[0]));
}

CAMLprim value ml_getsockopt(value sockfd, value level, value optname, value payload, value len) {
    return Val_int(getsockopt(Int_val(sockfd),
                              Int_val(level),
                              Int_val(optname),
                              Caml_ba_data_val(payload),
                              Caml_ba_data_val(len)));
}

CAMLprim value ml_setsockopt(value sockfd, value level, value optname, value payload) {
    return Val_int(setsockopt(Int_val(sockfd),
                              Int32_val(level),
                              Int32_val(optname),
                              Caml_ba_data_val(payload),
                              Caml_ba_array_val(payload)->dim[0]));
}

CAMLprim value sizeof_ip_mreqn(value unit) {
    return Val_int(sizeof(struct ip_mreqn));
}
CAMLprim value set_ip_mreqn(value buf, value saddr, value iface) {
    struct ip_mreqn *mreq = Caml_ba_data_val(buf);
    struct sockaddr_in *saddr_in = Caml_ba_data_val(saddr);
    memcpy(&mreq->imr_multiaddr, &saddr_in->sin_addr, sizeof(struct in_addr));
    memset(&mreq->imr_address, 0, sizeof(struct in_addr));
    mreq->imr_ifindex = Int_val(iface);
    return Val_unit;
}

CAMLprim value sizeof_ipv6_mreq(value unit) {
    return Val_int(sizeof(struct ipv6_mreq));
}
CAMLprim value set_ipv6_mreq(value buf, value saddr) {
    struct ipv6_mreq *mreq = Caml_ba_data_val(buf);
    struct sockaddr_in6 *saddr_in6 = Caml_ba_data_val(saddr);
    memcpy(&mreq->ipv6mr_multiaddr, &saddr_in6->sin6_addr, sizeof(struct in6_addr));
    mreq->ipv6mr_interface = saddr_in6->sin6_scope_id;
    return Val_unit;
}

CAMLprim value levels(value buf)
{
    uint32_t *a = Caml_ba_data_val(buf);
    a[0] = SOL_SOCKET;
    a[1] = IPPROTO_IP;
    a[2] = IPPROTO_IPV6;
    a[3] = IPPROTO_ICMP;
    a[4] = IPPROTO_RAW;
    a[5] = IPPROTO_TCP;
    a[6] = IPPROTO_UDP;
    return Val_int(7 * sizeof(uint32_t));
}

CAMLprim value sockopts(value buf)
{
    uint32_t *a = Caml_ba_data_val(buf);
    a[0] = IP_MULTICAST_IF;
    a[1] = IP_MULTICAST_TTL;
    a[2] = IP_MULTICAST_LOOP;
    a[3] = IP_ADD_MEMBERSHIP;
    a[4] = IP_DROP_MEMBERSHIP;
    a[5] = IPV6_JOIN_GROUP;
    a[6] = IPV6_LEAVE_GROUP;
    a[7] = IPV6_MULTICAST_HOPS;
    a[8] = IPV6_MULTICAST_IF;
    a[9] = IPV6_MULTICAST_LOOP;
    a[10] = IPV6_UNICAST_HOPS;
    a[11] = IPV6_V6ONLY;
    return Val_int(12 * sizeof(uint32_t));
}

CAMLprim value families(value buf)
{
    uint32_t *a = Caml_ba_data_val(buf);
    a[0] = AF_INET;
    a[1] = AF_INET6;
    a[2] = AF_UNIX;
    a[3] = AF_UNSPEC;
    return Val_int(4 * sizeof(uint32_t));
}

CAMLprim value sendrecvflags(value buf)
{
    uint32_t *a = Caml_ba_data_val(buf);
#ifdef __linux__
    a[0] = MSG_CONFIRM;
    a[4] = MSG_MORE;
    a[5] = MSG_NOSIGNAL;
    a[7] = MSG_CMSG_CLOEXEC;
    a[8] = MSG_ERRQUEUE;
#endif
    a[1] = MSG_DONTROUTE;
    a[2] = MSG_DONTWAIT;
    a[3] = MSG_EOR;
    a[6] = MSG_OOB;
    a[9] = MSG_PEEK;
    a[10] = MSG_TRUNC;
    a[11] = MSG_WAITALL;
    return Val_int(12 * sizeof(uint32_t));
}

/*---------------------------------------------------------------------------
   Copyright (c) 2016 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
   --------------------------------------------------------------------------*/
