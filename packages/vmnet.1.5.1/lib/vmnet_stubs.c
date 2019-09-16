/*
 * Copyright (C) 2014 Anil Madhavapeddy <anil@recoil.org>
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

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <err.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/bigarray.h>
#include <caml/threads.h>

#include <sys/types.h>
#include <sys/uio.h>
#include <dispatch/dispatch.h>
#include <vmnet/vmnet.h>
#include <pthread.h>

static struct custom_operations vmnet_state_ops = {
  "org.openmirage.vmnet.vmnet_state",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

struct vmnet_state {
  interface_ref iref;
  pthread_mutex_t vmm;
  pthread_cond_t vmc;
  int last_event; /* incremented when an event is received */
  int seen_event; /* last event we saw */
};
#define Vmnet_state_val(v) (*((struct vmnet_state **) Data_custom_val(v)))

static value
alloc_vmnet_state(interface_ref i)
{
  value v = alloc_custom(&vmnet_state_ops, sizeof(struct vmnet_state *), 0, 1);
  struct vmnet_state *vms = malloc(sizeof(struct vmnet_state));
  if (!vms)
     caml_raise_out_of_memory();
  vms->iref = i;
  pthread_mutex_init(&vms->vmm, NULL);
  pthread_cond_init(&vms->vmc, NULL);
  vms->seen_event = 0;
  vms->last_event = 0;
  Vmnet_state_val(v) = vms;
  return v;
}

CAMLprim value
caml_init_vmnet(value v_mode)
{
  CAMLparam1(v_mode);
  CAMLlocal3(v_iface_ref,v_res,v_mac);
  xpc_object_t interface_desc = xpc_dictionary_create(NULL, NULL, 0);
  xpc_dictionary_set_uint64(interface_desc, vmnet_operation_mode_key, Int_val(v_mode));
  uuid_t uuid;
  uuid_generate_random(uuid);
  xpc_dictionary_set_uuid(interface_desc, vmnet_interface_id_key, uuid);
  __block interface_ref iface = NULL;
  __block vmnet_return_t iface_status = 0;
  __block unsigned char *mac = malloc(6);
  if (!mac) caml_raise_out_of_memory ();
  __block unsigned int mtu = 0;
  __block unsigned int max_packet_size = 0;
  dispatch_queue_t if_create_q = dispatch_queue_create("org.openmirage.vmnet.create", DISPATCH_QUEUE_SERIAL);
  dispatch_semaphore_t iface_created = dispatch_semaphore_create(0);
  iface = vmnet_start_interface(interface_desc, if_create_q,
    ^(vmnet_return_t status, xpc_object_t interface_param) {
      iface_status = status;
      if (status != VMNET_SUCCESS || !interface_param) {
         dispatch_semaphore_signal(iface_created);
         return;
      }
      //printf("mac desc: %s\n", xpc_copy_description(xpc_dictionary_get_value(interface_param, vmnet_mac_address_key)));
      const char *macStr = xpc_dictionary_get_string(interface_param, vmnet_mac_address_key);
      unsigned char lmac[6];
      if (sscanf(macStr, "%hhx:%hhx:%hhx:%hhx:%hhx:%hhx", &lmac[0], &lmac[1], &lmac[2], &lmac[3], &lmac[4], &lmac[5]) != 6)
        errx(1, "Unexpected MAC address received from vmnet");
      memcpy(mac, lmac, 6);
      mtu = xpc_dictionary_get_uint64(interface_param, vmnet_mtu_key);
      max_packet_size = xpc_dictionary_get_uint64(interface_param, vmnet_max_packet_size_key);
      dispatch_semaphore_signal(iface_created);
    });
  dispatch_semaphore_wait(iface_created, DISPATCH_TIME_FOREVER);
  dispatch_release(if_create_q);
  if (iface == NULL || iface_status != VMNET_SUCCESS) {
     value *v_exc = caml_named_value("vmnet_raw_return");
     if (!v_exc)
       caml_failwith("Vmnet.Error exception not registered");
     caml_raise_with_arg(*v_exc, Val_int(iface_status));
  }
  v_iface_ref = alloc_vmnet_state(iface);
  v_mac = caml_alloc_string(6);
  memcpy(String_val(v_mac),mac,6);
  v_res = caml_alloc_tuple(4);
  Field(v_res,0) = v_iface_ref;
  Field(v_res,1) = v_mac;
  Field(v_res,2) = Val_int(mtu);
  Field(v_res,3) = Val_int(max_packet_size);
  CAMLreturn(v_res);
}

CAMLprim value
caml_set_event_handler(value v_vmnet)
{
  CAMLparam1(v_vmnet);
  struct vmnet_state *vms = Vmnet_state_val(v_vmnet);
  interface_ref iface = vms->iref;
  /* TODO: release queue. */
  dispatch_queue_t iface_q = dispatch_queue_create("org.openmirage.vmnet.iface_q", 0);
  vmnet_interface_set_event_callback(iface, VMNET_INTERFACE_PACKETS_AVAILABLE, iface_q,
    ^(interface_event_t event_id, xpc_object_t event)
    {
      pthread_mutex_lock(&vms->vmm);
      vms->last_event ++;
      pthread_cond_broadcast(&vms->vmc);
      pthread_mutex_unlock(&vms->vmm);
    });
  CAMLreturn(Val_unit);
}

CAMLprim value
caml_wait_for_event(value v_vmnet)
{
  CAMLparam1(v_vmnet);
  struct vmnet_state *vms = Vmnet_state_val(v_vmnet);
  caml_release_runtime_system();
  pthread_mutex_lock(&vms->vmm);
  while (vms->seen_event == vms->last_event)
    pthread_cond_wait(&vms->vmc, &vms->vmm);
  vms->seen_event = vms->last_event;
  pthread_mutex_unlock(&vms->vmm);
  caml_acquire_runtime_system();
  CAMLreturn(Val_unit);
}

CAMLprim value
caml_vmnet_read(value v_vmnet, value v_ba, value v_ba_off, value v_ba_len)
{
  CAMLparam4(v_vmnet, v_ba, v_ba_off, v_ba_len);
  struct vmnet_state *vms = Vmnet_state_val(v_vmnet);
  interface_ref iface = vms->iref;
  struct iovec iov;
  iov.iov_base = Caml_ba_data_val(v_ba) + (Int_val(v_ba_off));
  iov.iov_len = Int_val(v_ba_len);
  struct vmpktdesc v;
  v.vm_pkt_size = Int_val(v_ba_len);
  v.vm_pkt_iov = &iov;
  v.vm_pkt_iovcnt = 1;
  v.vm_flags = 0; /* TODO no clue what this is */
  int pktcnt = 1;
  vmnet_return_t res = vmnet_read(iface, &v, &pktcnt);
  if (res != VMNET_SUCCESS)
    CAMLreturn(Val_int((-1)*(int32_t)res));
  else if (pktcnt <= 0)
    CAMLreturn(Val_int(0));
  else
    CAMLreturn(Val_int(v.vm_pkt_size));
}

CAMLprim value
caml_vmnet_write(value v_vmnet, value v_ba, value v_ba_off, value v_ba_len)
{
  CAMLparam4(v_vmnet, v_ba, v_ba_off, v_ba_len);
  struct vmnet_state *vms = Vmnet_state_val(v_vmnet);
  interface_ref iface = vms->iref;
  struct iovec iov;
  iov.iov_base = Caml_ba_data_val(v_ba) + (Int_val(v_ba_off));
  iov.iov_len = Int_val(v_ba_len);
  struct vmpktdesc v;
  v.vm_pkt_size = Int_val(v_ba_len);
  v.vm_pkt_iov = &iov;
  v.vm_pkt_iovcnt = 1;
  v.vm_flags = 0; /* TODO no clue what this is */
  int pktcnt = 1;
  vmnet_return_t res = vmnet_write(iface, &v, &pktcnt);
  if (res == VMNET_SUCCESS)
    CAMLreturn(Val_int(v.vm_pkt_size));
  else
    CAMLreturn(Val_int((-1)*(int32_t)res));
}
