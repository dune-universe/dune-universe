/* File: mindstorm_unix.c

   Copyright (C) 2007

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/anum/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation.  See the file
   LICENCE for more details.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. */


#include <unistd.h>
#include <sys/socket.h>

#include <bluetooth/bluetooth.h>
#include <bluetooth/rfcomm.h>
#include <bluetooth/hci.h>
#include <bluetooth/hci_lib.h>

#include <caml/config.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/intext.h>
#include <caml/unixsupport.h>

CAMLexport
value ocaml_mindstorm_connect(value vdest)
{
  /* noalloc */
  int sock;
  int status;
  struct sockaddr_rc addr = {0};

  sock = socket(AF_BLUETOOTH, SOCK_STREAM, BTPROTO_RFCOMM);
  addr.rc_family = AF_BLUETOOTH;
  addr.rc_channel = (uint8_t) 1;
  str2ba(String_val(vdest), &addr.rc_bdaddr);
  status = connect(sock, (struct sockaddr *)&addr, sizeof(addr));
  /* uerror available because we link with unix.cm[x]a */
  if (status < 0) uerror("Mindstorm.*.connect_bluetooth", vdest);

  /* a OCaml Unix.file_descr is just an int underneath (see
   * e.g. socket.c in the CVS directory of unix module). */
  return(Val_int(sock));
}

#ifdef HAS_USB

/* USB
*************************************************************************/

#include <libusb.h>

static libusb_context *ctx = NULL;
static int libusb_must_init = 1; /* global for the mindstorm lib */

/* Official LEGO firmware (the Mindstorm library uses the LEGO protocol). */
#define LEGO_ID 0x0694
#define PRODUCT_ID 0x0002

/* The type Mindstorm.USB.device
   ----------------------------------------------------------------------*/
#define USB_DEVICE_VAL(v) (* (libusb_device **) Data_custom_val(v))
#define ALLOC_DEVICE()                                                  \
  alloc_custom(&ocaml_mindstorm_usb_device_ops, sizeof(libusb_device*), 1, 50)

static void device_finalize(value vd)
{
  libusb_unref_device(USB_DEVICE_VAL(vd));
}

static int device_compare(value vd1, value vd2)
{
  libusb_device *d1 = USB_DEVICE_VAL(vd1);
  libusb_device *d2 = USB_DEVICE_VAL(vd2);
  if (d1 == d2) return(0);
  else if (d1 < d2) return(-1);
  else return(1);
}

static long device_hash(value vd)
{
  return((long) USB_DEVICE_VAL(vd)); /* the pointer */
}

static struct custom_operations ocaml_mindstorm_usb_device_ops = {
  "mindstorm_usb_device",
  &device_finalize,
  &device_compare,
  &device_hash,
  custom_serialize_default,
  custom_deserialize_default
};


value ocaml_mindstorm_bricks(value vunit)
{
  CAMLparam1(vunit);
  CAMLlocal3(vdev, vcons, vlist);
  int err, i;
  libusb_device **devices, *dev;
  ssize_t ndevices;
  struct libusb_device_descriptor desc;

  if (libusb_must_init) {
    err = libusb_init(&ctx);
#ifdef HAS_USB_SET_OPTION
    libusb_set_option(ctx, LIBUSB_OPTION_LOG_LEVEL, LIBUSB_LOG_LEVEL_INFO);
#else
    libusb_set_debug(ctx, 3);
#endif
    if (err != 0) failwith("Mindstorm.USB.bricks: cannot initialize libusb!");
    libusb_must_init = 0;
  }
  
  ndevices = libusb_get_device_list(ctx, &devices);
  if (ndevices == LIBUSB_ERROR_NO_MEM) {
    /* Do not call libusb_exit(ctx) because other connections may work. */
    failwith("Mindstorm.connect_usb: no memory available to list devices");
  }

  vlist = Val_int(0); /* empty list */
  for(i = 0; i < ndevices; i++) {
    dev = devices[i];
    err = libusb_get_device_descriptor(dev, &desc);
    /* If one cannot get the device descriptor, we skip it, just as if
       it is not a LEGO NXT brick. */
    if (err == LIBUSB_SUCCESS
        && desc.idVendor == LEGO_ID && desc.idProduct == PRODUCT_ID) {
      /* Increase the refcount to make is persist past the free of the list */
      libusb_ref_device(dev);
      /* Add the device to the list */
      vdev = ALLOC_DEVICE();
      USB_DEVICE_VAL(vdev) = dev;
      vcons = alloc_tuple(2);
      Store_field(vcons, 0, vdev);
      Store_field(vcons, 1, vlist);
      vlist = vcons;
    }
  }
  libusb_free_device_list(devices, 1);
  CAMLreturn(vlist);
}

value ocaml_mindstorm_exit(value vunit)
{
  /* noalloc */
  if (! libusb_must_init) libusb_exit(ctx);
  return(Val_unit);
}

/* The type Mindstorm.usb
   ----------------------------------------------------------------------*/
#define USB_HANDLE_PTR(v) ((libusb_device_handle **) Data_custom_val(v))
#define USB_HANDLE_VAL(v) (* USB_HANDLE_PTR(v))
#define USB_HANDLE_OUT(v) ((uint8_t *) (USB_HANDLE_PTR(v) + 1))[0]
#define USB_HANDLE_IN(v) ((uint8_t *) (USB_HANDLE_PTR(v) + 1))[1]
#define ALLOC_HANDLE()                                                  \
  alloc_custom(&ocaml_mindstorm_usb_handle_ops,                         \
               sizeof(libusb_device_handle*) + 2 * sizeof(uint8_t), 1, 50)

#define USB_HANDLE_CLOSE(vhandle)                       \
  libusb_release_interface(USB_HANDLE_VAL(vhandle), 0); \
  libusb_close(USB_HANDLE_VAL(vhandle));                \
  USB_HANDLE_VAL(vhandle) = NULL

static void handle_finalize(value vhandle)
{
  /* Closing (see ocaml_mindstorm_close_usb) the handle several times
   * triggers a "double free" in glibc which aborts the program.  On
   * the Caml side, freeing an already closed USB connection should
   * emit no noise. */
  if (USB_HANDLE_VAL(vhandle) != NULL) { USB_HANDLE_CLOSE(vhandle); }
}

static int handle_compare(value vd1, value vd2)
{
  libusb_device_handle *d1 = USB_HANDLE_VAL(vd1);
  libusb_device_handle *d2 = USB_HANDLE_VAL(vd2);
  if (d1 == d2) return(0);
  else if (d1 < d2) return(-1);
  else return(1);
}

static long handle_hash(value vd)
{
  return((long) USB_HANDLE_VAL(vd)); /* the pointer */
}

static struct custom_operations ocaml_mindstorm_usb_handle_ops = {
  "mindstorm_usb_device",
  &handle_finalize,
  &handle_compare,
  &handle_hash,
  custom_serialize_default,
  custom_deserialize_default
};


value ocaml_mindstorm_connect_usb(value vdev)
{
  CAMLparam1(vdev);
  CAMLlocal1(vhandle);
  int err;
  libusb_device_handle *handle;
  int bConfigurationValue;
  struct libusb_config_descriptor *conf;
  struct libusb_interface iface;
  struct libusb_interface_descriptor desc;
  uint8_t out, in;
  
  err = libusb_open(USB_DEVICE_VAL(vdev), &handle); /* => incr ref of [dev]. */
  switch (err) {
  case LIBUSB_SUCCESS:
    break;
  case LIBUSB_ERROR_NO_MEM:
    failwith("Mindstorm.connect_usb: no memory available to open \
		the device");
    break;
  case LIBUSB_ERROR_ACCESS:
    failwith("Mindstorm.connect_usb: you do not have permission to \
		access your chosen device");
    break;
  case LIBUSB_ERROR_NO_DEVICE:
    failwith("Mindstorm.connect_usb: the device you chose has been \
		disconnected");
    break;
  case LIBUSB_ERROR_BUSY:
    failwith("Mindstorm.connect_usb: the device you chose is already in use");
    break;
  default:
    failwith("Mindstorm.connect_usb: could not open de USB device!");
  }
  err = libusb_claim_interface(handle, 0);
  if (err) failwith("Mindstorm.connect_usb: cannot claim interface");
  libusb_reset_device(handle);
  if (err) failwith("Mindstorm.connect_usb: cannot reset device");  
  
  err = libusb_get_configuration(handle, &bConfigurationValue);
  if (err) failwith("Mindstorm.connect_usb: cannot get the \
       	bConfigurationValue");
  err = libusb_get_config_descriptor_by_value(USB_DEVICE_VAL(vdev), 
                                              bConfigurationValue, &conf);
  if (err) failwith("Mindstorm.connect_usb: could not retrieve the \
       	device configutation");
  if (conf->bNumInterfaces < 1) failwith("Mindstorm.connect_usb: no interface \
	found for the device!");
  iface = (conf->interface)[0];
  if (iface.num_altsetting < 1) failwith("Mindstorm.connect_usb: no settings \
	for device!");
  desc = iface.altsetting[0];
  if (desc.bNumEndpoints < 2) failwith("Mindstorm.connect_usb: cannot find \
	two endpoints!");
  /* FIXME: can we rely on the order ? */
  out = (desc.endpoint[0]).bEndpointAddress;
  in = (desc.endpoint[1]).bEndpointAddress;
  
  libusb_free_config_descriptor(conf);
  
  vhandle = ALLOC_HANDLE();
  USB_HANDLE_VAL(vhandle) = handle;
  USB_HANDLE_OUT(vhandle) = out;
  USB_HANDLE_IN(vhandle) = in;
  CAMLreturn(vhandle);
}

value ocaml_mindstorm_close_usb(value vhandle)
{
  CAMLparam1(vhandle);
  /* To be coherent with the bluetooth connection, we raise an
   * exception if the handle is already closed. */
  if (USB_HANDLE_VAL(vhandle) != NULL) { USB_HANDLE_CLOSE(vhandle); }
  else invalid_argument("Mindstorm.close: USB connection already closed.");
  CAMLreturn(Val_unit);
}

value ocaml_mindstorm_usb_write(value vhandle,
                                value vdata, value vofs, value vlength)
{
  CAMLparam4(vhandle, vdata, vofs, vlength);
  libusb_device_handle *handle = USB_HANDLE_VAL(vhandle);
  int err, transferred;
  unsigned char *data = ((unsigned char *) String_val(vdata)) + Int_val(vofs);
  int length = Int_val(vlength);

  if (handle == NULL) invalid_argument("Mindstorm.USB: connection closed.");
  
  while (length > 0) {
    err = libusb_bulk_transfer(handle, USB_HANDLE_OUT(vhandle),
                               data, length, &transferred, 0);
    switch (err) {
    case LIBUSB_SUCCESS:
      break;
    case LIBUSB_ERROR_PIPE:
    case LIBUSB_ERROR_NO_DEVICE:
      failwith("Mindstorm.USB: write not performed because the endpoint is \
	disconnected");
      break;
    case LIBUSB_ERROR_IO:
      failwith("Mindstorm.USB: write not performed! (I/O error)");
      break;
    default:
      failwith("Mindstorm.USB: write not performed! (libusb error)");
    }
    length -= transferred;
  }
  CAMLreturn(Val_unit);
}

value ocaml_mindstorm_usb_really_input(value vhandle,
                                       value vdata, value vofs, value vlength)
{
  CAMLparam4(vhandle, vdata, vofs, vlength);
  libusb_device_handle *handle = USB_HANDLE_VAL(vhandle);
  int err, transferred;
  int length = Int_val(vlength);
  unsigned char *data = ((unsigned char *) String_val(vdata)) + Int_val(vofs);

  if (handle == NULL) invalid_argument("Mindstorm.USB: connection closed.");
  
  while (length > 0) {
    err = libusb_bulk_transfer(handle, USB_HANDLE_IN(vhandle),
                               data, length, &transferred, 0);
    switch (err) {
    case LIBUSB_SUCCESS:
      break;
    case LIBUSB_ERROR_PIPE:
    case LIBUSB_ERROR_NO_DEVICE:
      failwith("Mindstorm.USB: read not performed because the endpoint is \
	disconnected");
      break;
    case LIBUSB_ERROR_IO:
      failwith("Mindstorm.USB: read not performed! (I/O error)");
      break;
    default:
      failwith("Mindstorm.USB: read not performed! (libusb error)");
    }
    data += transferred;
    length -= transferred;
  }
  CAMLreturn(Val_unit);
}

#endif
