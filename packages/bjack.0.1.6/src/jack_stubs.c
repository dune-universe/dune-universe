/*
 * Copyright 2007-2008 Romain Beauxis
 *
 * This file is part of ocaml-bjack.
 *
 * ocaml-bjack is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-bjack is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-bjack; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * As a special exception to the GNU Library General Public License, you may
 * link, statically or dynamically, a "work that uses the Library" with a
 * publicly distributed version of the Library to produce an executable file
 * containing portions of the Library, and distribute that executable file under
 * terms of your choice, without any of the additional requirements listed in
 * clause 6 of the GNU Library General Public License. By "a publicly
 * distributed version of the Library", we mean either the unmodified Library as
 * distributed by The Savonet Team, or a modified version of the Library that is
 * distributed under the conditions defined in clause 3 of the GNU Library
 * General Public License. This exception does not however invalidate any other
 * reasons why the executable file might be covered by the GNU Library General
 * Public License.
 *
 */

/*
 * Ocaml blocking API to the jack audio connection kit.
 *
 * @author Romain Beauxis
 */

#include "jack_wrapper.h"
#include <jack/types.h>
#include <samplerate.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/signals.h>

static value caml_bjack_handle_error(int errnum) {
  switch (errnum) {
  case ERR_OPENING_JACK:
    caml_raise_constant(*caml_named_value("bio2jack_exn_open"));
  case ERR_BYTES_PER_OUTPUT_FRAME_INVALID:
    caml_raise_constant(
        *caml_named_value("bio2jack_exn_bytes_per_output_frame_invalid"));
  case ERR_BYTES_PER_INPUT_FRAME_INVALID:
    caml_raise_constant(
        *caml_named_value("bio2jack_exn_bytes_per_input_frame_invalid"));
  case ERR_TOO_MANY_OUTPUT_CHANNELS:
    caml_raise_constant(
        *caml_named_value("bio2jack_exn_too_many_output_channels"));
  case ERR_PORT_NAME_OUTPUT_CHANNEL_MISMATCH:
    caml_raise_constant(
        *caml_named_value("bio2jack_exn_port_name_output_channel_mismatch"));
  case ERR_PORT_NOT_FOUND:
    caml_raise_constant(*caml_named_value("bio2jack_exn_port_not_found"));
  case ERR_TOO_MANY_INPUT_CHANNELS:
    caml_raise_constant(
        *caml_named_value("bio2jack_exn_too_many_input_channels"));
  case ERR_PORT_NAME_INPUT_CHANNEL_MISMATCH:
    caml_raise_constant(
        *caml_named_value("bio2jack_exn_port_name_input_channel_mismatch"));
  default:
    caml_failwith("Failed to open device: Unknown error");
  }
}

CAMLprim value caml_bjack_priv_value_int(value name) {
  CAMLparam1(name);
  char *s = String_val(name);
  if (!strcmp(s, "PLAYED"))
    CAMLreturn(Val_int(PLAYED));
  if (!strcmp(s, "WRITTEN_TO_JACK"))
    CAMLreturn(Val_int(WRITTEN_TO_JACK));
  if (!strcmp(s, "WRITTEN"))
    CAMLreturn(Val_int(WRITTEN));

  /* Values from samplerate.h */
  if (!strcmp(s, "SRC_SINC_BEST_QUALITY"))
    CAMLreturn(Val_int(SRC_SINC_BEST_QUALITY));
  if (!strcmp(s, "SRC_SINC_MEDIUM_QUALITY"))
    CAMLreturn(Val_int(SRC_SINC_MEDIUM_QUALITY));
  if (!strcmp(s, "SRC_SINC_FASTEST"))
    CAMLreturn(Val_int(SRC_SINC_FASTEST));
  if (!strcmp(s, "SRC_ZERO_ORDER_HOLD"))
    CAMLreturn(Val_int(SRC_ZERO_ORDER_HOLD));
  if (!strcmp(s, "SRC_LINEAR"))
    CAMLreturn(Val_int(SRC_LINEAR));

  /* Values from jack/types.h */
  if (!strcmp(s, "JackPortIsInput"))
    CAMLreturn(Val_int(JackPortIsInput));
  if (!strcmp(s, "JackPortIsOutput"))
    CAMLreturn(Val_int(JackPortIsOutput));
  if (!strcmp(s, "JackPortIsPhysical"))
    CAMLreturn(Val_int(JackPortIsPhysical));
  if (!strcmp(s, "JackPortCanMonitor"))
    CAMLreturn(Val_int(JackPortCanMonitor));
  if (!strcmp(s, "JackPortIsTerminal"))
    CAMLreturn(Val_int(JackPortIsTerminal));

  caml_failwith("Invalid value");
}

#define Bjack_drv_val(v) (*((jack_driver_t **)Data_custom_val(v)))

static void finalize_bjack_drv(value d) {
  jack_driver_t *drv = Bjack_drv_val(d);
  JACK_Close(drv);
  free(drv);
}

static struct custom_operations bjack_drv_ops = {
    "ocaml_bjack_drv",   finalize_bjack_drv,       custom_compare_default,
    custom_hash_default, custom_serialize_default, custom_deserialize_default};

CAMLprim value caml_bjack_open(value bit_per_sample, value rate, value name,
                               value server, value input_channels,
                               value output_channels, value _jack_port_flags,
                               value size) {
  CAMLparam2(name, server);
  CAMLlocal1(driver);
  jack_driver_t *drv = JACK_CreateDriver();
  if (drv == NULL)
    caml_failwith("drv_malloc");
  unsigned long r = Unsigned_long_val(rate);
  int jack_ports_flags = Int_val(_jack_port_flags);

  int errnum =
      JACK_Open(drv, Int_val(bit_per_sample), &r, String_val(name),
                String_val(server), Int_val(input_channels),
                Int_val(output_channels), jack_ports_flags, Int_val(size));

  if (errnum != ERR_SUCCESS)
    caml_bjack_handle_error(errnum);

  driver = caml_alloc_custom(&bjack_drv_ops, sizeof(jack_driver_t *), 1, 0);
  Bjack_drv_val(driver) = drv;

  CAMLreturn(driver);
}

CAMLprim value caml_bjack_open_byte(value *argv, int argc) {
  return caml_bjack_open(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5],
                         argv[6], argv[7]);
}

CAMLprim value caml_bjack_close(value device) {
  CAMLparam1(device);
  jack_driver_t *drv = Bjack_drv_val(device);
  int errnum = JACK_Close(drv);
  if (errnum != ERR_SUCCESS)
    caml_bjack_handle_error(errnum);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_bjack_reset(value device) {
  CAMLparam1(device);

  JACK_Reset(Bjack_drv_val(device));

  CAMLreturn(Val_unit);
}

CAMLprim value caml_bjack_write(value device, value data) {
  CAMLparam2(device, data);
  int n = caml_string_length(data);
  jack_driver_t *drv = Bjack_drv_val(device);
  long ret;
  char *buf = malloc(n);
  memcpy(buf, String_val(data), n);

  if (drv->num_output_channels > 0) {
    caml_enter_blocking_section();
    ret = JACK_Write(drv, (unsigned char *)buf, n);
    caml_leave_blocking_section();
  } else {
    caml_raise_constant(
        *caml_named_value("bio2jack_exn_too_many_output_channels"));
  }

  if (ret < 0)
    caml_failwith("jack_write");

  free(buf);

  CAMLreturn(Val_long(ret));
}

CAMLprim value caml_bjack_read(value device, value len) {
  CAMLparam2(device, len);
  CAMLlocal1(ans);
  int n = Int_val(len);
  char *buf = malloc(n);
  jack_driver_t *drv = Bjack_drv_val(device);
  long ret;

  if (drv->num_input_channels > 0) {
    caml_enter_blocking_section();
    ret = JACK_Read(drv, (unsigned char *)buf, n);
    caml_leave_blocking_section();
  } else {
    caml_raise_constant(
        *caml_named_value("bio2jack_exn_too_many_input_channels"));
  }

  if (ret < 0)
    caml_failwith("jack_read");

  ans = caml_alloc_string(ret);
  memcpy(String_val(ans), buf, ret);
  free(buf);

  CAMLreturn(ans);
}

CAMLprim value caml_bjack_get_position(value d, value pos_type, value type) {
  CAMLparam3(d, pos_type, type);
  CAMLreturn(Val_long(
      JACK_GetPosition(Bjack_drv_val(d), Int_val(pos_type), Int_val(type))));
}

CAMLprim value caml_bjack_set_position(value d, value pos_type,
                                       value position) {
  CAMLparam3(d, pos_type, position);

  JACK_SetPosition(Bjack_drv_val(d), Int_val(pos_type), Int_val(position));

  CAMLreturn(Val_unit);
}

CAMLprim value caml_bjack_get_output_latency(value d) {
  CAMLparam1(d);
  CAMLreturn(Val_long(JACK_GetJackOutputLatency(Bjack_drv_val(d))));
}

CAMLprim value caml_bjack_get_input_latency(value d) {
  CAMLparam1(d);
  CAMLreturn(Val_long(JACK_GetJackInputLatency(Bjack_drv_val(d))));
}

CAMLprim value caml_bjack_set_state(value d, value state) {
  CAMLparam2(d, state);
  int ret = JACK_SetState(Bjack_drv_val(d), Int_val(state));
  if (ret != 0)
    caml_failwith("state");

  CAMLreturn(Val_unit);
}

CAMLprim value caml_bjack_get_state(value d) {
  CAMLparam1(d);

  CAMLreturn(Val_int(JACK_GetState(Bjack_drv_val(d))));
}

CAMLprim value caml_bjack_get_max_output_buffered_bytes(value d) {
  CAMLparam1(d);
  CAMLreturn(Val_long(JACK_GetMaxOutputBufferedBytes(Bjack_drv_val(d))));
}

CAMLprim value caml_bjack_get_max_input_buffered_bytes(value d) {
  CAMLparam1(d);
  CAMLreturn(Val_long(JACK_GetMaxInputBufferedBytes(Bjack_drv_val(d))));
}

CAMLprim value caml_bjack_get_jack_buffered_bytes(value d) {
  CAMLparam1(d);
  CAMLreturn(Val_long(JACK_GetJackBufferedBytes(Bjack_drv_val(d))));
}

CAMLprim value caml_bjack_set_volume_effect_type(value d, value type) {
  CAMLparam2(d, type);
  CAMLreturn(
      Val_int(JACK_SetVolumeEffectType(Bjack_drv_val(d), Int_val(type))));
}

CAMLprim value caml_bjack_set_all_volume(value d, value volume) {
  CAMLparam2(d, volume);
  int ret = JACK_SetAllVolume(Bjack_drv_val(d), Unsigned_int_val(volume));

  if (ret != 0)
    caml_failwith("volume");

  CAMLreturn(Val_unit);
}

CAMLprim value caml_bjack_set_channel_volume(value d, value channel,
                                             value volume) {
  CAMLparam3(d, channel, volume);
  int ret = JACK_SetVolumeForChannel(
      Bjack_drv_val(d), Unsigned_int_val(channel), Unsigned_int_val(volume));

  if (ret != 0)
    caml_failwith("volume");

  CAMLreturn(Val_unit);
}

CAMLprim value caml_bjack_get_channel_volume(value d, value channel) {
  CAMLparam2(d, channel);
  unsigned int volume;
  JACK_GetVolumeForChannel(Bjack_drv_val(d), Unsigned_int_val(channel),
                           &volume);

  CAMLreturn(Val_long(volume));
}

CAMLprim value caml_bjack_get_output_bytes_per_second(value d) {
  CAMLparam1(d);
  CAMLreturn(Val_long(JACK_GetOutputBytesPerSecond(Bjack_drv_val(d))));
}

CAMLprim value caml_bjack_get_input_bytes_per_second(value d) {
  CAMLparam1(d);
  CAMLreturn(Val_long(JACK_GetInputBytesPerSecond(Bjack_drv_val(d))));
}

CAMLprim value caml_bjack_get_bytes_stored(value d) {
  CAMLparam1(d);
  CAMLreturn(Val_long(JACK_GetBytesStored(Bjack_drv_val(d))));
}

CAMLprim value caml_bjack_get_bytes_free_space(value d) {
  CAMLparam1(d);
  CAMLreturn(Val_long(JACK_GetBytesFreeSpace(Bjack_drv_val(d))));
}

CAMLprim value caml_bjack_get_bytes_used_space(value d) {
  CAMLparam1(d);
  CAMLreturn(Val_long(JACK_GetBytesUsedSpace(Bjack_drv_val(d))));
}

CAMLprim value caml_bjack_get_bytes_per_output_frame(value d) {
  CAMLparam1(d);
  CAMLreturn(Val_long(JACK_GetBytesPerOutputFrame(Bjack_drv_val(d))));
}

CAMLprim value caml_bjack_get_bytes_per_input_frame(value d) {
  CAMLparam1(d);
  CAMLreturn(Val_long(JACK_GetBytesPerInputFrame(Bjack_drv_val(d))));
}

CAMLprim value caml_bjack_get_num_input_channels(value d) {
  CAMLparam1(d);
  CAMLreturn(Val_int(JACK_GetNumInputChannels(Bjack_drv_val(d))));
}

CAMLprim value caml_bjack_get_num_output_channels(value d) {
  CAMLparam1(d);
  CAMLreturn(Val_int(JACK_GetNumOutputChannels(Bjack_drv_val(d))));
}

CAMLprim value caml_bjack_get_sample_rate(value d) {
  CAMLparam1(d);
  CAMLreturn(Val_long(JACK_GetSampleRate(Bjack_drv_val(d))));
}

CAMLprim value caml_bjack_set_conversion_function(value n) {
  CAMLparam1(n);
  JACK_SetSampleRateConversionFunction(Int_val(n));
  CAMLreturn(Val_unit);
}
