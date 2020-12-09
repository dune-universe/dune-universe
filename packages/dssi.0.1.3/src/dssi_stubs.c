/*
 * OCaml bindings for DSSI
 *
 * Copyright 2003-2011 Savonet team
 *
 * This file is part of Ocaml-mad.
 * The libmad's stubs are based on madlld (see README for details).
 *
 * Ocaml-mad is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-mad is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-mad; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>

#include <assert.h>
#include <dlfcn.h>

#include <ladspa.h>
#include <ocaml_ladspa.h>

#include "ocaml_dssi_config.h"
#ifdef HAS_DSSI
#include <dssi.h>
#else
#include "dssi.h"
#endif

#define Descr_val(v) ((DSSI_Descriptor *)v)
#define Val_descr(d) ((value)d)

CAMLprim value ocaml_dssi_open(value fname) {
  void *handle = dlopen(String_val(fname), RTLD_LAZY);
  DSSI_Descriptor_Function dssi_descriptor;

  if (!handle)
    caml_raise_constant(*caml_named_value("ocaml_dssi_exn_not_a_plugin"));
  dssi_descriptor =
      (DSSI_Descriptor_Function)dlsym((void *)handle, "dssi_descriptor");
  if (dlerror() != NULL || !dssi_descriptor) {
    dlclose(handle);
    caml_raise_constant(*caml_named_value("ocaml_dssi_exn_not_a_plugin"));
  }

  return (value)handle;
}

CAMLprim value ocaml_dssi_close(value handle) {
  dlclose((void *)handle);

  return Val_unit;
}

CAMLprim value ocaml_dssi_descriptor(value handle, value n) {
  DSSI_Descriptor_Function dssi_descriptor =
      (DSSI_Descriptor_Function)dlsym((void *)handle, "dssi_descriptor");
  const DSSI_Descriptor *d = dssi_descriptor(Int_val(n));

  if (!d)
    caml_raise_constant(*caml_named_value("ocaml_dssi_exn_not_found"));

  return Val_descr(d);
}

CAMLprim value ocaml_dssi_api_version(value d) {
  return Val_int(Descr_val(d)->DSSI_API_Version);
}

CAMLprim value ocaml_dssi_ladspa(value d) {
  return Val_LADSPA_descr(Descr_val(d)->LADSPA_Plugin);
}

CAMLprim value ocaml_dssi_configure(value d, value i, value key, value v) {
  char *ans;

  if (!Descr_val(d)->configure)
    caml_raise_constant(*caml_named_value("ocaml_dssi_exn_not_implemented"));
  ans = Descr_val(d)->configure(Instance_val(i)->handle, String_val(key),
                                String_val(v));
  /* TODO */
  assert(ans);
  value ret = caml_copy_string(ans);
  free(ans);

  return ret;
}

CAMLprim value ocaml_dssi_get_program(value d, value i, value n) {
  CAMLparam1(d);
  CAMLlocal1(ret);
  const DSSI_Program_Descriptor *ans;

  if (!Descr_val(d)->get_program)
    caml_raise_constant(*caml_named_value("ocaml_dssi_exn_not_implemented"));
  ans = Descr_val(d)->get_program(Instance_val(i)->handle, Int_val(n));
  if (!ans)
    caml_raise_constant(*caml_named_value("ocaml_dssi_exn_not_found"));

  ret = caml_alloc_tuple(3);
  Store_field(ret, 0, Val_int(ans->Bank));
  Store_field(ret, 1, Val_int(ans->Program));
  Store_field(ret, 2, caml_copy_string(ans->Name));
  CAMLreturn(ret);
}

CAMLprim value ocaml_dssi_select_program(value d, value i, value bank,
                                         value program) {
  if (!Descr_val(d)->select_program)
    caml_raise_constant(*caml_named_value("ocaml_dssi_exn_not_implemented"));
  Descr_val(d)->select_program(Instance_val(i)->handle, Int_val(bank),
                               Int_val(program));
  return Val_unit;
}

CAMLprim value ocaml_dssi_get_midi_controller_for_port(value d, value i,
                                                       value port) {
  int ans;

  if (!Descr_val(d)->get_midi_controller_for_port)
    caml_raise_constant(*caml_named_value("ocaml_dssi_exn_not_implemented"));
  ans = Descr_val(d)->get_midi_controller_for_port(Instance_val(i)->handle,
                                                   Int_val(port));

  return Val_int(ans);
}

static snd_seq_event_t *events_of_array(value ev) {
  int event_count = Wosize_val(ev);
  snd_seq_event_t *events = malloc(event_count * sizeof(snd_seq_event_t));
  int i;
  value e;

  for (i = 0; i < event_count; i++) {
    events[i].time.tick = Int_val(Field(Field(ev, i), 0));
    e = Field(Field(ev, i), 1);

    if (Is_block(e)) {
      switch (Tag_val(e)) {
      case 2:
        events[i].type = SND_SEQ_EVENT_NOTEON;
        events[i].data.note.channel = Int_val(Field(e, 0));
        events[i].data.note.note = Int_val(Field(e, 1));
        events[i].data.note.velocity = Int_val(Field(e, 2));
        break;

      case 3:
        events[i].type = SND_SEQ_EVENT_NOTEOFF;
        events[i].data.note.channel = Int_val(Field(e, 0));
        events[i].data.note.note = Int_val(Field(e, 1));
        events[i].data.note.velocity = Int_val(Field(e, 2));
        break;
      }
    }
  }

  return events;
}

CAMLprim value ocaml_dssi_can_run_synth(value d) {
  return Val_bool(Descr_val(d)->run_synth);
}

CAMLprim value ocaml_dssi_can_run_synth_adding(value d) {
  return Val_bool(Descr_val(d)->run_synth_adding);
}

CAMLprim value ocaml_dssi_run_synth(value d, value vadd, value i, value sc,
                                    value ev) {
  DSSI_Descriptor *descr = Descr_val(d);
  LADSPA_Handle h = Instance_val(i)->handle;
  int sample_count = Int_val(sc);
  unsigned long event_count = Wosize_val(ev);
  snd_seq_event_t *events;
  int add = Bool_val(vadd);

  if ((!add && !Descr_val(d)->run_synth) ||
      (add && !Descr_val(d)->run_synth_adding))
    caml_raise_constant(*caml_named_value("ocaml_dssi_exn_not_implemented"));
  events = events_of_array(ev);

  caml_enter_blocking_section();
  if (add)
    descr->run_synth_adding(h, sample_count, events, event_count);
  else
    descr->run_synth(h, sample_count, events, event_count);
  caml_leave_blocking_section();

  free(events);
  return Val_unit;
}

CAMLprim value ocaml_dssi_can_run_multiple_synths(value d) {
  return Val_bool(Descr_val(d)->run_multiple_synths);
}

CAMLprim value ocaml_dssi_can_run_multiple_synths_adding(value d) {
  return Val_bool(Descr_val(d)->run_multiple_synths_adding);
}

CAMLprim value ocaml_dssi_run_multiple_synths(value d, value vadd, value inst,
                                              value sc, value ev) {
  DSSI_Descriptor *descr = Descr_val(d);
  int sample_count = Int_val(sc);
  int instance_count = Wosize_val(inst);
  LADSPA_Handle *h;
  unsigned long *event_count;
  snd_seq_event_t **events;
  int i;
  int add = Bool_val(vadd);

  if ((!add && !Descr_val(d)->run_multiple_synths) ||
      (add && !Descr_val(d)->run_multiple_synths_adding))
    caml_raise_constant(*caml_named_value("ocaml_dssi_exn_not_implemented"));
  if (Wosize_val(ev) != instance_count)
    caml_invalid_argument(
        "the number of events should be the same as the number of instances");

  h = malloc(instance_count * sizeof(LADSPA_Handle));
  event_count = malloc(instance_count * sizeof(unsigned long));
  events = malloc(instance_count * sizeof(snd_seq_event_t *));
  for (i = 0; i < instance_count; i++) {
    h[i] = Instance_val(Field(inst, i))->handle;
    event_count[i] = Wosize_val(Field(ev, i));
    events[i] = events_of_array(Field(ev, i));
  }

  caml_enter_blocking_section();
  if (add)
    descr->run_multiple_synths_adding(instance_count, h, sample_count, events,
                                      event_count);
  else
    descr->run_multiple_synths(instance_count, h, sample_count, events,
                               event_count);
  caml_leave_blocking_section();

  for (i = 0; i < instance_count; i++)
    free(events[i]);
  free(events);
  free(event_count);
  free(h);
  return Val_unit;
}
