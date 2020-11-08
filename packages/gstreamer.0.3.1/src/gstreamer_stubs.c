#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#ifndef Bytes_val
#define Bytes_val String_val
#endif

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include <glib.h>
#include <gst/app/gstappsink.h>
#include <gst/app/gstappsrc.h>
#include <gst/gst.h>
#include <gst/gstclock.h>
#include <gst/gsttypefind.h>

#include <pthread.h>

static pthread_key_t ocaml_c_thread_key;
static pthread_once_t ocaml_c_thread_key_once = PTHREAD_ONCE_INIT;

static void ocaml_gstreamer_on_thread_exit(void *key) {
  caml_c_thread_unregister();
}

static void ocaml_gstreamer_make_key() {
  pthread_key_create(&ocaml_c_thread_key, ocaml_gstreamer_on_thread_exit);
}

static void ocaml_gstreamer_register_thread() {
  static int initialized = 1;
  void *ptr;

  pthread_once(&ocaml_c_thread_key_once, ocaml_gstreamer_make_key);

  if ((ptr = pthread_getspecific(ocaml_c_thread_key)) == NULL) {
    pthread_setspecific(ocaml_c_thread_key, (void *)&initialized);
    caml_c_thread_register();
  }
}

CAMLprim value ocaml_gstreamer_init(value _argv) {
  CAMLparam1(_argv);
  char **argv = NULL;
  int argc = 0;
  int len, i;

  if (Is_block(_argv)) {
    _argv = Field(_argv, 0);
    argc = Wosize_val(_argv);
    argv = malloc(argc * sizeof(char *));
    for (i = 0; i < argc; i++) {
      len = caml_string_length(Field(_argv, i));
      argv[i] = malloc(len + 1);
      memcpy(argv[i], String_val(Field(_argv, i)), len + 1);
    }
  }

  caml_release_runtime_system();
  gst_init(&argc, &argv);
  for (i = 0; i < argc; i++)
    free(argv[i]);
  free(argv);
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_gstreamer_deinit(value unit) {
  CAMLparam0();

  caml_release_runtime_system();
  gst_deinit();
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_gstreamer_version(value unit) {
  CAMLparam0();
  CAMLlocal1(ans);

  unsigned int major, minor, micro, nano;
  gst_version(&major, &minor, &micro, &nano);

  ans = caml_alloc_tuple(4);
  Store_field(ans, 0, Val_int(major));
  Store_field(ans, 1, Val_int(minor));
  Store_field(ans, 2, Val_int(micro));
  Store_field(ans, 3, Val_int(nano));

  CAMLreturn(ans);
}

CAMLprim value ocaml_gstreamer_version_string(value unit) {
  CAMLparam0();
  CAMLreturn(caml_copy_string(gst_version_string()));
}

/***** Format *****/

#define formats_len 6
static const GstFormat formats[formats_len] = {
    GST_FORMAT_UNDEFINED, GST_FORMAT_DEFAULT, GST_FORMAT_BYTES,
    GST_FORMAT_TIME,      GST_FORMAT_BUFFERS, GST_FORMAT_PERCENT};

static GstFormat format_val(value v) { return formats[Int_val(v)]; }

/*
static value val_format(GstFormat fmt)
{
  int i;
  for (i = 0; i < formats_len; i++)
    if (fmt == formats[i])
      return Val_int(i);
  assert(0);
}
*/

CAMLprim value ocaml_gstreamer_format_to_string(value _f) {
  GstFormat f = format_val(_f);
  return caml_copy_string(gst_format_get_name(f));
}

/***** Event *****/

#define seek_flags_len 9
static const GstSeekFlags seek_flags[seek_flags_len] = {
    GST_SEEK_FLAG_NONE,        GST_SEEK_FLAG_FLUSH,
    GST_SEEK_FLAG_ACCURATE,    GST_SEEK_FLAG_KEY_UNIT,
    GST_SEEK_FLAG_SEGMENT,     GST_SEEK_FLAG_SKIP,
    GST_SEEK_FLAG_SNAP_BEFORE, GST_SEEK_FLAG_SNAP_AFTER,
    GST_SEEK_FLAG_SNAP_NEAREST};

static GstSeekFlags seek_flags_val(value v) { return seek_flags[Int_val(v)]; }

/***** Element ******/

#define Element_val(v) (*(GstElement **)Data_custom_val(v))

static void finalize_element(value v) {
  GstElement *e = Element_val(v);
  gst_object_unref(e);
}

static struct custom_operations element_ops = {
    "ocaml_gstreamer_element", finalize_element,
    custom_compare_default,    custom_hash_default,
    custom_serialize_default,  custom_deserialize_default};

#define value_of_element(e, ans)                                               \
  if (!e)                                                                      \
    caml_raise_not_found();                                                    \
  ans = caml_alloc_custom(&element_ops, sizeof(GstElement *), 0, 1);           \
  Element_val(ans) = e;

CAMLprim value ocaml_gstreamer_element_set_property_string(value e, value l,
                                                           value v) {
  CAMLparam3(e, l, v);
  g_object_set(Element_val(e), String_val(l), String_val(v), NULL);
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_gstreamer_element_set_property_int(value e, value l,
                                                        value v) {
  CAMLparam3(e, l, v);
  g_object_set(Element_val(e), String_val(l), Int_val(v), NULL);
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_gstreamer_element_set_property_bool(value e, value l,
                                                         value v) {
  CAMLparam3(e, l, v);
  g_object_set(Element_val(e), String_val(l), Bool_val(v), NULL);
  CAMLreturn(Val_unit);
}

#define states_len 5
static const GstState states[states_len] = {
    GST_STATE_VOID_PENDING, GST_STATE_NULL, GST_STATE_READY, GST_STATE_PAUSED,
    GST_STATE_PLAYING};

static GstState state_of_val(value v) {
  int n = Int_val(v);
  assert(n < states_len);
  return states[n];
}

static value val_of_state(GstState state) {
  int i;
  for (i = 0; i < states_len; i++)
    if (state == states[i])
      return Val_int(i);
  assert(0);
}

static value value_of_state_change_return(GstStateChangeReturn ret) {
  switch (ret) {
  case GST_STATE_CHANGE_FAILURE:
    caml_raise_constant(*caml_named_value("gstreamer_exn_failed"));

  case GST_STATE_CHANGE_SUCCESS:
    return Val_int(0);

  case GST_STATE_CHANGE_ASYNC:
    return Val_int(1);

  case GST_STATE_CHANGE_NO_PREROLL:
    return Val_int(2);

  default:
    assert(0);
  }
}

CAMLprim value ocaml_gstreamer_element_string_of_state(value state) {
  CAMLparam1(state);
  CAMLreturn(caml_copy_string(gst_element_state_get_name(state_of_val(state))));
}

CAMLprim value ocaml_gstreamer_element_set_state(value _e, value _s) {
  CAMLparam2(_e, _s);
  GstElement *e = Element_val(_e);
  GstState s = state_of_val(_s);
  GstStateChangeReturn ret;

  caml_release_runtime_system();
  ret = gst_element_set_state(e, s);
  caml_acquire_runtime_system();

  CAMLreturn(value_of_state_change_return(ret));
}

CAMLprim value ocaml_gstreamer_element_get_state(value _e) {
  CAMLparam1(_e);
  CAMLlocal1(ans);
  GstElement *e = Element_val(_e);
  GstStateChangeReturn ret;
  GstState state, pending;
  GstClockTime timeout = GST_CLOCK_TIME_NONE; /* TODO */

  caml_release_runtime_system();
  ret = gst_element_get_state(e, &state, &pending, timeout);
  caml_acquire_runtime_system();

  ans = caml_alloc_tuple(3);
  Store_field(ans, 0, value_of_state_change_return(ret));
  Store_field(ans, 1, val_of_state(state));
  Store_field(ans, 2, val_of_state(pending));
  CAMLreturn(ans);
}

CAMLprim value ocaml_gstreamer_element_link(value _src, value _dst) {
  CAMLparam2(_src, _dst);
  GstElement *src = Element_val(_src);
  GstElement *dst = Element_val(_dst);
  gboolean ret;

  caml_release_runtime_system();
  ret = gst_element_link(src, dst);
  caml_acquire_runtime_system();

  if (!ret)
    caml_raise_constant(*caml_named_value("gstreamer_exn_failed"));
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_gstreamer_element_position(value _e, value _fmt) {
  CAMLparam2(_e, _fmt);
  GstElement *e = Element_val(_e);
  GstFormat fmt = format_val(_fmt);
  gint64 pos;
  gboolean ret;

  caml_release_runtime_system();
  ret = gst_element_query_position(e, fmt, &pos);
  caml_acquire_runtime_system();

  if (!ret)
    caml_raise_constant(*caml_named_value("gstreamer_exn_failed"));
  CAMLreturn(caml_copy_int64(pos));
}

CAMLprim value ocaml_gstreamer_element_duration(value _e, value _fmt) {
  CAMLparam2(_e, _fmt);
  GstElement *e = Element_val(_e);
  GstFormat fmt = format_val(_fmt);
  gint64 dur;
  gboolean ret;

  caml_release_runtime_system();
  ret = gst_element_query_duration(e, fmt, &dur);
  caml_acquire_runtime_system();

  if (!ret)
    caml_raise_constant(*caml_named_value("gstreamer_exn_failed"));
  CAMLreturn(caml_copy_int64(dur));
}

CAMLprim value ocaml_gstreamer_element_seek_simple(value _e, value _fmt,
                                                   value _flags, value _pos) {
  CAMLparam4(_e, _fmt, _flags, _pos);
  GstElement *e = Element_val(_e);
  GstFormat fmt = format_val(_fmt);
  GstSeekFlags flags = 0;
  gint64 pos = Int64_val(_pos);
  gboolean ret;
  int i;

  for (i = 0; i < Wosize_val(_flags); i++)
    flags |= seek_flags_val(Field(_flags, i));

  caml_release_runtime_system();
  ret = gst_element_seek_simple(e, fmt, flags, pos);
  caml_acquire_runtime_system();

  if (!ret)
    caml_raise_constant(*caml_named_value("gstreamer_exn_failed"));
  CAMLreturn(Val_unit);
}

/***** Element factory *****/

CAMLprim value ocaml_gstreamer_element_factory_make(value factname,
                                                    value name) {
  CAMLparam2(factname, name);
  CAMLlocal1(ans);
  GstElement *e;

  e = gst_element_factory_make(String_val(factname), String_val(name));
  value_of_element(e, ans);

  CAMLreturn(ans);
}

/**** Message *****/

#define message_types_len 33
static const GstMessageType message_types[message_types_len] = {
    GST_MESSAGE_UNKNOWN,
    GST_MESSAGE_EOS,
    GST_MESSAGE_ERROR,
    GST_MESSAGE_WARNING,
    GST_MESSAGE_INFO,
    GST_MESSAGE_TAG,
    GST_MESSAGE_BUFFERING,
    GST_MESSAGE_STATE_CHANGED,
    GST_MESSAGE_STATE_DIRTY,
    GST_MESSAGE_STEP_DONE,
    GST_MESSAGE_CLOCK_PROVIDE,
    GST_MESSAGE_CLOCK_LOST,
    GST_MESSAGE_NEW_CLOCK,
    GST_MESSAGE_STRUCTURE_CHANGE,
    GST_MESSAGE_STREAM_STATUS,
    GST_MESSAGE_APPLICATION,
    GST_MESSAGE_ELEMENT,
    GST_MESSAGE_SEGMENT_START,
    GST_MESSAGE_SEGMENT_DONE,
    GST_MESSAGE_DURATION_CHANGED,
    GST_MESSAGE_LATENCY,
    GST_MESSAGE_ASYNC_START,
    GST_MESSAGE_ASYNC_DONE,
    GST_MESSAGE_REQUEST_STATE,
    GST_MESSAGE_STEP_START,
    GST_MESSAGE_QOS,
    GST_MESSAGE_PROGRESS,
    GST_MESSAGE_TOC,
    GST_MESSAGE_RESET_TIME,
    GST_MESSAGE_STREAM_START,
    GST_MESSAGE_NEED_CONTEXT,
    GST_MESSAGE_HAVE_CONTEXT,
    GST_MESSAGE_ANY};

static GstMessageType message_type_of_int(int n) { return message_types[n]; }

static int int_of_message_type(GstMessageType msg) {
  int i;
  for (i = 0; i < message_types_len; i++) {
    if (msg == message_types[i])
      return i;
  }
  printf("error in message: %d\n", msg);
  assert(0);
}

#define Message_val(v) (*(GstMessage **)Data_custom_val(v))

static void finalize_message(value v) {
  GstMessage *e = Message_val(v);
  gst_message_unref(e);
}

static struct custom_operations message_ops = {
    "ocaml_gstreamer_message", finalize_message,
    custom_compare_default,    custom_hash_default,
    custom_serialize_default,  custom_deserialize_default};

#define value_of_message(msg, ans)                                             \
  ans = caml_alloc_custom(&message_ops, sizeof(GstMessage *), 0, 1);           \
  Message_val(ans) = msg;

CAMLprim value ocaml_gstreamer_message_type(value _msg) {
  CAMLparam1(_msg);
  GstMessage *msg = Message_val(_msg);
  CAMLreturn(Val_int(int_of_message_type(GST_MESSAGE_TYPE(msg))));
}

CAMLprim value ocaml_gstreamer_message_source_name(value _msg) {
  CAMLparam1(_msg);
  GstMessage *msg = Message_val(_msg);
  CAMLreturn(caml_copy_string(GST_MESSAGE_SRC_NAME(msg)));
}

CAMLprim value ocaml_gstreamer_message_parse_error(value _err) {
  CAMLparam1(_err);
  CAMLlocal1(ans);
  GError *err = NULL;

  gst_message_parse_error(Message_val(_err), &err, NULL);

  ans = caml_copy_string(err->message);
  g_error_free(err);

  CAMLreturn(ans);
}

CAMLprim value ocaml_gstreamer_message_parse_buffering(value _msg) {
  CAMLparam1(_msg);
  gint percent;
  gst_message_parse_buffering(Message_val(_msg), &percent);
  CAMLreturn(Val_int(percent));
}

CAMLprim value ocaml_gstreamer_message_parse_state_changed(value _msg) {
  CAMLparam1(_msg);
  CAMLlocal1(ans);
  GstState old_state, new_state, pending;

  gst_message_parse_state_changed(Message_val(_msg), &old_state, &new_state,
                                  &pending);

  ans = caml_alloc_tuple(3);
  Store_field(ans, 0, val_of_state(old_state));
  Store_field(ans, 1, val_of_state(new_state));
  Store_field(ans, 2, val_of_state(pending));

  CAMLreturn(ans);
}

CAMLprim value ocaml_gstreamer_message_parse_tag(value _msg) {
  CAMLparam1(_msg);
  CAMLlocal4(v, s, t, ans);
  GstMessage *msg = Message_val(_msg);
  GstTagList *tags = NULL;
  const GValue *val;
  const gchar *tag;
  int taglen;
  int i, j, n;

  caml_release_runtime_system();
  gst_message_parse_tag(msg, &tags);
  taglen = gst_tag_list_n_tags(tags);
  caml_acquire_runtime_system();

  ans = caml_alloc_tuple(taglen);
  for (i = 0; i < taglen; i++) {
    t = caml_alloc_tuple(2);

    // Tag name
    tag = gst_tag_list_nth_tag_name(tags, i);
    Store_field(t, 0, caml_copy_string(tag));

    // Tag fields
    n = gst_tag_list_get_tag_size(tags, tag);
    v = caml_alloc_tuple(n);
    for (j = 0; j < n; j++) {
      val = gst_tag_list_get_value_index(tags, tag, j);
      if (G_VALUE_HOLDS_STRING(val)) {
        s = caml_copy_string(g_value_get_string(val));
      } else if (GST_VALUE_HOLDS_DATE_TIME(val)) {
        GstDateTime *dt = g_value_get_boxed(val);
        gchar *dt_str = gst_date_time_to_iso8601_string(dt);
        s = caml_copy_string(dt_str);
        g_free(dt_str);
      } else {
        // TODO: better typed handling of non-string values?
        char *vc = g_strdup_value_contents(val);
        s = caml_copy_string(vc);
        free(vc);
      }
      Store_field(v, j, s);
    }
    Store_field(t, 1, v);

    Store_field(ans, i, t);
  }

  gst_tag_list_unref(tags);

  CAMLreturn(ans);
}

/**** Main Loop ****/

#define Loop_val(v) (*(GMainLoop **)Data_custom_val(v))

static void finalize_loop(value v) {
  GMainLoop *loop = Loop_val(v);
  g_main_loop_unref(loop);
}

static struct custom_operations loop_ops = {
    "ocaml_gstreamer_loop",   finalize_loop,
    custom_compare_default,   custom_hash_default,
    custom_serialize_default, custom_deserialize_default};

CAMLprim value ocaml_gstreamer_loop_create(value unit) {
  CAMLparam0();
  CAMLlocal1(ans);
  GMainLoop *loop = g_main_loop_new(NULL, FALSE);

  if (!loop)
    caml_raise_out_of_memory();

  ans = caml_alloc_custom(&loop_ops, sizeof(GMainLoop *), 0, 1);
  Loop_val(ans) = loop;

  CAMLreturn(ans);
}

CAMLprim value ocaml_gstreamer_loop_run(value l) {
  CAMLparam1(l);
  GMainLoop *loop = Loop_val(l);

  caml_release_runtime_system();
  g_main_loop_run(loop);
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_gstreamer_loop_quit(value l) {
  CAMLparam1(l);
  GMainLoop *loop = Loop_val(l);

  caml_release_runtime_system();
  g_main_loop_quit(loop);
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

/**** Bus ****/

#define Bus_val(v) (*(GstBus **)Data_custom_val(v))

static void finalize_bus(value v) {
  GstBus *bus = Bus_val(v);
  gst_object_unref(bus);
}

static struct custom_operations bus_ops = {
    "ocaml_gstreamer_bus",    finalize_bus,
    custom_compare_default,   custom_hash_default,
    custom_serialize_default, custom_deserialize_default};

CAMLprim value ocaml_gstreamer_bus_of_element(value _e) {
  CAMLparam1(_e);
  CAMLlocal1(ans);
  GstElement *e = Element_val(_e);

  ans = caml_alloc_custom(&bus_ops, sizeof(GstBus *), 0, 1);
  Bus_val(ans) = gst_element_get_bus(e);

  CAMLreturn(ans);
}

CAMLprim value ocaml_gstreamer_bus_pop_filtered(value _bus, value _filter) {
  CAMLparam2(_bus, _filter);
  CAMLlocal2(ans, ret);
  GstBus *bus = Bus_val(_bus);
  GstMessageType filter = 0;
  GstMessage *msg;
  int i;

  for (i = 0; i < Wosize_val(_filter); i++)
    filter |= message_type_of_int(Int_val(Field(_filter, i)));

  caml_release_runtime_system();
  msg = gst_bus_pop_filtered(bus, filter);
  caml_acquire_runtime_system();

  if (!msg)
    ans = Val_int(0);
  else {
    value_of_message(msg, ret);
    ans = caml_alloc_tuple(1);
    Store_field(ans, 0, ret);
  }

  CAMLreturn(ans);
}

CAMLprim value ocaml_gstreamer_bus_timed_pop_filtered(value _bus,
                                                      value _timeout,
                                                      value _filter) {
  CAMLparam3(_bus, _timeout, _filter);
  CAMLlocal1(ans);
  GstBus *bus = Bus_val(_bus);
  GstClockTime timeout = GST_CLOCK_TIME_NONE;
  GstMessageType filter = 0;
  GstMessage *msg;
  int i;

  if (Is_block(_timeout)) {
    timeout = (GstClockTime)Int64_val(Field(_timeout, 0));
  }

  for (i = 0; i < Wosize_val(_filter); i++)
    filter |= message_type_of_int(Int_val(Field(_filter, i)));

  caml_release_runtime_system();
  msg = gst_bus_timed_pop_filtered(bus, timeout, filter);
  caml_acquire_runtime_system();

  if (!msg)
    caml_raise_constant(*caml_named_value("gstreamer_exn_timeout"));

  value_of_message(msg, ans);

  CAMLreturn(ans);
}

/***** Bin ******/

#define Bin_val(v) GST_BIN(Element_val(v))

CAMLprim value ocaml_gstreamer_bin_add(value _bin, value _e) {
  CAMLparam2(_bin, _e);
  GstBin *bin = Bin_val(_bin);
  GstElement *e = Element_val(_e);
  gboolean ret;

  caml_release_runtime_system();
  /* gst_bin_add takes ownership but we want to keep it alive
   * and clean it with the Gc. */
  gst_object_ref(e);
  ret = gst_bin_add(bin, e);
  caml_acquire_runtime_system();

  if (!ret)
    caml_raise_constant(*caml_named_value("gstreamer_exn_failed"));
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_gstreamer_bin_get_by_name(value _bin, value _name) {
  CAMLparam2(_bin, _name);
  CAMLlocal1(ans);
  GstBin *bin = Bin_val(_bin);
  GstElement *e;

  e = gst_bin_get_by_name(bin, String_val(_name));

  value_of_element(e, ans);

  CAMLreturn(ans);
}

/***** Pipeline *****/

#define Pipeline_val(v) GST_PIPELINE(Element_val(v))

CAMLprim value ocaml_gstreamer_pipeline_create(value s) {
  CAMLparam1(s);
  CAMLlocal1(ans);
  GstElement *e;

  e = gst_pipeline_new(String_val(s));

  value_of_element(e, ans);

  CAMLreturn(ans);
}

CAMLprim value ocaml_gstreamer_pipeline_parse_launch(value s) {
  CAMLparam1(s);
  CAMLlocal2(ans, _err);
  GError *err = NULL;
  GstElement *e;

  e = gst_parse_launch(String_val(s), &err);
  if (err || e == NULL) {
    _err = caml_copy_string(err->message);
    g_error_free(err);
    caml_raise_with_arg(*caml_named_value("gstreamer_exn_error"), _err);
  }

  value_of_element(e, ans);

  CAMLreturn(ans);
}

/***** Buffer ******/

#define Buffer_val(v) (*(GstBuffer **)Data_custom_val(v))

static void finalize_buffer(value v) {
  GstBuffer *b = Buffer_val(v);
  gst_buffer_unref(b);
}

static struct custom_operations buffer_ops = {
    "ocaml_gstreamer_buffer", finalize_buffer,
    custom_compare_default,   custom_hash_default,
    custom_serialize_default, custom_deserialize_default};

#define value_of_buffer(b, ans)                                                \
  ans = caml_alloc_custom(&buffer_ops, sizeof(GstElement *), 0, 1);            \
  Buffer_val(ans) = b;

#define buffer_fill(gstbuf, data, ofs, len)                                    \
  GstMapInfo map;                                                              \
  gboolean bret;                                                               \
  caml_release_runtime_system();                                               \
  bret = gst_buffer_map(gstbuf, &map, GST_MAP_WRITE);                          \
  caml_acquire_runtime_system();                                               \
  if (!bret)                                                                   \
    caml_raise_out_of_memory();                                                \
  memcpy(map.data, (unsigned char *)data + ofs, len);                          \
  caml_release_runtime_system();                                               \
  gst_buffer_unmap(gstbuf, &map);                                              \
  caml_acquire_runtime_system();

CAMLprim value ocaml_gstreamer_buffer_create(value _len) {
  CAMLparam0();
  CAMLlocal1(ans);

  int len = Int_val(_len);
  GstBuffer *gstbuf;

  caml_release_runtime_system();
  gstbuf = gst_buffer_new_allocate(NULL, len, NULL);
  caml_acquire_runtime_system();

  if (!gstbuf)
    caml_raise_out_of_memory();

  value_of_buffer(gstbuf, ans);
  CAMLreturn(ans);
}

CAMLprim value ocaml_gstreamer_buffer_set_data(value _buf, value _bufoff,
                                               value _ba, value _baoff,
                                               value _len) {
  CAMLparam2(_buf, _ba);
  CAMLlocal1(ans);

  GstBuffer *buf = Buffer_val(_buf);
  int buf_off = Int_val(_bufoff);
  unsigned char *data = Caml_ba_data_val(_ba);
  int data_off = Int_val(_baoff);
  int len = Int_val(_len);
  GstMapInfo map;
  gboolean bret;

  caml_release_runtime_system();
  bret = gst_buffer_map(buf, &map, GST_MAP_WRITE);
  caml_acquire_runtime_system();

  if (!bret)
    caml_raise_out_of_memory();

  caml_release_runtime_system();
  memcpy(map.data + buf_off, data + data_off, len);
  gst_buffer_unmap(buf, &map);
  caml_acquire_runtime_system();

  CAMLreturn(ans);
}

CAMLprim value ocaml_gstreamer_buffer_of_string(value s, value _off,
                                                value _len) {
  CAMLparam1(s);
  CAMLlocal1(ans);

  int bufoff = Int_val(_off);
  int buflen = Int_val(_len);
  GstBuffer *gstbuf;
  char *data;

  assert(buflen + bufoff <= caml_string_length(s));

  caml_release_runtime_system();
  gstbuf = gst_buffer_new_allocate(NULL, buflen, NULL);
  caml_acquire_runtime_system();

  if (!gstbuf)
    caml_raise_out_of_memory();

  data = (char *)Bytes_val(s);
  buffer_fill(gstbuf, data, bufoff, buflen);

  value_of_buffer(gstbuf, ans);

  CAMLreturn(ans);
}

CAMLprim value ocaml_gstreamer_buffer_of_data(value _ba, value _off,
                                              value _len) {
  CAMLparam1(_ba);
  CAMLlocal1(ans);

  int bufoff = Int_val(_off);
  int buflen = Int_val(_len);
  GstBuffer *gstbuf;
  char *data;

  assert(buflen + bufoff <= Caml_ba_array_val(_ba)->dim[0]);

  caml_release_runtime_system();
  gstbuf = gst_buffer_new_allocate(NULL, buflen, NULL);
  caml_acquire_runtime_system();

  if (!gstbuf)
    caml_raise_out_of_memory();

  data = Caml_ba_data_val(_ba);
  buffer_fill(gstbuf, data, bufoff, buflen);

  value_of_buffer(gstbuf, ans);

  CAMLreturn(ans);
}

CAMLprim value ocaml_gstreamer_buffer_of_data_list(value dol) {
  CAMLparam1(dol);
  CAMLlocal2(tmp, ans);

  int buflen = 0;

  // Compute the total length;
  tmp = dol;
  while (Is_block(tmp)) {
    buflen += Int_val(Field(Field(tmp, 0), 2));
    tmp = Field(tmp, 1);
  }

  GstBuffer *gstbuf;

  caml_release_runtime_system();
  gstbuf = gst_buffer_new_allocate(NULL, buflen, NULL);
  caml_acquire_runtime_system();

  if (!gstbuf)
    caml_raise_out_of_memory();

  tmp = dol;
  GstMapInfo map;
  gboolean bret;
  int bufoff = 0;

  caml_release_runtime_system();
  bret = gst_buffer_map(gstbuf, &map, GST_MAP_WRITE);
  caml_acquire_runtime_system();
  if (!bret)
    caml_raise_out_of_memory();

  while (Is_block(tmp)) {
    unsigned char *data = Caml_ba_data_val(Field(Field(tmp, 0), 0));
    int off = Int_val(Field(Field(tmp, 0), 1));
    int len = Int_val(Field(Field(tmp, 0), 2));
    assert(off + len <= Caml_ba_array_val(Field(Field(tmp, 0), 0))->dim[0]);
    memcpy(map.data + bufoff, data + off, len);
    bufoff += len;
    tmp = Field(tmp, 1);
  }

  caml_release_runtime_system();
  gst_buffer_unmap(gstbuf, &map);
  caml_acquire_runtime_system();

  value_of_buffer(gstbuf, ans);
  CAMLreturn(ans);
}

CAMLprim value ocaml_gstreamer_buffer_to_string(value _buf) {
  CAMLparam1(_buf);
  CAMLlocal1(ans);
  GstBuffer *buf = Buffer_val(_buf);
  GstMapInfo map;

  caml_release_runtime_system();
  gboolean ret = gst_buffer_map(buf, &map, GST_MAP_READ);
  caml_acquire_runtime_system();

  if (!ret)
    caml_raise_out_of_memory();

  intnat len = map.size;

  ans = caml_alloc_string(len);
  memcpy(Bytes_val(ans), map.data, len);

  caml_release_runtime_system();
  gst_buffer_unmap(buf, &map);
  caml_acquire_runtime_system();

  CAMLreturn(ans);
}

CAMLprim value ocaml_gstreamer_buffer_to_data(value _buf) {
  CAMLparam1(_buf);
  CAMLlocal1(ans);
  GstBuffer *buf = Buffer_val(_buf);
  GstMapInfo map;

  caml_release_runtime_system();
  gboolean ret = gst_buffer_map(buf, &map, GST_MAP_READ);
  caml_acquire_runtime_system();

  if (!ret)
    caml_raise_out_of_memory();

  intnat len = map.size;

  ans = caml_ba_alloc(CAML_BA_C_LAYOUT | CAML_BA_UINT8, 1, NULL, &len);
  memcpy(Caml_ba_data_val(ans), map.data, len);

  caml_release_runtime_system();
  gst_buffer_unmap(buf, &map);
  caml_acquire_runtime_system();

  CAMLreturn(ans);
}

CAMLprim value ocaml_gstreamer_buffer_set_presentation_time(value _buf,
                                                            value _t) {
  CAMLparam2(_buf, _t);
  GstBuffer *b = Buffer_val(_buf);
  GstClockTime t = Int64_val(_t);

  b->pts = t;

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_gstreamer_buffer_set_decoding_time(value _buf, value _t) {
  CAMLparam2(_buf, _t);
  GstBuffer *b = Buffer_val(_buf);
  GstClockTime t = Int64_val(_t);

  b->dts = t;

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_gstreamer_buffer_set_duration(value _buf, value _t) {
  CAMLparam2(_buf, _t);
  GstBuffer *b = Buffer_val(_buf);
  GstClockTime t = Int64_val(_t);

  b->duration = t;

  CAMLreturn(Val_unit);
}

/***** Appsrc *****/

typedef struct {
  GstAppSrc *appsrc;
  value element;
  value need_data_cb;   // Callback function
  gulong need_data_hid; // Callback handler ID
} appsrc;

#define Appsrc_val(v) (*(appsrc **)Data_custom_val(v))

static void disconnect_need_data(appsrc *as) {
  if (as->need_data_hid) {
    g_signal_handler_disconnect(as->appsrc, as->need_data_hid);
    as->need_data_hid = 0;
  }
  if (as->need_data_cb) {
    caml_remove_generational_global_root(&as->need_data_cb);
    as->need_data_cb = 0;
  }
}

static void finalize_appsrc(value v) {
  appsrc *as = Appsrc_val(v);
  disconnect_need_data(as);
  if (as->element) {
    caml_remove_generational_global_root(&as->element);
    as->element = 0;
  }
  free(as);
}

static struct custom_operations appsrc_ops = {
    "ocaml_gstreamer_appsrc", finalize_appsrc,
    custom_compare_default,   custom_hash_default,
    custom_serialize_default, custom_deserialize_default};

CAMLprim value ocaml_gstreamer_appsrc_of_element(value _e) {
  CAMLparam1(_e);
  CAMLlocal1(ans);

  GstElement *e = Element_val(_e);

  appsrc *as = malloc(sizeof(appsrc));
  if (as == NULL)
    caml_raise_out_of_memory();

  as->appsrc = GST_APP_SRC(e);
  as->need_data_cb = 0;
  as->need_data_hid = 0;
  as->element = _e;
  caml_register_global_root(&as->element);

  ans = caml_alloc_custom(&appsrc_ops, sizeof(appsrc *), 0, 1);
  Appsrc_val(ans) = as;

  CAMLreturn(ans);
}

CAMLprim value ocaml_gstreamer_appsrc_to_element(value _as) {
  CAMLparam1(_as);
  CAMLlocal1(ans);
  appsrc *as = Appsrc_val(_as);
  GstElement *e = GST_ELEMENT(as->appsrc);
  value_of_element(e, ans);
  CAMLreturn(ans);
}

CAMLprim value ocaml_gstreamer_appsrc_push_buffer_bytes_n(
    value _as, value _pres_time, value _dur, value _buf, value _ofs,
    value _len) {
  CAMLparam4(_as, _pres_time, _dur, _buf);
  appsrc *as = Appsrc_val(_as);
  GstBuffer *gstbuf;
  GstFlowReturn ret;
  int64_t pres_time = Int64_val(_pres_time);
  int64_t dur = Int64_val(_dur);
  unsigned char *data;

  caml_release_runtime_system();
  gstbuf = gst_buffer_new_allocate(NULL, Int_val(_len), NULL);
  caml_acquire_runtime_system();

  if (!gstbuf)
    caml_raise_out_of_memory();

  if (pres_time >= 0)
    gstbuf->pts = pres_time;

  if (dur >= 0)
    gstbuf->duration = dur;

  data = Bytes_val(_buf);
  buffer_fill(gstbuf, data, Int_val(_ofs), Int_val(_len));

  caml_release_runtime_system();
  ret = gst_app_src_push_buffer(as->appsrc, gstbuf);
  caml_acquire_runtime_system();

  if (ret != GST_FLOW_OK)
    caml_raise_constant(*caml_named_value("gstreamer_exn_failed"));
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_gstreamer_appsrc_push_buffer_bytes_b(value *argv,
                                                          int argn) {
  return ocaml_gstreamer_appsrc_push_buffer_bytes_n(argv[0], argv[1], argv[2],
                                                    argv[3], argv[4], argv[5]);
}

CAMLprim value ocaml_gstreamer_appsrc_push_buffer(value _as, value _buf) {
  CAMLparam2(_as, _buf);
  appsrc *as = Appsrc_val(_as);
  GstBuffer *gstbuf = Buffer_val(_buf);
  GstFlowReturn ret;

  caml_release_runtime_system();
  g_signal_emit_by_name(GST_ELEMENT(as->appsrc), "push-buffer", gstbuf, &ret);
  caml_acquire_runtime_system();

  if (ret != GST_FLOW_OK)
    caml_raise_constant(*caml_named_value("gstreamer_exn_failed"));
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_gstreamer_appsrc_push_buffer_data_n(value _as,
                                                         value _pres_time,
                                                         value _dur, value _buf,
                                                         value _ofs,
                                                         value _len) {
  CAMLparam4(_as, _pres_time, _dur, _buf);
  appsrc *as = Appsrc_val(_as);
  GstBuffer *gstbuf;
  GstFlowReturn ret;
  int64_t pres_time = Int64_val(_pres_time);
  int64_t dur = Int64_val(_dur);
  char *data;

  caml_release_runtime_system();
  gstbuf = gst_buffer_new_allocate(NULL, Int_val(_len), NULL);
  caml_acquire_runtime_system();

  if (!gstbuf)
    caml_raise_out_of_memory();

  if (pres_time >= 0)
    gstbuf->pts = pres_time;

  if (dur >= 0)
    gstbuf->duration = dur;

  data = Caml_ba_data_val(_buf);
  buffer_fill(gstbuf, data, Int_val(_ofs), Int_val(_len));

  caml_release_runtime_system();
  ret = gst_app_src_push_buffer(as->appsrc, gstbuf);
  caml_acquire_runtime_system();

  if (ret != GST_FLOW_OK)
    caml_raise_constant(*caml_named_value("gstreamer_exn_failed"));
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_gstreamer_appsrc_push_buffer_data_b(value *argv,
                                                         int argn) {
  return ocaml_gstreamer_appsrc_push_buffer_data_n(argv[0], argv[1], argv[2],
                                                   argv[3], argv[4], argv[5]);
}

static void appsrc_need_data_cb(GstAppSrc *gas, guint length,
                                gpointer user_data) {
  appsrc *as = (appsrc *)user_data;

  ocaml_gstreamer_register_thread();
  caml_acquire_runtime_system();
  caml_callback(as->need_data_cb, Val_int(length));
  caml_release_runtime_system();
}

CAMLprim value ocaml_gstreamer_appsrc_connect_need_data(value _as, value f) {
  CAMLparam2(_as, f);
  appsrc *as = Appsrc_val(_as);
  disconnect_need_data(as);

  as->need_data_cb = f;
  caml_register_generational_global_root(&as->need_data_cb);

  caml_release_runtime_system();
  as->need_data_hid = g_signal_connect(as->appsrc, "need-data",
                                       G_CALLBACK(appsrc_need_data_cb), as);
  caml_acquire_runtime_system();

  if (!as->need_data_hid)
    caml_raise_constant(*caml_named_value("gstreamer_exn_failed"));
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_gstreamer_appsrc_end_of_stream(value _as) {
  CAMLparam1(_as);
  appsrc *as = Appsrc_val(_as);
  GstFlowReturn ret;

  caml_release_runtime_system();
  g_signal_emit_by_name(as->appsrc, "end-of-stream", &ret);
  caml_acquire_runtime_system();

  if (ret != GST_FLOW_OK)
    caml_raise_constant(*caml_named_value("gstreamer_exn_failed"));
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_gstreamer_appsrc_set_format(value _as, value _fmt) {
  CAMLparam2(_as, _fmt);
  appsrc *as = Appsrc_val(_as);
  GstFormat fmt = format_val(_fmt);

  caml_release_runtime_system();
  g_object_set(G_OBJECT(as->appsrc), "format", fmt, NULL);
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

/***** Appsink *****/

typedef struct {
  GstAppSink *appsink;
  value element;
  value new_sample_cb;   // Callback function
  gulong new_sample_hid; // Callback handler ID
} appsink;

#define Appsink_val(v) (*(appsink **)Data_custom_val(v))

static void disconnect_new_sample(appsink *as) {
  if (as->new_sample_hid) {
    g_signal_handler_disconnect(as->appsink, as->new_sample_hid);
    as->new_sample_hid = 0;
  }
  if (as->new_sample_cb) {
    caml_remove_generational_global_root(&as->new_sample_cb);
    as->new_sample_cb = 0;
  }
}

static void finalize_appsink(value v) {
  appsink *as = Appsink_val(v);
  disconnect_new_sample(as);
  if (as->element) {
    caml_remove_generational_global_root(&as->element);
    as->element = 0;
  }
  free(as);
}

static struct custom_operations appsink_ops = {
    "ocaml_gstreamer_appsink", finalize_appsink,
    custom_compare_default,    custom_hash_default,
    custom_serialize_default,  custom_deserialize_default};

CAMLprim value ocaml_gstreamer_appsink_of_element(value _e) {
  CAMLparam1(_e);
  CAMLlocal1(ans);

  GstElement *e = Element_val(_e);
  appsink *as = malloc(sizeof(appsink));

  if (as == NULL)
    caml_raise_out_of_memory();

  as->appsink = GST_APP_SINK(e);
  as->element = _e;
  as->new_sample_cb = 0;
  as->new_sample_hid = 0;
  as->element = _e;
  caml_register_generational_global_root(&as->element);

  ans = caml_alloc_custom(&appsink_ops, sizeof(appsink *), 0, 1);
  Appsink_val(ans) = as;

  CAMLreturn(ans);
}

CAMLprim value ocaml_gstreamer_appsink_emit_signals(value _as) {
  CAMLparam0();
  appsink *as = Appsink_val(_as);

  caml_release_runtime_system();
  gst_app_sink_set_emit_signals(as->appsink, TRUE);
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_gstreamer_appsink_pull_buffer(value _as) {
  CAMLparam1(_as);
  CAMLlocal1(ans);
  appsink *as = Appsink_val(_as);
  GstSample *gstsample;
  GstBuffer *gstbuf;

  caml_release_runtime_system();
  gstsample = gst_app_sink_pull_sample(as->appsink);
  caml_acquire_runtime_system();

  if (!gstsample) {
    if (gst_app_sink_is_eos(as->appsink))
      caml_raise_constant(*caml_named_value("gstreamer_exn_eos"));
    else
      caml_raise_constant(*caml_named_value("gstreamer_exn_stopped"));
  }

  caml_release_runtime_system();
  gstbuf = gst_sample_get_buffer(gstsample);
  caml_acquire_runtime_system();

  if (!gstbuf)
    caml_raise_out_of_memory();

  gst_buffer_ref(gstbuf);
  gst_sample_unref(gstsample);

  value_of_buffer(gstbuf, ans);
  CAMLreturn(ans);
}

CAMLprim value ocaml_gstreamer_appsink_is_eos(value _as) {
  CAMLparam1(_as);
  appsink *as = Appsink_val(_as);
  gboolean ret;

  caml_release_runtime_system();
  ret = gst_app_sink_is_eos(as->appsink);
  caml_acquire_runtime_system();

  CAMLreturn(Val_bool(ret));
}

static GstFlowReturn appsink_new_sample_cb(GstAppSink *gas,
                                           gpointer user_data) {
  appsink *as = (appsink *)user_data;

  ocaml_gstreamer_register_thread();
  caml_acquire_runtime_system();
  caml_callback(as->new_sample_cb, Val_unit);
  caml_release_runtime_system();

  return GST_FLOW_OK;
}

CAMLprim value ocaml_gstreamer_appsink_connect_new_sample(value _as, value f) {
  CAMLparam2(_as, f);
  appsink *as = Appsink_val(_as);
  disconnect_new_sample(as);

  as->new_sample_cb = f;
  caml_register_generational_global_root(&as->new_sample_cb);

  caml_release_runtime_system();
  as->new_sample_hid = g_signal_connect(as->appsink, "new-sample",
                                        G_CALLBACK(appsink_new_sample_cb), as);
  caml_acquire_runtime_system();

  if (!as->new_sample_hid)
    caml_raise_constant(*caml_named_value("gstreamer_exn_failed"));
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_gstreamer_appsink_set_max_buffers(value _as, value _n) {
  CAMLparam2(_as, _n);
  appsink *as = Appsink_val(_as);
  int n = Int_val(_n);

  caml_release_runtime_system();
  gst_app_sink_set_max_buffers(as->appsink, n);
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

/***** GstCaps *****/

#define Caps_val(v) (*(GstCaps **)Data_custom_val(v))

static void finalize_caps(value v) {
  GstCaps *c = Caps_val(v);
  gst_caps_unref(c);
}

static struct custom_operations caps_ops = {
    "ocaml_gstreamer_caps",   finalize_caps,
    custom_compare_default,   custom_hash_default,
    custom_serialize_default, custom_deserialize_default};

static value value_of_caps(GstCaps *c) {
  value ans = caml_alloc_custom(&caps_ops, sizeof(GstCaps *), 0, 1);
  Caps_val(ans) = c;
  return ans;
}

CAMLprim value ocaml_gstreamer_caps_to_string(value _c) {
  CAMLparam1(_c);
  CAMLlocal1(ans);
  GstCaps *c = Caps_val(_c);
  char *s;

  caml_release_runtime_system();
  s = gst_caps_to_string(c);
  caml_acquire_runtime_system();

  ans = caml_copy_string(s);
  free(s);

  CAMLreturn(ans);
}

/***** Typefind element *****/

typedef struct {
  GstElement *tf;
  value have_type_cb;   // Callback function
  gulong have_type_hid; // Callback handler ID
} typefind_element;

#define Typefind_element_data_val(v) (*(typefind_element **)Data_custom_val(v))
#define Typefind_element_val(v) (Typefind_element_data_val(v)->tf)

static void disconnect_have_type(typefind_element *tf) {
  if (tf->have_type_hid) {
    g_signal_handler_disconnect(tf->tf, tf->have_type_hid);
    tf->have_type_hid = 0;
  }
  if (tf->have_type_cb) {
    caml_remove_global_root(&tf->have_type_cb);
    tf->have_type_cb = 0;
  }
}

static void finalize_typefind_element(value v) {
  typefind_element *tf = Typefind_element_data_val(v);
  disconnect_have_type(tf);
  free(tf);
}

static struct custom_operations typefind_element_ops = {
    "ocaml_gstreamer_typefind_element",
    finalize_typefind_element,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default};

static value value_of_typefind_element(GstElement *e) {
  value ans = caml_alloc_custom(&typefind_element_ops,
                                sizeof(typefind_element *), 0, 1);
  typefind_element *tf = malloc(sizeof(typefind_element));
  tf->tf = e;
  tf->have_type_cb = 0;
  tf->have_type_hid = 0;
  Typefind_element_data_val(ans) = tf;
  return ans;
}

CAMLprim value ocaml_gstreamer_typefind_element_of_element(value _e) {
  CAMLparam1(_e);
  GstElement *e = Element_val(_e);
  // TODO: we don't have access to GST_TYPE_FIND_ELEMENT...
  CAMLreturn(value_of_typefind_element(GST_ELEMENT(e)));
}

static void typefind_element_have_type_cb(GstElement *_typefind,
                                          guint probability, GstCaps *caps,
                                          gpointer user_data) {
  typefind_element *tf = (typefind_element *)user_data;
  assert(_typefind);
  assert(caps);

  caml_acquire_runtime_system();
  caml_callback2(tf->have_type_cb, Val_int(probability), value_of_caps(caps));
  caml_release_runtime_system();
}

CAMLprim value ocaml_gstreamer_typefind_element_connect_have_type(value _tf,
                                                                  value f) {
  CAMLparam2(_tf, f);
  typefind_element *tf = Typefind_element_data_val(_tf);
  disconnect_have_type(tf);

  tf->have_type_cb = f;
  caml_register_global_root(&tf->have_type_cb);

  caml_release_runtime_system();
  tf->have_type_hid = g_signal_connect(
      tf->tf, "have-type", G_CALLBACK(typefind_element_have_type_cb), tf);
  caml_acquire_runtime_system();

  if (!tf->have_type_hid)
    caml_raise_constant(*caml_named_value("gstreamer_exn_failed"));
  CAMLreturn(Val_unit);
}

/***** TagSetter element *****/

#define TagSetter_val(v) GST_TAG_SETTER(Element_val(v))

#define merge_modes_len 8
static const GstTagMergeMode merge_modes[merge_modes_len] = {
    GST_TAG_MERGE_UNDEFINED, GST_TAG_MERGE_REPLACE_ALL, GST_TAG_MERGE_REPLACE,
    GST_TAG_MERGE_APPEND,    GST_TAG_MERGE_PREPEND,     GST_TAG_MERGE_KEEP,
    GST_TAG_MERGE_KEEP_ALL,  GST_TAG_MERGE_COUNT};

static GstTagMergeMode merge_mode_of_int(int n) { return merge_modes[n]; }

/*
static int int_of_merge_mode(GstTagMergeMode msg)
{
  int i;
  for (i = 0; i < merge_modes_len; i++)
    {
      if (msg == merge_modes[i])
        return i;
    }
  printf("error in tag merge mode: %d\n", msg);
  assert(0);
}
*/

CAMLprim value ocaml_gstreamer_tag_setter_add_tag(value _t, value _mode,
                                                  value _name, value _v) {
  gst_tag_setter_add_tags(TagSetter_val(_t), merge_mode_of_int(_mode),
                          String_val(_name), String_val(_v), NULL);
  return Val_unit;
}
