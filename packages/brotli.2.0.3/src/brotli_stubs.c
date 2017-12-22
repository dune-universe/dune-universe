// -*- c++ -*-

// OCaml declarations
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#include <brotli/decode.h>
#include <brotli/encode.h>
// C++
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <memory>
#include <vector>

#define Val_none Val_int(0)

extern "C" {

static char *brotli_decoder_version = NULL;
static char *brotli_encoder_version = NULL;

__attribute__((constructor)) void set_version(void) {
  char version[16];
  uint32_t v_d = BrotliDecoderVersion();

  snprintf(version, sizeof(version), "%d.%d.%d", v_d >> 24, (v_d >> 12) & 0xFFF,
           v_d & 0xFFF);
  brotli_decoder_version = (char *)malloc(16);
  strcpy(brotli_decoder_version, version);

  uint32_t v_e = BrotliEncoderVersion();
  snprintf(version, sizeof(version), "%d.%d.%d", v_e >> 24, (v_e >> 12) & 0xFFF,
           v_e & 0xFFF);
  brotli_encoder_version = (char *)malloc(16);
  strcpy(brotli_encoder_version, version);
}

CAMLprim value ml_brotli_decoder_version(__attribute__((unused)) value) {
  CAMLparam0();
  CAMLlocal1(v);
  v = caml_copy_string(brotli_decoder_version);
  free(brotli_decoder_version);
  CAMLreturn(v);
}

CAMLprim value ml_brotli_encoder_version(__attribute__((unused)) value) {
  CAMLparam0();
  CAMLlocal1(v);
  v = caml_copy_string(brotli_encoder_version);
  free(brotli_encoder_version);
  CAMLreturn(v);
}

CAMLprim value ml_brotli_compress(value part_compress_opt, value params,
                                  value compress_me) {
  CAMLparam3(part_compress_opt, params, compress_me);
  CAMLlocal2(compressed_string, part_compress_cb);

  bool ml_compress_cb = (part_compress_opt == Val_none) == false;
  BROTLI_BOOL ok = BROTLI_TRUE;
  uint8_t *input = (uint8_t *)String_val(compress_me);

  size_t available_in = caml_string_length(compress_me);
  size_t available_out = 0;
  const uint8_t *next_in = input;
  uint8_t *next_out = nullptr;

  std::vector<uint8_t> output;
  BrotliEncoderState *enc =
      BrotliEncoderCreateInstance(nullptr, nullptr, nullptr);

  // Setting the compression parameters
  BrotliEncoderSetParameter(enc, BROTLI_PARAM_MODE, Int_val(Field(params, 0)));
  BrotliEncoderSetParameter(enc, BROTLI_PARAM_QUALITY,
                            Int_val(Field(params, 1)));
  BrotliEncoderSetParameter(enc, BROTLI_PARAM_LGWIN, Int_val(Field(params, 2)));
  BrotliEncoderSetParameter(enc, BROTLI_PARAM_LGBLOCK,
                            Int_val(Field(params, 3)));

  if (ml_compress_cb == true)
    part_compress_cb = Field(part_compress_opt, 0);

  caml_release_runtime_system();
  while (ok) {
    ok = BrotliEncoderCompressStream(enc, BROTLI_OPERATION_FINISH,
                                     &available_in, &next_in, &available_out,
                                     &next_out, nullptr);
    if (!ok)
      break;
    size_t buffer_length = 0;
    const uint8_t *buffer = BrotliEncoderTakeOutput(enc, &buffer_length);
    if (buffer_length) {
      if (ml_compress_cb == true) {
        caml_acquire_runtime_system();
        caml_callback(part_compress_cb, caml_copy_nativeint(buffer_length));
        caml_release_runtime_system();
      }
      output.insert(output.end(), buffer, buffer + buffer_length);
    }
    if (available_in || BrotliEncoderHasMoreOutput(enc))
      continue;
    break;
  }
  caml_acquire_runtime_system();

  bool result_is_good = BrotliEncoderIsFinished(enc);
  BrotliEncoderDestroyInstance(enc);

  if (result_is_good) {
    size_t compressed_size = output.size() - available_out;
    compressed_string = caml_alloc_string(compressed_size);
    memmove(String_val(compressed_string), output.data(), compressed_size);
    CAMLreturn(compressed_string);
  } else {
    caml_failwith("Compression failure");
  }
}

CAMLprim value ml_brotli_decompress(value part_decompress_opt,
                                    value decompress_me) {
  CAMLparam2(part_decompress_opt, decompress_me);
  CAMLlocal2(decompressed_string, part_completed_cb);

  const uint8_t *input = (uint8_t *)String_val(decompress_me);
  size_t available_in = caml_string_length(decompress_me);

  std::vector<uint8_t> output;
  const uint8_t *next_in = input;
  uint8_t *next_out = nullptr;
  size_t available_out = 0;

  BrotliDecoderState *dec =
      BrotliDecoderCreateInstance(nullptr, nullptr, nullptr);
  BrotliDecoderResult result = BROTLI_DECODER_RESULT_NEEDS_MORE_OUTPUT;

  if ((part_decompress_opt == Val_none) == false) {
    part_completed_cb = Field(part_decompress_opt, 0);
  }

  caml_release_runtime_system();
  while (result == BROTLI_DECODER_NEEDS_MORE_OUTPUT) {
    result = BrotliDecoderDecompressStream(dec, &available_in, &next_in,
                                           &available_out, &next_out, nullptr);
    size_t buffer_length = 0;
    const uint8_t *buffer = BrotliDecoderTakeOutput(dec, &buffer_length);
    if (buffer_length != 0) {
      if ((part_decompress_opt == Val_none) == false) {
        caml_acquire_runtime_system();
        caml_callback(part_completed_cb, caml_copy_nativeint(buffer_length));
        caml_release_runtime_system();
      }
      output.insert(output.end(), buffer, buffer + buffer_length);
    }
  }
  caml_acquire_runtime_system();

  if ((result == BROTLI_DECODER_SUCCESS) == true) {
    decompressed_string = caml_alloc_string(output.size());
    memmove(String_val(decompressed_string), &output[0], output.size());
    BrotliDecoderDestroyInstance(dec);
    CAMLreturn(decompressed_string);
  } else {
    const char *s = BrotliDecoderErrorString(BrotliDecoderGetErrorCode(dec));
    BrotliDecoderDestroyInstance(dec);
    caml_failwith(s);
  }
}
}
