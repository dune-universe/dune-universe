#include <stdio.h>
#include <string.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

#include <qrencode.h>
#include <png.h>

/********************************/
/* Dealing with 'struct QRcode' */
/********************************/

static struct custom_operations qrcode_ops = {
  "tmf.qrencode.qrcode",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default
};

/* Accessing the QRcode * part of a Caml custom block */
#define QRcode_val(v) (*((QRcode **) Data_custom_val(v)))

/* Allocating a Caml custom block to hold the given QRcode * */
static value alloc_QRcode(QRcode * w) {
  value v = alloc_custom(&qrcode_ops, sizeof(QRcode *), 0, 1);
  QRcode_val(v) = w;
  return v;
}

/*********************************/
/* Dealing with 'struct QRinput' */
/*********************************/


/* Accessing the QRinput * part of a Caml custom block */
#define QRinput_val(v) (*((QRinput **) Data_custom_val(v)))

void ocaml_QRinput_finalize (value qrinput) {
  printf ("@@@ About to finalize qrinput value");
  QRinput_free (QRinput_val (qrinput));
  return ;
}

static struct custom_operations qrinput_ops = {
  "tmf.qrencode.qrcode",
  ocaml_QRinput_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default
};

/* Allocating a Caml custom block to hold the given QRinput * */
static value alloc_QRinput(QRinput * w) {
  value v = alloc_custom(&qrinput_ops, sizeof(QRinput *), 0, 1);
  QRinput_val(v) = w;
  return v;
}

/**********************************/
/* Input data                     */
/**********************************/

value ocaml_QRinput_new(value unit) {
  CAMLparam1 (unit);
  QRinput * new_input = QRinput_new ();
  CAMLreturn (alloc_QRinput (new_input));
}

value ocaml_QRinput_append(value input, value mode, value data) {
  CAMLparam3 (input, mode, data);

  int rc = QRinput_append (QRinput_val (input),
                           Int_val(mode),
                           caml_string_length(data),
                           (unsigned char*)String_val(data));

  CAMLreturn (Val_int(rc));
}

/***********************************/
/* Code data                       */
/***********************************/

value ocaml_QRcode_encodeInput (value input) {
  CAMLparam1(input);
  QRcode * new_code = QRcode_encodeInput (QRinput_val (input));
  CAMLreturn (alloc_QRcode (new_code));
}


/***********************************/
/* Write PNG                       */
/***********************************/

static int writePNG(QRcode *qrcode, int size, int margin, const char *outfile) {
  static FILE *fp; // avoid clobbering by setjmp.
  png_structp png_ptr;
  png_infop info_ptr;
  unsigned char *row, *p, *q;
  int x, y, xx, yy, bit;
  int realwidth;

  realwidth = (qrcode->width + margin * 2) * size;
  row = (unsigned char *)malloc((realwidth + 7) / 8);
  if(row == NULL) {
    fprintf(stderr, "Failed to allocate memory.\n");
    exit(EXIT_FAILURE);
  }

  if(outfile[0] == '-' && outfile[1] == '\0') {
    fp = stdout;
  } else {
    fp = fopen(outfile, "wb");
    if(fp == NULL) {
      fprintf(stderr, "Failed to create file: %s\n", outfile);
      perror(NULL);
      exit(EXIT_FAILURE);
    }
  }

  png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if(png_ptr == NULL) {
    fprintf(stderr, "Failed to initialize PNG writer.\n");
    exit(EXIT_FAILURE);
  }

  info_ptr = png_create_info_struct(png_ptr);
  if(info_ptr == NULL) {
    fprintf(stderr, "Failed to initialize PNG write.\n");
    exit(EXIT_FAILURE);
  }

  if(setjmp(png_jmpbuf(png_ptr))) {
    png_destroy_write_struct(&png_ptr, &info_ptr);
    fprintf(stderr, "Failed to write PNG image.\n");
    exit(EXIT_FAILURE);
  }

  png_init_io(png_ptr, fp);
  png_set_IHDR(png_ptr, info_ptr,
               realwidth, realwidth,
               1,
               PNG_COLOR_TYPE_GRAY,
               PNG_INTERLACE_NONE,
               PNG_COMPRESSION_TYPE_DEFAULT,
               PNG_FILTER_TYPE_DEFAULT);
  png_write_info(png_ptr, info_ptr);

  /* top margin */
  memset(row, 0xff, (realwidth + 7) / 8);
  for(y=0; y<margin * size; y++) {
    png_write_row(png_ptr, row);
  }

  /* data */
  p = qrcode->data;
  for(y=0; y<qrcode->width; y++) {
    bit = 7;
    memset(row, 0xff, (realwidth + 7) / 8);
    q = row;
    q += margin * size / 8;
    bit = 7 - (margin * size % 8);
    for(x=0; x<qrcode->width; x++) {
      for(xx=0; xx<size; xx++) {
        *q ^= (*p & 1) << bit;
        bit--;
        if(bit < 0) {
          q++;
          bit = 7;
        }
      }
      p++;
    }
    for(yy=0; yy<size; yy++) {
      png_write_row(png_ptr, row);
    }
  }
  /* bottom margin */
  memset(row, 0xff, (realwidth + 7) / 8);
  for(y=0; y<margin * size; y++) {
    png_write_row(png_ptr, row);
  }

  png_write_end(png_ptr, info_ptr);
  png_destroy_write_struct(&png_ptr, &info_ptr);

  fclose(fp);
  free(row);

  return 0;
}

value ocaml_writePNG (value code, value size, value margin, value outfile) {
  CAMLparam2 (code, outfile);
  writePNG(QRcode_val (code), Int_val(size), Int_val (margin), String_val (outfile));
  CAMLreturn (Val_unit);
}
