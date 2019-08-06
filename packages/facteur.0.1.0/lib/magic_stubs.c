#include <magic.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <string.h>

#define LE

static uint64_t load64(const void *src)
{
#if defined(LE)
  uint64_t w;
  memcpy(&w, src, sizeof w);
  return w;
#else
  const uint8_t *p = (const uint8_t *) src;
  return
    ((uint64_t)(p[0]) <<  0) |
    ((uint64_t)(p[1]) <<  8) |
    ((uint64_t)(p[2]) << 16) |
    ((uint64_t)(p[3]) << 24) |
    ((uint64_t)(p[4]) << 32) |
    ((uint64_t)(p[5]) << 40) |
    ((uint64_t)(p[6]) << 48) |
    ((uint64_t)(p[7]) << 56) ;
#endif
}

static void store64(void *dst, uint64_t w)
{
#if defined(LE)
  memcpy(dst, &w, sizeof w);
#else
  uint8_t *p = (uint8_t*) dst;
  p[0] = (uint8_t)(w >>  0);
  p[1] = (uint8_t)(w >>  8);
  p[2] = (uint8_t)(w >> 16);
  p[3] = (uint8_t)(w >> 24);
  p[4] = (uint8_t)(w >> 32);
  p[5] = (uint8_t)(w >> 40);
  p[6] = (uint8_t)(w >> 48);
  p[7] = (uint8_t)(w >> 56);
#endif
}

static const int magic_mask_table[] = {
  MAGIC_DEBUG,
  MAGIC_SYMLINK,
  MAGIC_COMPRESS,
  MAGIC_DEVICES,
  MAGIC_MIME_TYPE,
  MAGIC_MIME_ENCODING,
  MAGIC_CONTINUE,
  MAGIC_CHECK,
  MAGIC_PRESERVE_ATIME,
  MAGIC_RAW,
  MAGIC_ERROR,
  MAGIC_NO_CHECK_APPTYPE,
  MAGIC_NO_CHECK_ASCII,
  MAGIC_NO_CHECK_COMPRESS,
  MAGIC_NO_CHECK_ELF,
  MAGIC_NO_CHECK_FORTRAN,
  MAGIC_NO_CHECK_SOFT,
  MAGIC_NO_CHECK_TAR,
  MAGIC_NO_CHECK_TOKENS,
  MAGIC_NO_CHECK_TROFF
};

static inline int
magic_mask(value l)
{
  int r = 0;

  while (l != Val_emptylist)
    {
      value head = Field(l, 0);
      r |= magic_mask_table[Long_val(head)];
      l = Field(l, 1);
    }

  return r;
}

#define MAGIC_RAISE_SYS_ERROR(ERR) \
  do { caml_raise_sys_error(caml_copy_string("Magic: " ERR)); } \
  while (0)

CAMLprim value
caml_magic_init(value magic, value flags)
{
  magic_t *m = (magic_t *) String_val(magic);
  magic_t r = magic_open(magic_mask(flags));

  memcpy(m, &r, sizeof(magic_t));

  if (m == NULL)
    MAGIC_RAISE_SYS_ERROR("Initialization error");

  return magic;
}

#define Val_none Val_int(0)
#define Some_val(v) Field(v, 0)

CAMLprim value
caml_magic_load(value magic, value database)
{
  magic_t *m = (magic_t *) String_val(magic);
  int r = 0;

  if (magic_load(*m, (database == Val_none) ? NULL : String_val(Some_val(database))))
    MAGIC_RAISE_SYS_ERROR("Impossible to load database");

  return Val_unit;
}

CAMLprim value
caml_magic_file(value magic, value filename)
{
  CAMLparam2(magic, filename);
  CAMLlocal1(result);

  magic_t *m = (magic_t *) String_val(magic);

  const char *r = magic_file(*m, String_val(filename));
  result = caml_alloc_string(strlen(r));
  memcpy(String_val(result), r, strlen(r));

  CAMLreturn(result);
}

CAMLprim value
caml_magic_close(value magic)
{
  magic_t *m = (magic_t *) String_val(magic);

  magic_close(*m);

  return Val_unit;
}
