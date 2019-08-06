#include <caml/mlvalues.h>
#include <time.h>

extern long timezone;

CAMLprim value
caml_tz_set(value _unit)
{
  tzset();
}

CAMLprim value
caml_tz_get(value _unit)
{
  return Val_long(timezone);
}
