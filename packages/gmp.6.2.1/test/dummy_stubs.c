#include <caml/mlvalues.h>
#include <gmp.h>

value caml_compute(value unit) {
    mpz_t n;
    mpz_init(n);
    mpz_set_str(n,"100", 10);
    mpz_mul(n,n,n);

    gmp_printf ("MPZ: %Zd\n", n);
    
    mpz_clear(n);

    return unit;
}
