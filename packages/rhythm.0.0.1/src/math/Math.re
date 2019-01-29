

/**
 * Computes the floor of the log base 2 of an integer. This is very quick using
 * bit-shifting.
 *
 * http://aggregate.org/MAGIC/#Log2%20of%20an%20Integer
 */
let log2 = (u: int): int => {
  if (u < 0) {
    raise(Exceptions.Imaginary("log2"));
  };
  if (u === 0) {
    raise(Exceptions.Undefined("log2"));
  };
  let u = u lor u lsr 1;
  let u = u lor u lsr 2;
  let u = u lor u lsr 4;
  let u = u lor u lsr 8;
  let u = u lor u lsr 16;
  let u = u lsr 1;
  let u = u - u lsr 1 land 0x55555555;
  let u = u lsr 2 land 0x33333333 + u land 0x33333333;
  let u = (u lsr 4 + u) land 0x0f0f0f0f;
  let u = u + u lsr 8;
  let u = u + u lsr 16;
  u land 0x0000003f;
};
