type t = Optint.t

let equal a b = Optint.equal a b

let pp ppf v = Optint.pp ppf v

let default = Optint.one

let _base = 65521

let _nmax = 5552

let digest : type a. get:(a -> int -> char) -> a -> int -> int -> t -> t =
 fun ~get buf off len adler32 ->
  let a = ref Optint.(to_unsigned_int Infix.(adler32 >> 16 && of_int 0xFFFF)) in
  let b = ref Optint.(to_unsigned_int Infix.(adler32 && of_int 0xFFFF)) in
  let l = ref len in
  let o = ref off in
  if len = 0
  then adler32
  else if len = 1
  then (
    b := !b + (Char.code @@ get buf !o) ;
    if !b >= _base then b := !b - _base ;
    a := !a + !b ;
    if !a >= _base then a := !a - _base ;
    Optint.Infix.(Optint.of_unsigned_int !b || Optint.of_unsigned_int !a << 16))
  else if len < 16
  then (
    while !l <> 0 do
      b := !b + (Char.code @@ get buf !o) ;
      a := !a + !b ;
      incr o ;
      decr l
    done ;
    if !b >= _base then b := !b - _base ;
    a := !a mod _base ;
    Optint.Infix.(Optint.of_unsigned_int !b || Optint.of_unsigned_int !a << 16))
  else (
    while !l >= _nmax do
      l := !l - _nmax ;
      for _ = _nmax / 16 downto 1 do
        b := !b + (Char.code @@ get buf !o) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 1)) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 2)) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 3)) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 4)) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 5)) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 6)) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 7)) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 8)) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 9)) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 10)) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 11)) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 12)) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 13)) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 14)) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 15)) ;
        a := !a + !b ;
        o := !o + 16
      done ;
      b := !b mod _base ;
      a := !a mod _base
    done ;
    if !l > 0
    then (
      while !l >= 16 do
        l := !l - 16 ;
        b := !b + (Char.code @@ get buf !o) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 1)) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 2)) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 3)) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 4)) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 5)) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 6)) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 7)) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 8)) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 9)) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 10)) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 11)) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 12)) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 13)) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 14)) ;
        a := !a + !b ;
        b := !b + (Char.code @@ get buf (!o + 15)) ;
        a := !a + !b ;
        o := !o + 16
      done ;
      while !l > 0 do
        b := !b + (Char.code @@ get buf !o) ;
        a := !a + !b ;
        decr l ;
        incr o
      done ;
      b := !b mod _base ;
      a := !a mod _base) ;
    Optint.Infix.(Optint.of_unsigned_int !b || Optint.of_unsigned_int !a << 16))

let unsafe_digest_bytes a o l v = digest ~get:Bytes.unsafe_get a o l v

let digest_bytes a o l v = digest ~get:Bytes.get a o l v

let unsafe_digest_string a o l v = digest ~get:String.unsafe_get a o l v

let digest_string a o l v = digest ~get:String.get a o l v

type bigstring =
  ( char,
    Bigarray_compat.int8_unsigned_elt,
    Bigarray_compat.c_layout )
  Bigarray_compat.Array1.t

let unsafe_digest_bigstring a o l v =
  digest ~get:Bigarray_compat.Array1.unsafe_get a o l v

let digest_bigstring a o l v = digest ~get:Bigarray_compat.Array1.get a o l v
