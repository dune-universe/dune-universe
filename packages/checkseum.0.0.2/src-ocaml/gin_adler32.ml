type t = Optint.t

let equal = Optint.equal
let pp = Optint.pp
let default = Optint.one

let _base = 65521
let _nmax = 5552

let digest : type a. get:(a -> int -> char) -> a -> int -> int -> t -> t
  = fun ~get buf off len adler32 ->
  let a = ref Optint.(to_int Infix.((adler32 >> 16) && (of_int 0xFFFF))) in
  let b = ref Optint.(to_int Infix.((adler32 && (of_int 0xFFFF)))) in
  let l = ref len in
  let o = ref off in

  if len = 0
  then adler32
  else if len = 1
  then begin
      b := !b + (Char.code @@ get buf !o);
      if !b >= _base then b := !b - _base;
      a := !a + !b;
      if !a >= _base then a := !a - _base;

      Optint.Infix.((Optint.of_int !b) || ((Optint.of_int !a) << 16))
    end
  else if len < 16
  then begin
      while !l <> 0
      do b := !b + (Char.code @@ get buf !o);
         a := !a + !b;

         incr o;
         decr l;
      done;

      if !b >= _base then b := !b - _base;
      a := !a mod _base;

      Optint.Infix.((Optint.of_int !b) || ((Optint.of_int !a) << 16))
    end else begin
    while !l >= _nmax
    do l := !l - _nmax;

       for _ = _nmax / 16 downto 1
       do b := !b + (Char.code @@ get buf !o);
          a := !a + !b;
          b := !b + (Char.code @@ get buf (!o + 1));
          a := !a + !b;
          b := !b + (Char.code @@ get buf (!o + 2));
          a := !a + !b;
          b := !b + (Char.code @@ get buf (!o + 3));
          a := !a + !b;
          b := !b + (Char.code @@ get buf (!o + 4));
          a := !a + !b;
          b := !b + (Char.code @@ get buf (!o + 5));
          a := !a + !b;
          b := !b + (Char.code @@ get buf (!o + 6));
          a := !a + !b;
          b := !b + (Char.code @@ get buf (!o + 7));
          a := !a + !b;
          b := !b + (Char.code @@ get buf (!o + 8));
          a := !a + !b;
          b := !b + (Char.code @@ get buf (!o + 9));
          a := !a + !b;
          b := !b + (Char.code @@ get buf (!o + 10));
          a := !a + !b;
          b := !b + (Char.code @@ get buf (!o + 11));
          a := !a + !b;
          b := !b + (Char.code @@ get buf (!o + 12));
          a := !a + !b;
          b := !b + (Char.code @@ get buf (!o + 13));
          a := !a + !b;
          b := !b + (Char.code @@ get buf (!o + 14));
          a := !a + !b;
          b := !b + (Char.code @@ get buf (!o + 15));
          a := !a + !b;

          o := !o + 16
       done;

       b := !b mod _base;
       a := !a mod _base;
    done;

    if !l > 0
    then begin
      while !l >= 16
      do l := !l - 16;

         b := !b + (Char.code @@ get buf !o);
         a := !a + !b;
         b := !b + (Char.code @@ get buf (!o + 1));
         a := !a + !b;
         b := !b + (Char.code @@ get buf (!o + 2));
         a := !a + !b;
         b := !b + (Char.code @@ get buf (!o + 3));
         a := !a + !b;
         b := !b + (Char.code @@ get buf (!o + 4));
         a := !a + !b;
         b := !b + (Char.code @@ get buf (!o + 5));
         a := !a + !b;
         b := !b + (Char.code @@ get buf (!o + 6));
         a := !a + !b;
         b := !b + (Char.code @@ get buf (!o + 7));
         a := !a + !b;
         b := !b + (Char.code @@ get buf (!o + 8));
         a := !a + !b;
         b := !b + (Char.code @@ get buf (!o + 9));
         a := !a + !b;
         b := !b + (Char.code @@ get buf (!o + 10));
         a := !a + !b;
         b := !b + (Char.code @@ get buf (!o + 11));
         a := !a + !b;
         b := !b + (Char.code @@ get buf (!o + 12));
         a := !a + !b;
         b := !b + (Char.code @@ get buf (!o + 13));
         a := !a + !b;
         b := !b + (Char.code @@ get buf (!o + 14));
         a := !a + !b;
         b := !b + (Char.code @@ get buf (!o + 15));
         a := !a + !b;

         o := !o + 16;
      done;

      while !l > 0
      do b := !b + (Char.code @@ get buf !o);
         a := !a + !b;
         decr l;
         incr o;
      done;

      b := !b mod _base;
      a := !a mod _base;
    end;

    Optint.Infix.((Optint.of_int !b) || ((Optint.of_int !a) << 16))
  end

let digest_bytes =
  let get = Bytes.get in
  digest ~get

let digest_string =
  let get = String.get in
  digest ~get

type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let digest_bigstring =
  let get : bigstring -> int -> char = Bigarray.Array1.get in
  digest ~get
