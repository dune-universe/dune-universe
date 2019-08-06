let kdf ~size ~salt password =
  Scrypt_kdf.scrypt_kdf ~password ~salt ~dk_len:size ~r:8 ~p:2 ~n:8192
