let const x _ = x

let make ~name (module M : Checkseum.S) input expected =
  let checkseum = Alcotest.testable M.pp M.equal in
  ( name,
    `Quick,
    fun () ->
      Alcotest.check checkseum name
        M.(digest_string input 0 (String.length input) default)
        expected )

let () =
  Alcotest.run "checkseum"
    [
      ( "crc32c",
        [
          make ~name:"0" (module Checkseum.Crc32c) "" Optint.zero;
          make ~name:"1"
            (module Checkseum.Crc32c)
            "\x00"
            (Optint.of_unsigned_int32 0x527d5351l);
          make ~name:"2"
            (module Checkseum.Crc32c)
            "\xff\xff\xff\xff"
            (Optint.of_unsigned_int32 0xffffffffl);
          make ~name:"3"
            (module Checkseum.Crc32c)
            "123456789"
            (Optint.of_unsigned_int32 0xe3069283l);
          make ~name:"4"
            (module Checkseum.Crc32c)
            "Thou hast made me, and shall thy work decay?"
            (Optint.of_unsigned_int32 0x866374c0l);
        ] );
      ( "crc32",
        [
          make ~name:"0" (module Checkseum.Crc32) "" Optint.zero;
          make ~name:"1"
            (module Checkseum.Crc32)
            "\x00"
            (Optint.of_unsigned_int32 0xd202ef8dl);
          make ~name:"2"
            (module Checkseum.Crc32)
            "\xff\xff\xff\xff"
            (Optint.of_unsigned_int32 0xffffffffl);
          make ~name:"3"
            (module Checkseum.Crc32)
            "123456789"
            (Optint.of_unsigned_int32 0xcbf43926l);
          make ~name:"4"
            (module Checkseum.Crc32)
            "Thou hast made me, and shall thy work decay?"
            (Optint.of_unsigned_int32 0xf1fabe1dl);
          make ~name:"5"
            (module Checkseum.Crc32)
            (String.concat "%" (List.init 1000 (const "abcdef")))
            (Optint.of_unsigned_int32 0xadc436fl);
        ] );
      ( "crc24",
        [
          make ~name:"0"
            (module Checkseum.Crc24)
            ""
            (Optint.of_unsigned_int32 0xb704cel);
          make ~name:"1"
            (module Checkseum.Crc24)
            "a"
            (Optint.of_unsigned_int32 0xf25713l);
          make ~name:"2"
            (module Checkseum.Crc24)
            "abc"
            (Optint.of_unsigned_int32 0xba1c7bl);
          make ~name:"3"
            (module Checkseum.Crc24)
            "message digest"
            (Optint.of_unsigned_int32 0xdbf0b6l);
        ] );
    ]
