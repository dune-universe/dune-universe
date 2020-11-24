
(* THIS FILE MUST BE ENCODED IN ISO-8859-1 (Latin1) *)

open Mysql_protocol

let db_name = "test_ocaml_ocmp_latin1"

let insert_var_string = "insert varstring : abcdefghijklmnopqrstuvwxyzéèçàù & - _  ) = $ % * ? . ; / ! < > 123456789 "
let update_var_string = "update varstring : abcdefghijklmnopqrstuvwxyzéèçàù & - _  ) = $ % * ? . ; / ! < > 123456789 "

let blobtext1 = Buffer.create 1024
let blobtext2 = Buffer.create 1024
let blobblob1 = Buffer.create 1024
let blobblob2 = Buffer.create 1024
let blobtiny1 = Buffer.create 1024
let blobtiny2 = Buffer.create 1024
let blobmedium1 = Buffer.create 1024
let blobmedium2 = Buffer.create 1024
let bloblong1 = Buffer.create 1024
let bloblong2 = Buffer.create 1024
let blobimg1 = Buffer.create 1024
let blobimg2 = Buffer.create 1024
let binary1 = Buffer.create 1024
let binary2 = Buffer.create 1024
let varbinary1 = Buffer.create 1024
let varbinary2 = Buffer.create 1024
let bit1 = 
  (* "0000000000000000000000000000000000000000000000000000001010101010" *)
  let b = Bitstring.zeroes_bitstring 64 in
  let () = Bitstring.set b 54 in
  let () = Bitstring.set b 56 in
  let () = Bitstring.set b 58 in
  let () = Bitstring.set b 60 in
  let () = Bitstring.set b 62 in
  b

let bit2 = 
  Bitstring.ones_bitstring 64

let records = [
  [Mp_data.data_longlongint (Big_int.big_int_of_int 1);
   Mp_data.data_longint (Int64.of_int 10);
   Mp_data.data_smallint (9);
   Mp_data.data_decimal (Num.num_of_string "1001234/10000");
   Mp_data.data_datetime ((2010, 12, 29), (14, 56, 34, Int64.zero));
   Mp_data.data_float (147.569869995);
   Mp_data.data_double (1234567891.123450);
   Mp_data.data_int24 (1677721);
   Mp_data.data_date (2010, 12, 29);
   Mp_data.data_time (Mp_data.Positive, 14, 56, 34, Int64.zero);
   Mp_data.data_year (2011);
   (* /!\ : String has one trailing space at end in fixture but MySQL remove it silently because it's a CHAR column *)
   Mp_data.data_string ("string : abcdefghijklmnopqrstuvwxyzéèçàù & - _  ) = $ % * ? . ; / ! < > 123456789"); (* no trailing space at end *)
   Mp_data.data_varstring ("varstring : abcdefghijklmnopqrstuvwxyzéèçàù & - _  ) = $ % * ? . ; / ! < > 123456789 ");
   Mp_data.data_blob (Buffer.add_string blobtext1 "blobtext : Happy fox is running in crop field, et un peu de texte en français avec des caractères accentués..."; blobtext1);
   Mp_data.data_blob (Buffer.add_string blobblob1 "blobblob : 123456789azertyazerty%ù;:.!+=é(-èa^@à<>;.?!"; blobblob1);
   Mp_data.data_blob (Buffer.add_string blobtiny1 "blobtiny : dijenabyw26615'é))àù$*s:;<,x,ceiore"; blobtiny1);
   Mp_data.data_blob (Buffer.add_string blobmedium1 "blobmedium : dijenabyw26615'é))àù$*s:;<,x,ceiore + dijenabyw26615'é))àù$*s:;<,x,ceiore"; blobmedium1);
   Mp_data.data_blob (Buffer.add_string bloblong1 "bloblong : dijenabyw26615'é))àù$*s:;<,x,ceiore + dijenabyw26615'é))àù$*s:;<,x,ceiore + dijenabyw26615'é))àù$*s:;<,x,ceiore"; bloblong1);
   Mp_data.data_blob (Buffer.add_bytes blobimg1 (Fixture_config.content_testfile1); blobimg1);
   Mp_data.data_enum ("enum2");
   Mp_data.data_set ("set1");
   Mp_data.data_null;
   Mp_data.data_timestamp ((2011, 01, 02), (8, 9, 10, Int64.zero));
   Mp_data.data_bit bit1;
   Mp_data.data_tinyint 127;
   Mp_data.data_tinyint 255;
   Mp_data.data_longint (Int64.of_string "2147483647");
   Mp_data.data_longint (Int64.of_string "4294967295");
   Mp_data.data_smallint (32767);
   Mp_data.data_smallint (65535);
   Mp_data.data_decimal (Num.num_of_string "99999999999999999999999999999999999999999999999999999999999999999/100000000000000000000"); 
   Mp_data.data_decimal (Num.num_of_string "99999999999999999999999999999999999999999999999999999999999999999/100000000000000000000"); 
   Mp_data.data_float (3.402823466E+38);
   Mp_data.data_float (3.402823466E+38);
   Mp_data.data_double (1.7976931348623E+308); (* should be 1.7976931348623157E+308 but this value is not accepted with some MySQL version *)
   Mp_data.data_double (1.7976931348623E+308); (* should be 1.7976931348623157E+308 but this value is not accepted with some MySQL version *)
   Mp_data.data_int24 (8388607);
   Mp_data.data_int24 (16777215);
   Mp_data.data_longlongint (Big_int.big_int_of_string "9223372036854775807");
   Mp_data.data_longlongint (Big_int.big_int_of_string "18446744073709551615");
   Mp_data.data_null;
   Mp_data.data_date (1000, 1, 1);
   Mp_data.data_datetime ((1000, 1, 1), (0, 0, 0, Int64.zero));
   Mp_data.data_timestamp ((1970, 1, 1), (1, 0, 1, Int64.zero));
   Mp_data.data_time (Mp_data.Negative, 838, 59, 59, Int64.zero);
   Mp_data.data_enum ("11E");
   (* binary value has right padding with 0x00 so avoid the problem by putting exactly the same number of characters than the column size *)
   Mp_data.data_binary (Buffer.add_string binary1 "binary : ààéé\"\"çç''__((èè--èè&&ôö$£µ*ù1234567890auebsqtxbneu<>?,;./:§!}]@^`|[{#~¹azerty¤====+-*/.'\"abcdefghijklmnopqrstuvwx"; binary1);
   Mp_data.data_varbinary (Buffer.add_string varbinary1 "varbinary : &ôö$£µ*eizrssù1sqtxbne ààééerjsfdjoz\"\"çç''__((èè--èè&u<>?,;./uerngpd:§!}]@^`|[{#~¹a==+-*/2zaoizhqsdptp-----erty¤==34567890aueb.'\""; varbinary1);
   Mp_data.data_longlongint (Big_int.big_int_of_int (- 15687691));
   Mp_data.data_longint (Int64.of_int (- 94367));
   Mp_data.data_smallint (- 594);
   Mp_data.data_decimal (Num.num_of_string "- 756812/10000000");
   Mp_data.data_float (- 75618.937810);
   Mp_data.data_double (- 9782518624.394);
   Mp_data.data_int24 (- 146823);
   Mp_data.data_tinyint (- 53);
  ];
  [Mp_data.data_longlongint (Big_int.big_int_of_int 2);
   Mp_data.data_longint (Int64.of_int 20);
   Mp_data.data_smallint (90);
   Mp_data.data_decimal (Num.num_of_string "2004321/10000");
   Mp_data.data_datetime ((1971, 07, 24), (8, 1, 2, Int64.zero));
   Mp_data.data_float (987415.8750);
   Mp_data.data_double (9876543.987654e+4);
   Mp_data.data_int24 (12345);
   Mp_data.data_date (1971, 07, 24);
   Mp_data.data_time (Mp_data.Positive, 8, 1, 2, Int64.zero);
   Mp_data.data_year (1971);
   Mp_data.data_string ("string : ABCDEFGHIJKLMNOPQRSTUVWXYZ");
   Mp_data.data_varstring ("varstring : ABCDEFGHIJKLMNOPQRSTUVWXYZ");
   Mp_data.data_blob (Buffer.add_string blobtext2 "blobtext : lopsem lopsem lopsem three times, trois fois lopsem et une série d'accents é è ç à ù %%% "; blobtext2);
   Mp_data.data_blob (Buffer.add_string blobblob2 "blobblob : oeritdfgnkjnuerhdfgkdxeoirtijrendlkfnklxlkxerpzerpzejjffdidfosidjfiezrdsnfskdfpzerzeprsdfsdfksdfpoeporzerezp"; blobblob2);
   Mp_data.data_blob (Buffer.add_string blobtiny2 "blobtiny : 002çç\"ééén<znsdizpre89*-56211sf1sdf6azeae"; blobtiny2);
   Mp_data.data_blob (Buffer.add_string blobmedium2 "blobmedium : 002çç\"ééén<znsdizpre89*-56211sf1sdf6azeae + 002çç\"ééén<znsdizpre89*-56211sf1sdf6azeae"; blobmedium2);
   Mp_data.data_blob (Buffer.add_string bloblong2 "bloblong : 002çç\"ééén<znsdizpre89*-56211sf1sdf6azeae + 002çç\"ééén<znsdizpre89*-56211sf1sdf6azeae + 002çç\"ééén<znsdizpre89*-56211sf1sdf6azeae"; bloblong2);
   Mp_data.data_blob (Buffer.add_bytes blobimg2 (Fixture_config.content_testfile2); blobimg2);
   Mp_data.data_enum ("enum3");
   Mp_data.data_set ("set1,set2"); (* no space after or before comma between values*)
   Mp_data.data_null;
   Mp_data.data_timestamp ((2012, 4, 21), (12, 50, 59, Int64.zero));
   Mp_data.data_bit bit2;
   Mp_data.data_tinyint (-128);
   Mp_data.data_tinyint 0;
   Mp_data.data_longint (Int64.of_string "-2147483648");
   Mp_data.data_longint (Int64.zero);
   Mp_data.data_smallint (-32768);
   Mp_data.data_smallint (0);
   Mp_data.data_decimal (Num.num_of_string "-99999999999999999999999999999999999999999999999999999999999999999/100000000000000000000"); 
   Mp_data.data_decimal (Num.num_of_string "0");
   Mp_data.data_float (-1.175494351E-38);
   Mp_data.data_float (0.0);
   Mp_data.data_double (-2.2250738585072014E-308);
   Mp_data.data_double (0.0);
   Mp_data.data_int24 (-8388608);
   Mp_data.data_int24 (0);
   Mp_data.data_longlongint (Big_int.big_int_of_string "-9223372036854775808");
   Mp_data.data_longlongint (Big_int.big_int_of_string "0");
   Mp_data.data_null;
   Mp_data.data_date (9999, 12, 31);
   Mp_data.data_datetime ((9999, 12, 31), (23, 59, 59, Int64.zero));
   Mp_data.data_timestamp ((2038, 1, 19), (3, 14, 7, Int64.zero));
   Mp_data.data_time (Mp_data.Positive, 838, 59, 59, Int64.zero);
   Mp_data.data_null;
   (* binary value has right padding with 0x00 so avoid the problem by putting exactly the same number of characters than the column size *)
   Mp_data.data_binary (Buffer.add_string binary2 "binary : ààéé\"\"çç''__((èè--èè&&ôö$£µ*ù1234567890auebsqtxbneu<>?,;./:§!}]@^`|[{#~¹azerty¤====+-*/.'\"abcdefghijklmnopqrstuvwx"; binary2);
   Mp_data.data_varbinary (Buffer.add_string varbinary2 "varbinary : \032&ôö$£µ*eizrssù1sqtxbne ààééerjsfdjoz\"\"çç''__((èè--èè&u<>?,;./uerngpd:§!}]@^`|[{#~¹a==+-*/2zaoizhqsdptp-----erty¤==34567890aueb.'\""; varbinary2);
   Mp_data.data_longlongint (Big_int.big_int_of_int (- 10954041));
   Mp_data.data_longint (Int64.of_int (- 59140367));
   Mp_data.data_smallint (- 2964);
   Mp_data.data_decimal (Num.num_of_string "- 5694/6250");
   Mp_data.data_float (- 18.810);
   Mp_data.data_double (- 6657425211.1111111);
   Mp_data.data_int24 (- 95470);
   Mp_data.data_tinyint (- 61);
  ]
]

(* Big blog with the PDF file *)
let blobbig = Buffer.create 16384
let records_blobbig = [
  [Mp_data.data_blob (Buffer.add_bytes blobbig (Fixture_config.content_testfile3); blobbig)];
]

(* Several blogs in the same record *)
let manyblob = Buffer.create 2097152
let () = Buffer.add_bytes manyblob (Fixture_config.content_testfile4)
let records_manyblobs = [
  [Mp_data.data_blob manyblob;
   Mp_data.data_blob manyblob;
   Mp_data.data_blob manyblob;
   Mp_data.data_blob manyblob;
   Mp_data.data_blob manyblob;
   Mp_data.data_blob manyblob;
   Mp_data.data_blob manyblob;
   Mp_data.data_blob manyblob;
   Mp_data.data_blob manyblob;
   Mp_data.data_blob manyblob;
  ];
  [Mp_data.data_blob manyblob;
   Mp_data.data_blob manyblob;
   Mp_data.data_blob manyblob;
   Mp_data.data_blob manyblob;
   Mp_data.data_blob manyblob;
   Mp_data.data_blob manyblob;
   Mp_data.data_blob manyblob;
   Mp_data.data_blob manyblob;
   Mp_data.data_blob manyblob;
   Mp_data.data_blob manyblob;
  ];
]

(* more tests for date/time/datetime/timestamp and subseconds *)
let records_date vendor version =
  match vendor with
    | Test_types.MySQL -> (
        if version >= 5611 then
          [
            [Mp_data.data_date (0, 0, 0);
             Mp_data.data_time (Mp_data.Positive, 0, 0, 0, Int64.zero);
             Mp_data.data_datetime ((0, 0, 0), (0, 0, 0, Int64.zero));
             Mp_data.data_timestamp ((0, 0, 0), (0, 0, 0, Int64.zero));
            ];
            [Mp_data.data_date (1992, 12, 31);
             Mp_data.data_time (Mp_data.Positive, 21, 19, 49, Int64.zero);
             Mp_data.data_datetime ((1990, 11, 5), (13, 17, 5, Int64.zero)); (* 5 seconds if version >= 5611 *)
             Mp_data.data_timestamp ((2000, 1, 1), (1, 1, 15, Int64.zero));
            ];
          ]
        else
          [
            [Mp_data.data_date (0, 0, 0);
             Mp_data.data_time (Mp_data.Positive, 0, 0, 0, Int64.zero);
             Mp_data.data_datetime ((0, 0, 0), (0, 0, 0, Int64.zero));
             Mp_data.data_timestamp ((0, 0, 0), (0, 0, 0, Int64.zero));
            ];
            [Mp_data.data_date (1992, 12, 31);
             Mp_data.data_time (Mp_data.Positive, 21, 19, 49, Int64.zero);
             Mp_data.data_datetime ((1990, 11, 5), (13, 17, 4, Int64.zero));
             Mp_data.data_timestamp ((2000, 1, 1), (1, 1, 15, Int64.zero));
            ];
          ]
      )
    | Test_types.MariaDB -> (
        [
          [Mp_data.data_date (0, 0, 0);
           Mp_data.data_time (Mp_data.Positive, 0, 0, 0, Int64.zero);
           Mp_data.data_datetime ((0, 0, 0), (0, 0, 0, Int64.zero));
           Mp_data.data_timestamp ((0, 0, 0), (0, 0, 0, Int64.zero));
          ];
          [Mp_data.data_date (1992, 12, 31);
           Mp_data.data_time (Mp_data.Positive, 21, 19, 49, Int64.zero);
           Mp_data.data_datetime ((1990, 11, 5), (13, 17, 4, Int64.zero));
           Mp_data.data_timestamp ((2000, 1, 1), (1, 1, 15, Int64.zero));
          ];
        ]
    )

let bigbinarybuffer1 = Buffer.create 255
let bigbinarybuffer2 = Buffer.create 255
let records_bigstring = [
  [
    Mp_data.data_string (Fixture_config.build_string 255);
    Mp_data.data_binary (Buffer.add_string bigbinarybuffer1 (Fixture_config.build_string 255); bigbinarybuffer1);
  ];
  [
    Mp_data.data_string (Fixture_config.build_string 255);
    Mp_data.data_binary (Buffer.add_string bigbinarybuffer2 (Fixture_config.build_string 255); bigbinarybuffer2);
  ];
]

let records_bigvarchar = [
  [Mp_data.data_varstring (Fixture_config.build_string 65532)];
  [Mp_data.data_varstring (Fixture_config.build_string 65532)];
]

let bigvarbinarybuffer1 = Buffer.create 65532
let bigvarbinarybuffer2 = Buffer.create 65532
let records_bigvarbinary = [
  [Mp_data.data_varbinary (Buffer.add_string bigvarbinarybuffer1 (Fixture_config.build_string 65532); bigvarbinarybuffer1);];
  [Mp_data.data_varbinary (Buffer.add_string bigvarbinarybuffer2 (Fixture_config.build_string 65532); bigvarbinarybuffer2);];
]

let records_proc_one_result vendor =
  match vendor with
  | Test_types.MySQL -> 
    [
      [Mp_data.data_longlongint (Big_int.big_int_of_int 100);
        Mp_data.data_varstring ("ABC");
      ]
    ]
  | Test_types.MariaDB ->
    [
      [Mp_data.data_longint (Int64.of_int 100);
        Mp_data.data_varstring ("ABC");
      ]
    ]

let records_proc_multiple_results vendor =
  match vendor with
  | Test_types.MySQL ->  [
      [Mp_data.data_longlongint (Big_int.big_int_of_int 1);
       Mp_data.data_varstring ("A");
      ];
      [Mp_data.data_longlongint (Big_int.big_int_of_int 2);
       Mp_data.data_varstring ("B");
      ];
      [Mp_data.data_longlongint (Big_int.big_int_of_int 3);
       Mp_data.data_varstring ("C");
      ]
    ]
  | Test_types.MariaDB ->  [
      [Mp_data.data_longint (Int64.of_int 1);
       Mp_data.data_varstring ("A");
      ];
      [Mp_data.data_longint (Int64.of_int 2);
       Mp_data.data_varstring ("B");
      ];
      [Mp_data.data_longint (Int64.of_int 3);
       Mp_data.data_varstring ("C");
      ]
    ]

(* right value for the iter test *)
let ok_value_iter = "2010-12-29" ^ "varstring : abcdefghijklmnopqrstuvwxyzéèçàù & - _  ) = $ % * ? . ; / ! < > 123456789 "

let sql =
  "
USE " ^ db_name ^ ";

SET NAMES 'latin1';
SET time_zone='+0:0';

DROP TABLE IF EXISTS test_ocmp;
CREATE TABLE test_ocmp (
f_autoinc_not_null_no_def BIGINT AUTO_INCREMENT NOT NULL,
f_int_null_no_def INT,
f_smallint_null_no_def SMALLINT,
f_decimal_12_4_null_no_def DECIMAL(12,4),
f_datetime_null_no_def DATETIME,
f_float_null_no_def FLOAT(17,9),
f_double_null_no_def DOUBLE(24,6),
f_int24_null_no_def MEDIUMINT,
f_date_null_no_def DATE,
f_time_null_no_def TIME,
f_year_null_no_def YEAR,
f_string_null_no_def CHAR(150),
f_varstring_null_no_def VARCHAR(250),
f_blobtext_null_no_def TEXT,
f_blobblob_null_no_def BLOB,
f_blobtiny_null_no_def TINYBLOB,
f_blobmedium_null_no_def MEDIUMBLOB,
f_bloblong_null_no_def LONGBLOB,
f_blobimg_null_no_def BLOB,
f_enum_null_no_def ENUM('enum1', 'enum2', 'enum3'),
f_set_null_no_def SET('set1', 'set2', 'set3'),
f_int_default_null INT DEFAULT NULL,
f_timestamp_null_no_def TIMESTAMP,
f_bit_null_no_def BIT(64),
f_tinyint_null_no_def_signed TINYINT,
f_tinyint_null_no_def_unsigned TINYINT UNSIGNED,
f_int_null_no_def_signed INT,
f_int_null_no_def_unsigned INT UNSIGNED,
f_smallint_null_no_def_signed SMALLINT,
f_smallint_null_no_def_unsigned SMALLINT UNSIGNED,
f_decimal_65_20_null_no_def_signed DECIMAL(65,20),
f_decimal_65_20_null_no_def_unsigned DECIMAL(65,20) UNSIGNED,
f_float_null_no_def_signed FLOAT,
f_float_null_no_def_unsigned FLOAT UNSIGNED,
f_double_null_no_def_signed DOUBLE,
f_double_null_no_def_unsigned DOUBLE UNSIGNED,
f_int24_null_no_def_signed MEDIUMINT,
f_int24_null_no_def_unsigned MEDIUMINT UNSIGNED,
f_bigint_null_no_def_signed BIGINT,
f_bigint_null_no_def_unsigned BIGINT UNSIGNED,
f_string_default_null CHAR(10) DEFAULT NULL,
f_date_not_null_def20110101 DATE NOT NULL DEFAULT '2011-01-01',
f_datetime_not_null_def20111011140534 DATETIME NOT NULL DEFAULT '2011-10-11 14:05:34',
f_timestamp_not_null_def20110510 TIMESTAMP NOT NULL DEFAULT '2011-05-10',
f_time_not_null_def214702 TIME NOT NULL DEFAULT '21:47:02',
f_big_enum_default_null ENUM(" ^ Fixture_config.big_enum_column ^ "),
f_binary_default_null BINARY(123),
f_varbinary_default_null VARBINARY(381),
f_bigint_def_0 BIGINT,
f_int_def_0 INT,
f_smallint_def_0 SMALLINT,
f_decimal_20_9_def_0 DECIMAL(20,9),
f_float_def_0 FLOAT,
f_double_def_0 DOUBLE,
f_int24_def_0 MEDIUMINT,
f_tinyint_def_0 TINYINT,
PRIMARY KEY (f_autoinc_not_null_no_def)
);

INSERT INTO test_ocmp (f_autoinc_not_null_no_def, f_int_null_no_def, f_smallint_null_no_def, f_decimal_12_4_null_no_def, f_datetime_null_no_def, f_float_null_no_def, f_double_null_no_def, f_int24_null_no_def, f_date_null_no_def, f_time_null_no_def, f_year_null_no_def, f_string_null_no_def, f_varstring_null_no_def, f_blobtext_null_no_def, f_blobblob_null_no_def, f_blobtiny_null_no_def, f_blobmedium_null_no_def, f_bloblong_null_no_def, f_blobimg_null_no_def, f_enum_null_no_def, f_set_null_no_def, f_int_default_null, f_timestamp_null_no_def, f_bit_null_no_def, f_tinyint_null_no_def_signed, f_tinyint_null_no_def_unsigned, f_int_null_no_def_signed, f_int_null_no_def_unsigned, f_smallint_null_no_def_signed, f_smallint_null_no_def_unsigned, f_decimal_65_20_null_no_def_signed, f_decimal_65_20_null_no_def_unsigned, f_float_null_no_def_signed, f_float_null_no_def_unsigned, f_double_null_no_def_signed, f_double_null_no_def_unsigned, f_int24_null_no_def_signed, f_int24_null_no_def_unsigned, f_bigint_null_no_def_signed, f_bigint_null_no_def_unsigned, f_string_default_null, f_date_not_null_def20110101, f_datetime_not_null_def20111011140534, f_timestamp_not_null_def20110510, f_time_not_null_def214702, f_big_enum_default_null, f_binary_default_null, f_varbinary_default_null, f_bigint_def_0, f_int_def_0, f_smallint_def_0, f_decimal_20_9_def_0, f_float_def_0, f_double_def_0, f_int24_def_0, f_tinyint_def_0) 
  VALUES (1, 10, 9, 100.1234, '2010-12-29 14:56:34', 147.569869995, 1234567891.123450, 1677721, '2010-12-29', '14:56:34', 2011, 'string : abcdefghijklmnopqrstuvwxyzéèçàù & - _  ) = $ % * ? . ; / ! < > 123456789 ', 'varstring : abcdefghijklmnopqrstuvwxyzéèçàù & - _  ) = $ % * ? . ; / ! < > 123456789 ', 'blobtext : Happy fox is running in crop field, et un peu de texte en français avec des caractères accentués...', 'blobblob : 123456789azertyazerty%ù;:.!+=é(-èa^@à<>;.?!', 'blobtiny : dijenabyw26615\\'é))àù$*s:;<,x,ceiore', 'blobmedium : dijenabyw26615\\'é))àù$*s:;<,x,ceiore + dijenabyw26615\\'é))àù$*s:;<,x,ceiore', 'bloblong : dijenabyw26615\\'é))àù$*s:;<,x,ceiore + dijenabyw26615\\'é))àù$*s:;<,x,ceiore + dijenabyw26615\\'é))àù$*s:;<,x,ceiore', LOAD_FILE('" ^ Fixture_config.testfile1 ^ "'), 'enum2', 'set1', NULL, '2011-01-02 08:09:10', b'1010101010', 127, 255, 2147483647, 4294967295, 32767, 65535, 999999999999999999999999999999999999999999999.99999999999999999999, 999999999999999999999999999999999999999999999.99999999999999999999, 3.402823466E+38, 3.402823466E+38, 1.7976931348623E+308, 1.7976931348623E+308, 8388607, 16777215, 9223372036854775807, 18446744073709551615, NULL, '1000-01-01', '1000-01-01 00:00:00', '1970-01-01 01:00:01', '-838:59:59', '11E', 'binary : ààéé\"\"çç\\'\\'__((èè--èè&&ôö$£µ*ù1234567890auebsqtxbneu<>?,;./:§!}]@^`|[{#~¹azerty¤====+-*/.\\'\"abcdefghijklmnopqrstuvwx', 'varbinary : &ôö$£µ*eizrssù1sqtxbne ààééerjsfdjoz\"\"çç\\'\\'__((èè--èè&u<>?,;./uerngpd:§!}]@^`|[{#~¹a==+-*/2zaoizhqsdptp-----erty¤==34567890aueb.\\'\"', - 15687691, - 94367, - 594, - 0.0756812, - 75618.937810, - 9782518624.394, - 146823, - 53);
INSERT INTO test_ocmp (f_autoinc_not_null_no_def, f_int_null_no_def, f_smallint_null_no_def, f_decimal_12_4_null_no_def, f_datetime_null_no_def, f_float_null_no_def, f_double_null_no_def, f_int24_null_no_def, f_date_null_no_def, f_time_null_no_def, f_year_null_no_def, f_string_null_no_def, f_varstring_null_no_def, f_blobtext_null_no_def, f_blobblob_null_no_def, f_blobtiny_null_no_def, f_blobmedium_null_no_def, f_bloblong_null_no_def, f_blobimg_null_no_def, f_enum_null_no_def, f_set_null_no_def, f_int_default_null, f_timestamp_null_no_def, f_bit_null_no_def, f_tinyint_null_no_def_signed, f_tinyint_null_no_def_unsigned, f_int_null_no_def_signed, f_int_null_no_def_unsigned, f_smallint_null_no_def_signed, f_smallint_null_no_def_unsigned, f_decimal_65_20_null_no_def_signed, f_decimal_65_20_null_no_def_unsigned, f_float_null_no_def_signed, f_float_null_no_def_unsigned, f_double_null_no_def_signed, f_double_null_no_def_unsigned, f_int24_null_no_def_signed, f_int24_null_no_def_unsigned, f_bigint_null_no_def_signed, f_bigint_null_no_def_unsigned, f_string_default_null, f_date_not_null_def20110101, f_datetime_not_null_def20111011140534, f_timestamp_not_null_def20110510, f_time_not_null_def214702, f_big_enum_default_null, f_binary_default_null, f_varbinary_default_null, f_bigint_def_0, f_int_def_0, f_smallint_def_0, f_decimal_20_9_def_0, f_float_def_0, f_double_def_0, f_int24_def_0, f_tinyint_def_0) 
  VALUES (2, 20, 90, 200.4321, '1971-07-24 08:01:02', 987415.8750, 9876543.987654e+4, 12345, '1971-07-24', '08:01:02', 1971, 'string : ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'varstring : ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'blobtext : lopsem lopsem lopsem three times, trois fois lopsem et une série d\\'accents é è ç à ù %%% ', 'blobblob : oeritdfgnkjnuerhdfgkdxeoirtijrendlkfnklxlkxerpzerpzejjffdidfosidjfiezrdsnfskdfpzerzeprsdfsdfksdfpoeporzerezp', 'blobtiny : 002çç\"ééén<znsdizpre89*-56211sf1sdf6azeae', 'blobmedium : 002çç\"ééén<znsdizpre89*-56211sf1sdf6azeae + 002çç\"ééén<znsdizpre89*-56211sf1sdf6azeae', 'bloblong : 002çç\"ééén<znsdizpre89*-56211sf1sdf6azeae + 002çç\"ééén<znsdizpre89*-56211sf1sdf6azeae + 002çç\"ééén<znsdizpre89*-56211sf1sdf6azeae', LOAD_FILE('" ^ Fixture_config.testfile2 ^ "'), 'enum3', 'set1,set2', NULL, '2012-04-21 12:50:59', b'1111111111111111111111111111111111111111111111111111111111111111', -128, 0, -2147483648, 0, -32768, 0, -999999999999999999999999999999999999999999999.99999999999999999999, 0, -1.175494351E-38, 0, -2.2250738585072014E-308, 0, -8388608, 0, -9223372036854775808, 0, NULL, '9999-12-31', '9999-12-31 23:59:59', '2038-01-19 03:14:07', '838:59:59', NULL, 'binary : ààéé\"\"çç\\'\\'__((èè--èè&&ôö$£µ*ù1234567890auebsqtxbneu<>?,;./:§!}]@^`|[{#~¹azerty¤====+-*/.\\'\"abcdefghijklmnopqrstuvwx', 'varbinary : \032&ôö$£µ*eizrssù1sqtxbne ààééerjsfdjoz\"\"çç\\'\\'__((èè--èè&u<>?,;./uerngpd:§!}]@^`|[{#~¹a==+-*/2zaoizhqsdptp-----erty¤==34567890aueb.\\'\"', - 10954041, - 59140367, - 2964, - 0.91104, - 18.810, - 6657425211.1111111, - 95470, - 61);

DROP TABLE IF EXISTS test_ocmp_geo;
CREATE TABLE test_ocmp_geo (
f_geo_point_null_no_def POINT
);

INSERT INTO test_ocmp_geo (f_geo_point_null_no_def) 
  VALUES (POINT(231,4));
INSERT INTO test_ocmp_geo (f_geo_point_null_no_def) 
  VALUES (POINT(-2, 5));

DROP TABLE IF EXISTS test_ocmp_blobbig;
CREATE TABLE test_ocmp_blobbig (
f_blobbig_null_no_def LONGBLOB
);

INSERT INTO test_ocmp_blobbig (f_blobbig_null_no_def) 
  VALUES (LOAD_FILE('" ^ Fixture_config.testfile3 ^ "'));

DROP TABLE IF EXISTS test_ocmp_date;
CREATE TABLE test_ocmp_date (
f_date_null_no_def DATE,
f_time_null_no_def TIME,
f_datetime_null_no_def DATETIME,
f_timestamp_null_no_def TIMESTAMP
);

INSERT INTO test_ocmp_date (f_date_null_no_def, f_time_null_no_def, f_datetime_null_no_def, f_timestamp_null_no_def)
  VALUES ('0000-00-00', '00:00:00', '0000-00-00 00:00:00', '0000-00-00 00:00:00');
INSERT INTO test_ocmp_date (f_date_null_no_def, f_time_null_no_def, f_datetime_null_no_def, f_timestamp_null_no_def)
  VALUES (DATE_ADD('1992-12-31', INTERVAL '1.999999' SECOND_MICROSECOND), 
	  '21:19:49.07', 
	  DATE_ADD('1990-11-05 13:17:01.000001', INTERVAL '3.567891' SECOND_MICROSECOND), 
	  DATE_ADD('2000-01-01 01:01:01', INTERVAL '14.1781' SECOND_MICROSECOND));

DROP TABLE IF EXISTS test_ocmp_bigstring;
CREATE TABLE test_ocmp_bigstring (
f_bigstring_char255_def_null CHAR(255) DEFAULT NULL,
f_bigstring_binary255_def_null BINARY(255) DEFAULT NULL
);

INSERT INTO test_ocmp_bigstring (f_bigstring_char255_def_null, f_bigstring_binary255_def_null)
  VALUES ('" ^ (Fixture_config.mysql_escape_string (Fixture_config.build_string 255)) ^ "', '" ^ (Fixture_config.mysql_escape_string (Fixture_config.build_string 255)) ^ "');
INSERT INTO test_ocmp_bigstring (f_bigstring_char255_def_null, f_bigstring_binary255_def_null)
  VALUES ('" ^ (Fixture_config.mysql_escape_string (Fixture_config.build_string 255)) ^ "', '" ^ (Fixture_config.mysql_escape_string (Fixture_config.build_string 255)) ^ "');

DROP TABLE IF EXISTS test_ocmp_bigvarchar;
CREATE TABLE test_ocmp_bigvarchar (
f_bigstring_varchar65532_def_empty VARCHAR(65532) DEFAULT ''
);

INSERT INTO test_ocmp_bigvarchar (f_bigstring_varchar65532_def_empty)
  VALUES ('" ^ (Fixture_config.mysql_escape_string (Fixture_config.build_string 65532)) ^ "');
INSERT INTO test_ocmp_bigvarchar (f_bigstring_varchar65532_def_empty)
  VALUES ('" ^ (Fixture_config.mysql_escape_string (Fixture_config.build_string 65532)) ^ "');

DROP TABLE IF EXISTS test_ocmp_bigvarbinary;
CREATE TABLE test_ocmp_bigvarbinary (
f_bigstring_varbinary65532_def_null VARBINARY(65532) DEFAULT NULL
);

INSERT INTO test_ocmp_bigvarbinary (f_bigstring_varbinary65532_def_null)
  VALUES ('" ^ (Fixture_config.mysql_escape_string (Fixture_config.build_string 65532)) ^ "');
INSERT INTO test_ocmp_bigvarbinary (f_bigstring_varbinary65532_def_null)
  VALUES ('" ^ (Fixture_config.mysql_escape_string (Fixture_config.build_string 65532)) ^ "');

DROP TABLE IF EXISTS test_ocmp_manyblobs;
CREATE TABLE test_ocmp_manyblobs (
f_blob1_def_null MEDIUMBLOB DEFAULT NULL,
f_blob2_def_null MEDIUMBLOB DEFAULT NULL,
f_blob3_def_null MEDIUMBLOB DEFAULT NULL,
f_blob4_def_null MEDIUMBLOB DEFAULT NULL,
f_blob5_def_null MEDIUMBLOB DEFAULT NULL,
f_blob6_def_null MEDIUMBLOB DEFAULT NULL,
f_blob7_def_null MEDIUMBLOB DEFAULT NULL,
f_blob8_def_null MEDIUMBLOB DEFAULT NULL,
f_blob9_def_null MEDIUMBLOB DEFAULT NULL,
f_blob10_def_null MEDIUMBLOB DEFAULT NULL
);

INSERT INTO test_ocmp_manyblobs (f_blob1_def_null, f_blob2_def_null, f_blob3_def_null, f_blob4_def_null, f_blob5_def_null, f_blob6_def_null, f_blob7_def_null, f_blob8_def_null, f_blob9_def_null, f_blob10_def_null)
  VALUES (LOAD_FILE('" ^ Fixture_config.testfile4 ^ "'), LOAD_FILE('" ^ Fixture_config.testfile4 ^ "'), LOAD_FILE('" ^ Fixture_config.testfile4 ^ "'), LOAD_FILE('" ^ Fixture_config.testfile4 ^ "'), LOAD_FILE('" ^ Fixture_config.testfile4 ^ "'), LOAD_FILE('" ^ Fixture_config.testfile4 ^ "'), LOAD_FILE('" ^ Fixture_config.testfile4 ^ "'), LOAD_FILE('" ^ Fixture_config.testfile4 ^ "'), LOAD_FILE('" ^ Fixture_config.testfile4 ^ "'), LOAD_FILE('" ^ Fixture_config.testfile4 ^ "'));
INSERT INTO test_ocmp_manyblobs (f_blob1_def_null, f_blob2_def_null, f_blob3_def_null, f_blob4_def_null, f_blob5_def_null, f_blob6_def_null, f_blob7_def_null, f_blob8_def_null, f_blob9_def_null, f_blob10_def_null)
  VALUES (LOAD_FILE('" ^ Fixture_config.testfile4 ^ "'), LOAD_FILE('" ^ Fixture_config.testfile4 ^ "'), LOAD_FILE('" ^ Fixture_config.testfile4 ^ "'), LOAD_FILE('" ^ Fixture_config.testfile4 ^ "'), LOAD_FILE('" ^ Fixture_config.testfile4 ^ "'), LOAD_FILE('" ^ Fixture_config.testfile4 ^ "'), LOAD_FILE('" ^ Fixture_config.testfile4 ^ "'), LOAD_FILE('" ^ Fixture_config.testfile4 ^ "'), LOAD_FILE('" ^ Fixture_config.testfile4 ^ "'), LOAD_FILE('" ^ Fixture_config.testfile4 ^ "'));

DROP TABLE IF EXISTS test_ocmp_auto_increment_ui;
CREATE TABLE test_ocmp_auto_increment_ui (
f_autoinc INT UNSIGNED AUTO_INCREMENT NOT NULL,
f_int INT,
PRIMARY KEY (f_autoinc)
);

DROP TABLE IF EXISTS test_ocmp_auto_increment_ubi;
CREATE TABLE test_ocmp_auto_increment_ubi (
f_autoinc BIGINT UNSIGNED AUTO_INCREMENT NOT NULL,
f_int INT,
PRIMARY KEY (f_autoinc)
);

DROP TABLE IF EXISTS test_ocmp_auto_increment_sbi;
CREATE TABLE test_ocmp_auto_increment_sbi (
f_autoinc BIGINT SIGNED AUTO_INCREMENT NOT NULL,
f_int INT,
PRIMARY KEY (f_autoinc)
);

DROP PROCEDURE IF EXISTS test_ocmp_proc_one_result;
delimiter //
CREATE PROCEDURE test_ocmp_proc_one_result()
BEGIN
  SELECT 100 AS oh, 'ABC' AS abc;
END//
delimiter ;

DROP PROCEDURE IF EXISTS test_ocmp_proc_multiple_results;
delimiter //
CREATE PROCEDURE test_ocmp_proc_multiple_results()
BEGIN
  SELECT 1 AS one, 'A' AS a;
  SELECT 2 AS two, 'B' AS b;
  SELECT 3 AS three, 'C' AS c;
END//
delimiter ;

"
