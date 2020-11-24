(**

   To run those tests, you should first create the following databases and user :

   DROP DATABASE IF EXISTS test_ocaml_ocmp_latin1;
   DROP DATABASE IF EXISTS test_ocaml_ocmp_utf8;
   CREATE DATABASE test_ocaml_ocmp_latin1 CHARACTER SET latin1;
   CREATE DATABASE test_ocaml_ocmp_utf8 CHARACTER SET utf8;

   GRANT ALL PRIVILEGES ON test_ocaml_ocmp_latin1.* TO 'user_ocaml_ocmp'@'localhost' IDENTIFIED BY 'ocmp';
   GRANT ALL PRIVILEGES ON test_ocaml_ocmp_utf8.* TO 'user_ocaml_ocmp'@'localhost';

   GRANT ALL PRIVILEGES ON test_ocaml_ocmp_latin1.* TO 'u_ocmp_npauth'@'localhost' IDENTIFIED WITH mysql_native_password;
   GRANT ALL PRIVILEGES ON test_ocaml_ocmp_utf8.* TO 'u_ocmp_npauth'@'localhost';
   SET PASSWORD FOR 'u_ocmp_npauth'@'localhost' = PASSWORD('ocmpnpauth');

   GRANT ALL PRIVILEGES ON test_ocaml_ocmp_latin1.* TO 'u_ocmp_npauth_2'@'localhost' IDENTIFIED WITH mysql_native_password;
   GRANT ALL PRIVILEGES ON test_ocaml_ocmp_utf8.* TO 'u_ocmp_npauth_2'@'localhost';
   SET PASSWORD FOR 'u_ocmp_npauth_2'@'localhost' = PASSWORD('ocmpnpauth2');

   GRANT FILE ON test_ocaml_ocmp_latin1.* TO 'user_ocaml_ocmp'@'localhost';
   GRANT FILE ON test_ocaml_ocmp_utf8.* TO 'user_ocaml_ocmp'@'localhost';

   GRANT GRANT OPTION ON test_ocaml_ocmp_latin1.* TO 'user_ocaml_ocmp'@'localhost';
   GRANT GRANT OPTION ON test_ocaml_ocmp_utf8.* TO 'user_ocaml_ocmp'@'localhost';


   For the test with several blob columns, you may have to increase innodb_log_file_size in my.cnf configuration file:

   [mysqld]
   innodb_log_file_size = 64M

   For the GRANT FILE, if you get the following error:
   ERROR 1221 (HY000): Incorrect usage of DB GRANT and GLOBAL PRIVILEGES
   you can add FILE privilege for each database:
   GRANT FILE ON *.* TO 'user_ocaml_ocmp'@'localhost';

   For the GRANT rights, if you get the following error:
   Errno: 33285 / Sql state: 42000 / Message: You are not allowed to create a user with GRANT
   you can set the privilege from a mysql client:
   USE mysql;
   UPDATE user SET Create_user_priv='Y' WHERE User LIKE '%ocmp%';

*)

let testfile f = 
  let dir = Unix.getcwd () in
  let subdir = "" in
  Filename.concat (dir ^ (Filename.dir_sep) ^ subdir) f

let testfile1 = testfile "caml-inria-fr.128x58.gif"
let testfile2 = testfile "logo-full-thumb.png"
let testfile3 = testfile "ocaml-3.12-refman.pdf"
let testfile4 = testfile "twomega.bin"

let content_file f = 
  let handle = open_in_bin f in
  let length = in_channel_length handle in
  let buffer = Bytes.make length '\000' in
  let () = really_input handle buffer 0 length in
  buffer

let content_testfile1 = content_file testfile1
let content_testfile2 = content_file testfile2
let content_testfile3 = content_file testfile3
let content_testfile4 = content_file testfile4

let big_enum_column = 
  let s = ref "" in
  let () = 
    for i = 0 to 300 do
      let e = Printf.sprintf "'%X', " i in
      s := !s ^ e
    done
  in
  String.sub !s 0 ((String.length !s) - 2)

let mysql_escape_string s = 
  let escape = ref "" in
  let f c = 
    let backslash = (String.make 1 (Char.chr 92)) in
    let c_0 = Char.chr 0 in (* ASCII NULL *)
    let c_39 = Char.chr 39 in (* \' *)
    let c_34 = Char.chr 34 in (* "\"" *)
    let c_8 = Char.chr 8 in (* \b *)
    let c_10 = Char.chr 10 in (* \n *)
    let c_13 = Char.chr 13 in (* \r *)
    let c_9 = Char.chr 9 in (* \t *)
    let c_26 = Char.chr 26 in (* \Z *)
    let c_92 = Char.chr 92 in (* \\ *)
    let ok = 
      if ( c = c_0 ) then
        backslash ^ "0"
      else if (c = c_39) then
        backslash ^ (String.make 1 c) 
      else if (c = c_34) then
        backslash ^ (String.make 1 c)
      else if (c = c_8) then
        backslash ^ "b"
      else if (c = c_10) then
        backslash ^ "n"
      else if (c = c_13) then
        backslash ^ "r"
      else if (c = c_9) then
        backslash ^ "t"
      else if (c = c_26) then
        backslash ^ "Z"
      else if (c = c_92 ) then
        backslash ^ (String.make 1 c) 
      else
        (String.make 1 c)
    in
    escape := !escape ^ ok
  in
  let () = String.iter f s in
  !escape

let build_string n = 
  let s = ref "" in
  let () = 
    let c = ref 32 in
    while (String.length !s < n) do
      let () = c := 
          if (!c = 127) then 32 else !c
      in
      let () = 
        s := !s ^ (String.make 1 (Char.chr !c)) 
      in
      incr c
    done
  in
  !s
