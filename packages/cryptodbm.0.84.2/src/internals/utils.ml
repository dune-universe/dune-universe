
(* The Random module is sufficient to create salts. *)
let gen = Random.State.make_self_init ()

let random_int gen x = Random.State.int gen (max 1 x)

(* Creates a random string of given length. *)
let random_string gen len =
  assert (len >= 0) ;
  let res = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set res i (Char.chr (random_int gen 256)) ;
  done ;
  Bytes.to_string res

let date_to_string date =
  Unix.(
  let tm = localtime date in
  
  Printf.sprintf "%d-%02d-%02d--%02dH%02dM%02ds"
    (1900 + tm.tm_year) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec
 )


(* Tests if file is readable, appendable, exists *)
open FileUtil

let test_unix_open flags f =
  try
    let descr = Unix.openfile f flags 0 in
    Unix.close descr ;
    true
  with _ -> false

let is_readable   f = test Is_file f && test_unix_open [Unix.O_RDONLY] f
let is_appendable f = test Is_file f && test_unix_open [Unix.O_APPEND] f
let file_exists f = test Exists f
let read_perm file = int_of_permission (stat file).permission
let cp f1 f2 = cp ~force:Force [f1] f2
let remove files = rm ~force:Force files
let mkdir dir = mkdir dir

(* Careful! This is rm -rf *)
let rmrf dir = rm ~force:Force ~recurse:true [dir]

