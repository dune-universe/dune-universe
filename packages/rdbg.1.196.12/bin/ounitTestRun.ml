(* Time-stamp: <modified the 06/09/2019 (at 14:02) by Erwan Jahier> *)

open OUnit2

let test_string_to_string_list _test_ctxt = 
  OUnit2.assert_equal
    (Mypervasives.string_to_string_list "a    aa aaa aa")  
    ["a";"aa";"aaa";"aa"]
    ~printer:(String.concat ";")
    ~msg:"Mypervasives.string_to_string_list"

let string_of_list_int l = String.concat "," (List.map string_of_int l)

let test_list_rm_dup _test_ctxt = 
  let l =  [2;2;2;3;4;3;4;2;0;-1] in
  let lres = List.sort compare (Mypervasives.list_rm_dup l) in
  OUnit2.assert_equal lres [-1;0;2;3;4] 
                      ~printer:string_of_list_int 
                      ~msg:"Mypervasives.list_rm_dup"



(* Name the test cases and group them together *)
let suite =
"rdbg unit test suite">:::
 [
   "Mypervasives.string_to_string_list">:: test_string_to_string_list;
   "Mypervasives.list_rm_dup">::           test_list_rm_dup
 ]
;;

let () =
  run_test_tt_main suite
;;

