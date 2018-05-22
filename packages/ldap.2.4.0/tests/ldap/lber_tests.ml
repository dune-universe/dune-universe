open Lber

let encode_decode_int32 i =
  let e_i32 = encode_ber_int32 i in
  let rb = readbyte_of_string e_i32 in
    decode_ber_int32 rb

let rec test_positive_encode_decode_int32 i =
  if i < Int32.max_int then
    let result =
      try encode_decode_int32 i
      with exn ->
	print_endline ("unhandled exception: " ^ (Printexc.to_string exn) ^
			 " with int: " ^ (Int32.to_string i));
	exit 0
    in
      if  result <> i then
	failwith ("I encode: " ^ (Int32.to_string i) ^
		    " and I get: " ^ (Int32.to_string result))
      else
	((if Int32.rem i 1000000l = 0l then
	    print_endline ("i:" ^ (Int32.to_string i)));
	 test_positive_encode_decode_int32 (Int32.succ i))

let rec test_negative_encode_decode_int32 i =
  if i > Int32.min_int then
    let result =
      try encode_decode_int32 i
      with exn ->
	print_endline ("unhandled exception: " ^ (Printexc.to_string exn) ^
			 " with int: " ^ (Int32.to_string i));
	exit 0
    in
      if  result <> i then
	failwith ("I encode: " ^ (Int32.to_string i) ^
		    " and I get: " ^ (Int32.to_string result))
      else
	((if Int32.rem i (-1000000l) = 0l then
	    print_endline ("i:" ^ (Int32.to_string i)));
	 test_negative_encode_decode_int32 (Int32.pred i))

let main () =
(*  print_endline "testing integer encoder/decoder with positive numbers";
  test_positive_encode_decode_int32 0l; *)
  print_endline "testing integer encoder/decoder with negative numbers";
  test_negative_encode_decode_int32 0l
;;

main ()
