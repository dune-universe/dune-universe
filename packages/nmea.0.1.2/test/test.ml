open OUnit2;;

let parse_data = [
  "$GPGGA,100412.326,5231.139,N,01324.930,E,1,12,1.0,0.0,M,0.0,M,,*6F";
	"$GPGGA,123519,4807.038,N,01131.000,E,1,08,0.9,545.4,M,46.9,M,,*47";
  "$GPGSA,A,3,01,02,03,04,05,06,07,08,09,10,11,12,1.0,1.0,1.0*30";
  "$GPRMC,100412.326,A,5231.139,N,01324.930,E,1862.5,266.2,100520,000.0,W*45";
  "$GPGGA,100413.326,5231.083,N,01324.086,E,1,12,1.0,0.0,M,0.0,M,,*6A";
  "$GPGSA,A,3,01,02,03,04,05,06,07,08,09,10,11,12,1.0,1.0,1.0*30";
  "$GPRMC,100413.326,A,5231.083,N,01324.086,E,1502.9,029.2,100520,000.0,W*4E";
  "$GPGGA,100414.326,5231.478,N,01324.307,E,1,12,1.0,0.0,M,0.0,M,,*67";
  "$GPGSA,A,3,01,02,03,04,05,06,07,08,09,10,11,12,1.0,1.0,1.0*30";
  "$GPRMC,100414.326,A,5231.478,N,01324.307,E,1502.9,029.2,100520,000.0,W*43";
  "$GPGSV,2,1,08,02,22,112,,10,02,264,,12,72,328,,15,14,177,*7E";
  "$GPGSV,2,2,08,17,04,036,,19,23,043,,24,70,107,,25,48,276,*77";  
  "$GPGSV,3,1,11,03,03,111,00,04,15,270,00,06,01,010,00,13,06,292,00*74";
  "$GPGSV,3,2,11,14,25,170,00,16,57,208,39,18,67,296,40,19,40,246,00*74";
  "$GPGSV,3,3,11,22,42,067,42,24,14,311,43,27,05,244,00,,,,*4D";
	"$GPGLL,4916.45,N,12311.12,W,225444.00,A*32";
	"$GPGLL,,,,,080904.00,V,N*4F";
  "$GPGLL,,,,,080904.00,V*4F";
  "$GPRMC,083344.00,V,,,,,,,090520,,,N*7B";
  "$GPGGA,083224.00,,,,,0,00,99.99,,,,,,*69";
	"$GPGGA,123519,4807.038,N,01131.000,E,1,08,0.9,545.4,M,46.9,M,,*47";
	"$GPGGA,134658.00,5106.9792,N,11402.3003,W,2,09,1.0,1048.47,M,-16.27,M,08,AAAA*60";
];;

let parse_fail_test s octx =
  try Nmea.Parse.parse s |> ignore; assert_equal true false 
  with | _ -> assert_equal true true
;;

let parse_test s octx = 
  Nmea.Parse.parse s |> ignore; assert_equal true true
;;

let rec loop_parse ch n octx = if n = 0 then () else
  match Nmea.Parse.next ch with
  | None -> assert(false); loop_parse ch (n-1) octx
  | Some(_) -> assert(true); loop_parse ch (n-1) octx
;;

let suite = "nmea" >::: [] 
  @ [ "stream" >:: loop_parse (open_in "test/data.txt") 6 ]
  @ List.mapi (fun i x -> (Printf.sprintf "parse_test.%d" i) >:: parse_test x) parse_data;;

let () = run_test_tt_main suite;;