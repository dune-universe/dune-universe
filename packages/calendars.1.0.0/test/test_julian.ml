(* Original test is from Scott E. Lee
 * Copyright 1993-1995, Scott E. Lee, all rights reserved.
 * Permission granted to use, copy, modify, distribute and sell so long as
 * the above copyright and this permission statement are retained in all
 * copies.  THERE IS NO WARRANTY - USE AT YOUR OWN RISK.
 *
 * OCaml port is from Julien Sagot
 * Copyright 2019, Julien Sagot
 *)

open OUnit
open Calendars

let febLength year : int =
  if (if year < 0 then year + 1 else year) mod 4 = 0 then 29 else 28

let monthLength : int array = [| 31 ; 28 ; 31 ; 30 ; 31 ; 30 ; 31 ; 31 ; 30 ; 31 ; 30 ; 31 |]

let assert_equal_dmy =
  assert_equal
    ~printer:(fun { day ; month ; year ; _ } ->
        Printf.sprintf "{ day:(%d) ; month:(%d) ; year:(%d) }" day month year )

let assert_equal_sdn =
  assert_equal ~printer:string_of_int

let _ =
  run_test_tt_main begin
    "Julian <-> SDN" >::
    fun _ ->
      let sdn = ref 0 in
      for year = -4713 to 10000 do
        if year <> 0 then
          for month = 1 to 12 do
            for day = 1 to (if month = 2 then febLength (year) else Array.get monthLength @@ month - 1) do
              let d = { day ; month ; year ; delta = 0 } in
              let sdn' = sdn_of_julian d in
              assert_equal_sdn !sdn sdn' ;
              assert_equal_dmy d (julian_of_sdn sdn') ;
              incr sdn
            done
          done
      done
  end
