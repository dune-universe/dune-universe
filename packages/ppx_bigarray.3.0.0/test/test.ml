open OUnit
open Bigarray

let test_array1_int_c () =
  let mk arr = Array1.of_array int c_layout arr in
  "0" @? (mk [||] = [%bigarray1.int.c [||]]);
  "1" @? (mk [|42|] = [%bigarray1.int.c [|42|]]);
  "3" @? (mk [|11; 12; 13|] = [%bigarray1.int.c [|11; 12; 13|] ])

let test_array1_int_fortran () =
  let mk arr = Array1.of_array int fortran_layout arr in
  "0" @? (mk [||] = [%bigarray1.int.fortran [||]]);
  "1" @? (mk [|42|] = [%bigarray1.int.fortran [|42|]]);
  "3" @? (mk [|11; 12; 13|] = [%bigarray1.int.fortran [|11; 12; 13|] ])

let test_array2_int_c () =
  let mk arr = Array2.of_array int c_layout arr in
  "1x1" @? (mk [|[|42|]|] = [%bigarray2.int.c [|[|42|]|]]);
  "0x0" @? (mk [||] = [%bigarray2.int.c [||]]);
  "2x0" @? (mk [|[||]; [||]|] = [%bigarray2.int.c [|[||]; [||]|]]);
  "2x3" @? (mk [|[|11; 12; 13|]; [|21; 22; 23|]|]
            = [%bigarray2.int.c [|[|11; 12; 13|]; [|21; 22; 23|]|] ])

let test_array2_int_fortran () =
  let mk arr = Array2.of_array int fortran_layout arr in
  "1x1" @? (mk [|[|42|]|] = [%bigarray2.int.fortran [|[|42|]|]]);
  "0x0" @? (mk [||] = [%bigarray2.int.fortran [||]]);
  "2x0" @? (mk [|[||]; [||]|] = [%bigarray2.int.fortran [|[||]; [||]|]]);
  "2x3" @? (mk [|[|11; 12; 13|]; [|21; 22; 23|]|]
            = [%bigarray2.int.fortran [|[|11; 12; 13|]; [|21; 22; 23|]|] ])

let test_array3_int_c () =
  let mk arr = Array3.of_array int c_layout arr in
  "1x1x1" @? (mk [|[|[|42|]|]|] = [%bigarray3.int.c [|[|[|42|]|]|]]);
  "0x0x0" @? (mk [||] = [%bigarray3.int.c [||]]);
  "2x0x0" @? (mk [|[||]; [||]|] = [%bigarray3.int.c [|[||]; [||]|]]);
  "2x3x0" @?
  (mk [|[|[||]; [||]; [||]|]; [|[||]; [||]; [||]|]|]
   = [%bigarray3.int.c [|[|[||]; [||]; [||]|]; [|[||]; [||]; [||]|]|] ]);
  "2x3x3" @?
  (mk [|[|[|111; 112; 113|]; [|121; 122; 123|]; [|131; 132; 133|]|];
        [|[|211; 212; 213|]; [|221; 222; 223|]; [|231; 232; 233|]|]|]
   = [%bigarray3.int.c
     [|[|[|111; 112; 113|]; [|121; 122; 123|]; [|131; 132; 133|]|];
       [|[|211; 212; 213|]; [|221; 222; 223|]; [|231; 232; 233|]|]|] ])

let test_array3_int_fortran () =
  let mk arr = Array3.of_array int fortran_layout arr in
  "1x1x1" @? (mk [|[|[|42|]|]|] = [%bigarray3.int.fortran [|[|[|42|]|]|]]);
  "0x0x0" @? (mk [||] = [%bigarray3.int.fortran [||]]);
  "2x0x0" @? (mk [|[||]; [||]|] = [%bigarray3.int.fortran [|[||]; [||]|]]);
  "2x3x0" @?
  (mk [|[|[||]; [||]; [||]|]; [|[||]; [||]; [||]|]|]
   = [%bigarray3.int.fortran [|[|[||]; [||]; [||]|]; [|[||]; [||]; [||]|]|] ]);
  "2x3x3" @?
  (mk [|[|[|111; 112; 113|]; [|121; 122; 123|]; [|131; 132; 133|]|];
        [|[|211; 212; 213|]; [|221; 222; 223|]; [|231; 232; 233|]|]|]
   = [%bigarray3.int.fortran
     [|[|[|111; 112; 113|]; [|121; 122; 123|]; [|131; 132; 133|]|];
       [|[|211; 212; 213|]; [|221; 222; 223|]; [|231; 232; 233|]|]|] ])

let test_padding_c () =
  let mk arr = Array3.of_array int c_layout arr in
  "2x3x3" @?
  (mk [|[|[|111; 112; 113|]; [|121; 122; 123|]; [|131; 0; 0|]|];
        [|[|211; 212; 213|]; [|221; 222;   0|]; [|  0; 0; 0|]|]|]
   = [%bigarray3.int.c
     [|[|[|111; 112; 113|]; [|121; 122; 123|]; [|131;         |]|];
       [|[|211; 212; 213|]; [|221; 222;    |];                |]|]
       [@bigarray.padding 0] ])

let test_padding_fortran () =
  let mk arr = Array3.of_array int fortran_layout arr in
  "2x3x3" @?
  (mk [|[|[|111; 112; 113|]; [|121; 122; 123|]; [|131; 0; 0|]|];
        [|[|211; 212; 213|]; [|221; 222;   0|]; [|  0; 0; 0|]|]|]
   = [%bigarray3.int.fortran
     [|[|[|111; 112; 113|]; [|121; 122; 123|]; [|131;         |]|];
       [|[|211; 212; 213|]; [|221; 222;    |];                |]|]
       [@bigarray.padding 0] ])

let test_dynamic_layout () =
  let x = c_layout in
  let y = fortran_layout in
  let mk layout arr = Array3.of_array int layout arr in
  "x" @?
  (mk x [|[|[|111; 112; 113|]; [|121; 122; 123|]; [|131; 132; 133|]|];
          [|[|211; 212; 213|]; [|221; 222; 223|]; [|231; 232; 233|]|]|]
   = [%bigarray3.int.x
     [|[|[|111; 112; 113|]; [|121; 122; 123|]; [|131; 132; 133|]|];
       [|[|211; 212; 213|]; [|221; 222; 223|]; [|231; 232; 233|]|]|] ]);
  "y" @?
  (mk y [|[|[|111; 112; 113|]; [|121; 122; 123|]; [|131; 132; 133|]|];
          [|[|211; 212; 213|]; [|221; 222; 223|]; [|231; 232; 233|]|]|]
   = [%bigarray3.int.y
     [|[|[|111; 112; 113|]; [|121; 122; 123|]; [|131; 132; 133|]|];
       [|[|211; 212; 213|]; [|221; 222; 223|]; [|231; 232; 233|]|]|] ])

let test_alias () =
  let open Ppx_bigarray_runtime in
  let ppx_bigarray__x = { kind = Bigarray.int;
                          layout = Bigarray.c_layout; } in
  let ppx_bigarray__y = { kind = Bigarray.int16_signed;
                          layout = Bigarray.fortran_layout; } in
  let mk alias arr = Array3.of_array alias.kind alias.layout arr in
  "x" @?
  (mk ppx_bigarray__x
     [|[|[|111; 112; 113|]; [|121; 122; 123|]; [|131; 132; 133|]|];
       [|[|211; 212; 213|]; [|221; 222; 223|]; [|231; 232; 233|]|]|]
   = [%bigarray3.x
     [|[|[|111; 112; 113|]; [|121; 122; 123|]; [|131; 132; 133|]|];
       [|[|211; 212; 213|]; [|221; 222; 223|]; [|231; 232; 233|]|]|] ]);
  "y" @?
  (mk ppx_bigarray__y
     [|[|[|111; 112; 113|]; [|121; 122; 123|]; [|131; 132; 133|]|];
       [|[|211; 212; 213|]; [|221; 222; 223|]; [|231; 232; 233|]|]|]
   = [%bigarray3.y
     [|[|[|111; 112; 113|]; [|121; 122; 123|]; [|131; 132; 133|]|];
       [|[|211; 212; 213|]; [|221; 222; 223|]; [|231; 232; 233|]|]|] ])

let suite =
  "ppx_bigarray" >::: [
    "array1,kind=int,layout=C" >:: test_array1_int_c;
    "array1,kind=int,layout=Fortran" >:: test_array1_int_fortran;
    "array2,kind=int,layout=C" >:: test_array2_int_c;
    "array2,kind=int,layout=Fortran" >:: test_array2_int_fortran;
    "array3,kind=int,layout=C" >:: test_array3_int_c;
    "array3,kind=int,layout=Fortran" >:: test_array3_int_fortran;
    "padding,layout=C" >:: test_padding_c;
    "padding,layout=Fortran" >:: test_padding_fortran;
    "dynamic-layout" >:: test_dynamic_layout;
    "alias" >:: test_alias;
  ]

let () = run_test_tt_main suite |> ignore
