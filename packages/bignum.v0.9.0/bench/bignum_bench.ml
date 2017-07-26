open Core
open Bignum.Std
open Bignum

let number_literals =
    [ "-100.00000000";
      "100.00000000";
      "0.00000000";
      "-200.00000000";
      "200.00000000";
      "-300.00000000";
      "300.00000000";
      "-400.00000000";
      "-1000.00000000";
      "1000.00000000";
      "-1.00000000";
      "400.00000000";
      "-500.00000000";
      "1.00000000";
      "500.00000000";
      "-600.00000000";
      "-2000.00000000";
      "2.00000000";
      "-2.00000000";
      "600.00000000";
      "0.20720000";
      "-0.20227524";
      "0.18800000";
      "0.16550000";
      "0.15950000";
      "0.13000000";
      "0.12950000";
      "0.11950000";
      "-0.07232871";
      "0.05950000";
      "-0.05424653";
      "0.04600437";
      "0.04600000";
      "0.04050000";
      "-0.03616435";
      "0.03550391";
      "0.03550000";
      "0.02000000";
      "0.01950000";
      "0.01050000";
      "-316673.67291835";
      "3423.123456789";
      "-3423.1234567891";
    ]
;;

let scientific_literals =
  [ "8.24519553715492287e5"
  ; "-4.28570523991592777e37"
  ; "2.18204973736406361e86"
  ; "-2.60593290918158878e41"
  ; "-1.79980831979083856e-41"
  ; "3.38698564095146792e2"
  ; "-1.73780370352669731e-19"
  ; "-3.45013408656372044e80"
  ; "7.77199265223893632e-80"
  ; "4.79538294005178895e16"
  ; "1.99060465631274649e-76"
  ; "-6.83185520073336019e-39"
  ; "3.85194997042382733e10"
  ; "-1.83559331519703530e26"
  ; "2.68009403746226355e52"
  ; "-1.15702637735287027e50"
  ; "1.23026999154386972e-80"
  ; "-6.40677287378442009e36"
  ; "-1.78715017537117271e36"
  ; "-8.53370546551037981e-20"
  ; "-4.18890464251814377e35"
  ; "2.15140580926985290e50"
  ; "-3.15966058800821580e57"
  ; "-6.5351404796330480e-55"
  ; "-8.89655833226289008e54"
  ; "6.71358069394046099e60"
  ; "-2.11336369987876480e73"
  ; "1.75954441289825824e-67"
  ; "9.4964032650847118e11"
  ; "2.33544686974449128e-37"
  ; "-7.46423195727629164e85"
  ; "5.80271472928164262e6"
  ; "-7.3724793773695281e10"
  ; "-4.95886455013940102e29"
  ; "5.35364968642061596e-14"
  ; "2.32475308311864725e85"
  ; "1.53771199684946413e91"
  ; "7.12936505610979064e-70"
  ; "6.98555528473250708e14"
  ; "3.51244299794558383e71"
  ; "4.6462236999917593e-17"
  ; "7.71057505458522988e-15"
  ; "4.8489280022172023e-10"
  ]
;;

let fraction_literals =
  [ "44160789807965575/4829426549353870"
  ; "30939674654694147/6322829755914550"
  ; "-72428369358047507/5195095119360269"
  ; "-53737643075719198/6087445392550459"
  ; "-71929533019447655/1113041887496084"
  ; "5219047178260591/7163389730123283"
  ; "99042382321897552/3490424022614859"
  ; "-45751344425379902/1721437534821349"
  ; "-74746005231671836/7966364964691721"
  ; "-34739622790035318/496397132184469"
  ; "62228184750686697/5477651051019211"
  ; "-15194044929039901/5117411024806969"
  ; "-79361324855554022/9814827237061012"
  ; "-88391936350009406/8178593781327831"
  ; "95184845839450540/8166440588734285"
  ; "-48724667931563936/7089378564856185"
  ; "74585463996569066/5765737794233683"
  ; "-60054429901422974/6459232999238932"
  ; "60303663465038373/5449299391621362"
  ; "30796371699843189/1493195114700822"
  ; "95031587105217735/8173467417055182"
  ; "57725370314579029/2702654208651274"
  ; "85669045471174690/2610811638228058"
  ; "59952168764027711/1660319521308050"
  ; "92762599951257346/9032155865899525"
  ; "35435472284007576/4301647727457785"
  ; "64900617253170319/9606285049692550"
  ; "22425925701481557/7024328874214221"
  ; "34835377315112917/4729757958046577"
  ; "-29870928430533082/6082663646464600"
  ; "5055033876465838/1826158032470184"
  ; "54706809574838730/9090003651414655"
  ; "55207827381482296/445761198258308"
  ; "-25193794770638457/9635062653093763"
  ; "48752489871065991/1780115067648190"
  ; "-71196915255645433/3188067365661100"
  ; "9173803675692446/6902827425721030"
  ; "48949010983309632/9966256704200818"
  ; "90393195842509424/4310462036284360"
  ; "21139118103047194/4745871355142052"
  ; "-61616642439476948/6340173038108857"
  ; "86787735713527764/9756236291252226"
  ; "18115717877327883/4432870485149862"
  ]
;;

let numbers_decimal = List.map number_literals ~f:of_string
let numbers_scientific = List.map scientific_literals ~f:of_string
let numbers_fraction = List.map fraction_literals ~f:of_string

let sexps_decimal = List.map numbers_decimal ~f:sexp_of_t
let sexps_scientific = List.map numbers_scientific ~f:sexp_of_t
let sexps_fraction = List.map numbers_fraction ~f:sexp_of_t

let%bench_module "Bignum of_string/to_string" =
  (module struct
    let%bench "of_string (decimal)" = List.iter number_literals ~f:(fun b ->
      let (_ : Bignum.t) = Bignum.of_string b in
      ())
    ;;

    let%bench "of_string (scientific)" = List.iter scientific_literals ~f:(fun b ->
      let (_ : Bignum.t) = Bignum.of_string b in
      ())
    ;;

    let%bench "of_string (fraction)" = List.iter fraction_literals ~f:(fun b ->
      let (_ : Bignum.t) = Bignum.of_string b in
      ())
    ;;

    let%bench "to_string (decimal)" = List.iter numbers_decimal ~f:(fun b ->
      let (_ : string) = to_string b in
      ())
    ;;

    let%bench "to_string (scientific)" = List.iter numbers_scientific ~f:(fun b ->
      let (_ : string) = to_string b in
      ())
    ;;

    let%bench "to_string (fraction)" = List.iter numbers_fraction ~f:(fun b ->
      let (_ : string) = to_string b in
      ())
    ;;
  end)
;;

let%bench_module "Bignum of_sexp/to_sexp" =
  (module struct
    let%bench "of_sexp (decimal)" = List.iter sexps_decimal ~f:(fun s ->
      let (_ : Bignum.t) = t_of_sexp s in
      ())
    ;;

    let%bench "of_sexp (scientific)" = List.iter sexps_scientific ~f:(fun s ->
      let (_ : Bignum.t) = t_of_sexp s in
      ())
    ;;

    let%bench "of_sexp (fraction)" = List.iter sexps_fraction ~f:(fun s ->
      let (_ : Bignum.t) = t_of_sexp s in
      ())
    ;;

    let%bench "to_sexp (decimal)" = List.iter numbers_decimal ~f:(fun b ->
      let (_ : Sexp.t) = sexp_of_t b in
      ())
    ;;

    let%bench "to_sexp (scientific)" = List.iter numbers_scientific ~f:(fun b ->
      let (_ : Sexp.t) = sexp_of_t b in
      ())
    ;;

    let%bench "to_sexp (fraction)" = List.iter numbers_fraction ~f:(fun b ->
      let (_ : Sexp.t) = sexp_of_t b in
      ())
    ;;
  end)
;;

let%bench_module "Bignum binprot" = (module struct

  let buf = Bigstring.create 128

  let%bench "roundtrip compact" = List.iter numbers_decimal ~f:(fun b ->
    let (_ : int) =
      Stable.V2.bin_writer_t.Bin_prot.Type_class.write buf ~pos:0 b
    in
    let (_ : Stable.V2.t) =
      Stable.V2.bin_reader_t.Bin_prot.Type_class.read buf ~pos_ref:(ref 0)
    in
    ())
  ;;

  let%bench "roundtrip classic" = List.iter numbers_decimal ~f:(fun b ->
    let (_ : int) =
      Stable.V1.bin_writer_t.Bin_prot.Type_class.write buf ~pos:0 b
    in
    let (_ : Stable.V1.t) =
      Stable.V1.bin_reader_t.Bin_prot.Type_class.read buf ~pos_ref:(ref 0)
    in
    ())
  ;;
end)

let%bench_module "round" = (module struct
  let%bench_fun "round_decimal" [@indexed digits = [0;3;6;9]] =
    fun () ->
    List.iter numbers_decimal ~f:(fun number -> ignore (round_decimal number ~digits : t))
  ;;

  let%bench "round" =
    List.iter numbers_decimal ~f:(fun number -> ignore (round number : t))
  ;;
end)


