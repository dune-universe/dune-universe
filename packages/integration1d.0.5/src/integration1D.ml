(* File: integration1d.ml

   Copyright (C) 2008

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

(* Quadrature routines inspired from QUADPACK
   <http://www.netlib.org/quadpack/>. *)

open Printf

(* Specialized min/max functions that deal correctly with NaNs
   provided their first argument is not a NaN. *)
let min_float (a: float) b =
  assert(a = a);               (* [a] is not NaN *)
  if b <= a (* && b not NaN *) then b else a

let max_float (a:float) b =
  assert(a = a);               (* [a] is not NaN *)
  if b >= a (* && b not NaN *) then b else a

DEFINE IS_NOT_FINITE (x) = not(neg_infinity < x && x < infinity);;

(***********************************************************************
 *                  Elementary quadratures (Konrod)
 ***********************************************************************)

type qk_result = {
  result: float;  (* approximation to the integral of [f] *)
  abserr: float;  (* estimate of the modulus of the absolute error,
                     which should not exceed abs(i-result) *)
  resabs: float;  (* approximation to the integral of [abs f] *)
  resasc: float;  (* approximation to the integral of abs(f-i/(b-a)) *)
}
;;

exception Function_not_finite of float

let () =
  (* Install a custom printer (the default one does not print floats
     which is very annoying for debugging).
     CANVAS: http://caml.inria.fr/mantis/view.php?id=5040 *)
  let printer = function
    | Function_not_finite f ->
       Some("Integration1D.Function_not_finite(" ^ string_of_float f ^ ")")
    | _ -> None in
  Printexc.register_printer printer


let epsilon_float50 = epsilon_float *. 50.
let epsilon_float100 = epsilon_float *. 100.
let epsilon_float100p1 = epsilon_float *. 100. +. 1.
let min_float1000 = Pervasives.min_float *. 1000.
let small_resabs = Pervasives.min_float /. epsilon_float50

DEFINE INIT_GAUSS_ODD = 0.;;
DEFINE INIT_GAUSS_EVEN(n2) = fc *. wg.(n2);;
(* n0 = n-1, n1 = (n-3)/2 and n2 = n/2 - 1.
   [fv1.(0 .. n0)] i.e. [dim fv1 = n] -- idem for [fv2]. *)
DEFINE QK(n0, n1, n2, init_gauss, wg, xgk, wgk, fv1, fv2) =
  let hlgth = 0.5 *. (b -. a) in        (* half length *)
  let centr = a +. hlgth in
  let abs_hlgth = abs_float hlgth in
  (* Compute the kronrod approximation to the integral, and estimate
     the absolute error. *)
  let fc = f centr in
  if IS_NOT_FINITE(fc) then
    raise(Function_not_finite centr);
  let res_gauss = ref(init_gauss) in
  let res_kronrod = ref(fc *. wgk.(n0)) in
  let resabs = ref(abs_float !res_kronrod) in
  for j = 0 to n1 do
    let jtw = j * 2 + 1 in
    let absc = hlgth *. xgk.(jtw) in
    let val1 = centr -. absc in
    let fval1 = f val1 in
    if IS_NOT_FINITE(fval1) then
      raise(Function_not_finite val1);
    let val2 = centr +. absc in
    let fval2 = f val2 in
    if IS_NOT_FINITE(fval2) then
      raise(Function_not_finite val2);
    fv1.(jtw) <- fval1;
    fv2.(jtw) <- fval2;
    let fsum = fval1 +. fval2 in
    res_gauss := !res_gauss +. wg.(j) *. fsum;
    res_kronrod := !res_kronrod +. wgk.(jtw) *. fsum;
    resabs := !resabs +. wgk.(jtw) *. (abs_float fval1 +. abs_float fval2);
  done;
  for j = 0 to n2 do
    let jtw = j * 2 in
    let absc = hlgth *. xgk.(jtw) in
    let val1 = centr -. absc in
    let fval1 = f val1 in
    if IS_NOT_FINITE(fval1) then
      raise(Function_not_finite val1);
    let val2 = centr +. absc in
    let fval2 = f val2 in
    if IS_NOT_FINITE(fval2) then
      raise(Function_not_finite val2);
    fv1.(jtw) <- fval1;
    fv2.(jtw) <- fval2;
    res_kronrod := !res_kronrod +. wgk.(jtw) *. (fval1 +. fval2);
    resabs := !resabs +. wgk.(jtw) *. (abs_float fval1 +. abs_float fval2)
  done;
  let mean = !res_kronrod *. 0.5 in
  let resasc = ref(wgk.(n0) *. abs_float(fc -. mean)) in
  for j = 0 to n0 - 1 do
    resasc := !resasc +. wgk.(j) *. (abs_float(fv1.(j) -. mean)
                                   +. abs_float(fv2.(j) -. mean));
  done;
  (* Scale by the width of the integration region. *)
  resabs := !resabs *. abs_hlgth;
  resasc := !resasc *. abs_hlgth;
  let abserr = abs_float((!res_kronrod -. !res_gauss) *. hlgth) in
  let abserr =
    if !resasc = 0. || abserr = 0. then abserr
    else !resasc *. min_float 1. ((200. *. abserr /. !resasc)**1.5) in
  { result = !res_kronrod *. hlgth;
    resabs = !resabs;
    resasc = !resasc;
    abserr = (if !resabs <= small_resabs then abserr
              else max_float abserr (epsilon_float50 *. !resabs))
  }
;;

(* Gauss quadrature weights and kronrod quadrature abscissae and
   weights as evaluated with 80 decimal digit arithmetic by
   L. W. Fullerton, Bell Labs, Nov. 1981. *)

(* Weights of the 7-point gauss rule. *)
let wg  = [| 0.1294849661_6886969327_0611432679_082;
             0.2797053914_8927666790_1467771423_780;
             0.3818300505_0511894495_0369775488_975;
             0.4179591836_7346938775_5102040816_327 |]
(* Abscissae of the 15-point kronrod rule. *)
let xgk = [| 0.9914553711_2081263920_6854697526_329;
             0.9491079123_4275852452_6189684047_851;
             0.8648644233_5976907278_9712788640_926;
             0.7415311855_9939443986_3864773280_788;
             0.5860872354_6769113029_4144838258_730;
             0.4058451513_7739716690_6606412076_961;
             0.2077849550_0789846760_0689403773_245;
             0.0000000000_0000000000_0000000000_000 |]
(* Weights of the 15-point kronrod rule. *)
let wgk = [| 0.0229353220_1052922496_3732008058_970;
             0.0630920926_2997855329_0700663189_204;
             0.1047900103_2225018383_9876322541_518;
             0.1406532597_1552591874_5189590510_238;
             0.1690047266_3926790282_6583426598_550;
             0.1903505780_6478540991_3256402421_014;
             0.2044329400_7529889241_4161999234_649;
             0.2094821410_8472782801_2999174891_714 |]

let qk15 fv1 fv2 f a b = (* n=8 *)
  QK(7, 2, 3, INIT_GAUSS_EVEN(3), wg, xgk, wgk, fv1, fv2);;


let wg  = [| 0.0666713443_0868813759_3568809893_332;
             0.1494513491_5058059314_5776339657_697;
             0.2190863625_1598204399_5534934228_163;
             0.2692667193_0999635509_1226921569_469;
             0.2955242247_1475287017_3892994651_338 |]
let xgk = [| 0.9956571630_2580808073_5527280689_003;
             0.9739065285_1717172007_7964012084_452;
             0.9301574913_5570822600_1207180059_508;
             0.8650633666_8898451073_2096688423_493;
             0.7808177265_8641689706_3717578345_042;
             0.6794095682_9902440623_4327365114_874;
             0.5627571346_6860468333_9000099272_694;
             0.4333953941_2924719079_9265943165_784;
             0.2943928627_0146019813_1126603103_866;
             0.1488743389_8163121088_4826001129_720;
             0.0000000000_0000000000_0000000000_000 |]
let wgk = [| 0.0116946388_6737187427_8064396062_192;
             0.0325581623_0796472747_8818972459_390;
             0.0547558965_7435199603_1381300244_580;
             0.0750396748_1091995276_7043140916_190;
             0.0931254545_8369760553_5065465083_366;
             0.1093871588_0229764189_9210590325_805;
             0.1234919762_6206585107_7958109831_074;
             0.1347092173_1147332592_8054001771_707;
             0.1427759385_7706008079_7094273138_717;
             0.1477391049_0133849137_4841515972_068;
             0.1494455540_0291690566_4936468389_821 |]

let qk21 fv1 fv2 f a b = (* n=11 *)
  QK(10, 4, 4, INIT_GAUSS_ODD, wg, xgk, wgk, fv1, fv2);;

let wg  = [| 0.0307532419_9611726835_4628393577_204;
             0.0703660474_8810812470_9267416450_667;
             0.1071592204_6717193501_1869546685_869;
             0.1395706779_2615431444_7804794511_028;
             0.1662692058_1699393355_3200860481_209;
             0.1861610000_1556221102_6800561866_423;
             0.1984314853_2711157645_6118326443_839;
             0.2025782419_2556127288_0620199967_519 |]
let xgk = [| 0.9980022986_9339706028_5172840152_271;
             0.9879925180_2048542848_9565718586_613;
             0.9677390756_7913913425_7347978784_337;
             0.9372733924_0070590430_7758947710_209;
             0.8972645323_4408190088_2509656454_496;
             0.8482065834_1042721620_0648320774_217;
             0.7904185014_4246593296_7649294817_947;
             0.7244177313_6017004741_6186054613_938;
             0.6509967412_9741697053_3735895313_275;
             0.5709721726_0853884753_7226737253_911;
             0.4850818636_4023968069_3655740232_351;
             0.3941513470_7756336989_7207370981_045;
             0.2991800071_5316881216_6780024266_389;
             0.2011940939_9743452230_0628303394_596;
             0.1011420669_1871749902_7074231447_392;
             0.0000000000_0000000000_0000000000_000 |]
let wgk = [| 0.0053774798_7292334898_7792051430_128;
             0.0150079473_2931612253_8374763075_807;
             0.0254608473_2671532018_6874001019_653;
             0.0353463607_9137584622_2037948478_360;
             0.0445897513_2476487660_8227299373_280;
             0.0534815246_9092808726_5343147239_430;
             0.0620095678_0067064028_5139230960_803;
             0.0698541213_1872825870_9520077099_147;
             0.0768496807_5772037889_4432777482_659;
             0.0830805028_2313302103_8289247286_104;
             0.0885644430_5621177064_7275443693_774;
             0.0931265981_7082532122_5486872747_346;
             0.0966427269_8362367850_5179907627_589;
             0.0991735987_2179195933_2393173484_603;
             0.1007698455_2387559504_4946662617_570;
             0.1013300070_1479154901_7374792767_493 |]

let qk31 fv1 fv2 f a b = (* n=16 *)
  QK(15, 6, 7, INIT_GAUSS_EVEN(7), wg, xgk, wgk, fv1, fv2);;

let wg  = [| 0.0176140071_3915211831_1861962351_853;
             0.0406014298_0038694133_1039952274_932;
             0.0626720483_3410906356_9506535187_042;
             0.0832767415_7670474872_4758143222_046;
             0.1019301198_1724043503_6750135480_350;
             0.1181945319_6151841731_2377377711_382;
             0.1316886384_4917662689_8494499748_163;
             0.1420961093_1838205132_9298325067_165;
             0.1491729864_7260374678_7828737001_969;
             0.1527533871_3072585069_8084331955_098 |]
let xgk = [| 0.9988590315_8827766383_8315576545_863;
             0.9931285991_8509492478_6122388471_320;
             0.9815078774_5025025919_3342994720_217;
             0.9639719272_7791379126_7666131197_277;
             0.9408226338_3175475351_9982722212_443;
             0.9122344282_5132590586_7752441203_298;
             0.8782768112_5228197607_7442995113_078;
             0.8391169718_2221882339_4529061701_521;
             0.7950414288_3755119835_0638833272_788;
             0.7463319064_6015079261_4305070355_642;
             0.6932376563_3475138480_5490711845_932;
             0.6360536807_2651502545_2836696226_286;
             0.5751404468_1971031534_2946036586_425;
             0.5108670019_5082709800_4364050955_251;
             0.4435931752_3872510319_9992213492_640;
             0.3737060887_1541956067_2548177024_927;
             0.3016278681_1491300432_0555356858_592;
             0.2277858511_4164507808_0496195368_575;
             0.1526054652_4092267550_5220241022_678;
             0.0765265211_3349733375_4640409398_838;
             0.0000000000_0000000000_0000000000_000 |]
let wgk = [| 0.0030735837_1852053150_1218293246_031;
             0.0086002698_5564294219_8661787950_102;
             0.0146261692_5697125298_3787960308_868;
             0.0203883734_6126652359_8010231432_755;
             0.0258821336_0495115883_4505067096_153;
             0.0312873067_7703279895_8543119323_801;
             0.0366001697_5820079803_0557240707_211;
             0.0416688733_2797368626_3788305936_895;
             0.0464348218_6749767472_0231880926_108;
             0.0509445739_2372869193_2707670050_345;
             0.0551951053_4828599474_4832372419_777;
             0.0591114008_8063957237_4967220648_594;
             0.0626532375_5478116802_5870122174_255;
             0.0658345971_3361842211_1563556969_398;
             0.0686486729_2852161934_5623411885_368;
             0.0710544235_5344406830_5790361723_210;
             0.0730306903_3278666749_5189417658_913;
             0.0745828754_0049918898_6581418362_488;
             0.0757044976_8455667465_9542775376_617;
             0.0763778676_7208073670_5502835038_061;
             0.0766007119_1799965644_5049901530_102 |]

let qk41 fv1 fv2 f a b = (* n = 21 *)
  QK(20, 9, 9, INIT_GAUSS_ODD, wg, xgk, wgk, fv1, fv2);;

let wg  = [| 0.0113937985_0102628794_7902964113_235;
             0.0263549866_1503213726_1901815295_299;
             0.0409391567_0130631265_5623487711_646;
             0.0549046959_7583519192_5936891540_473;
             0.0680383338_1235691720_7187185656_708;
             0.0801407003_3500101801_3234959669_111;
             0.0910282619_8296364981_1497220702_892;
             0.1005359490_6705064420_2206890392_686;
             0.1085196244_7426365311_6093957050_117;
             0.1148582591_4571164833_9325545869_556;
             0.1194557635_3578477222_8178126512_901;
             0.1222424429_9031004168_8959518945_852;
             0.1231760537_2671545120_3902873079_050 |]
let xgk = [| 0.9992621049_9260983419_3457486540_341;
             0.9955569697_9049809790_8784946893_902;
             0.9880357945_3407724763_7331014577_406;
             0.9766639214_5951751149_8315386479_594;
             0.9616149864_2584251241_8130033660_167;
             0.9429745712_2897433941_4011169658_471;
             0.9207471152_8170156174_6346084546_331;
             0.8949919978_7827536885_1042006782_805;
             0.8658470652_9327559544_8996969588_340;
             0.8334426287_6083400142_1021108693_570;
             0.7978737979_9850005941_0410904994_307;
             0.7592592630_3735763057_7282865204_361;
             0.7177664068_1308438818_6654079773_298;
             0.6735663684_7346836448_5120633247_622;
             0.6268100990_1031741278_8122681624_518;
             0.5776629302_4122296772_3689841612_654;
             0.5263252843_3471918259_9623778158_010;
             0.4730027314_4571496052_2182115009_192;
             0.4178853821_9303774885_1814394594_572;
             0.3611723058_0938783773_5821730127_641;
             0.3030895389_3110783016_7478909980_339;
             0.2438668837_2098843204_5190362797_452;
             0.1837189394_2104889201_5969888759_528;
             0.1228646926_1071039638_7359818808_037;
             0.0615444830_0568507888_6546392366_797;
             0.0000000000_0000000000_0000000000_000 |]
let wgk = [| 0.0019873838_9233031592_6507851882_843;
             0.0055619321_3535671375_8040236901_066;
             0.0094739733_8617415160_7207710523_655;
             0.0132362291_9557167481_3656405846_976;
             0.0168478177_0912829823_1516667536_336;
             0.0204353711_4588283545_6568292235_939;
             0.0240099456_0695321622_0092489164_881;
             0.0274753175_8785173780_2948455517_811;
             0.0307923001_6738748889_1109020215_229;
             0.0340021302_7432933783_6748795229_551;
             0.0371162714_8341554356_0330625367_620;
             0.0400838255_0403238207_4839284467_076;
             0.0428728450_2017004947_6895792439_495;
             0.0455029130_4992178890_9870584752_660;
             0.0479825371_3883671390_6392255756_915;
             0.0502776790_8071567196_3325259433_440;
             0.0523628858_0640747586_4366712137_873;
             0.0542511298_8854549014_4543370459_876;
             0.0559508112_2041231730_8240686382_747;
             0.0574371163_6156783285_3582693939_506;
             0.0586896800_2239420796_1974175856_788;
             0.0597203403_2417405997_9099291932_562;
             0.0605394553_7604586294_5360267517_565;
             0.0611285097_1705304830_5859030416_293;
             0.0614711898_7142531666_1544131965_264;
             (* note: wgk (26) was calculated from the values of wgk(1..25) *)
             0.0615808180_6783293507_8759824240_066 |]

let qk51 fv1 fv2 f a b = (* n = 26 *)
  QK(25, 11, 12, INIT_GAUSS_EVEN(12), wg, xgk, wgk, fv1, fv2);;

let wg  = [| 0.0079681924_9616660561_5465883474_674;
             0.0184664683_1109095914_2302131912_047;
             0.0287847078_8332336934_9719179611_292;
             0.0387991925_6962704959_6801936446_348;
             0.0484026728_3059405290_2938140422_808;
             0.0574931562_1761906648_1721689402_056;
             0.0659742298_8218049512_8128515115_962;
             0.0737559747_3770520626_8243850022_191;
             0.0807558952_2942021535_4694938460_530;
             0.0868997872_0108297980_2387530715_126;
             0.0921225222_3778612871_7632707087_619;
             0.0963687371_7464425963_9468626351_810;
             0.0995934205_8679526706_2780282103_569;
             0.1017623897_4840550459_6428952168_554;
             0.1028526528_9355884034_1285636705_415 |]
let xgk = [| 0.9994844100_5049063757_1325895705_811;
             0.9968934840_7464954027_1630050918_695;
             0.9916309968_7040459485_8628366109_486;
             0.9836681232_7974720997_0032581605_663;
             0.9731163225_0112626837_4693868423_707;
             0.9600218649_6830751221_6871025581_798;
             0.9443744447_4855997941_5831324037_439;
             0.9262000474_2927432587_9324277080_474;
             0.9055733076_9990779854_6522558925_958;
             0.8825605357_9205268154_3116462530_226;
             0.8572052335_4606109895_8658510658_944;
             0.8295657623_8276839744_2898119732_502;
             0.7997278358_2183908301_3668942322_683;
             0.7677774321_0482619491_7977340974_503;
             0.7337900624_5322680472_6171131369_528;
             0.6978504947_9331579693_2292388026_640;
             0.6600610641_2662696137_0053668149_271;
             0.6205261829_8924286114_0477556431_189;
             0.5793452358_2636169175_6024932172_540;
             0.5366241481_4201989926_4169793311_073;
             0.4924804678_6177857499_3693061207_709;
             0.4470337695_3808917678_0609900322_854;
             0.4004012548_3039439253_5476211542_661;
             0.3527047255_3087811347_1037207089_374;
             0.3040732022_7362507737_2677107199_257;
             0.2546369261_6788984643_9805129817_805;
             0.2045251166_8230989143_8957671002_025;
             0.1538699136_0858354696_3794672743_256;
             0.1028069379_6673703014_7096751318_001;
             0.0514718425_5531769583_3025213166_723;
             0.0000000000_0000000000_0000000000_000 |]
let wgk = [| 0.0013890136_9867700762_4551591226_760;
             0.0038904611_2709988405_1267201844_516;
             0.0066307039_1593129217_3319826369_750;
             0.0092732796_5951776342_8441146892_024;
             0.0118230152_5349634174_2232898853_251;
             0.0143697295_0704580481_2451432443_580;
             0.0169208891_8905327262_7572289420_322;
             0.0194141411_9394238117_3408951050_128;
             0.0218280358_2160919229_7167485738_339;
             0.0241911620_7808060136_5686370725_232;
             0.0265099548_8233310161_0601709335_075;
             0.0287540487_6504129284_3978785354_334;
             0.0309072575_6238776247_2884252943_092;
             0.0329814470_5748372603_1814191016_854;
             0.0349793380_2806002413_7499670731_468;
             0.0368823646_5182122922_3911065617_136;
             0.0386789456_2472759295_0348651532_281;
             0.0403745389_5153595911_1995279752_468;
             0.0419698102_1516424614_7147541285_970;
             0.0434525397_0135606931_6831728117_073;
             0.0448148001_3316266319_2355551616_723;
             0.0460592382_7100698811_6271735559_374;
             0.0471855465_6929915394_5261478181_099;
             0.0481858617_5708712914_0779492298_305;
             0.0490554345_5502977888_7528165367_238;
             0.0497956834_2707420635_7811569379_942;
             0.0504059214_0278234684_0893085653_585;
             0.0508817958_9874960649_2297473049_805;
             0.0512215478_4925877217_0656282604_944;
             0.0514261285_3745902593_3862879215_781;
             0.0514947294_2945156755_8340433647_099 |]

let qk61 fv1 fv2 f a b = (* n = 31 *)
  QK(30, 14, 14, INIT_GAUSS_ODD, wg, xgk, wgk, fv1, fv2);;


(***********************************************************************
 *                        Adaptive schemes
 ***********************************************************************)

(* Names taken to be compatible with the GSL bindings. *)
type integrator =
  | GAUSS15 (* 7 - 15 points *)
  | GAUSS21 (* 10 - 21 points *)
  | GAUSS31 (* 15 - 31 points *)
  | GAUSS41 (* 20 - 41 points *)
  | GAUSS51 (* 25 - 51 points *)
  | GAUSS61 (* 30 - 61 points *)

let string_of_integrator = function
  | GAUSS15 -> "GAUSS15"
  | GAUSS21 -> "GAUSS21"
  | GAUSS31 -> "GAUSS31"
  | GAUSS41 -> "GAUSS41"
  | GAUSS51 -> "GAUSS51"
  | GAUSS61 -> "GAUSS61"

type reliability =
  | OK
  | Limit
  | Roundoff
  | Bad_integrand

type result = {
  res: float;
  err: float;
  neval: int;
  nsub: int;
  msg: reliability;
}

(* Names identical to the QUADPACK ones. *)
type workspace = {
  integ: integrator; (* to enable checks *)
  fv1: float array;  (* working space for the [qk15],... routines *)
  fv2: float array;
  alist: float array;
  blist: float array;
  elist: float array;
  iord: int array;
  rlist: float array;
}

(* The only way to create workspaces from the interface. *)
let workspace integ limit =
  let dimfv = match integ with
    | GAUSS15 -> 7 (* n - 1 *)
    | GAUSS21 -> 10
    | GAUSS31 -> 15
    | GAUSS41 -> 20
    | GAUSS51 -> 25
    | GAUSS61 -> 30 in
  { integ = integ;
    fv1 = Array.make dimfv 0.;
    fv2 = Array.make dimfv 0.;
    alist = Array.make limit 0.;
    blist = Array.make limit 0.;
    elist = Array.make limit 0.;
    iord  = Array.make limit 0;
    rlist = Array.make limit 0. }

let check_workspace name integ limit w =
  (* FIXME: Enable use with other integrators as long as there is
     enough space? *)
  if w.integ <> integ then
    invalid_arg(sprintf "Integration1D.%s: workspace for %s used with %s"
                        name (string_of_integrator w.integ)
                        (string_of_integrator integ));
  if Array.length w.alist < limit (* not need to check others because
                                     of the restricted way of creating
                                     workspaces *) then
    invalid_arg(sprintf "Integration1D.%s: limit = %i exceeds the \
                         workspace size = %i"
                        name limit (Array.length w.alist))
;;

DEFINE QPSRT(limit, last, maxerr, errmax, elist, iord, nrmax) =
  if last < 2 then (
    iord.(0) <- 0;
    iord.(1) <- 1;
    (* maxerr := iord.(!nrmax); (* !nrmax=0 => !maxerr = 0 = iord.(0) *) *)
    errmax := elist.(0) (* elist.(!maxerr) may have changed since init. *)
  ) else (
    (* This part of the routine is only executed if, due to a
       difficult integrand, subdivision increased the error estimate.
       In the normal case the insert procedure should start after the
       [nrmax]-th largest error estimate. *)
    errmax := elist.(!maxerr);
    (* FIXME: nrmax = 0 always (why do they do this?) *)
    while !nrmax > 0 && !errmax > elist.(iord.(!nrmax-1)) do
      iord.(!nrmax) <- iord.(!nrmax-1);
      decr nrmax;
    done;
    (* Compute the number of elements in the list to be maintained in
       descending order.  This number depends on the number of
       subdivisions still allowed. *)
    let jupbn = if last >= limit/2 + 2 then limit+1-last else last in
    let errmin = elist.(last) (* save smaller sub-interval error *) in
    (* Insert [errmax] by traversing the list top-down, starting
       comparison from the element [elist.(iord.(!nrmax+1))]. *)
    let i = ref(!nrmax + 1) in
    while !i < jupbn && !errmax < elist.(iord.(!i)) do
      iord.(!i-1) <- iord.(!i);
      incr i;
    done;
    (* Insert [errmin] by traversing the list bottom-up. *)
    iord.(!i-1) <- !maxerr;
    let k = ref(jupbn - 1) in
    let i_1 = !i - 1 in
    while !k > i_1 && errmin >= elist.(iord.(!k)) do
      iord.(!k+1) <- iord.(!k);
      decr k;
    done;
    iord.(!k+1) <- last;
    maxerr := iord.(!nrmax);
    errmax := elist.(!maxerr)
  )
;;

let too_small_epsrel = max_float epsilon_float50 0.5E-28

let check_integration_params name ~(epsabs: float) ~(epsrel: float)
                             (a: float) (b: float) =
  if epsabs <= 0. then
    invalid_arg(sprintf "Integration1D.%s: epsabs = %g <= 0" name epsabs);
  if epsabs <> epsabs then
    invalid_arg(sprintf "Integration1D.%s: epsabs cannot be NaN" name);
  if epsrel <= too_small_epsrel then
    invalid_arg(sprintf "Integration1D.%s: epsrel too small" name);
  if epsrel <> epsrel then
    invalid_arg(sprintf "Integration1D.%s: epsrel cannot be NaN" name);
  if a <> a then
    invalid_arg(sprintf "Integration1D.%s: the integration interval \
                         left bound cannot be NaN" name);
  if b <> b then
    invalid_arg(sprintf "Integration1D.%s: the integration interval \
                         right bound cannot be NaN" name)

exception Result of ((* last *) int * reliability)

let qag ?(limit=50) ?workspace:w integ =
  let w = match w with
    | None -> workspace integ limit
    | Some w -> check_workspace "qag" integ limit w; w in
  let integ, compute_neval = match integ with
    | GAUSS15 -> qk15, (fun neval -> 30 * neval + 15)
    | GAUSS21 -> qk21, (fun neval -> 42 * neval + 21) (* cst. * (2*neval+1) *)
    | GAUSS31 -> qk31, (fun neval -> 62 * neval + 31)
    | GAUSS41 -> qk41, (fun neval -> 82 * neval + 41)
    | GAUSS51 -> qk51, (fun neval -> 102 * neval + 51)
    | GAUSS61 -> qk61, (fun neval -> 122 * neval + 61) in

  fun ?(epsabs=1.49E-8) ?(epsrel=1.49E-8) f (a: float) (b: float) ->
  check_integration_params "qag" ~epsabs ~epsrel a b;
  (* First approximation to the integral *)
  let i = integ w.fv1 w.fv2 f a b in
  (* Test on accuracy *)
  let errbnd = max_float epsabs (epsrel *. abs_float i.result) in
  if (i.abserr <= errbnd && i.abserr <> i.resasc) || i.abserr = 0. then
    { res = i.result;  err = i.abserr;  neval = compute_neval 0;  nsub = 1;
      msg = OK }
  else if i.abserr <= epsilon_float50 *. i.resabs && i.abserr > errbnd then
    { res = i.result;  err = i.abserr;  neval = compute_neval 0;  nsub = 1;
      msg = Roundoff }
  else if limit = 1 then
    { res = i.result;  err = i.abserr;  neval = compute_neval 0;  nsub = 1;
      msg = Limit }
  else begin
      (* Initialization of state *)
      w.alist.(0) <- a;
      w.blist.(0) <- b;
      w.rlist.(0) <- i.result;
      w.elist.(0) <- i.abserr;
      w.iord.(0) <- 0;
      let neval = ref 0
      and errmax = ref i.abserr (* = w.elist.(0) *)
      and maxerr = ref 0 (* index of max of error *)
      and area = ref i.result
      and errsum = ref i.abserr (* = errmax *)
      and nrmax = ref 0  (* maxerr = iord(nrmax) *)
      and iroff1 = ref 0 (* roundoff of type 1 *)
      and iroff2 = ref 0 (* roundoff of type 2 *) in
      let last, msg_val =
        try
          for last = 1 to limit - 1 do
            (* Bisect the subinterval with the largest error estimate. *)
            let ai = w.alist.(!maxerr) in
            let bi = w.blist.(!maxerr) in
            let mid = 0.5 *. (ai +. bi) in
            let i1 = integ w.fv1 w.fv2 f ai mid in
            let i2 = integ w.fv1 w.fv2 f mid bi in
            (* Improve previous approximations to integral and error and
             test for accuracy. *)
            incr neval;
            let area12 = i1.result +. i2.result
            and abserr12 = i1.abserr +. i2.abserr in
            errsum := !errsum +. abserr12 -. !errmax;
            area := !area +. area12 -. w.rlist.(!maxerr);
            if i1.resasc <> i1.abserr && i2.resasc <> i2.abserr then (
              if abs_float(w.rlist.(!maxerr) -. area12)
                 <= 0.1E-4 *. abs_float area12
                 && abserr12 >= 0.99 *. !errmax then incr iroff1;
              if last >= 10 && abserr12 > !errmax then incr iroff2;
            );
            w.rlist.(!maxerr) <- i1.result;
            w.rlist.(last) <- i2.result;
            let errbnd = max_float epsabs (epsrel *. abs_float !area) in
            if !errsum > errbnd then (
              (* Test for roundoff error and eventually set error flag. *)
              if !iroff1 >= 6 || !iroff2 >= 20 then
                raise(Result(last, Roundoff));
              (* Bad integrand behaviour at a point of the integration
               range (interval too small). *)
              if max_float (abs_float ai) (abs_float bi)
                 <= epsilon_float100p1 *. (abs_float mid +. min_float1000) then
                raise(Result(last, Bad_integrand));
            );
            (* Append the newly-created intervals to the list. *)
            if i2.abserr <= i1.abserr then (
              w.alist.(last) <- mid;
              w.blist.(!maxerr) <- mid;
              w.blist.(last) <- bi;
              w.elist.(!maxerr) <- i1.abserr;
              w.elist.(last) <- i2.abserr;
            ) else (
              w.alist.(!maxerr) <- mid;
              w.alist.(last) <- ai;
              w.blist.(last) <- mid;
              w.rlist.(!maxerr) <- i2.result;
              w.rlist.(last) <- i1.result;
              w.elist.(!maxerr) <- i2.abserr;
              w.elist.(last) <- i1.abserr;
            );
            (* Maintain the descending ordering in the list of error
             estimates and select the subinterval with the largest error
             estimate (to be bisected next). *)
            QPSRT(limit, last, maxerr, errmax, w.elist, w.iord, nrmax);
            if !errsum <= errbnd then raise(Result(last, OK));
          done;
          limit-1, Limit
        with Result r -> r in
      let result = ref 0. in
      for k = 0 to last do result := !result +. w.rlist.(k) done;
      { res = !result;
        err = !errsum;
        neval = compute_neval !neval;
        nsub = last + 1;
        msg = msg_val }
    end
;;

let quags f a b =
  failwith "TBD"


let quagi f a b =
  failwith "TBD"



(************************************************************************)

(* Algorithme d'intégration adaptatif basé sur la formule
   d'intégration de Cavalieri-Simpson as described in "Méthodes
   numériques pour le calcul scientifique" of Quarteroni & al.,
   p. 300. *)

(* Very old; change *)
let simp_adapt ?fh ?(tol=1E-6) ?hmin f a b =
  let simp len fa fmid fb = len /. 6. *. (fa +. 4. *. fmid +. fb)
  and save_point x len = match fh with
    | None -> ()
    | Some file -> fprintf file "%.13f %.13f %.13f\n" x (f x) (1. /. len)
  and hmin = match hmin with
    | None -> 1E-6 *. abs_float(b -. a)
    | Some h -> h
  and len0 = b -. a
  and fev0 = Array.make 5 0. in
  (* compute f at the initial nodes on [a,b] *)
  let step = len0 /. 4. in
  for i = 0 to 4 do
    let x = a +. (float i) *. step in
    fev0.(i) <- f x;
    save_point x len0
  done;

  let rec adapt  fev alpha beta len s =
    (* [len] is the length of the interval, i.e., [beta -. alpha].
       [s] = the Simpson eval of the integral on [alpha, beta].  This
       is to avoid multiple computations. *)
    let s_left = simp (len *. 0.5) fev.(0) fev.(1) fev.(2)
    and s_right = simp (len *. 0.5) fev.(2) fev.(3) fev.(4) in
    let s2 = s_left +. s_right in
    let tol_rv = tol *. len /. len0
    and err_rv = abs_float(s -. s2) *. 0.1 in
    if err_rv <= tol_rv then
      (* Estimate s2 is OK *)
      s2, err_rv
    else
      (* Refine current interval by dividing it in two *)
      let len' = len *. 0.5 in
      if len' < hmin then
        failwith(sprintf "simp_adapt: pas de discrétisation %g top petit \
                 en x = %g" len' alpha)
      else begin
        if len' <= 11. *. hmin then
          eprintf "Pas de discrétisation proche de hmin = %g en x = %g\n"
            hmin alpha;
        let mid = 0.5 *. (alpha +. beta)
        and h = 0.125 *. len in
        let integ1, e1 = adapt [| fev.(0); f (alpha +. h);
                                  fev.(1); f(mid -. h); fev.(2) |]
          alpha mid len' s_left
        and integ2, e2 = adapt [| fev.(2); f (mid +. h);
                                  fev.(3); f(beta -. h); fev.(4) |]
          mid beta len' s_right
        in
        save_point (alpha +. h) len; save_point (mid -. h) len;
        save_point (mid +. h) len; save_point (beta -. h) len;
        integ1 +. integ2, e1 +. e2
      end

  in adapt fev0 a b len0 (simp len0 fev0.(0) fev0.(2) fev0.(4))



(* Local Variables: *)
(* compile-command: "make -k -C .." *)
(* End: *)
