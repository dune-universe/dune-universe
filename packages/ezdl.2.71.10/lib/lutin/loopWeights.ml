
open Printf

(*--------------------------------------------------------------
The module provides two functions:
interval_weights min max x -> (goon_weight, stop_weight)

The result is undefined unless:
    0 <= x
    (0 <= min <= max)

Algo
- shift min to 0
- d = x-min = relative position to 0
- w = max-x = remaining possiblities

* d < 0   -> cont=1, stop=0
* w <= 0  -> cont=0, stop=1
* else    -> cont=w, stop=1 
--------------------------------------------------------------*)
let interval min max x = (
	let d = x - min in
	let width = max - x in
	if(d < 0) then (1,0)
	else if(width > 0) then (width,1)
	else (0,1)
)
(* a test *)
let _test_interval () = (
	let min = 2 in
	let max = 6 in

	printf "loop[%d,%d] weights\n" min max ;
	printf "|-----|-------|-------|\n";
	printf "| cpt |  cont |  stop |\n";
	printf "|-----|-------|-------|\n";
	for k = 0 to 9 do
      let (wg,ws) = interval min max k in
      printf "| %3d | %5d | %5d |\n"  k  wg ws;
   done ;
	printf "|-----|-------|-------|\n" ;
)

(*--------------------------s-----------------------------------------
Average weights finctions are returning "normalized"
so the user is suppose to ignore it !

average_weights mu sigma x

The result is undefined unless:
    0 <= x
    0 < sigma < mu
Moreover, the result is not "accurate" unless:
    4*sigma < mu


--------------------------------------------------------------------*)
let samples_per_unit = 128
let total_samples = 4 * samples_per_unit
let sample_grain = 10000

let samples_tab : int array = [|
 5000;  5032;  5063;  5094;  5125;  5156;  5187;  5219;
 5250;  5281;  5312;  5343;  5374;  5405;  5436;  5467;
 5498;  5529;  5560;  5591;  5621;  5652;  5683;  5714;
 5744;  5775;  5805;  5836;  5866;  5897;  5927;  5957;
 5988;  6018;  6048;  6078;  6108;  6138;  6168;  6197;
 6227;  6257;  6286;  6316;  6345;  6375;  6404;  6433;
 6462;  6491;  6520;  6549;  6578;  6606;  6635;  6663;
 6692;  6720;  6748;  6776;  6804;  6832;  6860;  6888;
 6915;  6943;  6970;  6997;  7024;  7051;  7078;  7105;
 7132;  7158;  7185;  7211;  7237;  7263;  7289;  7315;
 7341;  7366;  7392;  7417;  7442;  7467;  7492;  7517;
 7542;  7566;  7591;  7615;  7639;  7663;  7687;  7711;
 7734;  7758;  7781;  7804;  7827;  7850;  7873;  7895;
 7918;  7940;  7963;  7985;  8006;  8028;  8050;  8071;
 8093;  8114;  8135;  8156;  8176;  8197;  8218;  8238;
 8258;  8278;  8298;  8318;  8337;  8357;  8376;  8395;
 8414;  8433;  8451;  8470;  8488;  8507;  8525;  8543;
 8560;  8578;  8596;  8613;  8630;  8647;  8664;  8681;
 8698;  8714;  8730;  8747;  8763;  8779;  8794;  8810;
 8825;  8841;  8856;  8871;  8886;  8901;  8915;  8930;
 8944;  8958;  8972;  8986;  9000;  9014;  9027;  9041;
 9054;  9067;  9080;  9093;  9105;  9118;  9130;  9143;
 9155;  9167;  9179;  9191;  9202;  9214;  9225;  9236;
 9248;  9259;  9270;  9280;  9291;  9302;  9312;  9322;
 9332;  9342;  9352;  9362;  9372;  9382;  9391;  9400;
 9410;  9419;  9428;  9437;  9446;  9454;  9463;  9471;
 9480;  9488;  9496;  9504;  9512;  9520;  9528;  9535;
 9543;  9550;  9558;  9565;  9572;  9579;  9586;  9593;
 9600;  9607;  9613;  9620;  9626;  9632;  9639;  9645;
 9651;  9657;  9663;  9669;  9674;  9680;  9686;  9691;
 9697;  9702;  9707;  9712;  9717;  9722;  9727;  9732;
 9737;  9742;  9746;  9751;  9756;  9760;  9764;  9769;
 9773;  9777;  9781;  9785;  9789;  9793;  9797;  9801;
 9805;  9808;  9812;  9816;  9819;  9823;  9826;  9829;
 9833;  9836;  9839;  9842;  9845;  9848;  9851;  9854;
 9857;  9860;  9863;  9865;  9868;  9871;  9873;  9876;
 9878;  9881;  9883;  9886;  9888;  9890;  9892;  9895;
 9897;  9899;  9901;  9903;  9905;  9907;  9909;  9911;
 9913;  9915;  9916;  9918;  9920;  9922;  9923;  9925;
 9927;  9928;  9930;  9931;  9933;  9934;  9936;  9937;
 9938;  9940;  9941;  9942;  9944;  9945;  9946;  9947;
 9949;  9950;  9951;  9952;  9953;  9954;  9955;  9956;
 9957;  9958;  9959;  9960;  9961;  9962;  9963;  9964;
 9965;  9965;  9966;  9967;  9968;  9969;  9969;  9970;
 9971;  9971;  9972;  9973;  9973;  9974;  9975;  9975;
 9976;  9977;  9977;  9978;  9978;  9979;  9979;  9980;
 9980;  9981;  9981;  9982;  9982;  9983;  9983;  9984;
 9984;  9984;  9985;  9985;  9986;  9986;  9986;  9987;
 9987;  9987;  9988;  9988;  9988;  9989;  9989;  9989;
 9990;  9990;  9990;  9990;  9991;  9991;  9991;  9991;
 9992;  9992;  9992;  9992;  9993;  9993;  9993;  9993;
 9993;  9994;  9994;  9994;  9994;  9994;  9994;  9995;
 9995;  9995;  9995;  9995;  9995;  9995;  9996;  9996;
 9996;  9996;  9996;  9996;  9996;  9996;  9997;  9997;
 9997;  9997;  9997;  9997;  9997;  9997;  9997;  9997;
 9998;  9998;  9998;  9998;  9998;  9998;  9998;  9998;
 9998;  9998;  9998;  9998;  9998;  9998;  9999;  9999;
 9999;  9999;  9999;  9999;  9999;  9999;  9999;  9999;
 9999;  9999;  9999;  9999;  9999;  9999;  9999;  9999;
 9999;  9999;  9999;  9999;  9999; 10000; 10000; 10000;
10000; 10000; 10000; 10000; 10000; 10000; 10000; 10000;
10000; 10000; 10000; 10000; 10000; 10000; 10000; 10000;
10000; 10000; 10000; 10000; 10000; 10000; 10000; 10000;
10000; 10000; 10000; 10000; 10000; 10000; 10000; 10000
|]


(* Proba (1 <=> sample_grain) for the number of loops
	to be >= x *)
let average_proba_sup mu ec x = (
	let t = x - mu in
	let sigma = if(ec > 0) then  ec else (14*mu/100) in
	if ( t < 0) then (
	(* Negative: the mean is not yet reached *)
	(* Normalisation in 1/SAMPLES_PER_UNIT *)
		let nt = -t in
		let ix = (nt * samples_per_unit)/sigma in
(* Printf.printf "x=%d, < mu, take ix=%d\n" x ix; *)
		if (ix < total_samples) then 
			let goon = samples_tab.(ix) in
			goon
		else sample_grain
	) else (
	(* Positive: the mean is already reached *)
	(* Normalisation in 1/SAMPLES_PER_UNIT *)
		let nt = t in
		let ix = (nt*samples_per_unit)/sigma in
(* Printf.printf "x=%d, >= mu, take ix=%d\n" x ix; *)
		if (ix < total_samples) then 
			let stop = samples_tab.(ix) in
			sample_grain - stop
		else 0
	)
)

(* goon x is the proba to continue after x loops,
   that is: the proba to be >= x+1, knowing
	that there were already x loops
	P(x+1) * P(x)
*)

let average mu ec x = (
	if (x = 0) then (sample_grain, 0) else (
		let px = average_proba_sup mu ec (x-1) in
		let pxp1 = average_proba_sup mu ec (x) in
		let goon = (pxp1 * sample_grain) / px in
		(goon, sample_grain - goon)
	)
)
let _test_average () = (
   let m = 10 in
   let s = 2 in
   for k=0 to 2*m do
      let (wg,ws) = average m s k in
      printf "%3d %5d %5d\n" k wg ws;
	done;
)

(*

*)
let new_goon_stop m s cpt = (
	let (supcpt,_) = average m s cpt in
	let (supcptm1,_) =
		if (cpt = 0) then (sample_grain,0)
		else average m s (cpt - 1)
	in 
	let goon = (supcpt * supcptm1) / sample_grain in
	let stop = sample_grain - goon in
	(goon,stop)
)

let _test_stop_average () = (
   let m = 10 in
   let s = 2 in
	let tab = Array.make (2*m) 0 in 
	let tot = 10000000 in
	for _nbtries = 1 to tot do
		let rec run cpt = (
			(* let (goon,stop) = average_weights	m s cpt in *)
			let (goon,stop) = new_goon_stop m s cpt in
         let s = goon + stop in
			let alea = assert (s>0); Random.int (s) in
			if (alea <= goon) then run (cpt+1)
			else cpt
		) in
		let r = run 0 in
		Array.set tab r (tab.(r)+1);
		(* Printf.printf "%d\n" r *)
	done;
	for k = 0 to (2*m -1) do Printf.printf "%3d " k done ;
	Printf.printf "\n";
	for k = 0 to (2*m -1) do
		let pm = (tab.(k) * 100) / tot in
		Printf.printf "%3d " pm
	done ;
	Printf.printf "\n";
)

(* let _ = test_stop_average () *)

let _probas_stop () = (
   let m = 10 in
   let s = 2 in
	
	let pb x = (float_of_int x)/. (float_of_int sample_grain) in
	let gotab = Array.make (2*m) 0.0 in
	let stoptab = Array.make (2*m) 0.0 in
	for k = 0 to (2*m-1) do
		let (g,s) = new_goon_stop m s k in
		gotab.(k) <- pb g;
		stoptab.(k) <- pb s;
	done;

	let pupto = Array.make (2*m) 0.0 in
	pupto.(0) <- 1.0;
	for k = 1 to (2*m)-1 do
		pupto.(k) <- pupto.(k-1) *. gotab.(k-1)
	done ;

	let pexact = Array.make (2*m) 0.0 in
	pexact.(0) <- stoptab.(0);
	for k = 1 to (2*m)-1 do
		pexact.(k) <- pupto.(k-1) *. stoptab.(k)
	done ;

	for k = 0 to (2*m -1) do Printf.printf "%4d " k done ;
	Printf.printf "\n";
	for k = 0 to (2*m -1) do Printf.printf "%.2f "
		pexact.(k)
	 done ;
)



