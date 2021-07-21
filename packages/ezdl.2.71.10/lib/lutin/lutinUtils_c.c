/*--------------------------------------------------------------
0.1 -> 15/02/2006 
-----------------
   The interval algorithm was completely false !!
----------------------------------------------------------------

C-Implementation of the Lutin compiler pre-defined weight functions

N.B. For the time being, all functions are returning "normalized"
weights on the range [0,10000], but t may change: don't make
any use of this property!

"GAUSSIAN" LOOPS
----------------
int gauss_continue(int x, int mu, int sigma)
int gauss_stop(int x, int mu, int sigma)

The result is undefined unless:
    0 <= x
    0 < sigma < mu
Moreover, the result is not "accurate" unless:
    4*sigma < mu

--------------------------------------------------------------*/

#include <stdio.h>

#ifdef WIN32
#define EXPORT __declspec(dllexport)
#else
#define EXPORT
#endif

 
#define SAMPLES_PER_UNIT 128
#define TOTAL_SAMPLES (4*SAMPLES_PER_UNIT)
#define SAMPLE_GRAIN 10000

static int GD_SAMPLES[TOTAL_SAMPLES] = {
 5000,  5032,  5063,  5094,  5125,  5156,  5187,  5219,
 5250,  5281,  5312,  5343,  5374,  5405,  5436,  5467,
 5498,  5529,  5560,  5591,  5621,  5652,  5683,  5714,
 5744,  5775,  5805,  5836,  5866,  5897,  5927,  5957,
 5988,  6018,  6048,  6078,  6108,  6138,  6168,  6197,
 6227,  6257,  6286,  6316,  6345,  6375,  6404,  6433,
 6462,  6491,  6520,  6549,  6578,  6606,  6635,  6663,
 6692,  6720,  6748,  6776,  6804,  6832,  6860,  6888,
 6915,  6943,  6970,  6997,  7024,  7051,  7078,  7105,
 7132,  7158,  7185,  7211,  7237,  7263,  7289,  7315,
 7341,  7366,  7392,  7417,  7442,  7467,  7492,  7517,
 7542,  7566,  7591,  7615,  7639,  7663,  7687,  7711,
 7734,  7758,  7781,  7804,  7827,  7850,  7873,  7895,
 7918,  7940,  7963,  7985,  8006,  8028,  8050,  8071,
 8093,  8114,  8135,  8156,  8176,  8197,  8218,  8238,
 8258,  8278,  8298,  8318,  8337,  8357,  8376,  8395,
 8414,  8433,  8451,  8470,  8488,  8507,  8525,  8543,
 8560,  8578,  8596,  8613,  8630,  8647,  8664,  8681,
 8698,  8714,  8730,  8747,  8763,  8779,  8794,  8810,
 8825,  8841,  8856,  8871,  8886,  8901,  8915,  8930,
 8944,  8958,  8972,  8986,  9000,  9014,  9027,  9041,
 9054,  9067,  9080,  9093,  9105,  9118,  9130,  9143,
 9155,  9167,  9179,  9191,  9202,  9214,  9225,  9236,
 9248,  9259,  9270,  9280,  9291,  9302,  9312,  9322,
 9332,  9342,  9352,  9362,  9372,  9382,  9391,  9400,
 9410,  9419,  9428,  9437,  9446,  9454,  9463,  9471,
 9480,  9488,  9496,  9504,  9512,  9520,  9528,  9535,
 9543,  9550,  9558,  9565,  9572,  9579,  9586,  9593,
 9600,  9607,  9613,  9620,  9626,  9632,  9639,  9645,
 9651,  9657,  9663,  9669,  9674,  9680,  9686,  9691,
 9697,  9702,  9707,  9712,  9717,  9722,  9727,  9732,
 9737,  9742,  9746,  9751,  9756,  9760,  9764,  9769,
 9773,  9777,  9781,  9785,  9789,  9793,  9797,  9801,
 9805,  9808,  9812,  9816,  9819,  9823,  9826,  9829,
 9833,  9836,  9839,  9842,  9845,  9848,  9851,  9854,
 9857,  9860,  9863,  9865,  9868,  9871,  9873,  9876,
 9878,  9881,  9883,  9886,  9888,  9890,  9892,  9895,
 9897,  9899,  9901,  9903,  9905,  9907,  9909,  9911,
 9913,  9915,  9916,  9918,  9920,  9922,  9923,  9925,
 9927,  9928,  9930,  9931,  9933,  9934,  9936,  9937,
 9938,  9940,  9941,  9942,  9944,  9945,  9946,  9947,
 9949,  9950,  9951,  9952,  9953,  9954,  9955,  9956,
 9957,  9958,  9959,  9960,  9961,  9962,  9963,  9964,
 9965,  9965,  9966,  9967,  9968,  9969,  9969,  9970,
 9971,  9971,  9972,  9973,  9973,  9974,  9975,  9975,
 9976,  9977,  9977,  9978,  9978,  9979,  9979,  9980,
 9980,  9981,  9981,  9982,  9982,  9983,  9983,  9984,
 9984,  9984,  9985,  9985,  9986,  9986,  9986,  9987,
 9987,  9987,  9988,  9988,  9988,  9989,  9989,  9989,
 9990,  9990,  9990,  9990,  9991,  9991,  9991,  9991,
 9992,  9992,  9992,  9992,  9993,  9993,  9993,  9993,
 9993,  9994,  9994,  9994,  9994,  9994,  9994,  9995,
 9995,  9995,  9995,  9995,  9995,  9995,  9996,  9996,
 9996,  9996,  9996,  9996,  9996,  9996,  9997,  9997,
 9997,  9997,  9997,  9997,  9997,  9997,  9997,  9997,
 9998,  9998,  9998,  9998,  9998,  9998,  9998,  9998,
 9998,  9998,  9998,  9998,  9998,  9998,  9999,  9999,
 9999,  9999,  9999,  9999,  9999,  9999,  9999,  9999,
 9999,  9999,  9999,  9999,  9999,  9999,  9999,  9999,
 9999,  9999,  9999,  9999,  9999, 10000, 10000, 10000,
10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000,
10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000,
10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000,
10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000
};

//last computation cache
static int last_a_t = 0;
static int last_a_sigma = 1;
static int last_a_goon = (SAMPLE_GRAIN/2);
static int last_a_stop = (SAMPLE_GRAIN/2);

static void compute_average(int t, int  sigma){
	last_a_t = t;
	last_a_sigma = sigma;
	last_a_t = t;
	if ( t < 0) {
		//Negative: the mean is not yet reached
		//Normalisation in 1/SAMPLES_PER_UNIT
		t = (-t*SAMPLES_PER_UNIT)/sigma;
		if (t < TOTAL_SAMPLES) {
			last_a_goon = GD_SAMPLES[t];
			last_a_stop = SAMPLE_GRAIN - last_a_goon;
		} else {
			last_a_goon = SAMPLE_GRAIN;
			last_a_stop = 0;
		}
	} else {
		//Positive: the mean is already reached
		//Normalisation in 1/SAMPLES_PER_UNIT
		t = (t*SAMPLES_PER_UNIT)/sigma;
		if (t < TOTAL_SAMPLES) {
			last_a_stop = GD_SAMPLES[t];
			last_a_goon = SAMPLE_GRAIN - last_a_stop;
		} else {
			last_a_stop = SAMPLE_GRAIN;
			last_a_goon = 0;
		}
	}
}

EXPORT int gauss_continue(int x, int mu, int  ec){
	int t = x - mu;
	int sigma = (ec > 0)? ec : (14*mu/100); 
	//The return value is not accurate when mu < 4*sigma
	if(mu < 4*sigma){
		fprintf(stderr, "WARNING: sigma too big compared to mu\n"); 
	}
	if((mu != last_a_t)||(sigma != last_a_sigma)){
		compute_average(t, sigma);
	}
	return last_a_goon;
}

EXPORT int gauss_stop(int x, int mu, int  ec){

	int t = x - mu;
	int sigma = (ec > 0)? ec : (14*mu/100); 
	//The return value is not accurate when mu < 4*sigma
	if(mu < 4*sigma){
		fprintf(stderr, "WARNING: sigma too big compared to mu\n"); 
	}
	if((t != last_a_t)||(sigma != last_a_sigma)){
		compute_average(t, sigma);
	}
	return last_a_stop;
}

/* un test ...
int main(){

   int m = 200;
   int s = 30;

   int k;

   for(k=0; k < 400; k++){
      int wg = gauss_continue(k,m,s);
      int ws = gauss_stop(k,m,s);
      printf("%3d %5d %5d\n", k, wg, ws);
   }
   return 1;
}
*/
/*--------------------------------------------------------------

C-Implementation of the Lutin compiler pre-defined weight functions

N.B. For the time being, all functions are returning "normalized"
weights on the range [0,10000], but t may change: don't make
any use of this property!

INTERVAL LOOPS
--------------
int interval_continue(int x, int min, int max)
int interval_stop(int x, int min, int max)

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

--------------------------------------------------------------*/

//last computation cache
//sets to something correct ! 
static int last_i_d = 0;
static int last_i_width = 0;
static int last_i_goon = 0;
static int last_i_stop = 1;

static void compute_interval(int d, int width){
	last_i_d = d;
	last_i_width = width;
	if(d < 0) {
		last_i_stop = 0;
		last_i_goon = 1;
	} else if(width > 0){
		last_i_stop = 1;
		last_i_goon = width;
	} else {
		last_i_stop = 1;
		last_i_goon = 0;
	}
}

EXPORT int interval_continue(int x, int min, int max){
	int d = x - min;
	int width = max - x;
	if((d != last_i_d)||(width != last_i_width)){
		compute_interval(d, width);
	}
	return last_i_goon;
}

EXPORT int interval_stop(int x, int min, int max){
	int d = x - min;
	int width = max - x;
	if((d != last_i_d)||(width != last_i_width)){
		compute_interval(d, width);
	}
	return last_i_stop;
}

/* un test 
int main(){

   int min = 2;
   int max = 6;

   int k;

	printf("loop[%d,%d] weights\n", min, max);
	printf("|-----|-------|-------|\n");
	printf("| cpt |  cont |  stop |\n");
	printf("|-----|-------|-------|\n");
   for(k=0; k < 10; k++){
      int wg = interval_continue(k,min,max);
      int ws = interval_stop(k,min,max);
      printf("| %3d | %5d | %5d |\n", k, wg, ws);
   }
	printf("|-----|-------|-------|\n");
   return 1;
}
*/
