(** {1 Bbattery}

   This module contains predefined batteries of statistical tests for sources of
   random bits or sequences of uniform random numbers in the interval \[0,1). To
   test a RNG for general use, one could first apply the small and fast battery
   {!SmallCrush}. If it passes, one could then apply the more stringent battery
   {!Crush}, and finally the yet more time-consuming battery {!BigCrush}.The
   batteries {!Alphabit} and {!Rabbit} can be applied on a binary file
   considered as a source of random bits. They can also be applied on a
   programmed generator. {!Alphabit} has been defined primarily to test hardware
   random bits generators. The battery {!PseudoDIEHARD} applies most of the
   tests in the well-known DIEHARD suite of Marsaglia. The battery {!FIPS-140-2}
   implements the small suite of tests of the FIPS-140-2 standard from NIST.

   The batteries described in this module will write the results of each test
   (on standard output) with a standard level of details (assuming that the
   boolean switches of module {!Swrite} have their default values), followed by
   a summary report of the suspect p-values obtained from the specific tests
   included in the batteries. It is also possible to get only the summary report
   in the output, with no detailed output from the tests, by setting the boolean
   switch {!Swrite.set_basic} to [false].

   Some of the tests compute more than one statistic using the same stream of
   random numbers and these statistics are thus not independent. That is why the
   number of statistics in the summary reports is larger than the number of
   tests in the description of the batteries. *)

(** {2 SmallCrush} *)

val small_crush : Unif01.gen -> unit
val small_crush_file : string -> unit

(** Both functions applies [SmallCrush], a small and fast battery of tests, to a
   RNG. The function {!small_crush_file} applies [SmallCrush] to a RNG given as
   a text file of floating-point numbers in \[0,1); the file requires slightly
   less than 51320000 random numbers. The file will be rewound to the beginning
   before each test. Thus {!small_crush} applies the tests on one unbroken
   stream of successive numbers, while {!small_crush_file} applies each test on
   the same sequence of numbers. Some of these tests assume that the generator
   returns at least 30bits of resolution; if this is not the case, then the
   generator is most likely to fail these particular tests.

    The following tests are applied:
    + {!Smarsa.birthday_spacings} with N = 1, n = 5×10⁶, r = 0, d = 2³⁰, t = 2, p = 1.
    + {!Sknuth.collision}         with N = 1, n = 5×10⁶, r = 0, d = 2¹⁶, t = 2.
    + {!Sknuth.gap}               with N = 1, n = 2×10⁵, r = 22, Alpha = 0, Beta = 1/256.
    + {!Sknuth.simp_poker}        with N = 1, n = 4×10⁵, r = 24, d = 64, k = 64.
    + {!Sknuth.coupon_collector}  with N = 1, n = 5×10⁵, r = 26, d = 16.
    + {!Sknuth.max_oft}           with N = 1, n = 2×10⁶, r = 0, d = 10⁵, t = 6.
    + {!Svaria.weight_distrib}    with N = 1, n = 2×10⁵, r = 27, k = 256, Alpha = 0, Beta = 1/8.
    + {!Smarsa.matrix_rank}       with N = 1, n = 20000, r = 20, s = 10, L = k = 60.
    + {!Sstring.hamming_indep}    with N = 1, n= 5×10⁵, r = 20, s = 10, L = 300, d = 0.
    + {!Swalk.random_walk_1}      with N = 1, n = 10⁶, r = 0, s = 30, L₀ = 150, L₁ = 150. *)

val repeat_small_crush : Unif01.gen -> int array -> unit

(** [repeat_small_crush gen rep] applies specific tests of [SmallCrush] on
    generator [gen]. Test numbered {i i} in the enumeration above will be applied
    [rep[i]] times successively on [gen]. Those tests with [rep[i]] = 0 will not
    be applied. This is useful when a test in [SmallCrush] had a suspect p-value,
    and one wants to reapply the test a few more times to find out whether the
    generator failed the testor whether the suspect p-value was a statistical
    fluke. Restriction: Array [rep] must have one more element than the number of
    tests in [SmallCrush]. *)

val ntests_small_crush : int
(** Number of tests in [SmallCrush]: 10. *)

(** {2 Crush} *)

val crush : Unif01.gen -> unit

val repeat_crush : Unif01.gen -> int array -> unit
(** Similar to {!repeat_small_crush} above but applied on [Crush]. *)

val ntests_crush : int
(** Number of tests in [Crush]: 96. *)

(** {2 BigCrush} *)

val big_crush : Unif01.gen -> unit

val repeat_big_crush : Unif01.gen -> int array -> unit
(** Similar to {!repeat_small_crush} above but applied on [BigCrush]. *)

val ntests_big_crush : int
(** Number of tests in [BigCrush]: 106. *)

(** {2 Rabbit} *)

val rabbit : Unif01.gen -> float -> unit
(** Applies the [Rabbit] battery of tests to the generator [gen] using at most
   [nb] bits for each test. See the description of the tests in {!rabbit_file}. *)

val rabbit_file : string -> float -> unit

(** Applies the [Rabbit] battery of tests to the first [nb] bits (or less, if
   [nb] is too large) of the binary file [filename]. For each test, the file is
   reset and the test is applied to the bit stream starting at the beginning of
   the file. The bits themselves are processed in nearly all the tests as blocks
   of 32 bits (unsigned integers); the two exceptions are
   {!Svaria.appearance_spacings}, which uses blocks of 30 bits (and discards the
   last 2 bits out of each block of 32), and {!Sstring.periods_in_strings} which
   uses blocks of 31 bits (and discards 1 bit out of 32). The parameters of each
   test are chosen automatically as a function of [nb], in order to make the
   test reasonably sensitive. On a PC with an Athlon processor of clock speed
   1733 MHz running under Linux, [Rabbit] will takeabout 5 seconds to test a
   stream of 220bits, 90 seconds to test a stream of 225bits, and 28 minutes to
   test a stream of 230bits. Restriction: [nb] ≥ 500.

   + {!Smultin.multinomial_bits_over}
   + {!Snpair.close_pairs_bit_match} in t = 2 dimensions
   + {!Snpair.close_pairs_bit_match} in t = 4 dimensions.
   + {!Svaria.appearance_spacings}
   + {!Scomp.linear_comp}
   + {!Scomp.lempel_ziv}
   + {!Sspectral.fourier_1}
   + {!Sspectral.fourier_3}
   + {!Sstring.longest_head_run}
   + {!Sstring.periods_in_strings}
   + {!Sstring.hamming_weight} with blocks of L = 32 bits.
   + {!Sstring.hamming_corr} with blocks of L = 32 bits.
   + {!Sstring.hamming_corr} with blocks of L = 64 bits.
   + {!Sstring.hamming_corr} with blocks of L = 128 bits.
   + {!Sstring.hamming_indep} with blocks of L = 16 bits.
   + {!Sstring.hamming_indep} with blocks of L = 32 bits.
   + {!Sstring.hamming_indep} with blocks of L = 64 bits.
   + {!Sstring.auto_cor} with a lag d = 1.
   + {!Sstring.auto_cor} with a lag d = 2.
   + {!Sstring.run}
   + {!Smarsa.matrix_rank} with 32×32 matrices.
   + {!Smarsa.matrix_rank} with 320×320 matrices.
   + {!Smarsa.matrix_rank} with 1024×1024 matrices.
   + {!Swalk.random_walk_1} with walks of length L= 128.
   + {!Swalk.random_walk_1} with walks of length L= 1024.
   + {!Swalk.random_walk_1} with walks of length L= 10016. *)

val repeat_rabbit : Unif01.gen -> float -> int array -> unit
(** Similar to {!repeat_small_crush} above but applied on [Rabbit]. *)

val ntests_rabbit : int
(** Number of tests in [Rabbit]: 26. *)

(** {2 Alphabit} *)

val alphabit : Unif01.gen -> float -> int -> int -> unit
(** [alphabit gen nb r s] applies the [Alphabit] battery of tests to the
   generator [gen] using at most [nb] bits for each test. The bits themselves
   are processed as blocks of 32 bits (unsigned integers). For each block of 32
   bits, the r most significant bits are dropped, and the test is applied on the
   s following bits. If one wants to test all bits of the stream, one should set
   r = 0 and s = 32. If one wants to test only 1 bit out of 32, one should set s
   = 1. See the description of the tests in {!alphabit_file}. *)

val alphabit_file : string -> float -> unit

(** [alphabit_file filename nb] applies the [Alphabit] battery of tests to the
   first [nb] bits (or less, if [nb] is too large) of the binary file
   [filename]. Unlike the {!alphabit} function above, for each test, the file is
   rewound and the test is applied to the bit stream starting at the beginning
   of the file. On a PC with an Athlon processor of clock speed 1733 MHz running
   under Linux, [Alphabit] takes about 4.2 seconds to test a file of 2²⁵ bits,
   and 2.3 minutes to test a file of 2³⁰ bits. [Alphabit] and [AlphabitFile]
   have been designed primarily to test hardware random bits generators. The
   four [multinomial_bits_over] tests should detect correlations between
   successive bits by applying a [serial_over] test to overlapping blocks of 2,
   4, 8 and 16 bits. The [hamming_*] tests should detect correlations between
   the successive bits of overlapping blocks of 16 and 32 bits, and the
   [random_walk] tests consider blocks of 64 and 320 bits.

   + {!Smultin.multinomial_bits_over} with L = 2.
   + {!Smultin.multinomial_bits_over} with L = 4.
   + {!Smultin.multinomial_bits_over} with L = 8.
   + {!Smultin.multinomial_bits_over} with L = 16.
   + {!Sstring.hamming_indep} with blocks of L = 16 bits.
   + {!Sstring.hamming_indep} with blocks of L = 32 bits.
   + {!Sstring.hamming_corr} with blocks of L = 32 bits.
   + {!Swalk.random_walk_1} with walks of length L = 64.
   + {!Swalk.random_walk_1} with walks of length L = 320. *)

val repeat_alphabit : Unif01.gen -> float -> int -> int -> int array -> unit
(** Similar to {!repeat_small_crush} above but applied on [Alphabit]. *)

val ntests_alphabit : int
(** Number of tests in [Alphabit]: 9. *)

(** {2 BlockAlphabit} *)

val block_alphabit : Unif01.gen -> float -> int -> int -> unit
val block_alphabit_file : string -> float -> unit

(** Apply the Alphabit battery of tests repeatedly to the generator [gen] or to
   the binary file [filename] after reordering the bits as described in the
   filter {!Unif01.create_bit_block_gen}. [Alphabit] will be applied for the
   different values of w ∈ \{1,2,4,8,16,32\}. If s < 32, only values of w≤s will
   be used. Each test uses at most [nb] bits. See the description of the tests
   in {!alphabit_file}. *)

val repeat_block_alphabit : Unif01.gen -> float -> int -> int -> int array -> int -> unit
(** Similar to {!repeat_small_crush} above but applied on [BlockAlphabit]. The
   parameter w is the one described in {!block_alphabit}. Restrictions: w ∈
   \{1,2,4,8,16,32\} and w≤s. *)

val ntests_block_alphabit : int
(** Number of tests in [BlockAlphabit]: 9. *)

(** {2 PseudoDIEHARD} *)

val pseudo_diehard : Unif01.gen -> unit

(** Applies the battery [PseudoDIEHARD], which implements most of the tests in
   the popular battery DIEHARD or, in some cases, close approximations to them.
   We do not recommend this battery as it is not very stringent (we do not know
   of any generator that passes the batteries [Crush] and [BigCrush], and fails
   [PseudoDIEHARD], while we have seen the converse for several defective
   generators). It is included here only for convenience to the user. The
   DIEHARD tests and the corresponding tests in [PseudoDIEHARD] are:

   + The Birthday Spacings test. This corresponds to {!Smarsa.birthday_spacings}
     with n = 512, d = 224, t = 1 and r = 0,1,2,3,4,5,6,7,8,9 successively. The
     test with each value of r is repeated 500 times and a chi-square test is
     then applied.
   + The Overlapping 5-Permutation test. This test is not implemented in TestU01.
   + The Binary Rank Tests for Matrices. This corresponds to {!Smarsa.matrix_rank}.
   + The Bitstream test. Closely related to {!Smulti.multinomial_bits_over} with
     Delta = −1, n = 221, L = 20.
   + The OPSO test. This corresponds to {!Smarsa.collision_over} with n = 221,
     d = 1024, t = 2 and all values of r from 0 to 22.
   + The OQSO test. This corresponds to {!Smarsa.collision_over} with n = 221,
     d = 32, t = 4 and all values of r from 0 to 27.
   + The DNA test. This corresponds to {!Smarsa.collision_over} with n = 221,
     d = 4, t = 10 and all values of r from 0 to 30.
   + The Count-the-1's test is not implemented in TestU01. It is a 5-dimensional
     over-lapping version of {!Sstring.hamming_indep}.
   + The Parking Lot test is not implemented in TestU01.
   + The Minimum Distance test. Closely related to {!Snpair.close_pairs} with
     N = 100, n = 8000, t = 2, p = 2, m = 1.
   + The 3-D Spheres test. Closely related to {!Snpair.close_pairs} with N = 20,
     n = 4000, t = 3, p = 2, m = 1.
   + The Squeeze test. Closely related to {!Smarsa.savir_2}.
   + The Overlapping Sums test is not implemented in TestU01.
   + The Runs test. This corresponds to {!Sknuth.run}.
   + The Craps test is not implemented in TestU01. *)

val ntests_pseudo_diehard : int
(** Number of tests in [PseudoDIEHARD]: 15. *)

(** {2 NIST}

   The NIST (National Institute of Standards and Technology) of the U.S.
   federal government has proposed a statistical test suite for use in the
   evaluation of the randomness of bitstreams produced by cryptographic random
   number generators. The test parameters are not predetermined. The NIST tests
   and the equivalent tests in TestU01 are:

   + The Monobit test. This corresponds to {!Sstring.hamming_weight_2} with L = n.
   + The Frequency test within a Block. Corresponds to {!Sstring.hamming_weight_2}.
   + The Runs test. Is implemented as {!Sstring.run}.
   + The test for the Longest Run of Ones in a Block. Is implemented as the test {!Sstring.longest_head_run}.
   + The Binary Matrix rank test. Is implemented as {!Smarsa.matrix_rank}.
   + The Discrete Fourier Transform test. Is implemented as {!Sspectral.fourier_1}.
   + The Non-overlapping Template Matching test. Is implemented as the test {!Smarsa.cat_bits}.
   + The Overlapping Template Matching test. This test does not exist as such in TestU01, but a similar and more powerful test is {!Smultin.multinomial_bits_over}.
   + Maurer’s Universal Statistical test. This test is implemented as {!Svaria.appearance_spacings}.
   + The Lempel-Ziv Compression test. Is implemented as {!Scomp.lempel_ziv}.
   + The Linear Complexity test. Is implemented as part of {!Scomp.linear_comp}.
   + The Serial test. Corresponds to {!Smultin.multinomail_bits_over} with Delta = 1.
   + The Approximate Entropy test. Corresponds to {!Smultin.multinomial_bits_over} with Delta = 0, and to {!Sentrop.entropy_disc_over} or {!Sentrop.entropy_disc_over_2}.
   + The Cumulative Sums test. This test is closely related to the M statistic in {!Swalk.random_walk_1}.
   + The Random Excursions test. This test does not exist in TestU01, but closely related tests are in {!Swalk.random_walk_1}.
   + The Random Excursions Variant test. This test does not exist in TestU01, but a closely related test is based on the R statistic in {!Swalk.random_walk_1}. *)

(** {2 FIPS-140-2} *)

val fips_140_2 : Unif01.gen -> unit
val fips_140_2_file : string -> unit

(** These functions apply the four tests described in the NIST document {i FIPS
   PUB 140-2, Security Requirements for Cryptographic Modules}, page 35, with
   exactly the same parameters (see the WEB page at
   {{: http://csrc.nist.gov/rng/rng6_3.html}
   http://csrc.nist.gov/rng/rng6_3.html}). They report the values of the test
   statistics and their p-values (except for the runs test) and indicate which
   values fall outside theintervals specified by FIPS-140-2. The first function
   applies the tests on a generator [gen], and the second applies them on the
   file of bits [filename]. First, 20000 bits are generated and putin an array,
   then the tests are applied upon these. The tests applied are:

   + The Monobit test. This corresponds to {!Smultin.multinomal_bits} with s = 32, L = 1, n = 20000.
   + The “poker” test, which is in fact equivalent to {!Smultin.multinomial_bits} with s = 32, L = 4, n = 5000.
   + The Runs test, which is related to {!Sstring.run}.
   + The test for the Longest Run of Ones in a Block, which is implemented as {!Sstring.longest_head_run}. *)

val ntests_fips_140_2 : int
(** Number of tests in [FIPS-140-2]: 4. *)
