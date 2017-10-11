open OUnit2
open Binbin

let empty = (unsafe_b "")
let zero_bit = (unsafe_b "0")
let zero_byte = (unsafe_b "00000000")
let one_bit = (unsafe_b "1")
let one_byte = (unsafe_b "11111111")
let two_fifty_six_b = (unsafe_b "100000000")
let a_thousand_b = (unsafe_b "1111101000")
let freedom = "freedom"
let freedom_b = (unsafe_b "01100110011100100110010101100101011001000110111101101101")
let quick_brown_fox = "the quick brown fox!"
let quick_brown_fox_b = (unsafe_b "0111010001101000011001010010000001110001011101010110100101100011011010110010000001100010011100100110111101110111011011100010000001100110011011110111100000100001")
let f_pattern_b = (unsafe_b "110")
let s_pattern_b = (unsafe_b "1001")

let test_fixture = "Binbin" >:::
    [
        "size" >:: ( fun test_ctx ->
            assert_equal (size empty) 0;
            assert_equal (size zero_bit) 1;
            assert_equal (size one_bit) 1;
            assert_equal (size zero_byte) 8;
        );

        "concat" >:: ( fun test_ctx ->
            assert_equal (concat empty empty) empty;
            assert_equal (concat zero_bit one_bit) (unsafe_b "01");
            assert_equal (concat one_bit zero_bit) (unsafe_b "10");
            assert_equal (concat zero_byte one_bit) (unsafe_b "000000001");
            assert_equal (concat one_bit (concat zero_bit (concat zero_bit (concat zero_bit (concat zero_bit (concat zero_bit (concat zero_bit (concat zero_bit zero_bit)))))))) two_fifty_six_b
        );

        "of_int" >:: ( fun test_ctx ->
            (** Should add support for signed ints *)
            assert_equal (of_int 0) (unsafe_b "0");
            assert_equal (of_int 1) (unsafe_b "1");
            assert_equal (of_int 2) (unsafe_b "10");
            assert_equal (of_int 256) two_fifty_six_b; 
            assert_equal (of_int 1000) a_thousand_b; 
        );

        "of_char" >:: ( fun test_ctx ->
            let rec test_all_chars i =
                if i = 256 then ()
                else
                    let c = Char.chr i in
                    assert_equal (of_char c) (of_int i);
                    test_all_chars (i+1)
            in test_all_chars 0;
        );

        "of_string" >:: ( fun test_ctx ->
            assert_equal (of_string "") empty;
            assert_equal (of_string "a") (concat zero_bit (of_char 'a'));
            assert_equal (of_string "1") (concat (unsafe_b "00") (of_char '1'));
            assert_equal (of_string "1111") (unsafe_b "00110001001100010011000100110001");
            assert_equal (of_string freedom) freedom_b;
            assert_equal (of_string quick_brown_fox) quick_brown_fox_b;
        );

        "to_int" >:: ( fun test_ctx ->
            assert_equal (to_int zero_bit) 0;
            assert_equal (to_int zero_byte) 0;
            assert_equal (to_int one_bit) 1;
            assert_equal (to_int one_byte) 255;
            assert_equal (to_int two_fifty_six_b) 256;
			assert_equal (to_int a_thousand_b) 1000;
		);
        
        "to_char" >:: ( fun test_ctx ->
            assert_equal (to_char zero_bit) (Char.chr 0);
            assert_equal (to_char zero_byte) (Char.chr 0);
            assert_equal (to_char one_bit) (Char.chr 1);
            assert_equal (to_char one_byte) (Char.chr 255);
            (** Should raise our own exception *)
            let invalid_cast = fun () -> (to_char two_fifty_six_b) in
            assert_raises (Invalid_argument "Char.chr") invalid_cast;
        );

        "to_ascii" >:: ( fun test_ctx ->
            assert_equal (to_ascii empty) "";
            assert_equal (to_ascii quick_brown_fox_b) quick_brown_fox;
            assert_equal (to_ascii freedom_b) freedom;
        );

        "take" >:: ( fun test_ctx ->
            assert_equal (take 0 empty) empty;
            assert_equal (take 0 two_fifty_six_b) empty;
            assert_equal (take 1 zero_bit) zero_bit;
            assert_equal (take 1 one_bit) one_bit;
            assert_equal (take 8 zero_byte) zero_bit;
            assert_equal (take 8 one_byte) one_bit;
            assert_equal (take 6 a_thousand_b) zero_bit;
            assert_equal (take 7 a_thousand_b) one_bit;
        );

        "make" >:: ( fun test_ctx ->
            assert_equal (make 0 empty) empty;
            assert_equal (make 0 zero_bit) empty;
            assert_equal (make 0 zero_byte) empty;
            assert_equal (make 8 empty) empty;
            assert_equal (make 8 zero_bit) zero_byte;
            assert_equal (make 8 one_bit) one_byte;
            assert_equal (make 8 zero_byte) zero_byte;
            assert_equal (make 8 one_byte) one_byte;
            assert_equal (make 2 f_pattern_b) (unsafe_b "11");
            assert_equal (make 3 f_pattern_b) f_pattern_b; 
            assert_equal (make 9 f_pattern_b) (unsafe_b "110110110");
            assert_equal (make 11 f_pattern_b) (unsafe_b "11011011011");
            assert_equal (make 3 s_pattern_b) (unsafe_b "100");
            assert_equal (make 4 s_pattern_b) s_pattern_b;
            assert_equal (make 12 s_pattern_b) (unsafe_b "100110011001");
            assert_equal (make 15 s_pattern_b) (unsafe_b "100110011001100");
        );

        "flip" >:: ( fun test_ctx ->
            assert_equal (flip empty) empty;
            assert_equal (flip zero_bit) one_bit;
            assert_equal (flip one_bit) zero_bit;
            assert_equal (flip zero_byte) one_byte;
            assert_equal (flip one_byte) zero_byte;
            (** we need a deep equal *)
            assert_equal (flip two_fifty_six_b) (unsafe_b "011111111");
        );

        "flip_bit_at" >:: ( fun test_ctx ->
            assert_equal (flip_bit_at 0 empty) empty;
            assert_equal (flip_bit_at 0 zero_bit) zero_bit;
            assert_equal (flip_bit_at 0 one_bit) one_bit;
            assert_equal (flip_bit_at 0 zero_byte) zero_byte;
            assert_equal (flip_bit_at 1 zero_bit) one_bit;
            assert_equal (flip_bit_at 1 one_bit) zero_bit;
            (** we really need a deep equal *)
            assert_equal (flip_bit_at 8 zero_byte) (unsafe_b "00000001" );
            assert_equal (flip_bit_at 8 one_byte) (of_int 254);
            assert_equal (flip_bit_at 4 zero_byte) (unsafe_b "00010000");
            assert_equal (flip_bit_at 4 one_byte) (of_int 239);
        );

        "pad_left" >:: ( fun test_ctx ->
            assert_equal (pad_left 0 empty) empty;
            assert_equal (pad_left 0 zero_bit) zero_bit;
            assert_equal (pad_left 0 one_bit) one_bit;
            assert_equal (pad_left 8 empty) zero_byte;
            assert_equal (pad_left 7 zero_bit) zero_byte;
            assert_equal (pad_left 7 one_bit) (unsafe_b "00000001");
            assert_equal (to_int (pad_left 7 one_bit)) (to_int one_bit);
            assert_equal (to_int (pad_left 0 two_fifty_six_b)) (to_int two_fifty_six_b);
            assert_equal (to_int (pad_left 100 two_fifty_six_b)) (to_int two_fifty_six_b);
            assert_equal (to_int (pad_left 0 a_thousand_b)) (to_int a_thousand_b);
            assert_equal (to_int (pad_left 8 a_thousand_b)) (to_int a_thousand_b);
        );

        "pad_right" >:: ( fun test_ctx ->
            assert_equal (pad_right 0 empty) empty; 
            assert_equal (pad_right 0 zero_bit) zero_bit;
            assert_equal (pad_right 0 one_bit) one_bit;
            assert_equal (pad_right 8 empty) zero_byte; 
            assert_equal (pad_right 7 zero_bit) zero_byte;
            assert_equal (pad_right 1 one_bit) (of_int 2);
            assert_equal (pad_right 3 one_bit) (of_int 8);
            assert_equal (pad_right 17 one_bit) (of_int 131072);
            assert_equal (pad_right 7 one_bit) (of_int 128);
            assert_equal (pad_right 8 one_byte)  (of_int 65280);
            assert_equal (pad_right 10 two_fifty_six_b) (of_int 262144);
        );

        "reverse" >:: ( fun test_ctx ->
            assert_equal (reverse empty) empty;
            assert_equal (reverse zero_bit) zero_bit; 
            assert_equal (reverse one_bit) one_bit;
            assert_equal (reverse zero_byte) zero_byte;
            assert_equal (reverse one_byte) one_byte;
            assert_equal (to_int (reverse two_fifty_six_b)) 1;
            assert_equal (to_int (reverse a_thousand_b)) 95;
            assert_equal (to_int (reverse (reverse a_thousand_b))) 1000;
        );

        "normalize" >:: ( fun test_ctx ->
            let same_size tup_b = match tup_b with
                | (b1, b2) -> (size b1) = (size b2)
            in 
            assert_equal (normalize empty empty) (empty, empty);
            assert_equal (normalize one_bit one_bit) (one_bit, one_bit);
            assert_equal (normalize zero_bit one_bit) (zero_bit, one_bit);
            assert_equal (normalize zero_bit zero_byte) (zero_byte, zero_byte);
            assert_equal (normalize one_byte zero_bit) (one_byte, zero_byte);
            assert_equal (same_size (normalize zero_bit a_thousand_b)) true;
            assert_equal (same_size (normalize a_thousand_b one_bit)) true;
            assert_equal (same_size (normalize freedom_b quick_brown_fox_b)) true;
        );

        "cardinality" >:: ( fun test_ctx ->
            assert_equal (cardinality empty) (of_int 0);
            assert_equal (cardinality zero_bit) (of_int 0);
            assert_equal (cardinality one_bit) (of_int 1);
            assert_equal (cardinality zero_byte) (of_int 0);
            assert_equal (cardinality one_byte) (of_int 8);
            assert_equal (cardinality two_fifty_six_b) (of_int 1);
            assert_equal (cardinality a_thousand_b) (of_int 6);
            assert_equal (cardinality freedom_b) (of_int 30);
        );

        "hamming_distance" >:: ( fun test_ctx ->
            let b1 = (of_string "the plane") in
            let b2 = (of_string "a car driving fast") in
            let b3 = (of_string "roses are red") in
            let b4 = (of_string "polynomial") in
            let b5 = (of_string "Jean-Baptiste Say") in
            let b6 = (of_string "this is a test") in
            let b7 = (of_string "wokka wokka!!!") in
            assert_equal (hamming_distance b1 b1) (of_int 0);
            assert_equal (hamming_distance b5 b5) (of_int 0);
            assert_equal (hamming_distance b7 b7) (of_int 0);
            assert_equal (hamming_distance empty b1) (of_int 31);
            assert_equal (hamming_distance b1 b2) (of_int 46);
            assert_equal (hamming_distance b2 b3) (of_int 49);
            assert_equal (hamming_distance b3 b4) (of_int 41);
            assert_equal (hamming_distance b4 b5) (of_int 48); 
            assert_equal (hamming_distance b5 b6) (of_int 53);
            assert_equal (hamming_distance b6 b7) (of_int 37);
            assert_equal (hamming_distance b1 (flip_bit_at 2 b1)) (of_int 1);
            assert_equal (hamming_distance b3 (flip_bit_at 5 b3)) (of_int 1);
            assert_equal (hamming_distance b5 (flip_bit_at 7 b5)) (of_int 1);
            assert_equal (hamming_distance b7 (flip_bit_at 11 b7)) (of_int 1);
        );

        "find_first_one" >:: ( fun test_ctx ->
            let b1 = (unsafe_b "00000010000") in
            let b2 = (unsafe_b "00000100010") in
            let b3 = (unsafe_b "010") in
            let b4 = (unsafe_b "10000000000") in
            assert_equal (to_int (find_first_one empty))     0;
            assert_equal (to_int (find_first_one zero_bit))  0;
            assert_equal (to_int (find_first_one one_bit))   1;
            assert_equal (to_int (find_first_one zero_byte)) 0;
            assert_equal (to_int (find_first_one one_byte))  1;
            assert_equal (to_int (find_first_one b1))        7;
            assert_equal (to_int (find_first_one b2))        6;
            assert_equal (to_int (find_first_one b3))        2;
            assert_equal (to_int (find_first_one b4))        1;
        );

        "count_leading_zeros" >:: ( fun test_ctx ->
            let b1 = (unsafe_b "00000010000") in
            let b2 = (unsafe_b "00000100010") in
            let b3 = (unsafe_b "010") in
            let b4 = (unsafe_b "10000000000") in
            assert_equal (to_int (count_leading_zeros empty))     0;
            assert_equal (to_int (count_leading_zeros zero_bit))  1;
            assert_equal (to_int (count_leading_zeros one_bit))   0;
            assert_equal (to_int (count_leading_zeros zero_byte)) 8;
            assert_equal (to_int (count_leading_zeros one_byte))  0;
            assert_equal (to_int (count_leading_zeros b1))        6;
            assert_equal (to_int (count_leading_zeros b2))        5;
            assert_equal (to_int (count_leading_zeros b3))        1;
            assert_equal (to_int (count_leading_zeros b4))        0;
        );

        "count_trailing_zeros" >:: ( fun test_ctx ->
            let b1 = (unsafe_b "00000010000") in
            let b2 = (unsafe_b "00000100010") in
            let b3 = (unsafe_b "010") in
            let b4 = (unsafe_b "10000000000") in
            assert_equal (to_int (count_trailing_zeros empty))     0;
            assert_equal (to_int (count_trailing_zeros zero_bit))  1;
            assert_equal (to_int (count_trailing_zeros one_bit))   0;
            assert_equal (to_int (count_trailing_zeros zero_byte)) 8;
            assert_equal (to_int (count_trailing_zeros one_byte))  0;
            assert_equal (to_int (count_trailing_zeros b1))        4;
            assert_equal (to_int (count_trailing_zeros b2))        1;
            assert_equal (to_int (count_trailing_zeros b3))        1;
            assert_equal (to_int (count_trailing_zeros b4))        10;
        );

        "irreducible" >:: ( fun test_ctx ->
            let b1 = (unsafe_b "0001000000001000") in
            let b2 = (unsafe_b "0000000000000000") in
            let b3 = (unsafe_b "0001110100101010") in
            let b4 = (unsafe_b "1000000000000000") in
            let i1 = (unsafe_b "1000000001000") in
            let i3 = (unsafe_b "1110100101010") in
            assert_equal (irreducible empty) empty;
            assert_equal (irreducible b1) i1;
            assert_equal (irreducible b2) b2;
            assert_equal (irreducible b3) i3;
            assert_equal (irreducible b4) b4;
        );

        "map" >:: ( fun test_ctx ->
            let id x = x in
            assert_equal (map id empty) empty;
            assert_equal (map id zero_bit) zero_bit;
            assert_equal (map id one_bit) one_bit;
            assert_equal (map id zero_byte) zero_byte;
            assert_equal (map id one_byte) one_byte;
            assert_equal (map id two_fifty_six_b) two_fifty_six_b;
            assert_equal (map id two_fifty_six_b) two_fifty_six_b;
            assert_equal (map flip empty) empty;
            assert_equal (map flip zero_bit) one_bit;
            assert_equal (map flip one_bit) zero_bit;
            assert_equal (map flip zero_byte) one_byte;
            assert_equal (map flip one_byte) zero_byte;
        );
        
        "mapi" >:: ( fun test_ctx ->
            let id i x = (of_int (i mod 2)) in
            let byte_b = (unsafe_b "01010101") in
            let two_fifty_six_id_b  = (unsafe_b "010101010") in
            assert_equal (mapi id empty) empty;
            assert_equal (mapi id zero_bit) zero_bit;
            assert_equal (mapi id one_bit) zero_bit;
            assert_equal (mapi id zero_byte) byte_b;
            assert_equal (mapi id one_byte) byte_b;
            assert_equal (mapi id two_fifty_six_b) two_fifty_six_id_b;
        );
	]

let _ = run_test_tt_main test_fixture
