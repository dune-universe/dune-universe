# Bitlib 0️⃣1️⃣0️⃣1️⃣
A simple OCaml library for bit/byte operations. The purpose of this library is to provide aid to users writing binary files, NOT perform bitwise operations on objects in memory.

## Examples
Getting the binary representation of a number.

        to_bits 5           // returns [1;0;1]
Ensuring a binary sequence is of a certain length, e.g. 8 or 64 bits.

        pad 8 (to_bits 5)   // returns [0;0;0;0;0;1;0;1]

Converting a bit sequence to a byte sequence.


        to_bytes (pad 8 (to_bits 5)) 1          // returns [5]
        to_bytes (padding 32 (to_bits 256)) 4   // returns [0;0;1;0]

Allows you to generate byte sequences of any length in little or big endian order.

        little_endian_of_int 255 2       // returns [0;255]
        big_endian_of_int 255 2          // returns [255;0]

Writing a sequence of bits to a binary file.


        // Write a short int (2 bytes) to a binary file
        let bits = to_bits 512 in
        let padded = pad 16 bits in
        let bytes = to_bytes padded 2 in
        write_binary_file bytes "output.byte"
