###

    sed -i 's/@array\[\([a-z]*\)\]/\1_array_t/g' src/GT.ml

    sed -i 's/@array/array_t/g' src/GT.ml

