open Bigarray_compat

val genarray : ('a, 'b, 'c) Genarray.t -> ('a, 'b, 'c) Genarray.t -> (int * int array * int array) option
val array0 : ('a, 'b, 'c) Array0.t -> ('a, 'b, 'c) Array0.t -> bool
val array1 : ('a, 'b, 'c) Array1.t -> ('a, 'b, 'c) Array1.t -> (int * int * int) option
val array2 : ('a, 'b, 'c) Array2.t -> ('a, 'b, 'c) Array2.t -> (int * (int * int) * (int * int)) option
val array3 : ('a, 'b, 'c) Array3.t -> ('a, 'b, 'c) Array3.t -> (int * (int * int * int) * (int * int * int)) option
