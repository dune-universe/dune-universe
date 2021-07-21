open Core_kernel

type matrix = {
  id : string ;
  tf_name : string ;
  counts : int array array ;
}

val of_file : string -> (matrix, string) result
