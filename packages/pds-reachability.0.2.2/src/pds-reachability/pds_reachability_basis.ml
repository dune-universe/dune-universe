(*
   This module defines a module type signature used as the basis for the PDS
   reachability functor.
*)

open Pds_reachability_utils;;

(**
   A module type which serves as the basis for the functor which builds the
   PDS reachability implementation.
*)
module type Basis =
sig
  module State : Decorated_type
  module Stack_element : Decorated_type
end;;
