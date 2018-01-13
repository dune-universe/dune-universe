(* module PTest must provide the following functions:

  val test_unit : ?loc: Location.t -> string option -> (unit -> unit) -> unit
  val test : ?loc: Location.t -> string option -> (unit -> bool) -> unit

*)
module PTest = Ppx_test.Test

(* %TEST is for tests of type bool.

   This replaces pa_ounit's [TEST name = e]
*)
let %TEST equal = 1 = 1

(* %TEST with name ends with `_` is for tests of type unit,
   following Haskell function naming convention.

   This replaces pa_ounit's [TEST_UNIT name = e] 
 *)
let %TEST equal_ = assert (1 = 1)

(* %TEST_UNIT is for tests of type unit. 

   This replaces pa_ounit's [TEST_UNIT name = e]. 
*)
let %TEST_UNIT trivial = assert true

(* Currently there is no simple way to replace pa_ounit's
   [TEST_MODULE name = struct .. end], since OCaml 4.02.0 does not allow
   extensions appear after [module]:

     [module %TEST X = struct .. end]

   [module X = struct .. end [%%TEST]] is possible... but 'TEST' goes
   much later...

   But we can write an equivalent with [let %TEST_UNIT] in ppx_test:

     [let %TEST_UNIT name = let module M = struct .. end in ()]

   Or ppx_test allows to have multiple tests inside [%%TEST ..].
   See test2.ml
 *)

let %TEST_UNIT module_name =
  let module M = struct 
    let () = assert (2 = 2)
  end in ()

(* CR jfuruse: BUG. Nested tests by modules. 
   The inside is not added immediately.
   It is only added when the outer test is executed, and ... ignored.
*)

let %TEST_UNIT module_name2 =
  let module M = struct 
    let %TEST in_module = 2 = 2
  end in ()

module M = struct
  let %TEST in_M = 1 = 1
end

(* test in functor *)

module F(A : sig end) = struct
  let %TEST in_functor = 1 = 1
end

module FA = F(struct end)
  
(* Tests are executed by calling [PTest.collect ()] *)      
let () = PTest.collect ()
