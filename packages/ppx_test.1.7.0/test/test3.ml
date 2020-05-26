(* Tests in functor arguments.
*)

module PTest = Ppx_test.Test

module F(A : sig end) = struct
end

module M = F(struct
  let %TEST x = 1 = 1
end)

let () = PTest.collect ()
