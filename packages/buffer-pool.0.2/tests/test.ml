module BP_buffer = struct 
  include Cstruct

  let reset t = Cstruct.memset t 0
  let sub t = Cstruct.sub t
end 

module BP = Buffer_pool.Make(BP_buffer) 

(* Create two buffers editing one should not affect the other *)
let independence l = 
  let bp = BP.make () in
  let a = BP.alloc bp l in
  let b = BP.alloc bp l in
  Cstruct.memset a 5;
  Crowbar.check @@ not (a == b)

(* Size of a buffer that you get out is always >= what you request*)
let sizing l = 
  let bp = BP.make () in
  let a = BP.alloc bp l in
  Crowbar.check @@ (Cstruct.len a >= l)

(* When a buffer is released it is zero'd *)
let release l = 
  let bp = BP.make () in
  let buf = BP.alloc bp l in
  Cstruct.memset buf 5;
  BP.release bp buf;
  let buf' = BP.alloc bp l in
  Crowbar.check_eq 
    ~cmp:Cstruct.compare 
    (Cstruct.create (Cstruct.len buf)) buf'

let () =
  let open Crowbar in
  add_test ~name:"independence" [uint8] independence;
  add_test ~name:"sizing" [uint8] sizing;
  add_test ~name:"release" [uint8] release


