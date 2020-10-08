let id x = x

module PV : sig
  @type ('a, 'b) pv = [ `A of 'a | `B of 'b ] with show,gmap
end = struct
  @type ('a, 'b) pv = [ `A of 'a | `B of 'b ] with show,gmap
end

let _ =
  let open PV in
  Printf.printf "Original PV: %s\nMapped PV: %s\n"
      (GT.show pv id                id (`A "1"))
      (GT.show pv (GT.show GT.int)  id @@
       GT.gmap pv  int_of_string    id (`A "1"))

module PVExt : sig
  @type ('a, 'b) pv_ext = [ ('a, 'b) PV.pv | `C of 'a] with show,gmap
end = struct
  @type ('a, 'b) pv_ext = [ ('a, 'b) PV.pv | `C of 'a] with show,gmap
end

let _ =
  let open PV in
  let open PVExt in
  Printf.printf "****************************\n%!";
  Printf.printf "Original pv: %s\n" @@
      GT.show pv id id (`A "1");
  Printf.printf "Mapped pv and showed as a pv_ext: %s\n" @@
  GT.show pv_ext (GT.show GT.int) id @@
  ((GT.gmap pv      int_of_string   id (`A "1")) :> (_,_) pv_ext);
  Printf.printf "Original pv_ext: %s\n" @@
    GT.show pv_ext id id (`C "1");
  Printf.printf "Mapped PV_ext and showed as a pv_ext: %s\n" @@
  GT.show pv_ext (GT.show GT.int) id @@
  GT.gmap pv_ext int_of_string    id (`C "1");

module PVExt2 : sig
  @type ('a, 'b) pv_ext2 = [ ('a, 'b) PVExt.pv_ext | `D of 'a] with show,gmap
end = struct
  @type ('a, 'b) pv_ext2 = [ ('a, 'b) PVExt.pv_ext | `D of 'a] with show,gmap
end

let () =
  let open PVExt in
  let open PVExt2 in

  Printf.printf "****************************\n%!";
  Printf.printf "Original pv_ext: %s\n" @@
      GT.show pv_ext2 id id (`C "1");
  Printf.printf "Mapped pv_ext and showed as a pv_ext2: %s\n" @@
  GT.show pv_ext2 (GT.show GT.int) id @@
  ((GT.gmap pv_ext  int_of_string    id (`C "1")) :> (_,_) pv_ext2);
  Printf.printf "Original pv_ext2: %s\n" @@
    GT.show pv_ext2 id (GT.show GT.int)  (`D "1");
  Printf.printf "Mapped PV_ext2 and showed as a pv_ext2: %s\n" @@
  GT.show pv_ext2 (GT.show GT.int) id @@
  GT.gmap pv_ext2 int_of_string id (`D "1")

module PVExt3 : sig
  @type ('a, 'b, 'c) pv_ext3 = [ ('a, 'b) PVExt2.pv_ext2 | `E of 'c] with show,gmap
end = struct
  @type ('a, 'b, 'c) pv_ext3 = [ ('a, 'b) PVExt2.pv_ext2 | `E of 'c] with show,gmap
end

let () =
  let open PVExt2 in
  let open PVExt3 in

  (* let (_:int) = GT.show pv_ext3 in *)
  Printf.printf "****************************\n%!";
  Printf.printf "Original pv_ext2: %s\n" @@
  GT.show pv_ext2 id id  (`D "1");
  Printf.printf "Mapped pv_ext2 and showed as a pv_ext3: %s\n" @@
  GT.show pv_ext3 (GT.show GT.int) id id @@
  ((GT.gmap pv_ext2 (int_of_string) id (`D "1")) :> (_,_,_) pv_ext3);
  Printf.printf "Original pv_ext3: %s\n" @@
  GT.show pv_ext3 id id id  (`E "1");
  Printf.printf "Mapped PV_ext3 and showed as a pv_ext3: %s\n" @@
    GT.show pv_ext3 id id  (GT.show GT.float) @@
    GT.gmap pv_ext3 id id  float_of_string (`E "1.0");

module PVSum = struct
  @type ('a,'b) s = [ ('a,'b) PV.pv | ('a,'b) PVExt.pv_ext ] with show,gmap
end
module XXX = struct
  @type 'a xxx = [ `XXX of 'a ] with show,gmap
end
module YYY = struct
  @type 'a yyy = [ 'a XXX.xxx | ('a,'a) PV.pv ] with show,gmap
end
