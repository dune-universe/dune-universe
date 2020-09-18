open Apronext
open Geometry

type t = {
    (* content *)
    elems3     :  Drawable.t;
    (* projection variables *)
    abciss3    : string;
    ordinate3  : string;
    height3    : string;
    (* elems projected on the projection variables.*)
    bounded3   : vertex3d list;
  }

let create ~abciss ~ordinate ~height () = {
    elems3 = [];
    abciss3=abciss;
    ordinate3=ordinate;
    height3=height;
    bounded3 = [];
  }

let add r (d: Drawable.t) =
  let x,y,z= r.abciss3, r.ordinate3, r.height3 in
  let bounded3 =
    List.fold_left (fun acc pol ->
        let p3d = Apol.proj3D_s pol x y z in
        if Apol.is_bounded p3d then
          let pts =
            Apol.to_generator_list p3d
            |> List.rev_map (fun g -> Generatorext.to_vertices3D_s g x y z)
          in
          pts::acc
        else acc
      ) r.bounded3 d
  in
  {r with elems3 = Drawable.union d r.elems3; bounded3}
