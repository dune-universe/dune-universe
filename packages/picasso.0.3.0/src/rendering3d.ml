open Apronext
open Geometry

type t =
  { (* projection variables *)
    abciss3: string
  ; ordinate3: string
  ; height3: string
  ; (* elems projected on the projection variables.*)
    bounded3: (Colors.t * vertex3d) list }

let create ~abciss ~ordinate ~height () =
  {abciss3= abciss; ordinate3= ordinate; height3= height; bounded3= []}

let add r ((c, d) : Colors.t * Drawable.t) =
  let x, y, z = (r.abciss3, r.ordinate3, r.height3) in
  let bounded3 =
    List.fold_left
      (fun acc pol ->
        let p3d = Apol.proj3D_s pol x y z in
        if Apol.is_bounded p3d then
          let pts =
            Apol.to_generator_list p3d
            |> List.rev_map (fun g -> Generatorext.to_vertices3D_s g x y z)
          in
          (c, pts) :: acc
        else acc )
      r.bounded3 d
  in
  {r with bounded3}

let add_l = List.fold_left add
