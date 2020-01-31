
(* parmap-like wrapper using parany *)

let parmap ~ncores ?(csize = 1) f l =
  if ncores <= 1 then BatList.map f l
  else
    let input = ref l in
    let demux () = match !input with
      | [] -> raise Parany.End_of_input
      | x :: xs -> (input := xs; x) in
    let output = ref [] in
    let mux x =
      output := x :: !output in
    (* for safety *)
    Parany.set_copy_on_work ();
    Parany.set_copy_on_mux ();
    (* parallel work *)
    Parany.run ~verbose:false ~csize ~nprocs:ncores ~demux ~work:f ~mux;
    !output

let pariter ~ncores ?(csize = 1) f l =
  if ncores <= 1 then BatList.iter f l
  else
    let input = ref l in
    let demux () = match !input with
      | [] -> raise Parany.End_of_input
      | x :: xs -> (input := xs; x) in
    (* for safety *)
    Parany.set_copy_on_work ();
    (* parallel work *)
    Parany.run ~verbose:false ~csize ~nprocs:ncores ~demux ~work:f ~mux:ignore

let parfold ~ncores ?(csize = 1) f g init l =
  if ncores <= 1 then BatList.fold_left g init (BatList.map f l)
  else
    let input = ref l in
    let demux () = match !input with
      | [] -> raise Parany.End_of_input
      | x :: xs -> (input := xs; x) in
    let output = ref init in
    let mux x =
      output := g !output x in
    (* for safety *)
    Parany.set_copy_on_work ();
    Parany.set_copy_on_mux ();
    (* parallel work *)
    Parany.run ~verbose:false ~csize ~nprocs:ncores ~demux ~work:f ~mux;
    !output
