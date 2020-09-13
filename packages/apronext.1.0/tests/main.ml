let iof = int_of_float
let foi = float_of_int

let env = Environmentext.make_s [||] [|"x";"y";"z"|]

let draw (a,b) (c,d) =
  let a = int_of_float a and b = int_of_float b
  and c = int_of_float c and d = int_of_float d in
  Graphics.draw_segments [|a,b,c,d|]

let test_pol env pts =
  let gens = List.map (Generatorext.of_float_point env) pts in
  let pol = Apol.of_generator_list env gens in
  let (l,h) = Apol.bound_variable_fs pol "z" in
  for i = iof l to iof h do
    let _cut = Apol.assign_fs pol "z" (foi i) in
    Graphics.clear_graph ();
    (* Apol.draw2d draw (a,b) cut; *)
    Unix.sleepf 0.01
  done

let test_box env pts =
  let gens = List.map (Generatorext.of_float_point env) pts in
  let box = Abox.of_generator_list env gens in
  let (l,h) = Abox.bound_variable_fs box "z" in
  for i = iof l to iof h do
    let _cut = Abox.assign_fs box "z" (foi i) in
    Graphics.clear_graph ();
    (* Abox.draw2d draw (a,b) cut; *)
    Unix.sleepf 0.01
  done

let test_oct env pts =
  let gens = List.map (Generatorext.of_float_point env) pts in
  let oct = Aoct.of_generator_list env gens in
  let (l,h) = Aoct.bound_variable_fs oct "z" in
  for i = iof l to iof h do
    let _cut = Aoct.assign_fs oct "z" (foi i) in
    Graphics.clear_graph ();
    (* Aoct.draw2d draw (a,b) cut; *)
    Unix.sleepf 0.01
  done

let _ =
  Graphics.open_graph " 800x800";
  Graphics.set_window_title "Apron domains";
  Graphics.loop_at_exit [] (fun _ -> ());
  Random.self_init();
  let _dim = 3 in
  (* let nb_point = 12 in *)
  (* let rand_point () = *)
  (*   iter (fun acc -> ((Random.float 400.) +. 200.)::acc ) [] dim *)
  (* in *)
  (* let pts = iter (fun acc -> (rand_point ())::acc) [] nb_point in *)
  let pts = [[200.;200.;0.];
             [400.;200.;0.];
             [300.;400.;0.];
             [300.;300.;200.]]
  in
  test_oct env pts
           (* ; *)
  (* List.iter (fun l -> *)
  (*     let (a,b) = two_first l in *)
  (*     let a = int_of_float a and b = int_of_float b in *)
  (*     Graphics.fill_circle a b 4 *)
  (*   ) pts *)
