let () =
  Gdal.Lib.init_dynamic ();
  Gdal.Lib.register_all ();
  let src = Gdal.Data_set.of_source_exn "in.tif" in
  let dst = Gdal.Data_set.of_source_exn ~write:true "out.tif" in
  let bands = [1, 1] in
  let transformer = Gdal.Transform.make_gen_img @@ `data_set (src, dst) in
  let options = Gdal.Warp.Options.make ~src ~dst ~bands ~transformer () in
  Gdal.Warp.Operation.warp options
    ~offset:(0, 0) ~size:Gdal.Data_set.(get_x_size dst, get_y_size dst);
  Gdal.Data_set.close dst;
  Gdal.Data_set.close src;
  ()
