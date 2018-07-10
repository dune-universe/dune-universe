let () =
  let src_file = Sys.argv.(1) in
  let base = float_of_string Sys.argv.(2) in
  let step = float_of_string Sys.argv.(3) in
  Gdal.Lib.init_dynamic ();
  Gdal.Lib.register_all ();
  let mem_ogr = Gdal.Data_source.get_driver_by_name_exn "memory" in
  let ds = Gdal.Data_source.create_exn mem_ogr "contour_ds" in
  let spatial_reference = Gdal.Spatial_reference.make `name "EPSG:4326" in
  let layer =
    Gdal.Data_source.create_layer_exn
      ~spatial_reference ~geometry_type:Gdal.Geometry.LineString ds "the_contours"
  in
  let src = Gdal.Data_set.of_source_exn src_file in
  let band = Gdal.Data_set.get_band src 1 Gdal.Band.Data.Int16 in
  Gdal.Alg.generate_contours band layer @@ `interval (base, step);
  exit 0
