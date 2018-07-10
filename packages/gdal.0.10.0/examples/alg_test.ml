let () =
  Gdal.Lib.init_dynamic ();
  Gdal.Lib.register_all ();
  ()

let make_raster () =
  let mem = Gdal.Driver.get_by_name_exn "MEM" in
  let epsg4326 = Gdal.Spatial_reference.make `name "EPSG:4326" in
  let wkt = Gdal.Spatial_reference.to_wkt epsg4326 in
  let gt =
    Gdal.Geo_transform.make
      ~origin:(-180.0, 90.0)
      ~pixel_size:(1.0, -1.0)
      ~rotation:(0.0, 0.0)
  in
  let raster =
    Gdal.Data_set.create_exn
      ~bands:(1, Gdal.Band.Data.Float32) mem "mem" (360, 721)
  in
  Gdal.Data_set.set_projection raster wkt;
  Gdal.Geo_transform.set raster gt;
  raster

let () =
  let file = Sys.argv.(1) in
  let input = Gdal.Data_source.of_source_exn file in
  let layer = Gdal.Data_source.get_layer input 0 in
  let geoms = Gdal.Layer.map_features layer Gdal.Feature.get_geometry_copy in

  let raster = make_raster () in

  (* Rasterize geometries pulled from a layer *)
  Gdal.Alg.rasterize_geometries raster [1] (List.map (fun g -> g, [1.0]) geoms);

  (* Rasterize the layer directly *)
  Gdal.Alg.rasterize_layers raster [1] [layer, [1.0]];

  exit 0
