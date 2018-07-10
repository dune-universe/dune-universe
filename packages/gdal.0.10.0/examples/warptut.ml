let init () =
  Gdal.Lib.init_dynamic ();
  Gdal.Lib.register_all ();
  ()

let usage_and_exit () =
  prerr_endline "Usage: warptut [src] [dst]";
  prerr_endline "[src] must exist";
  prerr_endline "[dst] must not exist";
  exit 1

let parse_args () =
  try
    let s = Sys.argv.(1) in
    let d = Sys.argv.(2) in
    if Sys.file_exists s && not (Sys.file_exists d) then
      s, d
    else
      raise Exit
  with
  | _ ->
    usage_and_exit ()

let () =
  (* Initialize the GDAL library *)
  init ();

  let src_file, dst_file = parse_args () in

  (* Open the source file *)
  let src_ds = Gdal.Data_set.of_source_exn src_file in

  (* We want to convert all of the bands in the input file *)
  let dst_bands = Gdal.Data_set.get_count src_ds in

  (* Use 32 bit floating point values in the output bands *)
  let dst_dt = Gdal.Band.Data.Float32 in

  (* Get output driver (GeoTIFF) *)
  let driver = Gdal.Driver.get_by_name_exn "GTiff" in

  (* Output coordinate system will be WGS84 *)
  let dst_sr = Gdal.Spatial_reference.make `name "WGS84" in
  let dst_wkt = Gdal.Spatial_reference.to_wkt dst_sr in

  (* Create a transformer that maps from source pixel/line coordinates to
     destination georeferenced coordinates (not destination pixel/line).  We do
     this by omitting the destination dataset handle. *)
  let transform =
    Gdal.Transform.make_gen_img @@ `data_set_wkt (src_ds, dst_wkt)
  in

  (* Get GDAL's best estimate for reasonable output resolution bounds *)
  let warp_suggestions = Gdal.Warp.suggested_warp_output src_ds transform in

  (* Set the transformer's geo transform matrix to what was suggested by GDAL *)
  Gdal.Transform.set_dst_geo_transform transform
    warp_suggestions.Gdal.Warp.geo_transform;

  (* Create the output file *)
  let dst_ds =
    Gdal.Data_set.create_exn ~bands:(dst_bands, dst_dt) driver dst_file
      warp_suggestions.Gdal.Warp.dims
  in

  (* Set the destination projection and pixel transform information *)
  Gdal.Data_set.set_projection dst_ds dst_wkt;
  Gdal.Geo_transform.set dst_ds warp_suggestions.Gdal.Warp.geo_transform;

  (* Set the desired warp options *)

  (* Warp all bands *)
  let bands = Array.init dst_bands (fun i -> i + 1, i + 1) |> Array.to_list in

  let warp_options =
    Gdal.Warp.Options.make
      ~resample_alg:Gdal.Warp.Nearest_neighbor
      ~src:src_ds
      ~dst:dst_ds
      ~bands
      ~transformer:transform
      ()
  in

  (* Warp it! *)
  Gdal.Warp.Operation.warp warp_options;

  (* Close the input and output sets *)
  Gdal.Data_set.close src_ds;
  Gdal.Data_set.close dst_ds;
  ()
