(* A simple sanity check against the OGR bindings *)

let p i = Printf.printf "%d\n%!" i

let parse_args () =
  try
    Sys.argv.(1)
  with
  | _ ->
    prerr_endline "You must provide a shapefile to load";
    exit 1

let () =
  let shapefile = parse_args () in
  Gdal.Lib.init_dynamic ();
  Gdal.Lib.register_all ();
  let result =
  Gdal.Data_source.with_source shapefile (
    fun ds ->
      p 1;
      let layer = Gdal.Data_source.get_layer ds 0 in
      p 2;
      Gdal.Layer.reset_reading layer;
      p 3;
      Gdal.Layer.iter_features layer (
        fun feature ->
          p 4;
          let feature_defn = Gdal.Layer.get_layer_defn layer in
          p 5;
          let count = Gdal.Feature.Defn.get_field_count feature_defn in
          if count > 0 then begin
            p 6;
            let field_defn = Gdal.Feature.Defn.get_field_defn feature_defn 0 in
            p 7;
            let _field_type = Gdal.Field.Defn.get_type field_defn in
            p 8;
            for i = 0 to count - 1 do
              print_endline @@ Gdal.Feature.get_as_string feature i;
            done;
            p 9;
          end;
      );
  )
  in
  match result with
  | `Ok () -> print_endline "Ok"
  | `Invalid_source -> print_endline "Invalid source"
