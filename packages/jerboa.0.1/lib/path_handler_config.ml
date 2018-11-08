let filter_path_handler_config_by_meth path_handler_config meth =
  Base.List.filter path_handler_config ~f:(fun (path_handler: Path_handler.t) ->
      path_handler.meth = meth
    )

let find_path_handler_with_regex path_handler_config path =
  Base.List.find path_handler_config ~f:(fun (path_handler: Path_handler.t) ->
      let path_mapping = path_handler.path_mapping in 
      let composed_path_regex = Path_mapping.compose_path_regex path_mapping in
      let compiled_path_regex = Re.compile composed_path_regex in
      Re.execp compiled_path_regex path
    )

let find_path_handler path_handler_config meth path =
  let filtered_handler_config = filter_path_handler_config_by_meth path_handler_config meth in
  find_path_handler_with_regex filtered_handler_config path