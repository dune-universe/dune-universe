module C = Configurator.V1

let () =
  C.main ~name:"dssi-header" (fun c ->
      let has_dssi =
        try
          let defined =
            C.C_define.import c ~includes:["dssi.h"]
              [("DSSI_VERSION", C.C_define.Type.String)]
          in
          match defined with
            | [("DSSI_VERSION", C.C_define.Value.String _)] -> true
            | _ -> false
        with _ -> false
      in
      C.C_define.gen_header_file c ~fname:"ocaml_dssi_config.h"
        [("HAS_DSSI", C.C_define.Value.Switch has_dssi)])
