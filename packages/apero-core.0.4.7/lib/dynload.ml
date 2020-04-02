let ref:string array option ref = ref None

let loadfile plugin args = 
  ref := Some args;
  try
    Dynlink.loadfile @@ Dynlink.adapt_filename plugin;
  with
   | Dynlink.Error s -> failwith (Printf.sprintf "Error loading plugin %s: %s" plugin (Dynlink.error_message s))

let loadfile_private plugin args = 
  ref := Some args;
  try
    Dynlink.loadfile_private @@ Dynlink.adapt_filename plugin;
  with
   | Dynlink.Error s -> failwith (Printf.sprintf "Error loading plugin %s: %s" plugin (Dynlink.error_message s))
  