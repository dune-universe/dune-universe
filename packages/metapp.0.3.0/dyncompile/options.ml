type t = {
    packages : string list;
    directories : string list;
    flags : string list;
    plainsource : bool;
    debug_findlib : bool;
    verbose : bool;
  }

let empty = {
  packages = [];
  directories = [];
  flags = [];
  plainsource = false;
  debug_findlib = false;
  verbose = false;
}

let rev options =
  { options with
    packages = List.rev options.packages;
    directories = List.rev options.directories;
    flags = List.rev options.flags }

let add_directories directories options =
  { options with
    directories = List.rev_append directories options.directories }

let add_packages packages options =
  { options with
    packages = List.rev_append packages options.packages }

let add_flags flags options =
  { options with
    flags = List.rev_append flags options.flags }

let set_plainsource plainsource options =
  { options with plainsource }

let set_debug_findlib debug_findlib options =
  { options with debug_findlib }

let set_verbose verbose options =
  { options with verbose }
