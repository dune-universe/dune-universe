type version = {
    major : int;
    minor : int;
    subminor : int;
  }

val llvm_config : string

val version_string : string

val version : version

val includedir : string
