open! Core


module With_debug = struct
  let __UNUSED_VALUE__run_debug f = f ();;
end

module Without_debug = struct
  let run_debug _ = ();;
end

include Without_debug;;
