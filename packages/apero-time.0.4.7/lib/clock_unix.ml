open Clock
open Time_64bit

module Clock_unix : Clock with module Time = Time_64bit = struct

  module Time = Time_64bit

  let now () = Time.of_seconds @@ Unix.gettimeofday()

end
