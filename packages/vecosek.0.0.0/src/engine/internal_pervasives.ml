
include Nonstd
module String = Sosa.Native_string

module Time = struct
  let now () = Unix.gettimeofday ()
end

let dbg fmt = printf (">> Vecodbg: " ^^ fmt ^^ "\n%!")

let summarize s =
  String.sub s 0 40 |> Option.value ~default:s