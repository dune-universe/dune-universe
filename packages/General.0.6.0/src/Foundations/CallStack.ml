module OCSPE = OCamlStandard.Printexc

type t = OCSPE.raw_backtrace

let current ?(max_size=Int.greatest) () =
  OCSPE.get_callstack max_size

let to_string = OCSPE.raw_backtrace_to_string
let repr = to_string

module Location = struct
  type t = OCSPE.location = {
    filename: string;
    line_number: int;
    start_char: int;
    end_char: int;
  }

  let repr {filename; line_number; start_char; end_char} =
    Format.apply "{filename=%S; line_number=%n; start_char=%n; end_char=%n}" filename line_number start_char end_char

  module O = struct
    include Equate.Poly.O
    include Compare.Poly.O
  end

  include (Equate.Poly: module type of Equate.Poly with module O := O)
  include (Compare.Poly: module type of Compare.Poly with module O := O)
end

module Frame = struct
  type t = OCSPE.backtrace_slot

  let is_raise = OCSPE.Slot.is_raise

  let location = OCSPE.Slot.location

  let format = OCSPE.Slot.format
end

let frames bt =
  match OCSPE.backtrace_slots bt with
    | None -> [] (*BISECT-IGNORE*) (* Would require compiling without tag debug *)
    | Some frames -> List.of_array frames
