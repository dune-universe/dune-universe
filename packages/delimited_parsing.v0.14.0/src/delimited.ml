(** Read CSVs & CSV-like delimited formats (following the CSV quoting behaviour).

    These formats are loosely documented by RFC 4180: https://www.ietf.org/rfc/rfc4180.txt
*)
module Read = Read

(** Write CSVs & CSV-like delimited formats. *)
module Write = struct
  include Write

  (** Helper for converting from the old interface. Use this snippet in files
      to disable the expert interface:

      {[
        module Delimited = struct
          include Delimited
          module Write = Delimited.Write.Without_expert
        end
      ]}
  *)
  module Without_expert : module type of Write with module Expert := Write.Expert = Write
end

(** Parsers for non-CSV-like formats *)
module Non_csv = struct
  (** [Character_separated_without_quoting] parses fields separated by a character, where
      fields may contain escaped characters (e,g, [\n]) but fields may not be quoted (e.g.,
      ["foo bar"]). *)
  module Character_separated_without_quoting = Character_separated_without_quoting

  (** [Positional] parses fixed-width fields. *)
  module Positional = Positional
end


(** {1} Modules shared between multiple parsers. *)
module Shared = Shared
