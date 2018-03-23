module Delimited : sig
  module Row = Csv.Row

  include Deprecated_intf.Deprecated
    with module Row := Row
end
