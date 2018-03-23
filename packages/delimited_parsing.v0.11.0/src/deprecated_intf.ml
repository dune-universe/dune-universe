module type Deprecated = sig

  module Header = Header

  module Row : Row_intf.Row

  include Character_separated_without_quoting_intf.Character_separated_without_quoting
    with module Row := Row

  module Csv : Deprecated_csv_intf.Deprecated_csv
    with module Row := Row

  module Positional: Positional_intf.Positional
    with module Row := Row

end
