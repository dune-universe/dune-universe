module Bigstring_shared = Bigstring_shared
module Email = struct
  include Email
  module Content     = Email_content
  module Raw_content = Email_raw_content
  module Simple      = Email_simple
end

module Email_address = Email_address1
module Email_headers = Headers
module Email_selector = Selector
module Email_wrapper = Wrapper
module Mimestring = Mimestring
module Octet_stream = Octet_stream
module String_monoid = String_monoid

module Email_message_stable = struct
  module Email         = struct
    include Email.Stable
    module Raw_content = Email_raw_content.Stable
    module Simple      = Email_simple.Stable
  end
  module Email_address = Email_address.Stable
  module Email_wrapper = Email_wrapper.Stable
end

module Private = struct
  module Boundary = Boundary
  module Media_type = Media_type
  module Rfc = Rfc
end
