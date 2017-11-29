include Email_message

module Smtp_socket_address = Socket_address
module Smtp_extension      = Smtp_extension

module Smtp_envelope = struct
  include Envelope
  module Sender          = Sender
  module Sender_argument = Sender_argument
  module Container       = Envelope_container
  module Id              = Envelope_id
  module Info            = Envelope_info
  module Selector        = Envelope_selector
  module Retry_interval  = Retry_interval
  module Routed          = Envelope_routed
end

module Async_smtp_types_stable = struct
  module Smtp_socket_address = Socket_address.Stable

  module Smtp_envelope = struct
    include Envelope.Stable
    module Sender          = Sender.Stable
    module Sender_argument = Sender_argument.Stable
    module Id              = Envelope_id.Stable
    module Info            = Envelope_info.Stable
    module Retry_interval  = Retry_interval.Stable
  end
end
