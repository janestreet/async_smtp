include Email_message
module Smtp_extension = Smtp_extension

module Smtp_envelope = struct
  include Envelope

  module Bodiless = struct
    include Envelope_bodiless
    module Routed = Envelope_bodiless_routed
  end

  module Sender = Sender
  module Sender_argument = Sender_argument
  module Container = Envelope_container
  module Id = Envelope_id
  module Info = Envelope_info
  module Selector = Envelope_selector
  module Retry_interval = Retry_interval

  module Routed = struct
    include Envelope_routed
    module Batch = Envelope_routed_batch
  end
end

module Async_smtp_types_stable = struct
  module Smtp_envelope = struct
    include Envelope.Stable
    module Sender = Sender.Stable
    module Sender_argument = Sender_argument.Stable
    module Id = Envelope_id.Stable
    module Info = Envelope_info.Stable
    module Retry_interval = Retry_interval.Stable
    module Selector = Envelope_selector.Stable
  end
end
