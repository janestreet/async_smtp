module Smtp_server = Server
module Smtp_client = struct
  include Client
  module Config = Client_config
  module Raw    = Client_raw
  module Simple = Simplemail
end
module Address                     = Address
module Smtp_credentials            = Credentials
module Smtp_spool                  = Spool
module Smtp_rpc_intf               = Rpc_intf
module Smtp_events                 = Smtp_events
module Email                       = Email_message.Std.Email
module Email_address               = Email_message.Email_address
module Email_headers               = Email_message.Headers
module Multispool                  = Multispool
module Retry_interval              = Retry_interval
module Smtp_envelope               = Envelope
module Smtp_envelope_with_next_hop = Envelope.With_next_hop
module Smtp_envelope_selector      = Envelope_selector
module Smtp_session                = Session
module Smtp_reply                  = Smtp_reply
module Smtp_sender                 = Sender
module Smtp_sender_argument        = Sender_argument
module Smtp_command                = Smtp_command
module Smtp_mail_log               = Mail_log
module Simplemail                  = Simplemail
