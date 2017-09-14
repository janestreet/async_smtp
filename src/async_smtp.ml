module Smtp_server = Server
module Smtp_client = struct
  include Client
  module Config = Client_config
  module Raw    = Client_raw
  module Simple = Simplemail
end
module Address                     = Address
module Email_address               = Email_message.Email_address
module Email                       = Email_message.Email
module Email_headers               = Email_message.Email_headers
module Multispool                  = Multispool
module Retry_interval              = Retry_interval
module Simplemail                  = Simplemail
module Smtp_command                = Smtp_command
module Smtp_credentials            = Credentials
module Smtp_envelope               = Envelope
module Smtp_envelope_selector      = Envelope_selector
module Smtp_envelope_with_next_hop = Envelope.With_next_hop
module Smtp_events                 = Smtp_events
module Smtp_mail_log               = Mail_log
module Smtp_mail_log_tags          = Mail_log_tags
module Smtp_quarantine_reason      = Quarantine_reason
module Smtp_reply                  = Smtp_reply
module Smtp_rpc_intf               = Rpc_intf
module Smtp_sender_argument        = Sender_argument
module Smtp_sender                 = Sender
module Smtp_session                = Session
module Smtp_spool                  = Spool
module Smtp_expect_test_helper     = Test_helper

module Private = struct
  module Cache           = Cache
  module Client_cache    = Client_cache
  module Spooled_message = Message
end
