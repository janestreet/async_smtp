module Smtp_server = struct
  include Server
  module Config = Server_config
end
module Smtp_client = struct
  include Client
  module Config = Client_config
  module Raw    = Client_raw
  module Simple = Simplemail
end
module Address                     = Types.Address
module Smtp_credentials            = Types.Credentials
module Smtp_spool                  = Spool
module Smtp_rpc_intf               = Rpc_intf
module Smtp_events                 = Smtp_events
module Email                       = Email_message.Email
module Email_address               = Types.Email_address
module Email_headers               = Email_message.Headers
module Multispool                  = Multispool
module Smtp_envelope               = Types.Envelope
module Smtp_envelope_with_next_hop = Types.Envelope_with_next_hop
module Smtp_envelope_selector      = Envelope_selector
module Smtp_session                = Types.Session
module Smtp_reply                  = Types.Reply
module Smtp_sender                 = Types.Sender
module Smtp_sender_argument        = Types.Argument
module Smtp_command                = Types.Command
module Smtp_mail_log               = Mail_log
module Simplemail                  = Simplemail
