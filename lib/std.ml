module Smtp_server = struct
  include Server
  module Config = Server_config
end
module Smtp_client = struct
  include Client
  module Config = Client_config
  module Raw    = Client_raw
  module Simple = Client_simple
end
module Smtp_spool                  = Spool
module Smtp_rpc_intf               = Rpc_intf
module Email                       = Email_message.Email
module Email_address               = Types.Email_address
module Smtp_envelope               = Types.Envelope
module Smtp_envelope_with_next_hop = Types.Envelope_with_next_hop
module Smtp_session                = Types.Session
module Smtp_reply                  = Types.Reply
module Smtp_sender                 = Types.Sender
module Smtp_command                = Types.Command
module Simplemail                  = Smtp_client.Simple
