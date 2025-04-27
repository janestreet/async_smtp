module Smtp_server = Server

module Smtp_client = struct
  include Client
  module Config = Client_config
  module Raw = Client_raw
  module Simple = Simplemail
  module Credentials = Credentials
end

module Address_and_route = Address_and_route
module Email_address = Email_message.Email_address
module Email = Email_message.Email
module Email_headers = Email_message.Email_headers
module Multispool = Multispool
module Simplemail = Simplemail
module Smtp_auth = Auth
module Smtp_command = Smtp_command
module Smtp_envelope = Async_smtp_types.Smtp_envelope
module Smtp_events = Smtp_events
module Smtp_expect_test_helper = Test_helper
module Smtp_extension = Async_smtp_types.Smtp_extension
module Smtp_mail_log = Mail_log
module Smtp_mail_log_tags = Mail_log_tags
module Smtp_monad = Smtp_monad
module Smtp_quarantine_reason = Quarantine_reason
module Smtp_reply = Smtp_reply
module Smtp_rpc_impl = Rpc_impl
module Smtp_rpc_intf = Rpc_intf
module Smtp_session = Session
module Smtp_spool_monitor = Message_spool.On_disk_monitor
module Smtp_spool_queue = Message.Queue
module Smtp_spool = Spool
module Smtp_spool_message = Message

module Private = struct
  module Client_cache = Client_cache
  module Message_spool = Message_spool
  module Message = Message
end
