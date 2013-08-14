open Core.Std
open Async_smtp.Smtp

(* Test parsing of commands to server *)
TEST_MODULE = struct
  let check str comm =
    let com = Commands.of_string_opt str in
    match com with
    | None -> false
    | Some c -> Polymorphic_compare.equal c comm

  TEST_UNIT = assert(check "HELO hi" (Commands.Hello "hi"))
  TEST_UNIT = assert(check "MAIL FROM: hi" (Commands.Sender "hi"))
  TEST_UNIT = assert(check "RCPT TO: hi" (Commands.Receiver "hi"))
  TEST_UNIT = assert(check "DATA" Commands.Data)
  TEST_UNIT = assert(check "QUIT" Commands.Quit)
  TEST_UNIT = assert(check "HELP" Commands.Help)
  TEST_UNIT = assert(check "NOOP" Commands.Noop)
end

(* Test to_string and of_string_opt functions for symmetry *)
TEST_MODULE = struct
  let check comm =
    match Commands.of_string_opt (Commands.to_string comm) with
    | None -> false
    | Some c -> Polymorphic_compare.equal comm c

  TEST_UNIT = assert(check (Commands.Hello "Hello World!~"))
  TEST_UNIT = assert(check (Commands.Sender "Hello World!~"))
  TEST_UNIT = assert(check (Commands.Receiver "Hello World!~"))
  TEST_UNIT = assert(check Commands.Data)
  TEST_UNIT = assert(check Commands.Quit)
  TEST_UNIT = assert(check Commands.Help)
  TEST_UNIT = assert(check Commands.Noop)
end

TEST_MODULE = struct
  let check reply =
    let (r,_) = Replies.of_string (Replies.to_string (reply,"")) in
    Polymorphic_compare.equal reply r

  TEST_UNIT = assert(check Replies.(Ok System_status))
  TEST_UNIT = assert(check Replies.(Ok Help))
  TEST_UNIT = assert(check Replies.(Ok Service_ready))
  TEST_UNIT = assert(check Replies.(Ok Closing_connection))
  TEST_UNIT = assert(check Replies.(Ok Ok_completed))
  TEST_UNIT = assert(check Replies.(Ok Will_forward))
  TEST_UNIT = assert(check Replies.(Ok Will_attempt))
  TEST_UNIT = assert(check Replies.(Ok Start_mail_input))
  TEST_UNIT = assert(check Replies.(Bad Not_available))
  TEST_UNIT = assert(check Replies.(Bad Mailbox_unavailable_400))
  TEST_UNIT = assert(check Replies.(Bad Local_error))
  TEST_UNIT = assert(check Replies.(Bad Insufficient_storage))
  TEST_UNIT = assert(check Replies.(Bad Unable_to_accommodate))
  TEST_UNIT = assert(check Replies.(Really_bad Command_not_recognized))
  TEST_UNIT = assert(check Replies.(Really_bad Syntax_error))
  TEST_UNIT = assert(check Replies.(Really_bad Command_not_implemented))
  TEST_UNIT = assert(check Replies.(Really_bad Bad_sequence_of_commands))
  TEST_UNIT = assert(check Replies.(Really_bad Parameter_not_implemented))
  TEST_UNIT = assert(check Replies.(Really_bad Mailbox_unavailable_500))
  TEST_UNIT = assert(check Replies.(Really_bad User_not_local))
  TEST_UNIT = assert(check Replies.(Really_bad Exceeded_storage_allocation))
  TEST_UNIT = assert(check Replies.(Really_bad Mailbox_name_not_allowed))
  TEST_UNIT = assert(check Replies.(Really_bad Trasaction_failed))
  TEST_UNIT = assert(check Replies.(Really_bad From_to_parameters_bad))


end
