open! Core

type t = private
  { code :
      [ `Service_ready_220
      | `Closing_connection_221
      | `Authentication_successful_235
      | `Ok_completed_250
      | `Start_authentication_input_334
      | `Start_mail_input_354
      | `Service_unavailable_421
      | `Local_error_451
      | `Message_rate_exceeded_452
      | `Tls_temporarily_unavailable_454
      | `Unable_to_accommodate_455
      | `Command_not_recognized_500
      | `Syntax_error_501
      | `Command_not_implemented_502
      | `Bad_sequence_of_commands_503
      | `Parameter_not_implemented_504
      | `Authentication_required_530
      | `Authentication_credentials_invalid_535
      | `Mailbox_unavailable_550
      | `Exceeded_storage_allocation_552
      | `Transaction_failed_554
      | `From_to_parameters_bad_555
      | `Other of int
      ]
  ; raw_message : string list
  }
[@@deriving bin_io, sexp]

val code : t -> int
val service_ready_220 : string -> t
val closing_connection_221 : t
val authentication_successful_235 : t
val ok_completed_250 : string -> t
val start_authentication_input_334 : string -> t
val start_mail_input_354 : t
val service_unavailable_421 : t
val data_timeout_421 : t
val command_timeout_421 : t
val local_error_451 : string -> t
val insufficent_system_storage_452 : t
val message_rate_exceeded_452 : t
val unable_to_accommodate_455 : string -> t
val command_not_recognized_500 : string -> t
val syntax_error_501 : string -> t
val command_not_implemented_502 : Smtp_command.t -> t
val bad_sequence_of_commands_503 : Smtp_command.t -> t
val authentication_required_530 : t
val authentication_credentials_invalid_535 : t
val mailbox_unavailable_550 : string -> t
val exceeded_storage_allocation_552 : t
val transaction_failed_554 : string -> t
val from_to_parameters_bad_555 : string -> t
val is_ok : t -> bool
val is_permanent_error : t -> bool
val decorate : t -> additional_lines:string list -> t

(* No roundtrip. *)

val to_string : t -> string
val of_string : string -> t
val of_bigstring : Bigstring.t -> t

type partial

val parse : ?partial:partial -> string -> [ `Done of t | `Partial of partial ]
