open Core
open Async_smtp
module Regex = Re2

let readme =
  "The query language is the Blang language:\n\n\
   B ::=\n\
  \  | envelope_sender REGEX\n\
  \  | envelope_recipient REGEX\n\
  \  | recipient REGEX\n\
  \  | subject REGEX\n\
  \  | rfc822_id REGEX\n\
  \  | flows FLOWS\n\n\
   E ::=\n\
  \  | True\n\
  \  | False\n\
  \  | (and E E ...)\n\
  \  | (or E E ...)\n\
  \  | (not E)\n\
  \  | (if E E E)\n\
  \  | (B)\n\n\
   Examples:\n\n\
   (and (recipient foo) (envelope_sender bar))\n\
   (subject \"Awesome email subject\")\n"
;;

module Mail_fingerprint = Smtp_mail_log.Mail_fingerprint

module Base = struct
  type t =
    [ `envelope_sender of Regex.Stable.V1_no_options.t
    | `envelope_recipient of Regex.Stable.V1_no_options.t
    | `recipient of Regex.Stable.V1_no_options.t
    | `subject of Regex.Stable.V1_no_options.t
    | `rfc822_id of Regex.Stable.V1_no_options.t
    | `flows of Smtp_mail_log.Stable.Flows.V1.t
    ]
  [@@deriving sexp]

  let regex = function
    | `envelope_sender regex
    | `envelope_recipient regex
    | `recipient regex
    | `subject regex
    | `rfc822_id regex -> regex
    | `flows (flows : Smtp_mail_log.Flows.t) ->
      List.map (flows :> string list) ~f:Regex.escape
      |> List.map ~f:(sprintf "(%s)")
      |> String.concat ~sep:"|"
      |> Regex.of_string
  ;;

  let matches_message_header msg header regex =
    Option.value_map (Smtp_mail_log.Message.email msg) ~default:false ~f:(fun msg ->
      List.exists (Mail_fingerprint.headers msg) ~f:(fun (name, value) ->
        String.Caseless.equal header name && Regex.matches regex value))
  ;;

  let matches_message t msg =
    match t with
    | `envelope_sender regex ->
      Smtp_mail_log.Message.sender msg
      |> Option.map ~f:(function
        | `String str -> str
        | `Sender sender -> Smtp_envelope.Sender.to_string sender)
      |> Option.value_map ~default:false ~f:(Regex.matches regex)
    | `envelope_recipient regex | `recipient regex ->
      Smtp_mail_log.Message.recipients msg
      |> Option.value ~default:[]
      |> List.map ~f:(function
        | `String str -> str
        | `Email email -> Email_address.to_string email)
      |> List.exists ~f:(Regex.matches regex)
    | `subject regex -> matches_message_header msg "Subject" regex
    | `rfc822_id regex ->
      matches_message_header msg "Message-Id" regex
      || Smtp_mail_log.Message.rfc822_id msg
         |> Option.value_map ~default:false ~f:(Regex.matches regex)
    | `flows flows ->
      Smtp_mail_log.Flows.are_related flows (Smtp_mail_log.Message.flows msg)
  ;;
end

type t = Base.t Blang.t [@@deriving sexp]

let rec cnf = function
  | Blang.True -> [] (* The Empty conjunction is true *)
  | False -> [ [] ] (* The Empty disjunction is false *)
  | Base b -> [ [ `Base b ] ]
  | And (q1, q2) -> cnf q1 @ cnf q2
  | Or (q1, q2) ->
    List.map (List.cartesian_product (cnf q1) (cnf q2)) ~f:(fun (p, q) -> p @ q)
  | Not q ->
    (match q with
     | Base b -> [ [ `Not_base b ] ]
     | True -> cnf Blang.false_
     | False -> cnf Blang.true_
     | Not q -> cnf q
     | And (q1, q2) -> cnf (Blang.or_ [ Blang.not_ q1; Blang.not_ q2 ])
     | Or (q1, q2) -> cnf (Blang.and_ [ Blang.not_ q1; Blang.not_ q2 ])
     | If (c, q1, q2) -> cnf (Blang.if_ c (Blang.not_ q1) (Blang.not_ q2)))
  | If (c, qt, qf) ->
    (* Using the disjunctive representation of [If]. Its more intuitive, and its cnf
       rewrite contains additional non-negated disjuncts that make for a better
       [permissive_cnf] *)
    cnf (Blang.or_ [ Blang.and_ [ c; qt ]; Blang.and_ [ Blang.not_ c; qf ] ])
;;

let permissive_cnf t =
  let reduce_disjunction =
    let rec loop acc = function
      | `Base t :: ts -> loop (t :: acc) ts
      | `Not_base _ :: _ -> None
      | [] -> Some (List.rev acc)
    in
    loop []
  in
  let t = List.filter_map (cnf t) ~f:reduce_disjunction in
  (* Short circuit the case that there is a [False] in the conjunction *)
  if List.mem t [] ~equal:Poly.equal then [ [] ] else t
;;

let arg = Command.Arg_type.create (fun s -> t_of_sexp (Sexp.of_string s))

let matches_message t session =
  Blang.eval t (fun base -> Base.matches_message base session)
;;
