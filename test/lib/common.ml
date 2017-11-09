open Core

module Test_name_generator = struct
  module Unique_name = String

  type t = string

  let next (prefix : t) ~attempt =
    let attempt_str = sprintf "%06d" attempt in
    prefix ^ attempt_str
  ;;
end
