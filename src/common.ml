open Core
open! Async

let is_accessible_directory ?create_if_missing dir =
  Sys.file_exists dir
  >>= fun exists ->
  if exists = `No && Option.is_some create_if_missing
  then Unix.mkdir dir >>| Or_error.return
  else begin
    Sys.is_directory dir
    >>= function
    | `Unknown ->
      Deferred.Or_error.error_string
        (sprintf
           "could not determine whether or not %s was an accessible directory"
           dir)
    | `No ->
      Deferred.Or_error.error_string
        (sprintf "%s does not exist or is not a directory" dir)
    | `Yes -> begin
        Unix.access dir [ `Write; `Read; `Exec ]
        >>| function
        | Ok ()     -> Ok ()
        | Error exn ->
          Error.tag (Error.of_exn exn)
            ~tag:(sprintf "%s is not writable, readable, and executable by running user" dir)
          |> fun e ->
          Error e
      end
  end
;;

let safely_ls_dir dir =
  is_accessible_directory dir
  >>= function
  | Error e -> return (Error e)
  | Ok ()   ->
    Sys.ls_dir dir
    >>| fun contents ->
    Ok contents
;;

let unlink file =
  Deferred.Or_error.try_with (fun () -> Unix.unlink file)
;;

let rename ~src ~dst =
  Deferred.Or_error.try_with (fun () -> Unix.rename ~src ~dst)
;;

let urlbase64_encode_float ?(length=6) f =
  match Int64.of_float f with
  | exception _ -> invalid_arg "cannot encode a float that does not fit in an Int64"
  | n ->
    String.init 9 ~f:(fun i ->
      Int64.shift_right n (64 - 8*(i))
      |> Int64.bit_and 0xffL
      |> Int64.to_int_exn
      |> Char.of_int_exn)
    |> Base64.Websafe.encode
    |> String.sub ~pos:(12-length) ~len:length
;;

let%test_module "urlbase64_encode_float" =
  (module struct
    let%expect_test _ =
      printf "%s" (urlbase64_encode_float 1234.1235453123); [%expect_exact {|AAAATS|}]
    let%expect_test _ =
      printf "%s" (urlbase64_encode_float 1234.); [%expect_exact {|AAAATS|}]
    let%expect_test _ =
      printf "%s" (urlbase64_encode_float 1235.); [%expect_exact {|AAAATT|}]
    let%expect_test _ =
      printf "%s" (urlbase64_encode_float 123456.); [%expect_exact {|AAAeJA|}]
    let%expect_test _ =
      printf "%s" (urlbase64_encode_float Int64.(to_float (max_value - 1024L))); [%expect_exact {|____wA|}]
  end)
