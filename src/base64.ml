open Core.Std


let encode_int i =
  match i with
  | 0 -> 'A'
  | 1 -> 'B'
  | 2 -> 'C'
  | 3 -> 'D'
  | 4 -> 'E'
  | 5 -> 'F'
  | 6 -> 'G'
  | 7 -> 'H'
  | 8 -> 'I'
  | 9 -> 'J'
  | 10 -> 'K'
  | 11 -> 'L'
  | 12 -> 'M'
  | 13 -> 'N'
  | 14 -> 'O'
  | 15 -> 'P'
  | 16 -> 'Q'
  | 17 -> 'R'
  | 18 -> 'S'
  | 19 -> 'T'
  | 20 -> 'U'
  | 21 -> 'V'
  | 22 -> 'W'
  | 23 -> 'X'
  | 24 -> 'Y'
  | 25 -> 'Z'
  | 26 -> 'a'
  | 27 -> 'b'
  | 28 -> 'c'
  | 29 -> 'd'
  | 30 -> 'e'
  | 31 -> 'f'
  | 32 -> 'g'
  | 33 -> 'h'
  | 34 -> 'i'
  | 35 -> 'j'
  | 36 -> 'k'
  | 37 -> 'l'
  | 38 -> 'm'
  | 39 -> 'n'
  | 40 -> 'o'
  | 41 -> 'p'
  | 42 -> 'q'
  | 43 -> 'r'
  | 44 -> 's'
  | 45 -> 't'
  | 46 -> 'u'
  | 47 -> 'v'
  | 48 -> 'w'
  | 49 -> 'x'
  | 50 -> 'y'
  | 51 -> 'z'
  | 52 -> '0'
  | 53 -> '1'
  | 54 -> '2'
  | 55 -> '3'
  | 56 -> '4'
  | 57 -> '5'
  | 58 -> '6'
  | 59 -> '7'
  | 60 -> '8'
  | 61 -> '9'
  | 62 -> '-'
  | 63 -> '_'
  | bad_val -> failwithf "bug in Base64.encode - %d cannot be encoded" bad_val ()
;;

let encode_int_exn = encode_int

let decode_char c =
  match c with
  | 'A' -> 0
  | 'B' -> 1
  | 'C' -> 2
  | 'D' -> 3
  | 'E' -> 4
  | 'F' -> 5
  | 'G' -> 6
  | 'H' -> 7
  | 'I' -> 8
  | 'J' -> 9
  | 'K' -> 10
  | 'L' -> 11
  | 'M' -> 12
  | 'N' -> 13
  | 'O' -> 14
  | 'P' -> 15
  | 'Q' -> 16
  | 'R' -> 17
  | 'S' -> 18
  | 'T' -> 19
  | 'U' -> 20
  | 'V' -> 21
  | 'W' -> 22
  | 'X' -> 23
  | 'Y' -> 24
  | 'Z' -> 25
  | 'a' -> 26
  | 'b' -> 27
  | 'c' -> 28
  | 'd' -> 29
  | 'e' -> 30
  | 'f' -> 31
  | 'g' -> 32
  | 'h' -> 33
  | 'i' -> 34
  | 'j' -> 35
  | 'k' -> 36
  | 'l' -> 37
  | 'm' -> 38
  | 'n' -> 39
  | 'o' -> 40
  | 'p' -> 41
  | 'q' -> 42
  | 'r' -> 43
  | 's' -> 44
  | 't' -> 45
  | 'u' -> 46
  | 'v' -> 47
  | 'w' -> 48
  | 'x' -> 49
  | 'y' -> 50
  | 'z' -> 51
  | '0' -> 52
  | '1' -> 53
  | '2' -> 54
  | '3' -> 55
  | '4' -> 56
  | '5' -> 57
  | '6' -> 58
  | '7' -> 59
  | '8' -> 60
  | '9' -> 61
  | '-' -> 62
  | '_' -> 63
  | '=' -> 0
  | bad_val -> failwithf "%c is not a valid char in Base64 encoding" bad_val ()
;;

let encode_triplet a b c =
  let a = int_of_char a in
  let b = int_of_char b in
  let c = int_of_char c in
  let r = String.create 4 in
  r.[0] <- encode_int (a lsr 2);
  r.[1] <- encode_int (((a land 0b00000011) lsl 4) lxor (b lsr 4));
  r.[2] <- encode_int (((b land 0b00001111) lsl 2) lxor (c lsr 6));
  r.[3] <- encode_int (c land 0b00111111);
  r
;;

let encode str =
  let len = String.length str in
  let rec enc acc pos =
    if len - pos > 3 then
      begin
        let r = encode_triplet str.[pos] str.[pos+1] str.[pos+2] in
        enc (r :: acc) (pos + 3)
      end
    else
      begin
        let remainder = String.sub str ~pos ~len:(len - pos) in
        let buf = String.make 3 '\000' in
        let len = String.length remainder in
        String.blit ~src:remainder ~src_pos:0 ~dst:buf ~dst_pos:0 ~len;
        let r = encode_triplet buf.[0] buf.[1] buf.[2] in
        begin
          match len with
          | 0 -> ()
          | 1 -> r.[2] <- '='; r.[3] <- '='
          | 2 -> r.[3] <- '='
          | 3 -> ()
          | _bad -> failwithf "buf in Base64.encode - leftover chars in encode loop" ()
        end;
        String.concat ~sep:"" (List.rev (r :: acc))
      end
 in
 enc [] 0
;;

let real_decode enc_str =
  let raw_len = String.length enc_str in
  let len_mod =
    if enc_str.[raw_len - 2] = '=' then 2
    else if enc_str.[raw_len - 1] = '=' then 1
    else 0
  in
  let len = (raw_len / 4 * 3) - len_mod in
  let buf = String.create len in
  let rec dec raw_pos buf_pos =
    if raw_len - raw_pos >= 4 then
      begin
        let a = decode_char enc_str.[raw_pos] in
        let b = decode_char enc_str.[raw_pos + 1] in
        let c = decode_char enc_str.[raw_pos + 2] in
        let d = decode_char enc_str.[raw_pos + 3] in
        buf.[buf_pos] <- char_of_int ((a lsl 2) lxor (b lsr 4));
        if buf_pos + 1 < len then
          buf.[buf_pos + 1] <- char_of_int (((b land 0b00001111) lsl 4) lxor (c lsr 2));
        if buf_pos + 2 < len then
          buf.[buf_pos + 2] <- char_of_int (((c land 0b00000011) lsl 6) lxor d);
        dec (raw_pos + 4) (buf_pos + 3)
      end
  in
  dec 0 0;
  buf
;;

let decode enc_str =
  if enc_str = "" then ""
  else if (String.length enc_str) % 4 <> 0 then
    invalid_arg "Base64 strings must have a length that is a multiple of 4"
  else real_decode enc_str
;;

let encode_float ?(length=6) f =
  match Int64.of_float f with
  | exception _ -> invalid_arg "cannot encode a float that does not fit in an Int64"
  | n ->
    let n = ref n in
    let s = String.make length 'A' in
    for i = length - 1 downto 0 do
      s.[i] <- encode_int_exn Int64.(to_int_exn (!n % 64L));
      n := Int64.(!n / 64L);
    done;
    s
;;

let%test_unit _ =
  [%test_result: string] (encode_float 1234.1235453123) ~expect:"AAAATS"
let%test_unit _ =
  [%test_result: string] (encode_float 1234.) ~expect:"AAAATS"
let%test_unit _ =
  [%test_result: string] (encode_float 1235.) ~expect:"AAAATT"
let%test_unit _ =
  [%test_result: string] (encode_float 123456.) ~expect:"AAAeJA"
let%test_unit _ =
  [%test_result: string] (encode_float Int64.(to_float (max_value - 1024L))) ~expect:"____wA"
