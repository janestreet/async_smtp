
let to_hex digest =
  let result = String.create (String.length digest * 2) in
  let hex = "0123456789ABCDEF" in
  for i = 0 to String.length digest - 1 do
    let c = int_of_char digest.[i] in
    result.[2*i] <- hex.[c lsr 4];
    result.[2*i+1] <- hex.[c land 0xF]
  done;
  result
