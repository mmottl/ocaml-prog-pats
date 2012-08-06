let () =
  let lexbuf = Lexing.from_channel stdin in
  Lexers.Alternating.any_char lexbuf
