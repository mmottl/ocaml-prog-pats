(* This alternating lexer reads a char followed by a digit ad infinitum
   until EOF and prints each recognized token *)

(* This recursive module is needed for recursive lexer rule applications *)
module rec Alternating :
  (* Here we specialize the return types for the rules in the specification *)
  Lexer.Sig
    with type Spec.Any_char.t = unit
    with type Spec.Any_digit.t = unit =
  Lexer.Make (struct
    module Any_char = struct
      type t = unit

      let handle_char lexbuf c = print_char c; Alternating.any_digit lexbuf
      let handle_eof _lexbuf = ()
    end

    module Any_digit = struct
      type t = unit

      let handle_digit lexbuf d = print_int d; Alternating.any_char lexbuf
      let handle_eof _lexbuf = ()
    end
  end)
