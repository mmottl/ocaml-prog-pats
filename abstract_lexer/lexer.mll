{
  open Lexing

  (* The lexer specification consists of all rule specifications *)
  module type Spec = sig
    (* Each lexer rule gets its own module *)
    module Any_char : sig
      type t  (* Return type of rule *)

      (* One function for each pattern in the rule *)
      val handle_char : lexbuf -> char -> t
      val handle_eof : lexbuf -> t
    end

    module Any_digit : sig
      type t

      val handle_digit : lexbuf -> int -> t
      val handle_eof : lexbuf -> t
    end
  end

  (* This is the signature of lexers *)
  module type Sig = sig
    module Spec : Spec

    open Spec

    val any_char : lexbuf -> Any_char.t
    val any_digit : lexbuf -> Any_digit.t
  end

  (* This introduces the functor that creates a lexer from a specification *)
  module Make (Spec_ : Spec) : Sig with module Spec = Spec_ = struct
    module Spec = Spec_

    open Spec
}

rule any_char = parse
  | _ as c { Any_char.handle_char lexbuf c }
  | eof { Any_char.handle_eof lexbuf }

and any_digit = parse
  | ['0'-'9'] as c { Any_digit.handle_digit lexbuf (Char.code c - 48) }
  | eof { Any_digit.handle_eof lexbuf }

{
  (* Functor ends here *)
  end
}
