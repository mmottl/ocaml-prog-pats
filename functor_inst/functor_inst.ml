(* Signature of module to be substituted *)
module type Arg = sig val (+) : float -> float -> float end

(* Module implementing functions to be substituted *)
module Std_float : Arg = struct
  let (+) = (+.)
end

(* Signature of module resulting from functor instantiation *)
module type S = sig val f : float -> float end

(* Here we define the body of the functor in which we want to substitute
   arguments.  Note that we have to use a first-class module here, because
   the DEFINE macro can unfortunately only deal with expressions, not
   structures. *)
DEFINE Body =
  ((module struct
    let f x = Float.(x + x)
  end : S))

(* Here we instantiate the "functor" (actually: macro) using the macro
   preprocessor. *)
module Fast = struct
  module Float = Std_float
  include (val Body)
end

(* Here we define the generic functor using a function.  It is apparently
   impossible to use ordinary module functors, because we cannot call "include"
   on first-class modules within those due to a compiler limitation. *)
let func (module Float : Arg) =
  Body

(* Here we instantiate the generic functor, though using a function rather than
   a more traditional module functor due to the "include" restriction
   mentioned above.  It is apparently equivalent in performance to usual
   functor instantiations. *)
module Slow = (val func (module Std_float))

let main () =
  (* "Fast" is more than twice as fast as "Slow" *)
  let f x = Fast.(f x) in
  (* let f x = Slow.(f x) in *)
  for _i = 1 to 100_000_000 do ignore (f 42.) done

let () = main ()
