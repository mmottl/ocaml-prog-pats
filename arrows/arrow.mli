(** {6 Simple arrows} *)

module type SIMPLE_ARROW = sig
  type ('a, 'b) t (** Representation of arrows *)

  val arr : ('a -> 'b) -> ('a, 'b) t
  (** [arr f] projects an OCaml-function to a morphism (arrow) in the
      category of computations. *)

  val (>>>) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  (** [af >>> ag] composes the two computations [af] and [ag]. *)

  val app : unit -> (('a, 'b) t * 'a, 'b) t
  (** [app ()] @return an arrow that represents a computation which takes
      another arrow and a value as argument and returns the result of
      applying the latter to the former. *)

  val run : ('a, 'b) t -> 'a -> 'b
  (** [run af x] runs the computation represented by arrow [af] on input
      [x]. *)
end

(* Example implementations of simple arrows *)

module SimpleArrow : SIMPLE_ARROW

(** Uses continuation-passing internally *)
module SimpleContArrow : SIMPLE_ARROW

module SimpleDataContArrow : SIMPLE_ARROW


(** {6 Enriched arrows} *)

module type ARROW = sig
  include SIMPLE_ARROW

  val first : ('a, 'b) t -> ('a * 'c, 'b * 'c) t
  (** [first af] takes a computation [af] accepting argument [a].
      @return a computation, which takes a pair [(a, c)], and returns
      the pair [(b, c)], where [b] is the result of running computation
      [ag] on [a], and [c] is a passed-through variable. *)

  val second : ('a, 'b) t -> ('c * 'a, 'c * 'b) t
  (* [second af] is a dual of [first], and passes the constant variable
     as first argument. *)

  val ( *** ) : ('a, 'b) t -> ('c, 'd) t -> ('a * 'c, 'b * 'd) t
  (* [af *** ag] @return computation that performs computation [af] and
     [ag] on the first and respectively second argument of the input pair,
     returning the two results as a pair. *)

  val (&&&) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
  (* [af &&& ag] @return computation that passes its input to two
     computations [af] and [ag] and returns the pair of the results. *)

  val liftA2 : ('a -> 'b -> 'c) -> ('d, 'a) t -> ('d, 'b) t -> ('d, 'c) t
  (* [liftA2 f af ag] @return computation that applies the function [f]
     to the results of [af] and [ag], which both receive the input. *)

  type ('a, 'b) either = Left of 'a | Right of 'b

  val left : ('a, 'b) t -> (('a, 'c) either, ('b, 'c) either) t
  (* [left af] @return computation that applies computation [af] to
     [l] if the input is [Left l], returning [Left result] and otherwise
     passes [Right r] through unchanged. *)

  val right : ('a, 'b) t -> (('c, 'a) either, ('c, 'b) either) t
  (* [right af] is the dual of [left]. *)

  val (<+>) : ('a, 'c) t -> ('b, 'd) t -> (('a, 'b) either, ('c, 'd) either) t
  (* [af <+> ag] @return a computation that either performs [af] or
     [ag] depending on its input, returning either [Left res_af] or
     [Right res_ag] respectively. *)

  val (|||) : ('a, 'c) t -> ('b, 'c) t -> (('a, 'b) either, 'c) t
  (* [af ||| ag] @return a computation that either performs [af] or [ag]
     depending on input. *)

  val test : ('a, bool) t -> ('a, ('a, 'a) either) t
  (* [test acond] @return a computation that tests its input with [acond]
     and returns either [Left res] if the predicate is true or [Right res]
     otherwise. *)
end

(** Functor from simple arrows with "apply" to fully-featured arrows *)
module MkArrow (SA : SIMPLE_ARROW) : ARROW with type ('a, 'b) t = ('a, 'b) SA.t

(** Example implementations of fully-featured arrows *)

module Arrow : ARROW

(** Uses continuation-passing internally *)
module ContArrow : ARROW

module DataContArrow : ARROW


(** {6 Kleisli categories} *)

(** Monad specification *)
module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

  val run : 'a t -> 'a
end

(** Functor from arrow with apply operator to monad *)
module MkArrowMonad (SA : SIMPLE_ARROW) : MONAD with type 'a t = (unit, 'a) SA.t

(** Functor from monads to their Kleisli category *)
module MkKleisli (M : MONAD) : ARROW with type ('a, 'b) t = 'a -> 'b M.t
