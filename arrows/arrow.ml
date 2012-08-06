let id x = x

(* Simple arrows *)

module type SIMPLE_ARROW = sig
  type ('a, 'b) t

  val arr : ('a -> 'b) -> ('a, 'b) t
  val (>>>) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  val app : unit -> (('a, 'b) t * 'a, 'b) t
  val run : ('a, 'b) t -> 'a -> 'b
end

module SimpleArrow = struct
  type ('a, 'b) t = 'a -> 'b

  let arr f = f
  let (>>>) f g x = g (f x)
  let app () (f, x) = f x
  let run = arr
end

module SimpleContArrow = struct
  type ('a, 'b) t = { f : 'z. 'a -> ('b -> 'z) -> 'z }

  let arr f = { f = fun x cont -> cont (f x) }
  let (>>>) af ag = { f = fun x cont -> af.f x (fun yf -> ag.f yf cont) }
  let app () = { f = fun (af, x) -> af.f x }
  let run af x = af.f x id
end

module type DATA_ARROW = sig
  include SIMPLE_ARROW

  val run_cont : ('a, 'b) t -> 'a -> cont : ('b -> 'c) -> 'c
end

module rec SimpleDataContArrow : DATA_ARROW = struct
  type ('a, 'b) t =
    | Arr of ('a -> 'b)
    | Comp of ('a, 'b) comp
    | App of ('a, 'b) app

  and ('a, 'b) comp =
    {
      comp_open : 'z. ('a, 'b, 'z) comp_scope -> 'z
    }
  and ('a, 'b, 'z) comp_scope =
    {
      comp_bind : 'c. ('a, 'c) t -> ('c, 'b) t -> 'z
    }

  and ('a, 'b) app =
    {
      app_open : 'z. ('a, 'b, 'z) app_scope -> 'z
    }
  and ('a, 'b, 'z) app_scope =
    {
      app_bind :
        'c. ('a -> ('c, 'b) t * 'c) -> (('c, 'b) t * 'c, 'b) t -> 'z
    }

  let arr f = Arr f

  let (>>>) af ag = Comp { comp_open = fun scope -> scope.comp_bind af ag }

  let run_cont a x ~cont =
    match a with
    | Arr f -> cont (f x)
    | Comp comp ->
        comp.comp_open
          {
            comp_bind = fun af ag ->
              SimpleDataContArrow.run_cont af x
                ~cont:(SimpleDataContArrow.run_cont ag ~cont)
          }
    | App app ->
        app.app_open
          {
            app_bind = fun unpack af ->
              SimpleDataContArrow.run_cont af (unpack x) ~cont
          }

  let app () =
    App
      {
        app_open = fun scope ->
          let f (af, x) = SimpleDataContArrow.run_cont af x ~cont:id in
          scope.app_bind id (Arr f)
      }

  let run a x = run_cont a x ~cont:id
end

module type ARROW = sig
  include SIMPLE_ARROW

  type ('a, 'b) either = Left of 'a | Right of 'b

  val first : ('a, 'b) t -> ('a * 'c, 'b * 'c) t
  val second : ('a, 'b) t -> ('c * 'a, 'c * 'b) t

  val ( *** ) : ('a, 'b) t -> ('c, 'd) t -> ('a * 'c, 'b * 'd) t
  val ( &&& ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t

  val liftA2 : ('a -> 'b -> 'c) -> ('d, 'a) t -> ('d, 'b) t -> ('d, 'c) t

  val left : ('a, 'b) t -> (('a, 'c) either, ('b, 'c) either) t
  val right : ('a, 'b) t -> (('c, 'a) either, ('c, 'b) either) t
  val (<+>) : ('a, 'c) t -> ('b, 'd) t -> (('a, 'b) either, ('c, 'd) either) t
  val (|||) : ('a, 'c) t -> ('b, 'c) t -> (('a, 'b) either, 'c) t

  val test : ('a, bool) t -> ('a, ('a, 'a) either) t
end

module MkArrow (SA : SIMPLE_ARROW) = struct
  include SA

  let swap (x, y) = y, x
  let first af = arr (fun (a, c) -> af >>> arr (fun b -> b, c), a) >>> app ()
  let second af = arr swap >>> first af >>> arr swap
  let ( *** ) af ag = first af >>> second ag
  let ( &&& ) af ag = arr (fun x -> x, x) >>> af *** ag
  let liftA2 f af ag = af &&& ag >>> arr (fun (b, c) -> f b c)

  type ('a, 'b) either = Left of 'a | Right of 'b

  let left af =
    arr (function
      | Left l -> arr (fun () -> l) >>> af >>> arr (fun x -> Left x), ()
      | Right _ as right -> arr (fun () -> right), ())
    >>> app ()

  let mirror = function Left x -> Right x | Right x -> Left x
  let right af = arr mirror >>> left af >>> arr mirror
  let (<+>) af ag = left af >>> right ag
  let (|||) af ag = af <+> ag >>> arr (function Left x | Right x -> x)

  let test acond =
    acond &&& arr id >>> arr (fun (b, x) -> if b then Left x else Right x)
end

module Arrow = MkArrow (SimpleArrow)
module ContArrow = MkArrow (SimpleContArrow)
module DataContArrow = MkArrow (SimpleDataContArrow)


(* Kleisli categories *)

(* Monad specification *)
module type MONAD = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val run : 'a t -> 'a
end

(* Functor from arrow with apply operator to monad *)
module MkArrowMonad (SA : SIMPLE_ARROW) = struct
  open SA

  type 'a t = (unit, 'a) SA.t

  let return x = arr (fun () -> x)
  let (>>=) af g = af >>> arr (fun x -> g x, ()) >>> app ()
  let run af = SA.run af ()
end

(* Functor from monads to arrows with apply operator *)
module MkKleisli (M : MONAD) =
  MkArrow (struct
    open M

    type ('a, 'b) t = 'a -> 'b M.t

    let arr f x = return (f x)
    let (>>>) f g x = f x >>= g
    let app () (f, x) = f x
    let run f x = M.run (f x)
  end)
