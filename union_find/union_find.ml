(* NOTE: This does not work yet due to an OCaml 4.04 beta bug.  Should be
   fixed before the release of OCaml 4.04. *)

(* type ('a, 'kind) tree = *)
(*   | Root : { mutable value : 'a; mutable rank : int } -> ('a, [ `root ]) tree *)
(*   | Inner : { mutable parent : 'a node } -> ('a, [ `inner ]) tree *)
(*  *)
(* and 'a node = Node : ('a, _) tree -> 'a node  [@@ocaml.unboxed] *)
(*  *)
(* type 'a t = ('a, [ `inner ]) tree *)

(**)

type ('a, 'kind, 'parent) tree =
  | Root : { mutable value : 'a; mutable rank : int } ->
    ('a, [ `root ], 'parent) tree
  | Inner : { mutable parent : 'parent } -> ('a, [ `inner ], 'parent) tree

type 'a node = Node : ('a, _, 'a node) tree -> 'a node  [@@ocaml.unboxed]

type 'a t = ('a, [ `inner ], 'a node) tree

let invariant t =
  let rec loop (Inner inner) depth =
    match inner.parent with
    | Node (Inner _ as parent) -> loop parent (depth + 1)
    | Node (Root r) -> assert (depth <= r.rank)
  in
  loop t 0

let create v = Inner { parent = Node (Root { value = v; rank = 0 }) }

let rec compress ~repr:(Inner inner as repr) = function
  | Node (Root _ as root) -> repr, root
  | Node (Inner next_inner as repr) ->
      let repr, _ as res = compress ~repr next_inner.parent in
      inner.parent <- Node repr;
      res

let compress_inner (Inner inner as repr) = compress ~repr inner.parent

let get_root (Inner inner) =
  match inner.parent with
  | Node (Root _ as root) -> root  (* Avoids compression call *)
  | Node (Inner _ as repr) ->
      let repr, root = compress_inner repr in
      inner.parent <- Node repr;
      root

let get t = let Root r = get_root t in r.value

let set t x = let Root r = get_root t in r.value <- x

let same_class t1 t2 = get_root t1 == get_root t2

let union t1 t2 =
  let Inner inner1 as repr1, (Root r1 as root1) = compress_inner t1 in
  let Inner inner2 as repr2, (Root r2 as root2) = compress_inner t2 in
  if root1 == root2 then ()
  else
    let n1 = r1.rank in
    let n2 = r2.rank in
    if n1 < n2 then inner1.parent <- Node repr2
    else begin
      inner2.parent <- Node repr1;
      if n1 = n2 then r1.rank <- r1.rank + 1
    end
