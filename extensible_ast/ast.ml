(* No recursion until we close the knot *)

(* Numbers *)

type num = [ `Num of int ]

let eval_num (`Num n) = n


(* Addition *)

type 't add = [ `Add of 't * 't ]

let eval_add eval (`Add (l, r)) = eval l + eval r


(* Subtraction *)

type 't sub = [ `Sub of 't * 't ]

let eval_sub eval (`Sub (l, r)) = eval l - eval r


(* All of the above, but still an "open" language *)

type 't all_open = [ num | 't add | 't sub ]

let eval_all_open eval = function
  | #num as t -> eval_num t
  | #add as t -> eval_add eval t
  | #sub as t -> eval_sub eval t


(* Now tying the recursive knot... *)

type all_closed = all_closed all_open

let rec eval_all_closed t = eval_all_open eval_all_closed t

let () = Printf.printf "%d\n" (eval_all_closed (`Add ((`Num 3, `Num 42))))
