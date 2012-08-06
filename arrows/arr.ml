open Printf
open Arrow

open SimpleContArrow

let bump = arr succ

let bump_n_times n =
  let rec loop n arrow =
    if n <= 0 then arrow
    else loop (n - 1) (arrow >>> bump)
  in
  loop n (arr (fun n -> n))

let () =
  if Array.length Sys.argv < 2 then
    failwith "integer argument needed";
  let n = int_of_string Sys.argv.(1) in
  let arrow = bump_n_times n in
  let result = run arrow 0 in
  printf "%d\n" result
