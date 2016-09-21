open Printf

let () =
  let s1 = Union_find.create 1 in
  let s2 = Union_find.create 2 in
  printf "Before their union, sets s1 and s2 are distinct: %B\n"
    (Union_find.same_class s1 s2);
  Union_find.union s1 s2;
  printf "After their union, sets s1 and s2 are distinct: %B\n"
    (Union_find.same_class s1 s2)
