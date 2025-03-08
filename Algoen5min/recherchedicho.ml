let dicho tab m =
  let n = Array.length tab in
  let rec aux tab m min max =
    if min > max then -1 
    else
      let mid = (min + max) / 2 in
      if tab.(mid) < m then aux tab m (mid + 1) max
      else if tab.(mid) > m then aux tab m min (mid - 1)
      else mid 
  in
  aux tab m 0 (n - 1) ;;
let tab =[|1;2;35;97;100|];;

dicho tab 97