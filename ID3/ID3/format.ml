let fmt str =
  if str.[0] = 'r' then "r"
  else if str.[0] = 'd' then "d"
  else if str = "y" then "y"
  else if str = "?" && Random.int 2 = 0 then "y"
  else "n"

let f = open_in "house-votes-84.data"
let ff = open_out "house-votes-84-bin.data"

let () =
  while true do
    let l = input_line f in
    let s = String.split_on_char ',' l in
    let s = List.map fmt s in
    let l = String.concat "," s in
    output_string ff (l^"\n")
  done

let () = close_in f
let () = close_out ff
