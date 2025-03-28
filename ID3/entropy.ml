let log2 x = log x /. log 2.

let h p = if p = 0. || p = 1. then 0. else
  -. p *. log2 p -. (1. -. p) *. log2 (1. -. p)

let frac a b = float_of_int a /. float_of_int b

let ent a b = let p = frac a (a+b) in h p
