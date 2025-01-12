let decompositionbaseb a b =
    if a < b then [a]
    else let n = a mod b in
        n::(decompositionbaseb a/b b)