let rec min l tmp =
    match l with
    |[] -> tmp
    |t::q -> 
        if t < tmp then min q t
        else  min q tmp    