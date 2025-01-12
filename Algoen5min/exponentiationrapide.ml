let fastexp a b=
    if b = 0 then 1
    else if b = 1 then a
        else if b mod 2 = 1 then a * fastexp (a*a) ((b-1)/2)
            else fastexp (a*a) ((b)/2)