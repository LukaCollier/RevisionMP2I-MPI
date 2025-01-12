let horner tab x=
    let n = Array.length tab in
    let res = ref tab.(n-1) in
    for i = n-2 downto 0 do
        res:= !res*x + tab.(i)
    done;
    !res