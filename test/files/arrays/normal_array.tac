Func f
    goto temp1
Func temp0
    a := param 1
    temp2 @ 0 := 4
    temp2 @ 1 := 1
    temp2 @ 3 := 2
    temp2 @ 5 := 3
    temp2 @ 7 := 4
    return temp2
Label temp1
    call temp0
    return $SP
