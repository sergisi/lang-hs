Func f
    goto temp1
Func temp0
    a := param 1
    b := param 2
    c := param 3
    temp2 := b + c
    return temp2
Label temp1
    call temp0
    return $SP
