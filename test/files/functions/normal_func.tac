Func negate
    goto temp1
Func temp0
    a := param 1
    temp2 := - a
    return temp2
Label temp1
    call temp0
    return $SP
