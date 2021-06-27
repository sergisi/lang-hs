Func f
    goto temp4
Func temp3
    a := param 1
    param a
    call negate
    temp5 := $SP
    return temp5
Label temp4
    call temp3
    return $SP
Func negate
    goto temp1
Func temp0
    a := param 1
    temp2 := - a
    return temp2
Label temp1
    call temp0
    return $SP
