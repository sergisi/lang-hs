Func negate
    goto temp1
Func temp0
    a := param 1
Func Small
    temp2 @ 0 := 0
    return temp2
Func Medium
    temp3 @ 0 := 1
    return temp3
Func Tall
    temp4 @ 0 := 2
    return temp4
    return 0
Label temp1
    call temp0
    return $SP
