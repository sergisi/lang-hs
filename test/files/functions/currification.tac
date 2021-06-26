Func f
    goto temp4
Func temp3
    a := param 1
    goto temp6
Func temp5
    param a
    call doSomething
    return $SP
Label temp6
    return temp5
Label temp4
    call temp3
    return $SP
Func doSomething
    goto temp1
Func temp0
    b := param 1
    a := param 2
    temp2 := a + b
    return temp2
Label temp1
    call temp0
    return $SP
