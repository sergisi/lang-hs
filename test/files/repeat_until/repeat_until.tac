Func f
    goto temp1
Func temp0
    a := param 1
    goto temp3
Func temp2
    a := param 1
    temp4 := a < 3
    return temp4
Label temp3
    goto temp6
Func temp5
    a := param 1
    temp7 := a + 1
    return temp7
Label temp6
    temp8 := 0
Label temp9
    param temp8
    param temp8
    call temp5
    temp8 := $SP
    call temp2
    if false $SP goto temp9
    return temp8
Label temp1
    call temp0
    return $SP
