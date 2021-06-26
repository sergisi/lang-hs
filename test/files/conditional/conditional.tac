Func f
    goto temp1
Func temp0
    a := param 1
    temp2 := a == 3
    if false temp2 goto temp5
    temp3 := a + 1
    temp6 := temp3
    goto temp7
Label temp5
    temp4 := a + 2
    temp6 := temp4
Label temp7
    return temp6
Label temp1
    call temp0
    return $SP
