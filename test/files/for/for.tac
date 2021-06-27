Func f
    goto temp1
Func temp0
    a := param 1
    temp2 @ 0 := 4
    temp2 @ 1 := 1
    temp2 @ 3 := 2
    temp2 @ 5 := 3
    temp2 @ 7 := 4
    goto temp4
Func temp3
    acc := param 1
    a := param 2
    temp5 := acc + a
    return temp5
Label temp4
    temp6 := 0
    temp7 := 1
    temp8 := temp2 @ 0 * 2
    temp8 := temp8 + 1
Label temp9
    temp11 := temp7 < temp8
    if false temp11 goto temp10
    param temp2 @ temp7
    param temp6
    call temp3
    temp6 := $SP
    temp7 := temp7 + 2
    goto temp9
Label temp10
    return temp6
Label temp1
    call temp0
    return $SP
