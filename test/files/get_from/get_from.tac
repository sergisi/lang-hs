Func f
    goto temp5
Func temp4
    a := param 1
    temp6 @ 0 := 4
    temp6 @ 1 := 1
    temp6 @ 3 := 2
    temp6 @ 5 := 3
    temp6 @ 7 := 4
    temp7 := 0 * 2
    temp7 := temp7 + 1
    return temp6 @ temp7
Label temp5
    call temp4
    return $SP
Func f
    goto temp1
Func temp0
    a := param 1
    temp2 @ 0 := 4
    temp2 @ 1 := 1
    temp2 @ 3 := 2
    temp2 @ 5 := 3
    temp2 @ 7 := 4
    temp3 := 1 * 2
    temp3 := temp3 + 1
    return temp2 @ temp3
Label temp1
    call temp0
    return $SP
