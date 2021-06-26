Func f
    goto temp1
Func temp0
    a := param 1
    temp2 @ 0 := 3
    temp2 @ 1 := 2
    temp2 @ 3 := 5
    temp2 @ 5 := 9
    temp3 @ 0 := 1
    temp3 @ 1 := 1
    temp4 @ 0 := 2
    temp4 @ 1 := 1
    temp4 @ 3 := 2
    temp5 @ 0 := 3
    temp5 @ 1 := 1
    temp5 @ 3 := 2
    temp5 @ 5 := 3
    temp6 @ 0 := 4
    temp6 @ 1 := temp5
    temp6 @ 2 := temp4
    temp6 @ 3 := temp3
    temp6 @ 4 := temp2
    return temp6
Label temp1
    call temp0
    return $SP
