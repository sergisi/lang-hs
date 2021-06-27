Func f
    goto temp1
Func temp0
    a := param 1
Func Tall
    temp2 @ 0 := 0
    return temp2
Func Medium
    temp3 @ 0 := 1
    return temp3
Func Small
    temp4 @ 0 := 2
    return temp4
Func g
    goto temp6
Func temp5
    a := param 1
    goto temp31
    goto temp8
Func temp7
    return 3
Label temp8
    call temp7
    temp9 := $SP
Func temp10
    temp11 := param 0
    call temp9
    return $SP
    goto temp13
Func temp12
    return 2
Label temp13
    call temp12
    temp14 := $SP
Func temp15
    temp16 := param 0
    call temp14
    return $SP
    goto temp18
Func temp17
    return 1
Label temp18
    call temp17
    temp19 := $SP
Func temp20
    temp21 := param 0
    call temp19
    return $SP
Label temp31
    temp23 := a @ 0
    temp25 := temp23 != 0
    if false temp25 goto temp26
    temp27 := temp23 != 1
    if false temp27 goto temp28
    temp29 := temp23 != 2
    if false temp29 goto temp30
Label temp26
    param a
    call temp10
    goto temp24
Label temp28
    param a
    call temp15
    goto temp24
Label temp30
    param a
    call temp20
    goto temp24
Label temp24
    temp22 := $SP
    return temp22
Label temp6
    call temp5
    return $SP
    temp32 := $SP
    call Small
    param temp32
    call g
    temp33 := $SP
    return temp33
Label temp1
    call temp0
    return $SP
