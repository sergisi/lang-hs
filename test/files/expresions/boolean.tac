Func yy
    goto temp28
Func temp27
    b := param 1
    a := param 2
    temp29 := ¬ a
    return temp29
Label temp28
    call temp27
    return $SP
Func rr
    goto temp25
Func temp24
    b := param 1
    a := param 2
    temp26 := a != b
    return temp26
Label temp25
    call temp24
    return $SP
Func ee
    goto temp22
Func temp21
    b := param 1
    a := param 2
    temp23 := a == b
    return temp23
Label temp22
    call temp21
    return $SP
Func ww
    goto temp19
Func temp18
    b := param 1
    a := param 2
    temp20 := a || b
    return temp20
Label temp19
    call temp18
    return $SP
Func qq
    goto temp16
Func temp15
    b := param 1
    a := param 2
    temp17 := a && b
    return temp17
Label temp16
    call temp15
    return $SP
Func y
    goto temp13
Func temp12
    a := param 1
    temp14 := ¬ True
    return temp14
Label temp13
    call temp12
    return $SP
Func r
    goto temp10
Func temp9
    a := param 1
    temp11 := True != True
    return temp11
Label temp10
    call temp9
    return $SP
Func e
    goto temp7
Func temp6
    a := param 1
    temp8 := True == True
    return temp8
Label temp7
    call temp6
    return $SP
Func w
    goto temp4
Func temp3
    a := param 1
    temp5 := True || True
    return temp5
Label temp4
    call temp3
    return $SP
Func q
    goto temp1
Func temp0
    a := param 1
    temp2 := True && True
    return temp2
Label temp1
    call temp0
    return $SP
