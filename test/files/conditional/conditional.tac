Right [TacFuncLabel "f",TacGoto "temp1",TacFuncLabel "temp0",TacGetParam (RefVar "a") 1,TacOp (RefVar "temp2") (RefVar "a") OpEq (RefConstInt 3),TacIfExp (RefVar "temp2") "temp5",TacOp (RefVar "temp3") (RefVar "a") OpSum (RefConstInt 1),TacCopy (RefVar "temp6") (RefVar "temp3"),TacGoto "temp7",TacLabel "temp5",TacOp (RefVar "temp4") (RefVar "a") OpSum (RefConstInt 2),TacCopy (RefVar "temp6") (RefVar "temp4"),TacLabel "temp7",TacReturn (RefVar "temp6"),TacLabel "temp1",TacCall (RefVar "temp0"),TacReturn RefSP]