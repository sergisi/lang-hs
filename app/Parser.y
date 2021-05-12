{
module Parser where
import Lexer
import ParserData
}

%name calc
%tokentype { LexerT }
%error {happyError}

%lexer {lexWrapper} {LEOF}
%monad {Alex}


%token
   '*'      { LMult }
   '/'      { LDiv }
   '+'      { LSum }
   '-'      { LMinus }
   '('      { LLBrack }
   ')'      { LRBrack }
   int      { LInt $$ }
   double   { LDouble $$ }
   ';'      { LSync }
   rvar     { LRealReg $$ }
   ivar     { LIntReg $$ }
   '='      { LAssign }
   '%'      { LMod }
   "div"    { LDivInt }
   ">>"     { LRightShift }
   "<<"     { LLeftShift }
   '~'      { LCompAUn }
   '&'     { LAnd }
   '|'     { LOr }
   '^'     { LXor }
   "cast"    { LRealToInt }
   "real"    { LIntToReal }

%left '+' '-' '|' '^'
%left '*' '/' '%' '&' "<<" ">>" "div"
%left "cast" "real" '~'
%%

Line :: { [ Exp ] }
Line : Line ';' Assign {$3 : $1}
     | Line ';'        { $1 }
     | Assign          { [$1] }
     | {- empty -}     { [] }

Assign :: { Exp }
Assign : rvar '=' Exp { TRealAssign $1 $3 }
       | ivar '=' Exp { TIntAssign $1 $3 }
       | Exp          { $1 }

Exp :: { Exp }
Exp : Exp '+' Exp { TSum $1 $3 }
    | Exp '-' Exp { TMinus $1 $3 }
    | Exp '*' Exp { TMult $1 $3 }
    | Exp '/' Exp { TDiv $1 $3 }
    | Exp '%' Exp { TMod $1 $3 }
    | Exp "div" Exp { TDivInt $1 $3}
    | Exp ">>" Exp { TRightShift $1 $3 }
    | Exp "<<" Exp { TLeftShift $1 $3 }
    | Exp '&' Exp { TAnd $1 $3 }
    | Exp '|' Exp { TOr $1 $3 }
    | Exp '^' Exp { TXor $1 $3 }
    | "cast" Exp   { TRealToInt $2 }
    | "real" Exp  { TIntToReal $2 }
    | '~' Exp     { TCompAUn $2 }
    | '-' Exp     { TNegate $2}
    | '+' Exp     { TPositive $2}
    | int         { TVal $1 }
    | double      { TRealVal $1 }
    | '(' Exp ')' { TBrack $2 }
    | rvar        { TRealGet $1 }
    | ivar        { TIntGet $1 }


{


happyError :: LexerT -> Alex a
happyError tok = alexError $ "Happy error on token: " ++ show tok

runExpression s = runAlex s $ calc >>= traverse eval . reverse

}
