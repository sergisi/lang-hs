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
   '*'       { LMult }
   '/'       { LDiv }
   '+'       { LSum }
   '-'       { LMinus }
   '('       { LLBrack }
   ')'       { LRBrack }
   int       { LInt $$ }
   real      { LDouble $$ }
   bool      { LBool $$ }
   name      { LVar $$ }
   ';'       { LSync }
   '='       { LAssign }
   '%'       { LMod }
   "div"     { LDivInt }
   ">>"      { LRightShift }
   "<<"      { LLeftShift }
   '~'       { LCompAUn }
   '&'       { LAnd }
   '|'       { LSumType }
   '^'       { LXor }
   '{'       { LOpenDef }
   '}'       { LCloseDef }
   "case"    { LCase }
   "::"      { LTypeDef }
   "Real"    { LDefReal }
   "Int"     { LDefInt }
   "Bool"    { LDefBool }
   "->"      { LDefFunc }
   "data"    { LData }
   "return"  { LReturn }
   ','       { LComma }




%left '+' '-' '|' '^'
%left '*' '/' '%' '&' "<<" ">>" "div"
%left "cast" "real" '~'
%%

Line :: { [ Exp ] }
Line : Line ';' Assign { $3 : $1 }
     | Line ';'        { $1 }
     | Assign          { [$1] }
     | {- empty -}     { [] }

Assign :: { Exp }
Assign : "data" name '=' TypeDef { TNone }
       | name "::" Fun '=' Def   { TNone }

TypeDef :: { Exp }
TypeDef : MultType '|' TypeDef { TNone }
        | MultType             { TNone }

MultType :: { Exp }
MultType : name                           { TNone }
         | name ListOfFuns                { TNone }
         | name '{' NamedMultType '}'     { TNone }

ListOfFuns :: { Exp }
ListOfFuns : Fun                { TNone }
           | ListOfFuns Fun     { TNone }

NamedMultType :: { Exp }
NamedMultType : NamedMultType ',' name "::" TypeDef { TNone }
              | name "::" TypeDef                   { TNone }

Type :: { Exp }
Type : "Real" { TNone }
     | "Int"  { TNone }
     | "Bool" { TNone }
     | name   { TNone }

Fun :: { Exp }
Fun : Fun "->" Type { TNone }
    | Type          { TNone }

Names :: { Exp }
Names : Names name { TNone }
      | name       { TNone }

Def :: { Exp }
Def : int         { TNone }
    | real        { TNone }
    | bool        { TNone }
    | FunctionDef { TNone }
    | Names       { TNone }

FunctionDef :: { Exp }
FunctionDef : Names '{' Statements '}'   { TNone }

Statements :: { Exp }
Statements : Statements ';' Statement { TNone }
           | Statements ';'           { TNone }
           | Statement                { TNone }
           | {- empty -}              { TNone }

Statement :: { Exp }
Statement : Def          { TNone }
          | Assign       { TNone }
          | "return" Def { TNone }

-- Maybe we could use a lot from here.
MathExp :: { Exp }
MathExp : MathExp '+'   MathExp { TSum $1 $3 }
        | MathExp '-'   MathExp { TMinus $1 $3 }
        | MathExp '*'   MathExp { TMult $1 $3 }
        | MathExp '/'   MathExp { TDiv $1 $3 }
        | MathExp '%'   MathExp { TMod $1 $3 }
        | MathExp "div" MathExp { TDivInt $1 $3}
        | MathExp ">>"  MathExp { TRightShift $1 $3 }
        | MathExp "<<"  MathExp { TLeftShift $1 $3 }
        | MathExp '&'   MathExp { TAnd $1 $3 }
        | MathExp '|'   MathExp { TOr $1 $3 }
        | MathExp '^'   MathExp { TXor $1 $3 }
        | "cast"    MathExp { TRealToInt $2 }
        | "real"    MathExp { TIntToReal $2 }
        | '~' MathExp       { TCompAUn $2 }
        | '-' MathExp       { TNegate $2}
        | '+' MathExp       { TPositive $2}
        | int           { TVal $1 }
        | double        { TRealVal $1 }
        | '(' MathExp   ')' { TBrack $2 }
        | rvar          { TRealGet $1 }
        | ivar          { TIntGet $1 }


{


happyError :: LexerT -> Alex a
happyError tok = alexError $ "Happy error on token: " ++ show tok

-- runExpression s = runAlex s $ calc >>= traverse eval . reverse

}
