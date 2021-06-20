{
module Parser where
import Lexer
import ParserData
import SyntaxAnalysis
import Control.Applicative (liftA2)
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
   char      { LChar $$ }
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
   '!'       { LBoolNot }
   "=="      { LEq }
   "!="      { LNeq }
   "<="      { LLessEq }
   ">="      { LGreaterEq }
   "&&"      { LBoolAnd }
   "||"      { LBoolOr }
   '<'       { LLess }
   '>'       { LGreater }
   "case"    { LCase }
   "()"      { LUnit }
   "::"      { LTypeDef }
   "Real"    { LDefReal }
   "Int"     { LDefInt }
   "Bool"    { LDefBool }
   "Char"    { LDefChar }
   "->"      { LDefFunc }
   "data"    { LData }
   "fun"     { LFun }
   "if"      { LConditional }
   "else"    { LElse }
   ','       { LComma }



%nonassoc '<' '>' ">=" "<=" "==" "!="
%left '+' '-' '|' '^' "||" '^'
%left '*' '/' '%' '&' "<<" ">>" "div" "&&"
%left "cast" "real" '~' '!'
%right "->"
%%

Line :: { [ ThreeAddressCode ] }
Line : Line ';' Assign { $3 ++ $1 }
     | Line ';'        { $1 }
     | Assign          { $1 }
     | {- empty -}     { [] }

Assign :: { [ThreeAddressCode] }
Assign : "data" name '=' DataDef            {% defineData $2 $4 }
       | name "::" Fun  '=' Def             {% defineFunc $1 $3 $5}

Statement :: {Alex [ThreeAddressCode] }
Statement : "data" name '=' DataDef         { defineData $2 $4 }
          | name "::" Fun  '=' Def          { defineFunc' $1 $3 $5}

DataDef :: { [Name -> Alex MultDef] }
DataDef : DataDef '|' MultType { $3 : $1 }
        | MultType             { [$1] }

MultType :: { Name -> Alex MultDef }
MultType : name                           { defineConstructor $1 [] }
         | name ListOfFuns                { defineConstructor $1 $2 }

ListOfFuns :: { [Alex DataType] }
ListOfFuns : Type                { [$1] }
           | ListOfFuns Type     { $2 : $1 }

Type :: { Alex DataType }
Type : "Real"      { return TypeReal }
     | "Int"       { return TypeInt }
     | "Bool"      { return TypeBool }
     | "Char"      { return TypeChar }
     | "()"        { return TypeUnit }
     | name        { defineTypeName $1}
     | '(' Fun ')' { fmap (TypeFun . reverse) . sequenceA $ $2 }

Fun :: { [Alex DataType] }
Fun : Fun "->" Type { $3 : $1 }
    | Type          { [$1] }

Names :: { [String] }
Names : Names name   { $2 : $1 }
      | {- empty -}  { [] }

Parameters :: { [Exp] }
Parameters : Parameters '(' Def ')'        { $3 : $1 }
           | Parameters int                { guardedExp TypeInt  (return (RefConstInt  $2, [])): $1 }
           | Parameters bool               { guardedExp TypeBool (return (RefConstBool $2, [])): $1 }
           | Parameters real               { guardedExp TypeReal (return (RefConstReal $2, [])): $1 }
           | Parameters name               { getNameParam $2 : $1 }
           | Parameters "()"               { guardedExp TypeUnit (return (unit, [])) : $1}
           | {- empty -}                   { [] }

Def :: { Exp }
Def : IntExp                               { $1 }
    | RealExp                              { $1 }
    | BoolExp                              { $1 }
    | name Parameters                      { applyFunc $1 $2 }
    | "fun" Names '{' Statements Def '}'   { functionDef $2 $4 $5 }
    | "()"                                 { const $ return (unit, []) }
    | "if" Def '{' Statements Def '}'
      "else" '{' Statements Def '}'
      { defineConditional $2 $4 $5 $9 $10 }
    | '(' Def ')'                          { $2 }
    -- | if
    -- | while
    -- | repeat until
    -- | for

{-

if  cond {
cosa;
cosa2;
cosa3;
cosa + cosa2 + cosa3
}


-}


Statements :: { Alex [ThreeAddressCode] }
Statements : Statements Statement ';' { liftA2 (++) $1 $2 }
           | {- empty -}              { return [] }

IntExp :: { Exp }
IntExp : Def '+'   Def   { getExp TypeInt $1 OpSum $3 }
       | Def '-'   Def   { getExp TypeInt $1 OpMinus $3 }
       | Def '*'   Def   { getExp TypeInt $1 OpMult $3 }
       | Def '%'   Def   { getExp TypeInt $1 OpMod $3 }
       | Def "div" Def   { getExp TypeInt $1 OpDiv $3 }
       | Def "=="  Def   { getExp' TypeBool TypeInt $1 OpEq $3}
       | Def "!="  Def   { getExp' TypeBool TypeInt $1 OpNeq $3}
       | Def '<'   Def   { getExp' TypeBool TypeInt $1 OpLt $3}
       | Def "<="  Def   { getExp' TypeBool TypeInt $1 OpLEq $3}
       | Def '>'   Def   { getExp' TypeBool TypeInt $1 OpGt $3}
       | Def ">="  Def   { getExp' TypeBool TypeInt $1 OpGEq $3}
       | Def ">>"  Def   { getExp TypeInt $1 OpRightShift $3 }
       | Def "<<"  Def   { getExp TypeInt $1 OpLeftShift $3 }
       | Def '&'   Def   { getExp TypeInt $1 OpBitAnd $3 }
       | Def '|'   Def   { getExp TypeInt $1 OpBitOr $3 }
       | Def '^'   Def   { getExp TypeInt $1 OpBitXOR $3 }
       | '~'  Def        { getUnaryExp TypeInt UnaryComplement $2 }
       | '-'  Def        { getUnaryExp TypeInt UnaryMinus $2 }
       | int             { guardedExp TypeInt (return (RefConstInt $1, [])) }


RealExp :: { Exp }
RealExp : Def '+'   Def   { getExp TypeReal $1 OpSum $3 }
        | Def '-'   Def   { getExp TypeReal $1 OpMinus $3 }
        | Def '*'   Def   { getExp TypeReal $1 OpMult $3 }
        | Def '/'   Def   { getExp TypeReal $1 OpDiv $3 }
        | Def "=="  Def   { getExp' TypeBool TypeReal $1 OpEq $3}
        | Def "!="  Def   { getExp' TypeBool TypeReal $1 OpNeq $3}
        | Def '<'   Def   { getExp' TypeBool TypeReal $1 OpLt $3}
        | Def "<="  Def   { getExp' TypeBool TypeReal $1 OpLEq $3}
        | Def '>'   Def   { getExp' TypeBool TypeReal $1 OpGt $3}
        | Def ">="  Def   { getExp' TypeBool TypeReal $1 OpGEq $3}
        | '-' Def         { getUnaryExp TypeReal UnaryMinus $2 }
        | real            { guardedExp TypeReal ( return (RefConstReal $1, [])) }

BoolExp :: { Exp }
BoolExp : Def "&&"  Def   { getExp TypeBool $1 OpAnd $3 }
        | Def "||"  Def   { getExp TypeBool $1 OpOr $3 }
        | Def "=="  Def   { getExp TypeBool $1 OpEq $3 }
        | Def "!="  Def   { getExp TypeBool $1 OpNeq $3 }
        | Def '^'   Def   { getExp TypeBool $1 OpXOR $3}
        | '!' Def         { getUnaryExp TypeBool UnaryNot $2 }
        | bool            { guardedExp TypeBool (return (RefConstBool $1, [])) }


{


runExpression s = runAlex s calc

runExpression' s = runAlex s (calc *> alexGetUserState)
}
