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

Parameters :: { [DataType -> Alex (Ref, [ThreeAddressCode])] }
Parameters : Parameters '(' Def ')'        { $3 : $1 }
           | Parameters int                { paramDef TypeInt (RefInt (ConstantInt $2)) : $1 }
           | Parameters bool               { paramDef TypeBool (RefBool (ConstantBool $2)) : $1 }
           | Parameters real               { paramDef TypeReal (RefReal (ConstantReal $2)) : $1 }
           | Parameters "()"               { paramDef TypeUnit unit : $1}
           | {- empty -}                   { [] }

Def :: { DataType -> Alex (Ref, [ThreeAddressCode]) }
Def : IntExp                               { defineIntExp $1 }
    | RealExp                              { defineRealExp $1 }
    | BoolExp                              { defineBoolExp $1 }
    | name Parameters                      { applyFunc $1 $2 }
    | "fun" Names '{' Statements Def '}'   { functionDef $2 $4 $5 }
    | "()"                                 { const $ return (unit, []) }
    -- | if
    -- | while
    -- | repeat until
    -- | for


Statements :: { Alex [ThreeAddressCode] }
Statements : Statements Statement ';' { liftA2 (++) $1 $2 }
           | {- empty -}              { return [] }

IntExp :: { (TacInt, [ThreeAddressCode]) }
IntExp : IntExp '+'   IntExp   {% getIntExp $1 IntOpSum $3 }
       | IntExp '-'   IntExp   {% getIntExp $1 IntOpMinus $3 }
       | IntExp '*'   IntExp   {% getIntExp $1 IntOpMult $3 }
       | IntExp '%'   IntExp   {% getIntExp $1 IntOpMod $3 }
       | IntExp "div" IntExp   {% getIntExp $1 IntOpDiv $3 }
       | IntExp "=="  IntExp   {% getIntExp $1 IntOpEq $3}
       | IntExp "!="  IntExp   {% getIntExp $1 IntOpNeq $3}
       | IntExp '<'   IntExp   {% getIntExp $1 IntOpLt $3}
       | IntExp "<="  IntExp   {% getIntExp $1 IntOpLEq $3}
       | IntExp '>'   IntExp   {% getIntExp $1 IntOpGt $3}
       | IntExp ">="  IntExp   {% getIntExp $1 IntOpGEq $3}
       | IntExp ">>"  IntExp   {% getIntExp $1 IntOpRightShift $3 }
       | IntExp "<<"  IntExp   {% getIntExp $1 IntOpLeftShift $3 }
       | IntExp '&'   IntExp   {% getIntExp $1 IntOpBitAnd $3 }
       | IntExp '|'   IntExp   {% getIntExp $1 IntOpBitOr $3 }
       | IntExp '^'   IntExp   {% getIntExp $1 IntOpBitXOR $3 }
       | '~' IntExp            {% getIntUnaryExp UnaryIntComplement $2 }
       | '-' IntExp            {% getIntUnaryExp UnaryIntMinus $2 }
       | int                   { (ConstantInt $1, []) }
       | '(' IntExp   ')'      { $2 }


RealExp :: { (TacReal, [ThreeAddressCode]) }
RealExp : RealExp '+'   RealExp   {% getRealExp $1 RealOpSum $3 }
        | RealExp '-'   RealExp   {% getRealExp $1 RealOpMinus $3 }
        | RealExp '*'   RealExp   {% getRealExp $1 RealOpMult $3 }
        | RealExp '/'   RealExp   {% getRealExp $1 RealOpDiv $3 }
        | RealExp "=="  RealExp   {% getRealExp $1 RealOpEq $3}
        | RealExp "!="  RealExp   {% getRealExp $1 RealOpNeq $3}
        | RealExp '<'   RealExp   {% getRealExp $1 RealOpLt $3}
        | RealExp "<="  RealExp   {% getRealExp $1 RealOpLEq $3}
        | RealExp '>'   RealExp   {% getRealExp $1 RealOpGt $3}
        | RealExp ">="  RealExp   {% getRealExp $1 RealOpGEq $3}
        | '-' RealExp             {% getRealUnaryExp UnaryRealMinus $2 }
        | real                    { (ConstantReal $1, []) }
        | '(' RealExp   ')'       { $2 }

BoolExp :: { (TacBool, [ThreeAddressCode]) }
BoolExp : BoolExp "&&"  BoolExp   {% getBoolExp $1 BoolOpAnd $3 }
        | BoolExp "||"  BoolExp   {% getBoolExp $1 BoolOpOr $3 }
        | BoolExp "=="  BoolExp   {% getBoolExp $1 BoolOpEq $3 }
        | BoolExp "!="  BoolExp   {% getBoolExp $1 BoolOpNeq $3 }
        | BoolExp '^'   BoolExp   {% getBoolExp $1 BoolOpXOR $3}
        | '!' BoolExp             {% getBoolUnaryExp UnaryBoolNot $2 }
        | bool                    { (ConstantBool $1, []) }
        | '(' BoolExp   ')'       { $2 }


{


runExpression s = runAlex s calc

runExpression' s = runAlex s (calc *> alexGetUserState)
}
