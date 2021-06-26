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
   "mod"       { LMod }
   "div"     { LDivInt }
   ">>"      { LRightShift }
   "<<"      { LLeftShift }
   '~'       { LCompAUn }
   '&'       { LAnd }
   '|'       { LSumType }
   '^'       { LXor }
   '{'       { LOpenDef }
   '}'       { LCloseDef }
   '['       { LOpenList }
   ']'       { LCloseList }
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
   "for"     { LFor }
   "map"     { LMap }
   "repeat"  { LRepeat }
   "until"   { LUntil }
   "with"    { LWith }
   "do"      { LDo }
   ','       { LComma }
   "while"   { LWhile }
   "get"     { LGet }
   "from"    { LFrom }

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
Type : "Real"        { return TypeReal }
     | "Int"         { return TypeInt }
     | "Bool"        { return TypeBool }
     | "Char"        { return TypeChar }
     | '[' Type ']'  { fmap TypeArray $2 }
     | "()"          { return TypeUnit }
     | name          { defineTypeName $1}
     | '(' Fun ')'   { fmap (TypeFun . reverse) . sequenceA $ $2 }

Fun :: { [Alex DataType] }
Fun : Fun "->" Type { $3 : $1 }
    | Type          { [$1] }

Names :: { [String] }
Names : Names name   { $2 : $1 }
      | {- empty -}  { [] }

Parameters :: { [Exp] }
Parameters : Parameters '(' Def ')'        { $3 : $1 }
           | Parameters int                { return (Right (RefConstInt  $2, [], TypeInt )): $1 }
           | Parameters bool               { return (Right (RefConstBool $2, [], TypeBool)): $1 }
           | Parameters real               { return (Right (RefConstReal $2, [], TypeReal)): $1 }
           | Parameters name               { getNameParam $2 : $1 }
           | Parameters "()"               { (return (Right (unit, [], TypeUnit))) : $1}
           | {- empty -}                   { [] }

Def :: { Exp }
Def : PrimitiveExp                         { $1 }
    | name Parameters                      { applyFunc $1 $2 }
    | "fun" Names '{' Statements Def '}'   { return . Left $ functionDef $2 $4 $5 }
    | "()"                                 { return $ Right (unit, [], TypeUnit) }
    | '[' Defs ']'                         { defineArray $2 }
    | "if" Def '{' Statements Def '}'
      "else" '{' Statements Def '}'
      { defineConditional $2 $4 $5 $9 $10 }
    | '(' Def ')'                          { $2 }
    | "while" Def "with" Def "do" Def      { return . Left $ whileDef $2 $4 $6 }
    | "for" Def "with" Def "do" Def        { forDef $2 $4 $6 }
    | "map" Def "do" Def                   { mapDef $2 $4 }
    | "repeat" Def "until" Def "with" Def  { return . Left $ repeatUntilDef $2 $4 $6 } 
    | "get" Def "from" Def                 { getArrayElemDef $2 $4 }

Defs :: { [Exp] }
Defs : Defs ',' Def       { $3 :$1 }
     | Defs ','           { $1 }
     | Def                { [$1] }
     | {- empty -}        { [] }



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

PrimitiveExp :: { Exp }
PrimitiveExp : Def '+'   Def   { defineExp [TypeInt, TypeReal] $1 OpSum $3 }
             | Def '-'   Def   { defineExp [TypeInt, TypeReal] $1 OpMinus $3 }
             | Def '*'   Def   { defineExp [TypeInt, TypeReal] $1 OpMult $3 }
             | Def "mod" Def   { defineExp [TypeInt] $1 OpMod $3 }
             | Def "div" Def   { defineExp [TypeInt] $1 OpDiv $3 }
             | Def '/'   Def   { defineExp [TypeReal] $1 OpDiv $3 }
             | Def "=="  Def   { defineExp' TypeBool [TypeInt, TypeReal, TypeBool, TypeChar] $1 OpEq $3}
             | Def "!="  Def   { defineExp' TypeBool [TypeInt, TypeReal, TypeBool, TypeChar] $1 OpNeq $3}
             | Def '<'   Def   { defineExp' TypeBool [TypeInt, TypeReal, TypeChar] $1 OpLt $3}
             | Def "<="  Def   { defineExp' TypeBool [TypeInt, TypeReal, TypeChar] $1 OpLEq $3}
             | Def '>'   Def   { defineExp' TypeBool [TypeInt, TypeReal, TypeChar] $1 OpGt $3}
             | Def ">="  Def   { defineExp' TypeBool [TypeInt, TypeReal, TypeChar] $1 OpGEq $3}
             | Def ">>"  Def   { defineExp [TypeInt] $1 OpRightShift $3 }
             | Def "<<"  Def   { defineExp [TypeInt] $1 OpLeftShift $3 }
             | Def '&'   Def   { defineExp [TypeInt] $1 OpBitAnd $3 }
             | Def '|'   Def   { defineExp [TypeInt] $1 OpBitOr $3 }
             | Def '^'   Def   { defineExp [TypeInt] $1 OpBitXOR $3 }
             | Def "&&"  Def   { defineExp [TypeBool] $1 OpAnd $3 }
             | Def "||"  Def   { defineExp [TypeBool] $1 OpOr $3 }
             | '~'  Def        { defineUnaryExp [TypeInt] UnaryComplement $2 }
             | '-'  Def        { defineUnaryExp [TypeInt, TypeReal] UnaryMinus $2 }
             | '!' Def         { defineUnaryExp [TypeBool] UnaryNot $2 }
             | int             { return $ Right (RefConstInt $1, [], TypeInt) }
             | real            { return $ Right (RefConstReal $1, [], TypeReal) }
             | bool            { return $ Right (RefConstBool $1, [], TypeBool) }
             | char            { return $ Right (RefConstChar $1, [], TypeChar) }


{


runExpression s = runAlex s calc

runExpression' s = runAlex s (calc *> alexGetUserState)
}
