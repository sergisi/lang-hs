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
Assign : "data" name '=' DataDef { DataStatement $ DataDef $2 $4 }
       | name "::" Type '=' Def   { TNone }

DataDef :: { [MultDef] }
DataDef : DataDef '|' MultType { $3 : $1 }
        | MultType             { [$1] }

MultType :: { MultDef }
MultType : name                           { MultDef $1 [] }
         | name ListOfFuns                { MultDef $1 $ reverse $2 }
         -- | name '{' NamedMultType '}'     { MultDef $1 . Right $ reverse $3 }

ListOfFuns :: { [DataType] }
ListOfFuns : Type                { [$1] }
           | ListOfFuns Type     { $2 : $1 }

{- -- Unneeded Type.
NamedMultType :: { [(String, DataType)] }
NamedMultType : NamedMultType ',' name "::" Type    { ($3, $5) : $1 }
              | name "::" Type                      { [($1, $3)] }
-}

Type :: { DataType }
Type : "Real"      { TypeReal }
     | "Int"       { TypeInt }
     | "Bool"      { TypeBool }
     | name        { TypeDef $1 } -- TODO make a monad read or error.
     | '(' Fun ')' { TypeFun $ reverse $2 }

Fun :: { [DataType] }
Fun : Fun "->" Type { $3 : $1 }
    | Type          { [$1] }

Names :: { Exp }
Names : Names name { TNone }
      | name       { TNone }

Parameters :: { Exp }
Parameters : Parameters int         { TNone }
           | Parameters real        { TNone }
           | Parameters bool        { TNone }
           | Parameters name        { TNone }

Def :: { Exp }
Def : int             { TNone }
    | real            { TNone }
    | bool            { TNone }
    | FunctionDef     { TNone }
    | name Parameters { TNone }

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
MathExp : MathExp '+'   MathExp   { TSum $1 $3 }
        | MathExp '-'   MathExp   { TMinus $1 $3 }
        | MathExp '*'   MathExp   { TMult $1 $3 }
        | MathExp '/'   MathExp   { TDiv $1 $3 }
        | MathExp '%'   MathExp   { TMod $1 $3 }
        | MathExp "div" MathExp   { TDivInt $1 $3}
        | MathExp ">>"  MathExp   { TRightShift $1 $3 }
        | MathExp "<<"  MathExp   { TLeftShift $1 $3 }
        | MathExp '&'   MathExp   { TAnd $1 $3 }
        | MathExp '|'   MathExp   { TOr $1 $3 }
        | MathExp '^'   MathExp   { TXor $1 $3 }
        -- | "cast"    MathExp       { TRealToInt $2 }
        -- | "real"    MathExp       { TIntToReal $2 }
        | '~' MathExp             { TCompAUn $2 }
        | '-' MathExp             { TNegate $2}
        | '+' MathExp             { TPositive $2}
        | int                     { TVal $1 }
        | real                    { TRealVal $1 }
        | '(' MathExp   ')'       { TBrack $2 }


{


happyError :: LexerT -> Alex a
happyError tok = alexError $ "Happy error on token: " ++ show tok

runExpression s = runAlex s calc

}
