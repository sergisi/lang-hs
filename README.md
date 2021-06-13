# lang-hs
A new complier

## Definition

```
Name := String
Names = Name [Names] wd
DefStatement = "data" Name "=" TypeDef;
          | Name "::" Fun ["=" Def];
-- SumDef
TypeDef = TypeDef "|" MultType | {nothing} | FunctionDef
Fun = Fun "->" Name | Nothing
NamedMultType = Name "::" TypeDef | NamedMultType "," Name "::" TypeDef
MultType = Name [NamesOrFuns] | Name "{" NamedMultType "}"
Def = int | float | bool | FunctionDef | Names | IntExp | BoolExp | FloatExp
Statements = Statements; Def 
           | Statements; DefStatement 
           | Statements; return Def
FunctionDef = Names "{" Statements "}"
CaseOf = "case" Names "::" TypeDef "{" [Name "{" Statements "}"] "}"
```

### Example

```
someVar :: Int = 3

odd :: Int -> Bool = a { return a % 2 == 0 }

add :: Int -> Int -> Int = a b { return a + b }

add2 :: Int -> Int = add 2

main :: String -> Unit = as { print (odd 3); return unit;}


```


```
data SomeEnum = Ha | HaHa | HaHaHa;
printSomeEnum :: SomeEnum -> Unit = en { 
    return case en :: Unit {
        Ha {return print "Ha"}
        HaHa {return print "HaHa"}
        HaHaHa {return print "HaHaHa"}
    }
}-- Can we solve returns when it's unit?

data Fun = Int -> Bool;
odd :: Fun = a { return a % 2 == 0 }
```

```
data T = A Bool | B Int

odd :: Int -> Bool = a { return a % 2 == 0 }
---
{
    "T" : TypeD {_typeD = DSum { "A": DBool, "B": DInt },
    "odd": Definition {_type = DFuntion [DBool, DInt],
                       _value = Just address
                       }
                       


}
--- 
<label> parameters
r = a % 2
ret r


call parm
```


## Dissenys de llenguatge
### Constants vs variables:
#### Proposta
Constant per default, oferir variables pels bucles.

Crec que caldria llavors fer un analitzador sintàctic que optimitzes les crides. És a dir, si no es necessita, *no es crida*, si una constant deixa de ser utilitzada, pot agafar-se com a variable lliure. Això faria que s'evalués al revés les operacions.

Exemple

```
main :: String[] -> Int { 
  a = 2 + 3 * 4;
  b = 5 - 3;
  c = a - b;
  d = if b > a {
    b - a;
  } else {
    a - b;
  };
  while i < n with {
    acc :: Int = 0;
    n :: Int = 7;
    i :: Int = 1;
  } do {
    acc = acc + i;
    i = i + 1;
  }
  acc + b;
};
```

Passaria a 

```
r1 = 3 * 4;
r1 = r1 + 2; -- ja no es necessita r1
r2 = 5 - 3;
r1 = r1 - r2; -- ja no es necessita r1
r1 = 2 * r1; -- ...
r1 = b - r1;
RET r1
```

Forma de fer-ho, començar per sota i mantenir quines variables s'han necessitat fins ara.

Problema, com fem amb els ifs?

### Code 3@
Exemples de codi de tres adreces

```
a=f(v1, v2, v3);
------
param v3
param v2
param v1
call f

a := $SP

===== 

int t1[5];
t1[3]=4;
----
t1 @ 0 + 3 * 2 = 4 

===== 

if var then code else code (etiq2)

if false temp3 goto etiq2


=====

Func f
    b := param 1
    c := param 2
    a @ 0 = ( b := c * d )
    a @ 8 = ( t := b + 1 )
    a @ 16 = ( return t )
    return a

=====

Func f
    b := param 1
    c := param 2
    d := b + c
    return d
  
Func g
    t := param 1
    a := param 2
    param a
    param 2
    call t
    return $SP

Func main
    param f
    param 5
    call g
    ret := $SP
    halt
```

