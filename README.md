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
Def = int | float | bool | FunctionDef | Names
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
