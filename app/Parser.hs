{-# OPTIONS_GHC -w #-}
module Parser where
import Lexer
import ParserData
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn 
	= HappyTerminal (LexerT)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 ([ Exp ])
	| HappyAbsSyn5 (Exp)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
	-> (LexerT)
	-> HappyState (LexerT) (HappyStk HappyAbsSyn -> m HappyAbsSyn)
	-> [HappyState (LexerT) (HappyStk HappyAbsSyn -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52 :: () => Prelude.Int -> ({-HappyReduction (Alex) = -}
	   Prelude.Int 
	-> (LexerT)
	-> HappyState (LexerT) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)
	-> [HappyState (LexerT) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Alex) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28 :: () => ({-HappyReduction (Alex) = -}
	   Prelude.Int 
	-> (LexerT)
	-> HappyState (LexerT) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)
	-> [HappyState (LexerT) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Alex) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,121) ([46848,3137,14048,392,256,0,0,15360,15296,28160,6275,28096,784,3512,98,0,0,0,0,8,0,1,7024,196,33646,49176,4205,3,2,46848,3137,0,0,0,0,0,0,0,0,0,28096,784,3512,49250,48139,6147,6016,768,752,56192,1568,7024,196,33646,49176,4205,47107,25101,46848,3137,14048,392,1756,32817,8411,28678,50203,28160,6275,48,47,57350,5,0,0,0,0,0,0,0,0,32768,30721,12289,12032,0,0,0,0,0,0,61455,57358,56833,1,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_calc","Line","Assign","Exp","'*'","'/'","'+'","'-'","'('","')'","int","double","';'","rvar","ivar","'='","'%'","\"div\"","\">>\"","\"<<\"","'~'","'&'","'|'","'^'","\"cast\"","\"real\"","%eof"]
        bit_start = st Prelude.* 29
        bit_end = (st Prelude.+ 1) Prelude.* 29
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..28]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (9) = happyShift action_5
action_0 (10) = happyShift action_6
action_0 (11) = happyShift action_7
action_0 (13) = happyShift action_8
action_0 (14) = happyShift action_9
action_0 (16) = happyShift action_10
action_0 (17) = happyShift action_11
action_0 (23) = happyShift action_12
action_0 (27) = happyShift action_13
action_0 (28) = happyShift action_14
action_0 (4) = happyGoto action_15
action_0 (5) = happyGoto action_3
action_0 (6) = happyGoto action_4
action_0 _ = happyReduce_4

action_1 (9) = happyShift action_5
action_1 (10) = happyShift action_6
action_1 (11) = happyShift action_7
action_1 (13) = happyShift action_8
action_1 (14) = happyShift action_9
action_1 (16) = happyShift action_10
action_1 (17) = happyShift action_11
action_1 (23) = happyShift action_12
action_1 (27) = happyShift action_13
action_1 (28) = happyShift action_14
action_1 (4) = happyGoto action_2
action_1 (5) = happyGoto action_3
action_1 (6) = happyGoto action_4
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (15) = happyShift action_16
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_3

action_4 (7) = happyShift action_27
action_4 (8) = happyShift action_28
action_4 (9) = happyShift action_29
action_4 (10) = happyShift action_30
action_4 (19) = happyShift action_31
action_4 (20) = happyShift action_32
action_4 (21) = happyShift action_33
action_4 (22) = happyShift action_34
action_4 (24) = happyShift action_35
action_4 (25) = happyShift action_36
action_4 (26) = happyShift action_37
action_4 _ = happyReduce_7

action_5 (9) = happyShift action_5
action_5 (10) = happyShift action_6
action_5 (11) = happyShift action_7
action_5 (13) = happyShift action_8
action_5 (14) = happyShift action_9
action_5 (16) = happyShift action_18
action_5 (17) = happyShift action_19
action_5 (23) = happyShift action_12
action_5 (27) = happyShift action_13
action_5 (28) = happyShift action_14
action_5 (6) = happyGoto action_26
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (9) = happyShift action_5
action_6 (10) = happyShift action_6
action_6 (11) = happyShift action_7
action_6 (13) = happyShift action_8
action_6 (14) = happyShift action_9
action_6 (16) = happyShift action_18
action_6 (17) = happyShift action_19
action_6 (23) = happyShift action_12
action_6 (27) = happyShift action_13
action_6 (28) = happyShift action_14
action_6 (6) = happyGoto action_25
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (9) = happyShift action_5
action_7 (10) = happyShift action_6
action_7 (11) = happyShift action_7
action_7 (13) = happyShift action_8
action_7 (14) = happyShift action_9
action_7 (16) = happyShift action_18
action_7 (17) = happyShift action_19
action_7 (23) = happyShift action_12
action_7 (27) = happyShift action_13
action_7 (28) = happyShift action_14
action_7 (6) = happyGoto action_24
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_24

action_9 _ = happyReduce_25

action_10 (18) = happyShift action_23
action_10 _ = happyReduce_27

action_11 (18) = happyShift action_22
action_11 _ = happyReduce_28

action_12 (9) = happyShift action_5
action_12 (10) = happyShift action_6
action_12 (11) = happyShift action_7
action_12 (13) = happyShift action_8
action_12 (14) = happyShift action_9
action_12 (16) = happyShift action_18
action_12 (17) = happyShift action_19
action_12 (23) = happyShift action_12
action_12 (27) = happyShift action_13
action_12 (28) = happyShift action_14
action_12 (6) = happyGoto action_21
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (9) = happyShift action_5
action_13 (10) = happyShift action_6
action_13 (11) = happyShift action_7
action_13 (13) = happyShift action_8
action_13 (14) = happyShift action_9
action_13 (16) = happyShift action_18
action_13 (17) = happyShift action_19
action_13 (23) = happyShift action_12
action_13 (27) = happyShift action_13
action_13 (28) = happyShift action_14
action_13 (6) = happyGoto action_20
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (9) = happyShift action_5
action_14 (10) = happyShift action_6
action_14 (11) = happyShift action_7
action_14 (13) = happyShift action_8
action_14 (14) = happyShift action_9
action_14 (16) = happyShift action_18
action_14 (17) = happyShift action_19
action_14 (23) = happyShift action_12
action_14 (27) = happyShift action_13
action_14 (28) = happyShift action_14
action_14 (6) = happyGoto action_17
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (15) = happyShift action_16
action_15 (29) = happyAccept
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (9) = happyShift action_5
action_16 (10) = happyShift action_6
action_16 (11) = happyShift action_7
action_16 (13) = happyShift action_8
action_16 (14) = happyShift action_9
action_16 (16) = happyShift action_10
action_16 (17) = happyShift action_11
action_16 (23) = happyShift action_12
action_16 (27) = happyShift action_13
action_16 (28) = happyShift action_14
action_16 (5) = happyGoto action_52
action_16 (6) = happyGoto action_4
action_16 _ = happyReduce_2

action_17 _ = happyReduce_20

action_18 _ = happyReduce_27

action_19 _ = happyReduce_28

action_20 _ = happyReduce_19

action_21 _ = happyReduce_21

action_22 (9) = happyShift action_5
action_22 (10) = happyShift action_6
action_22 (11) = happyShift action_7
action_22 (13) = happyShift action_8
action_22 (14) = happyShift action_9
action_22 (16) = happyShift action_18
action_22 (17) = happyShift action_19
action_22 (23) = happyShift action_12
action_22 (27) = happyShift action_13
action_22 (28) = happyShift action_14
action_22 (6) = happyGoto action_51
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (9) = happyShift action_5
action_23 (10) = happyShift action_6
action_23 (11) = happyShift action_7
action_23 (13) = happyShift action_8
action_23 (14) = happyShift action_9
action_23 (16) = happyShift action_18
action_23 (17) = happyShift action_19
action_23 (23) = happyShift action_12
action_23 (27) = happyShift action_13
action_23 (28) = happyShift action_14
action_23 (6) = happyGoto action_50
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (7) = happyShift action_27
action_24 (8) = happyShift action_28
action_24 (9) = happyShift action_29
action_24 (10) = happyShift action_30
action_24 (12) = happyShift action_49
action_24 (19) = happyShift action_31
action_24 (20) = happyShift action_32
action_24 (21) = happyShift action_33
action_24 (22) = happyShift action_34
action_24 (24) = happyShift action_35
action_24 (25) = happyShift action_36
action_24 (26) = happyShift action_37
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (7) = happyShift action_27
action_25 (8) = happyShift action_28
action_25 (19) = happyShift action_31
action_25 (20) = happyShift action_32
action_25 (21) = happyShift action_33
action_25 (22) = happyShift action_34
action_25 (24) = happyShift action_35
action_25 _ = happyReduce_22

action_26 (7) = happyShift action_27
action_26 (8) = happyShift action_28
action_26 (19) = happyShift action_31
action_26 (20) = happyShift action_32
action_26 (21) = happyShift action_33
action_26 (22) = happyShift action_34
action_26 (24) = happyShift action_35
action_26 _ = happyReduce_23

action_27 (9) = happyShift action_5
action_27 (10) = happyShift action_6
action_27 (11) = happyShift action_7
action_27 (13) = happyShift action_8
action_27 (14) = happyShift action_9
action_27 (16) = happyShift action_18
action_27 (17) = happyShift action_19
action_27 (23) = happyShift action_12
action_27 (27) = happyShift action_13
action_27 (28) = happyShift action_14
action_27 (6) = happyGoto action_48
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (9) = happyShift action_5
action_28 (10) = happyShift action_6
action_28 (11) = happyShift action_7
action_28 (13) = happyShift action_8
action_28 (14) = happyShift action_9
action_28 (16) = happyShift action_18
action_28 (17) = happyShift action_19
action_28 (23) = happyShift action_12
action_28 (27) = happyShift action_13
action_28 (28) = happyShift action_14
action_28 (6) = happyGoto action_47
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (9) = happyShift action_5
action_29 (10) = happyShift action_6
action_29 (11) = happyShift action_7
action_29 (13) = happyShift action_8
action_29 (14) = happyShift action_9
action_29 (16) = happyShift action_18
action_29 (17) = happyShift action_19
action_29 (23) = happyShift action_12
action_29 (27) = happyShift action_13
action_29 (28) = happyShift action_14
action_29 (6) = happyGoto action_46
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (9) = happyShift action_5
action_30 (10) = happyShift action_6
action_30 (11) = happyShift action_7
action_30 (13) = happyShift action_8
action_30 (14) = happyShift action_9
action_30 (16) = happyShift action_18
action_30 (17) = happyShift action_19
action_30 (23) = happyShift action_12
action_30 (27) = happyShift action_13
action_30 (28) = happyShift action_14
action_30 (6) = happyGoto action_45
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (9) = happyShift action_5
action_31 (10) = happyShift action_6
action_31 (11) = happyShift action_7
action_31 (13) = happyShift action_8
action_31 (14) = happyShift action_9
action_31 (16) = happyShift action_18
action_31 (17) = happyShift action_19
action_31 (23) = happyShift action_12
action_31 (27) = happyShift action_13
action_31 (28) = happyShift action_14
action_31 (6) = happyGoto action_44
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (9) = happyShift action_5
action_32 (10) = happyShift action_6
action_32 (11) = happyShift action_7
action_32 (13) = happyShift action_8
action_32 (14) = happyShift action_9
action_32 (16) = happyShift action_18
action_32 (17) = happyShift action_19
action_32 (23) = happyShift action_12
action_32 (27) = happyShift action_13
action_32 (28) = happyShift action_14
action_32 (6) = happyGoto action_43
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (9) = happyShift action_5
action_33 (10) = happyShift action_6
action_33 (11) = happyShift action_7
action_33 (13) = happyShift action_8
action_33 (14) = happyShift action_9
action_33 (16) = happyShift action_18
action_33 (17) = happyShift action_19
action_33 (23) = happyShift action_12
action_33 (27) = happyShift action_13
action_33 (28) = happyShift action_14
action_33 (6) = happyGoto action_42
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (9) = happyShift action_5
action_34 (10) = happyShift action_6
action_34 (11) = happyShift action_7
action_34 (13) = happyShift action_8
action_34 (14) = happyShift action_9
action_34 (16) = happyShift action_18
action_34 (17) = happyShift action_19
action_34 (23) = happyShift action_12
action_34 (27) = happyShift action_13
action_34 (28) = happyShift action_14
action_34 (6) = happyGoto action_41
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (9) = happyShift action_5
action_35 (10) = happyShift action_6
action_35 (11) = happyShift action_7
action_35 (13) = happyShift action_8
action_35 (14) = happyShift action_9
action_35 (16) = happyShift action_18
action_35 (17) = happyShift action_19
action_35 (23) = happyShift action_12
action_35 (27) = happyShift action_13
action_35 (28) = happyShift action_14
action_35 (6) = happyGoto action_40
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (9) = happyShift action_5
action_36 (10) = happyShift action_6
action_36 (11) = happyShift action_7
action_36 (13) = happyShift action_8
action_36 (14) = happyShift action_9
action_36 (16) = happyShift action_18
action_36 (17) = happyShift action_19
action_36 (23) = happyShift action_12
action_36 (27) = happyShift action_13
action_36 (28) = happyShift action_14
action_36 (6) = happyGoto action_39
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (9) = happyShift action_5
action_37 (10) = happyShift action_6
action_37 (11) = happyShift action_7
action_37 (13) = happyShift action_8
action_37 (14) = happyShift action_9
action_37 (16) = happyShift action_18
action_37 (17) = happyShift action_19
action_37 (23) = happyShift action_12
action_37 (27) = happyShift action_13
action_37 (28) = happyShift action_14
action_37 (6) = happyGoto action_38
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (7) = happyShift action_27
action_38 (8) = happyShift action_28
action_38 (19) = happyShift action_31
action_38 (20) = happyShift action_32
action_38 (21) = happyShift action_33
action_38 (22) = happyShift action_34
action_38 (24) = happyShift action_35
action_38 _ = happyReduce_18

action_39 (7) = happyShift action_27
action_39 (8) = happyShift action_28
action_39 (19) = happyShift action_31
action_39 (20) = happyShift action_32
action_39 (21) = happyShift action_33
action_39 (22) = happyShift action_34
action_39 (24) = happyShift action_35
action_39 _ = happyReduce_17

action_40 _ = happyReduce_16

action_41 _ = happyReduce_15

action_42 _ = happyReduce_14

action_43 _ = happyReduce_13

action_44 _ = happyReduce_12

action_45 (7) = happyShift action_27
action_45 (8) = happyShift action_28
action_45 (19) = happyShift action_31
action_45 (20) = happyShift action_32
action_45 (21) = happyShift action_33
action_45 (22) = happyShift action_34
action_45 (24) = happyShift action_35
action_45 _ = happyReduce_9

action_46 (7) = happyShift action_27
action_46 (8) = happyShift action_28
action_46 (19) = happyShift action_31
action_46 (20) = happyShift action_32
action_46 (21) = happyShift action_33
action_46 (22) = happyShift action_34
action_46 (24) = happyShift action_35
action_46 _ = happyReduce_8

action_47 _ = happyReduce_11

action_48 _ = happyReduce_10

action_49 _ = happyReduce_26

action_50 (7) = happyShift action_27
action_50 (8) = happyShift action_28
action_50 (9) = happyShift action_29
action_50 (10) = happyShift action_30
action_50 (19) = happyShift action_31
action_50 (20) = happyShift action_32
action_50 (21) = happyShift action_33
action_50 (22) = happyShift action_34
action_50 (24) = happyShift action_35
action_50 (25) = happyShift action_36
action_50 (26) = happyShift action_37
action_50 _ = happyReduce_5

action_51 (7) = happyShift action_27
action_51 (8) = happyShift action_28
action_51 (9) = happyShift action_29
action_51 (10) = happyShift action_30
action_51 (19) = happyShift action_31
action_51 (20) = happyShift action_32
action_51 (21) = happyShift action_33
action_51 (22) = happyShift action_34
action_51 (24) = happyShift action_35
action_51 (25) = happyShift action_36
action_51 (26) = happyShift action_37
action_51 _ = happyReduce_6

action_52 _ = happyReduce_1

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_3 : happy_var_1
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 _
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1]
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_0  4 happyReduction_4
happyReduction_4  =  HappyAbsSyn4
		 ([]
	)

happyReduce_5 = happySpecReduce_3  5 happyReduction_5
happyReduction_5 (HappyAbsSyn5  happy_var_3)
	_
	(HappyTerminal (LRealReg happy_var_1))
	 =  HappyAbsSyn5
		 (TRealAssign happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  5 happyReduction_6
happyReduction_6 (HappyAbsSyn5  happy_var_3)
	_
	(HappyTerminal (LIntReg happy_var_1))
	 =  HappyAbsSyn5
		 (TIntAssign happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  5 happyReduction_7
happyReduction_7 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  6 happyReduction_8
happyReduction_8 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (TSum happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  6 happyReduction_9
happyReduction_9 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (TMinus happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  6 happyReduction_10
happyReduction_10 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (TMult happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  6 happyReduction_11
happyReduction_11 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (TDiv happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  6 happyReduction_12
happyReduction_12 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (TMod happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  6 happyReduction_13
happyReduction_13 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (TDivInt happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  6 happyReduction_14
happyReduction_14 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (TRightShift happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  6 happyReduction_15
happyReduction_15 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (TLeftShift happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  6 happyReduction_16
happyReduction_16 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (TAnd happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  6 happyReduction_17
happyReduction_17 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (TOr happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  6 happyReduction_18
happyReduction_18 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (TXor happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2  6 happyReduction_19
happyReduction_19 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (TRealToInt happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_2  6 happyReduction_20
happyReduction_20 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (TIntToReal happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  6 happyReduction_21
happyReduction_21 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (TCompAUn happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  6 happyReduction_22
happyReduction_22 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (TNegate happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_2  6 happyReduction_23
happyReduction_23 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (TPositive happy_var_2
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  6 happyReduction_24
happyReduction_24 (HappyTerminal (LInt happy_var_1))
	 =  HappyAbsSyn5
		 (TVal happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  6 happyReduction_25
happyReduction_25 (HappyTerminal (LDouble happy_var_1))
	 =  HappyAbsSyn5
		 (TRealVal happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  6 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (TBrack happy_var_2
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  6 happyReduction_27
happyReduction_27 (HappyTerminal (LRealReg happy_var_1))
	 =  HappyAbsSyn5
		 (TRealGet happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  6 happyReduction_28
happyReduction_28 (HappyTerminal (LIntReg happy_var_1))
	 =  HappyAbsSyn5
		 (TIntGet happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyNewToken action sts stk
	= lexWrapper(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	LEOF -> action 29 29 tk (HappyState action) sts stk;
	LMult -> cont 7;
	LDiv -> cont 8;
	LSum -> cont 9;
	LMinus -> cont 10;
	LLBrack -> cont 11;
	LRBrack -> cont 12;
	LInt happy_dollar_dollar -> cont 13;
	LDouble happy_dollar_dollar -> cont 14;
	LSync -> cont 15;
	LRealReg happy_dollar_dollar -> cont 16;
	LIntReg happy_dollar_dollar -> cont 17;
	LAssign -> cont 18;
	LMod -> cont 19;
	LDivInt -> cont 20;
	LRightShift -> cont 21;
	LLeftShift -> cont 22;
	LCompAUn -> cont 23;
	LAnd -> cont 24;
	LOr -> cont 25;
	LXor -> cont 26;
	LRealToInt -> cont 27;
	LIntToReal -> cont 28;
	_ -> happyError' (tk, [])
	})

happyError_ explist 29 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> Alex a
happyReturn = (Prelude.return)
happyThen1 :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen1 = happyThen
happyReturn1 :: () => a -> Alex a
happyReturn1 = happyReturn
happyError' :: () => ((LexerT), [Prelude.String]) -> Alex a
happyError' tk = (\(tokens, _) -> happyError tokens) tk
calc = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


happyError :: LexerT -> Alex a
happyError tok = alexError $ "Happy error on token: " ++ show tok

runExpression s = runAlex s $ calc >>= traverse eval . reverse
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
