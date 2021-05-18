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
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57 :: () => Prelude.Int -> ({-HappyReduction (Alex) = -}
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
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36 :: () => ({-HappyReduction (Alex) = -}
	   Prelude.Int 
	-> (LexerT)
	-> HappyState (LexerT) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)
	-> [HappyState (LexerT) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Alex) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,115) ([0,512,4096,0,512,4096,0,1024,0,0,0,0,0,0,128,0,512,0,0,1024,0,0,512,4096,0,2048,0,0,512,1792,0,0,0,0,2048,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,4,0,512,1808,0,960,0,0,512,1792,0,0,0,0,512,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,1792,0,0,2048,0,512,0,0,512,0,0,0,0,0,0,16416,0,0,128,0,0,2048,0,0,0,0,960,12288,0,0,0,0,0,0,0,1024,32,0,0,0,0,0,128,0,960,0,0,512,0,0,0,0,0,512,0,0,0,128,0,0,0,0,0,0,0,960,12288,0,0,0,0,0,0,0,512,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_calc","Line","Assign","TypeDef","MultType","ListOfFuns","NamedMultType","Type","Fun","Names","Def","FunctionDef","Statements","Statement","'*'","'/'","'+'","'-'","'('","')'","int","real","bool","name","';'","'='","'%'","\"div\"","\">>\"","\"<<\"","'~'","'&'","'|'","'^'","'{'","'}'","\"case\"","\"::\"","\"Real\"","\"Int\"","\"Bool\"","\"->\"","\"data\"","\"return\"","','","%eof"]
        bit_start = st Prelude.* 48
        bit_end = (st Prelude.+ 1) Prelude.* 48
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..47]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (26) = happyShift action_4
action_0 (45) = happyShift action_5
action_0 (4) = happyGoto action_6
action_0 (5) = happyGoto action_3
action_0 _ = happyReduce_4

action_1 (26) = happyShift action_4
action_1 (45) = happyShift action_5
action_1 (4) = happyGoto action_2
action_1 (5) = happyGoto action_3
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (27) = happyShift action_7
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_3

action_4 (40) = happyShift action_9
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (26) = happyShift action_8
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (27) = happyShift action_7
action_6 (48) = happyAccept
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (26) = happyShift action_4
action_7 (45) = happyShift action_5
action_7 (5) = happyGoto action_17
action_7 _ = happyReduce_2

action_8 (28) = happyShift action_16
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (26) = happyShift action_12
action_9 (41) = happyShift action_13
action_9 (42) = happyShift action_14
action_9 (43) = happyShift action_15
action_9 (10) = happyGoto action_10
action_9 (11) = happyGoto action_11
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_21

action_11 (28) = happyShift action_21
action_11 (44) = happyShift action_22
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_19

action_13 _ = happyReduce_16

action_14 _ = happyReduce_17

action_15 _ = happyReduce_18

action_16 (26) = happyShift action_20
action_16 (6) = happyGoto action_18
action_16 (7) = happyGoto action_19
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_1

action_18 _ = happyReduce_5

action_19 (35) = happyShift action_34
action_19 _ = happyReduce_8

action_20 (26) = happyShift action_12
action_20 (37) = happyShift action_33
action_20 (41) = happyShift action_13
action_20 (42) = happyShift action_14
action_20 (43) = happyShift action_15
action_20 (8) = happyGoto action_31
action_20 (10) = happyGoto action_10
action_20 (11) = happyGoto action_32
action_20 _ = happyReduce_9

action_21 (23) = happyShift action_27
action_21 (24) = happyShift action_28
action_21 (25) = happyShift action_29
action_21 (26) = happyShift action_30
action_21 (12) = happyGoto action_24
action_21 (13) = happyGoto action_25
action_21 (14) = happyGoto action_26
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (26) = happyShift action_12
action_22 (41) = happyShift action_13
action_22 (42) = happyShift action_14
action_22 (43) = happyShift action_15
action_22 (10) = happyGoto action_23
action_22 _ = happyFail (happyExpListPerState 22)

action_23 _ = happyReduce_20

action_24 (26) = happyShift action_39
action_24 (37) = happyShift action_40
action_24 _ = happyReduce_28

action_25 _ = happyReduce_6

action_26 _ = happyReduce_27

action_27 _ = happyReduce_24

action_28 _ = happyReduce_25

action_29 _ = happyReduce_26

action_30 _ = happyReduce_23

action_31 (26) = happyShift action_12
action_31 (41) = happyShift action_13
action_31 (42) = happyShift action_14
action_31 (43) = happyShift action_15
action_31 (10) = happyGoto action_10
action_31 (11) = happyGoto action_38
action_31 _ = happyReduce_10

action_32 (44) = happyShift action_22
action_32 _ = happyReduce_12

action_33 (26) = happyShift action_37
action_33 (9) = happyGoto action_36
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (26) = happyShift action_20
action_34 (6) = happyGoto action_35
action_34 (7) = happyGoto action_19
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_7

action_36 (38) = happyShift action_48
action_36 (47) = happyShift action_49
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (40) = happyShift action_47
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (44) = happyShift action_22
action_38 _ = happyReduce_13

action_39 _ = happyReduce_22

action_40 (23) = happyShift action_27
action_40 (24) = happyShift action_28
action_40 (25) = happyShift action_29
action_40 (26) = happyShift action_45
action_40 (45) = happyShift action_5
action_40 (46) = happyShift action_46
action_40 (5) = happyGoto action_41
action_40 (12) = happyGoto action_24
action_40 (13) = happyGoto action_42
action_40 (14) = happyGoto action_26
action_40 (15) = happyGoto action_43
action_40 (16) = happyGoto action_44
action_40 _ = happyReduce_33

action_41 _ = happyReduce_35

action_42 _ = happyReduce_34

action_43 (27) = happyShift action_53
action_43 (38) = happyShift action_54
action_43 _ = happyFail (happyExpListPerState 43)

action_44 _ = happyReduce_32

action_45 (40) = happyShift action_9
action_45 _ = happyReduce_23

action_46 (23) = happyShift action_27
action_46 (24) = happyShift action_28
action_46 (25) = happyShift action_29
action_46 (26) = happyShift action_30
action_46 (12) = happyGoto action_24
action_46 (13) = happyGoto action_52
action_46 (14) = happyGoto action_26
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (26) = happyShift action_20
action_47 (6) = happyGoto action_51
action_47 (7) = happyGoto action_19
action_47 _ = happyFail (happyExpListPerState 47)

action_48 _ = happyReduce_11

action_49 (26) = happyShift action_50
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (40) = happyShift action_56
action_50 _ = happyFail (happyExpListPerState 50)

action_51 _ = happyReduce_15

action_52 _ = happyReduce_36

action_53 (23) = happyShift action_27
action_53 (24) = happyShift action_28
action_53 (25) = happyShift action_29
action_53 (26) = happyShift action_45
action_53 (45) = happyShift action_5
action_53 (46) = happyShift action_46
action_53 (5) = happyGoto action_41
action_53 (12) = happyGoto action_24
action_53 (13) = happyGoto action_42
action_53 (14) = happyGoto action_26
action_53 (16) = happyGoto action_55
action_53 _ = happyReduce_31

action_54 _ = happyReduce_29

action_55 _ = happyReduce_30

action_56 (26) = happyShift action_20
action_56 (6) = happyGoto action_57
action_56 (7) = happyGoto action_19
action_56 _ = happyFail (happyExpListPerState 56)

action_57 _ = happyReduce_14

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

happyReduce_5 = happyReduce 4 5 happyReduction_5
happyReduction_5 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (TNone
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 5 5 happyReduction_6
happyReduction_6 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (TNone
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_3  6 happyReduction_7
happyReduction_7 _
	_
	_
	 =  HappyAbsSyn5
		 (TNone
	)

happyReduce_8 = happySpecReduce_1  6 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn5
		 (TNone
	)

happyReduce_9 = happySpecReduce_1  7 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn5
		 (TNone
	)

happyReduce_10 = happySpecReduce_2  7 happyReduction_10
happyReduction_10 _
	_
	 =  HappyAbsSyn5
		 (TNone
	)

happyReduce_11 = happyReduce 4 7 happyReduction_11
happyReduction_11 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (TNone
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_1  8 happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn5
		 (TNone
	)

happyReduce_13 = happySpecReduce_2  8 happyReduction_13
happyReduction_13 _
	_
	 =  HappyAbsSyn5
		 (TNone
	)

happyReduce_14 = happyReduce 5 9 happyReduction_14
happyReduction_14 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (TNone
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_3  9 happyReduction_15
happyReduction_15 _
	_
	_
	 =  HappyAbsSyn5
		 (TNone
	)

happyReduce_16 = happySpecReduce_1  10 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn5
		 (TNone
	)

happyReduce_17 = happySpecReduce_1  10 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn5
		 (TNone
	)

happyReduce_18 = happySpecReduce_1  10 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn5
		 (TNone
	)

happyReduce_19 = happySpecReduce_1  10 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn5
		 (TNone
	)

happyReduce_20 = happySpecReduce_3  11 happyReduction_20
happyReduction_20 _
	_
	_
	 =  HappyAbsSyn5
		 (TNone
	)

happyReduce_21 = happySpecReduce_1  11 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn5
		 (TNone
	)

happyReduce_22 = happySpecReduce_2  12 happyReduction_22
happyReduction_22 _
	_
	 =  HappyAbsSyn5
		 (TNone
	)

happyReduce_23 = happySpecReduce_1  12 happyReduction_23
happyReduction_23 _
	 =  HappyAbsSyn5
		 (TNone
	)

happyReduce_24 = happySpecReduce_1  13 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn5
		 (TNone
	)

happyReduce_25 = happySpecReduce_1  13 happyReduction_25
happyReduction_25 _
	 =  HappyAbsSyn5
		 (TNone
	)

happyReduce_26 = happySpecReduce_1  13 happyReduction_26
happyReduction_26 _
	 =  HappyAbsSyn5
		 (TNone
	)

happyReduce_27 = happySpecReduce_1  13 happyReduction_27
happyReduction_27 _
	 =  HappyAbsSyn5
		 (TNone
	)

happyReduce_28 = happySpecReduce_1  13 happyReduction_28
happyReduction_28 _
	 =  HappyAbsSyn5
		 (TNone
	)

happyReduce_29 = happyReduce 4 14 happyReduction_29
happyReduction_29 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (TNone
	) `HappyStk` happyRest

happyReduce_30 = happySpecReduce_3  15 happyReduction_30
happyReduction_30 _
	_
	_
	 =  HappyAbsSyn5
		 (TNone
	)

happyReduce_31 = happySpecReduce_2  15 happyReduction_31
happyReduction_31 _
	_
	 =  HappyAbsSyn5
		 (TNone
	)

happyReduce_32 = happySpecReduce_1  15 happyReduction_32
happyReduction_32 _
	 =  HappyAbsSyn5
		 (TNone
	)

happyReduce_33 = happySpecReduce_0  15 happyReduction_33
happyReduction_33  =  HappyAbsSyn5
		 (TNone
	)

happyReduce_34 = happySpecReduce_1  16 happyReduction_34
happyReduction_34 _
	 =  HappyAbsSyn5
		 (TNone
	)

happyReduce_35 = happySpecReduce_1  16 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn5
		 (TNone
	)

happyReduce_36 = happySpecReduce_2  16 happyReduction_36
happyReduction_36 _
	_
	 =  HappyAbsSyn5
		 (TNone
	)

happyNewToken action sts stk
	= lexWrapper(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	LEOF -> action 48 48 tk (HappyState action) sts stk;
	LMult -> cont 17;
	LDiv -> cont 18;
	LSum -> cont 19;
	LMinus -> cont 20;
	LLBrack -> cont 21;
	LRBrack -> cont 22;
	LInt happy_dollar_dollar -> cont 23;
	LDouble happy_dollar_dollar -> cont 24;
	LBool happy_dollar_dollar -> cont 25;
	LVar happy_dollar_dollar -> cont 26;
	LSync -> cont 27;
	LAssign -> cont 28;
	LMod -> cont 29;
	LDivInt -> cont 30;
	LRightShift -> cont 31;
	LLeftShift -> cont 32;
	LCompAUn -> cont 33;
	LAnd -> cont 34;
	LSumType -> cont 35;
	LXor -> cont 36;
	LOpenDef -> cont 37;
	LCloseDef -> cont 38;
	LCase -> cont 39;
	LTypeDef -> cont 40;
	LDefReal -> cont 41;
	LDefInt -> cont 42;
	LDefBool -> cont 43;
	LDefFunc -> cont 44;
	LData -> cont 45;
	LReturn -> cont 46;
	LComma -> cont 47;
	_ -> happyError' (tk, [])
	})

happyError_ explist 48 tk = happyError' (tk, explist)
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

-- runExpression s = runAlex s $ calc >>= traverse eval . reverse
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
