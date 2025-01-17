{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Bnfc.ParHpl where
import Bnfc.AbsHpl as AbsHpl
import Bnfc.LexHpl
import Bnfc.ErrM
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
newtype HappyWrap29 = HappyWrap29 ((Maybe (Int, Int), Ident))
happyIn29 :: ((Maybe (Int, Int), Ident)) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap29 x)
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> HappyWrap29
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
newtype HappyWrap30 = HappyWrap30 ((Maybe (Int, Int), Integer))
happyIn30 :: ((Maybe (Int, Int), Integer)) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap30 x)
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> HappyWrap30
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
newtype HappyWrap31 = HappyWrap31 ((Maybe (Int, Int), String))
happyIn31 :: ((Maybe (Int, Int), String)) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap31 x)
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> HappyWrap31
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
newtype HappyWrap32 = HappyWrap32 ((Maybe (Int, Int), Program (Maybe (Int, Int))))
happyIn32 :: ((Maybe (Int, Int), Program (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap32 x)
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> HappyWrap32
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
newtype HappyWrap33 = HappyWrap33 ((Maybe (Int, Int), FunctionOrRefOrType (Maybe (Int, Int))))
happyIn33 :: ((Maybe (Int, Int), FunctionOrRefOrType (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap33 x)
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> HappyWrap33
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
newtype HappyWrap34 = HappyWrap34 ((Maybe (Int, Int), [FunctionOrRefOrType (Maybe (Int, Int))]))
happyIn34 :: ((Maybe (Int, Int), [FunctionOrRefOrType (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap34 x)
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> HappyWrap34
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
newtype HappyWrap35 = HappyWrap35 ((Maybe (Int, Int), FunctionDef (Maybe (Int, Int))))
happyIn35 :: ((Maybe (Int, Int), FunctionDef (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap35 x)
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> HappyWrap35
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
newtype HappyWrap36 = HappyWrap36 ((Maybe (Int, Int), FunctionArg (Maybe (Int, Int))))
happyIn36 :: ((Maybe (Int, Int), FunctionArg (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap36 x)
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> HappyWrap36
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
newtype HappyWrap37 = HappyWrap37 ((Maybe (Int, Int), [FunctionArg (Maybe (Int, Int))]))
happyIn37 :: ((Maybe (Int, Int), [FunctionArg (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap37 x)
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> HappyWrap37
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
newtype HappyWrap38 = HappyWrap38 ((Maybe (Int, Int), [FunctionDef (Maybe (Int, Int))]))
happyIn38 :: ((Maybe (Int, Int), [FunctionDef (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap38 x)
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> HappyWrap38
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
newtype HappyWrap39 = HappyWrap39 ((Maybe (Int, Int), ValueStatement (Maybe (Int, Int))))
happyIn39 :: ((Maybe (Int, Int), ValueStatement (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap39 x)
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> HappyWrap39
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
newtype HappyWrap40 = HappyWrap40 ((Maybe (Int, Int), [ValueStatement (Maybe (Int, Int))]))
happyIn40 :: ((Maybe (Int, Int), [ValueStatement (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap40 x)
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> HappyWrap40
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
newtype HappyWrap41 = HappyWrap41 ((Maybe (Int, Int), RefDef (Maybe (Int, Int))))
happyIn41 :: ((Maybe (Int, Int), RefDef (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap41 x)
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> HappyWrap41
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
newtype HappyWrap42 = HappyWrap42 ((Maybe (Int, Int), Type (Maybe (Int, Int))))
happyIn42 :: ((Maybe (Int, Int), Type (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap42 x)
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> HappyWrap42
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
newtype HappyWrap43 = HappyWrap43 ((Maybe (Int, Int), [Type (Maybe (Int, Int))]))
happyIn43 :: ((Maybe (Int, Int), [Type (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap43 x)
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> HappyWrap43
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
newtype HappyWrap44 = HappyWrap44 ((Maybe (Int, Int), AlgType (Maybe (Int, Int))))
happyIn44 :: ((Maybe (Int, Int), AlgType (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap44 x)
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> HappyWrap44
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
newtype HappyWrap45 = HappyWrap45 ((Maybe (Int, Int), TypeArg (Maybe (Int, Int))))
happyIn45 :: ((Maybe (Int, Int), TypeArg (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap45 x)
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> HappyWrap45
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
newtype HappyWrap46 = HappyWrap46 ((Maybe (Int, Int), [TypeArg (Maybe (Int, Int))]))
happyIn46 :: ((Maybe (Int, Int), [TypeArg (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap46 x)
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> HappyWrap46
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
newtype HappyWrap47 = HappyWrap47 ((Maybe (Int, Int), AlgTypeVal (Maybe (Int, Int))))
happyIn47 :: ((Maybe (Int, Int), AlgTypeVal (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap47 x)
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> HappyWrap47
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
newtype HappyWrap48 = HappyWrap48 ((Maybe (Int, Int), [AlgTypeVal (Maybe (Int, Int))]))
happyIn48 :: ((Maybe (Int, Int), [AlgTypeVal (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap48 x)
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> HappyWrap48
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
newtype HappyWrap49 = HappyWrap49 ((Maybe (Int, Int), PatternMatch (Maybe (Int, Int))))
happyIn49 :: ((Maybe (Int, Int), PatternMatch (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap49 x)
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> HappyWrap49
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
newtype HappyWrap50 = HappyWrap50 ((Maybe (Int, Int), [PatternMatch (Maybe (Int, Int))]))
happyIn50 :: ((Maybe (Int, Int), [PatternMatch (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap50 x)
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> HappyWrap50
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
newtype HappyWrap51 = HappyWrap51 ((Maybe (Int, Int), Assignment (Maybe (Int, Int))))
happyIn51 :: ((Maybe (Int, Int), Assignment (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap51 x)
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> HappyWrap51
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
newtype HappyWrap52 = HappyWrap52 ((Maybe (Int, Int), [Assignment (Maybe (Int, Int))]))
happyIn52 :: ((Maybe (Int, Int), [Assignment (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap52 x)
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> HappyWrap52
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
newtype HappyWrap53 = HappyWrap53 ((Maybe (Int, Int), FunApplication (Maybe (Int, Int))))
happyIn53 :: ((Maybe (Int, Int), FunApplication (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap53 x)
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> HappyWrap53
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
newtype HappyWrap54 = HappyWrap54 ((Maybe (Int, Int), FunctionArgAppl (Maybe (Int, Int))))
happyIn54 :: ((Maybe (Int, Int), FunctionArgAppl (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn54 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap54 x)
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> HappyWrap54
happyOut54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut54 #-}
newtype HappyWrap55 = HappyWrap55 ((Maybe (Int, Int), [FunctionArgAppl (Maybe (Int, Int))]))
happyIn55 :: ((Maybe (Int, Int), [FunctionArgAppl (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn55 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap55 x)
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn ) -> HappyWrap55
happyOut55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut55 #-}
newtype HappyWrap56 = HappyWrap56 ((Maybe (Int, Int), TupleValueStatementr (Maybe (Int, Int))))
happyIn56 :: ((Maybe (Int, Int), TupleValueStatementr (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn56 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap56 x)
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn ) -> HappyWrap56
happyOut56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut56 #-}
newtype HappyWrap57 = HappyWrap57 ((Maybe (Int, Int), ValueStatementExpr (Maybe (Int, Int))))
happyIn57 :: ((Maybe (Int, Int), ValueStatementExpr (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn57 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap57 x)
{-# INLINE happyIn57 #-}
happyOut57 :: (HappyAbsSyn ) -> HappyWrap57
happyOut57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut57 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\xc4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x84\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x2a\x72\x00\x00\x00\x00\x00\x00\x00\x08\x00\xa0\x22\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x80\x00\x00\x40\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x08\x00\xa0\x22\x07\x00\x00\x00\x00\x00\x00\x80\x00\x00\x2a\x72\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x56\x76\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x2a\x72\x00\x00\x00\x00\x00\x00\x00\x08\x00\xa0\x22\x07\x00\x00\x00\x00\x00\x00\x80\x00\x00\x2a\x72\x00\x00\x00\x00\x00\x00\x00\x08\x00\xa0\x22\x07\x00\x00\x00\x00\x00\x00\x80\x00\x00\x2a\x72\x00\x00\x00\x00\x00\x00\x00\x08\x00\xa0\x22\x07\x00\x00\x00\x00\x00\x00\x80\x00\x00\x2a\x72\x00\x00\x00\x00\x00\x00\x00\x08\x00\xa0\x22\x07\x00\x00\x00\x00\x00\x00\x80\x00\x00\x2a\x72\x00\x00\x00\x00\x00\x00\x00\x08\x00\xa0\x22\x07\x00\x00\x00\x00\x00\x00\x80\x00\x00\x2a\x72\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x2a\x72\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x56\x76\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x2a\x72\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x04\x01\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x5e\x76\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x56\x76\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x08\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x48\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x48\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\xa0\x22\x07\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x50\x10\x00\x00\x00\x00\x00\x00\x00\x66\x65\x07\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\xa0\x22\x07\x00\x00\x00\x00\x00\x00\x80\x00\x00\x2a\x72\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x50\x10\x00\x00\x00\x00\x00\x00\x00\x08\x00\xa0\x22\x07\x00\x00\x00\x00\x00\x00\x80\x00\x00\x2a\x72\x00\x00\x00\x00\x00\x00\x00\x08\x00\xa0\x22\x07\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x2a\x72\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x2a\x72\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x56\x76\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x56\x76\x01\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\xa0\x22\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x2a\x72\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x56\x77\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x56\x77\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\xa0\x22\x07\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\xa0\x22\x07\x00\x00\x00\x00\x00\x00\x60\x56\x77\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x66\x75\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram_internal","%start_pFunctionOrRefOrType_internal","%start_pListFunctionOrRefOrType_internal","%start_pFunctionDef_internal","%start_pFunctionArg_internal","%start_pListFunctionArg_internal","%start_pListFunctionDef_internal","%start_pValueStatement_internal","%start_pListValueStatement_internal","%start_pRefDef_internal","%start_pType_internal","%start_pListType_internal","%start_pAlgType_internal","%start_pTypeArg_internal","%start_pListTypeArg_internal","%start_pAlgTypeVal_internal","%start_pListAlgTypeVal_internal","%start_pPatternMatch_internal","%start_pListPatternMatch_internal","%start_pAssignment_internal","%start_pListAssignment_internal","%start_pFunApplication_internal","%start_pFunctionArgAppl_internal","%start_pListFunctionArgAppl_internal","%start_pTupleValueStatementr_internal","%start_pValueStatementExpr_internal","Ident","Integer","String","Program","FunctionOrRefOrType","ListFunctionOrRefOrType","FunctionDef","FunctionArg","ListFunctionArg","ListFunctionDef","ValueStatement","ListValueStatement","RefDef","Type","ListType","AlgType","TypeArg","ListTypeArg","AlgTypeVal","ListAlgTypeVal","PatternMatch","ListPatternMatch","Assignment","ListAssignment","FunApplication","FunctionArgAppl","ListFunctionArgAppl","TupleValueStatementr","ValueStatementExpr","'!='","'%'","'('","')'","'*'","'+'","','","'-'","'->'","'/'","'::'","';'","'<'","'<='","'='","'=='","'>'","'>='","'data'","'else'","'force'","'fun'","'if'","'in'","'let'","'ref'","'sus'","'then'","'{'","'|'","'}'","L_ident","L_integ","L_quoted","%eof"]
        bit_start = st * 92
        bit_end = (st + 1) * 92
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..91]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\x66\x00\x00\x00\x82\x00\x16\x00\x16\x00\x00\x00\x11\x00\x11\x00\x28\x00\x07\x00\x07\x00\x39\x00\x40\x00\x40\x00\x40\x00\x40\x00\x16\x00\x16\x00\x09\x00\x00\x00\x40\x00\x11\x00\x11\x00\x63\x00\x2f\x02\x51\x00\x00\x00\x4f\x00\x11\x00\x11\x00\x11\x00\x11\x00\x11\x00\x11\x00\x11\x00\x11\x00\x11\x00\x11\x00\x11\x00\x4f\x00\x11\x00\x77\x00\x00\x00\x00\x00\x2f\x02\x00\x00\x74\x00\x81\x00\x00\x00\x8f\x00\x11\x00\x00\x00\x7a\x00\x00\x00\x00\x00\xa0\x00\xa0\x00\xfe\xff\xcf\x00\x00\x00\xce\x00\xb7\x00\x07\x00\x07\x00\x00\x00\x00\x00\xc4\x00\xb7\x00\x16\x00\x02\x00\xf6\x00\xd0\x00\xdb\x00\xdb\x00\x00\x00\xef\x00\xdd\x00\xdd\x00\xdd\x00\xe4\x00\xfe\x00\xfb\x00\xfb\x00\xfb\x00\xd5\x01\xfb\x00\x01\x00\x87\x01\x1e\x01\x05\x01\x26\x01\x08\x01\x08\x01\x07\x00\x19\x01\x20\x02\x09\x01\x00\x00\x00\x00\x00\x00\x09\x01\x66\x00\x00\x00\x07\x00\x1b\x01\x16\x00\x16\x00\x00\x00\x00\x00\x11\x00\x07\x00\x2d\x01\x25\x01\x25\x01\x07\x00\x31\x01\x16\x00\x28\x01\xf3\x00\x35\x01\x16\x00\x07\x00\x00\x00\x47\x01\x37\x00\x57\x00\x00\x00\x11\x00\x11\x00\x45\x01\x2f\x02\x2f\x02\x2f\x02\x2f\x02\x2f\x02\x2f\x02\x2f\x02\x2f\x02\x2f\x02\x2f\x02\x2f\x02\x00\x00\x4e\x01\x00\x00\x3b\x00\x11\x00\x11\x00\x11\x00\x4f\x01\x45\x00\x00\x00\x07\x00\x2b\x01\x00\x00\x00\x00\x50\x01\x00\x00\x00\x00\x37\x01\x00\x00\x00\x00\x00\x00\x52\x01\x57\x01\x3b\x01\x62\x01\x16\x00\x00\x00\x5c\x01\x00\x00\x11\x00\x60\x01\x11\x00\x00\x00\x33\x00\x2f\x02\xc1\x01\x11\x00\x00\x00\x2f\x02\x11\x00\x00\x00\xe7\x01\x00\x00\xf9\x01\x64\x01\x6b\x01\x16\x00\x6d\x01\x65\x01\x46\x01\x00\x00\x00\x00\x2f\x02\x6e\x01\x11\x00\x6f\x01\x11\x00\x0b\x02\x00\x00\x00\x00\x1d\x02\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xcc\x00\x15\x01\x72\x01\x6a\x01\xb8\x01\x46\x00\x74\x01\xb1\x00\x15\x00\x73\x01\x2f\x00\xa1\x00\x7c\x01\x0d\x00\x6d\x00\x10\x00\x08\x00\x2b\x00\x85\x01\x9e\x00\x78\x01\x04\x00\x8a\x00\x6a\x00\x66\x01\x5f\x01\x00\x00\x00\x00\x00\x00\xb5\x00\xbc\x00\xc0\x00\xdc\x00\xe0\x00\xe7\x00\xeb\x00\x07\x01\x0b\x01\x12\x01\x16\x01\x00\x00\x91\x00\x00\x00\x00\x00\x00\x00\x75\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x32\x01\x79\x01\x82\x01\x00\x00\x00\x00\x00\x00\x00\x00\xb8\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc8\x00\x49\x00\x00\x00\x00\x00\x00\x00\x00\x00\x94\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x93\x01\x00\x00\x00\x00\x00\x00\x00\x00\x95\x01\x00\x00\x95\x01\xb4\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x99\x00\x00\x00\x40\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x01\x00\x00\xfd\x00\xa1\x01\x97\x01\x9e\x01\x00\x00\x00\x00\x95\x00\xe5\x00\x00\x00\xd3\x00\x50\x00\x10\x01\x00\x00\x9b\x01\xb3\x01\x00\x00\x00\x00\x4a\x00\x01\x01\x00\x00\x00\x00\xb8\x00\x9f\x01\xa5\x01\x6e\x00\x75\x00\x00\x00\xa4\x01\xa4\x01\xa4\x01\xa4\x01\xa4\x01\xa4\x01\xa4\x01\xa4\x01\xa4\x01\xa4\x01\xa4\x01\x00\x00\x00\x00\x00\x00\xb8\x00\x36\x01\x3d\x01\x41\x01\x00\x00\x00\x00\x00\x00\x1a\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdf\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc4\x01\x00\x00\xad\x01\x00\x00\x00\x00\x00\x00\x5d\x01\x00\x00\x61\x01\x00\x00\xa9\x01\xa9\x01\xa9\x01\x68\x01\x00\x00\xa9\x01\x6c\x01\x00\x00\xa9\x01\x00\x00\xa9\x01\x00\x00\x00\x00\xb6\x01\x00\x00\x00\x00\xa7\x00\x00\x00\x00\x00\xa9\x01\x00\x00\x88\x01\x00\x00\x8c\x01\xa9\x01\x00\x00\x00\x00\xa9\x01\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xde\xff\x00\x00\xde\xff\x00\x00\x00\x00\xd9\xff\xd6\xff\x00\x00\xcb\xff\x00\x00\x00\x00\xc4\xff\x00\x00\x00\x00\xbf\xff\x00\x00\xbb\xff\x00\x00\xb4\xff\x00\x00\xaf\xff\x00\x00\x00\x00\xab\xff\x00\x00\x00\x00\x00\x00\xe5\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcb\xff\x00\x00\xcf\xff\xce\xff\xac\xff\xd0\xff\xaa\xff\x00\x00\xd1\xff\x00\x00\x00\x00\xaf\xff\x00\x00\xe4\xff\xe3\xff\x00\x00\x00\x00\x00\x00\x00\x00\xb0\xff\x00\x00\x00\x00\xc4\xff\x00\x00\xb7\xff\xb8\xff\xb3\xff\x00\x00\xb4\xff\x00\x00\x00\x00\xba\xff\x00\x00\x00\x00\xc0\xff\xbe\xff\x00\x00\x00\x00\x00\x00\x00\x00\xc3\xff\x00\x00\x00\x00\x00\x00\xca\xff\x00\x00\x00\x00\x00\x00\xd8\xff\x00\x00\xda\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe1\xff\xdf\xff\xe0\xff\x00\x00\xe2\xff\xdd\xff\x00\x00\x00\x00\xb4\xff\xd9\xff\xd5\xff\xcc\xff\xcb\xff\xc4\xff\x00\x00\xbf\xff\xbb\xff\x00\x00\x00\x00\xb4\xff\x00\x00\xc3\xff\x00\x00\x00\x00\xc4\xff\xae\xff\x00\x00\x00\x00\x00\x00\xaf\xff\xab\xff\xab\xff\x00\x00\x9f\xff\xa0\xff\x9e\xff\xa1\xff\xa2\xff\xa3\xff\xa6\xff\xa7\xff\xa4\xff\xa5\xff\x9d\xff\xa8\xff\x00\x00\xa9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc5\xff\x00\x00\x00\x00\xb2\xff\xb6\xff\x00\x00\xb9\xff\xbd\xff\xbf\xff\xc2\xff\xc9\xff\xd7\xff\x00\x00\x00\x00\x00\x00\x00\x00\xd9\xff\xb5\xff\x00\x00\xbc\xff\x00\x00\x00\x00\x00\x00\xc7\xff\x00\x00\xd4\xff\x00\x00\x00\x00\xad\xff\xd3\xff\x00\x00\xcd\xff\x00\x00\xc6\xff\x00\x00\x00\x00\x00\x00\xd9\xff\x00\x00\x00\x00\xbb\xff\xc8\xff\xb1\xff\xd2\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc1\xff\xdc\xff\x00\x00\xdb\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x03\x00\x01\x00\x02\x00\x00\x00\x03\x00\x05\x00\x06\x00\x00\x00\x08\x00\x03\x00\x0a\x00\x03\x00\x00\x00\x0d\x00\x0e\x00\x00\x00\x10\x00\x11\x00\x12\x00\x03\x00\x00\x00\x01\x00\x02\x00\x1a\x00\x03\x00\x12\x00\x13\x00\x18\x00\x10\x00\x20\x00\x0a\x00\x0b\x00\x23\x00\x12\x00\x1a\x00\x23\x00\x23\x00\x15\x00\x20\x00\x17\x00\x20\x00\x19\x00\x00\x00\x01\x00\x18\x00\x1d\x00\x00\x00\x1b\x00\x20\x00\x21\x00\x22\x00\x01\x00\x02\x00\x20\x00\x21\x00\x05\x00\x06\x00\x03\x00\x08\x00\x0d\x00\x0a\x00\x03\x00\x14\x00\x0d\x00\x0e\x00\x1a\x00\x10\x00\x11\x00\x12\x00\x00\x00\x01\x00\x03\x00\x00\x00\x00\x00\x01\x00\x13\x00\x07\x00\x08\x00\x18\x00\x00\x00\x1a\x00\x1f\x00\x18\x00\x0f\x00\x1a\x00\x0d\x00\x20\x00\x01\x00\x02\x00\x14\x00\x20\x00\x05\x00\x06\x00\x14\x00\x08\x00\x20\x00\x0a\x00\x12\x00\x13\x00\x0d\x00\x0e\x00\x03\x00\x10\x00\x11\x00\x12\x00\x00\x00\x01\x00\x02\x00\x00\x00\x00\x00\x01\x00\x02\x00\x20\x00\x23\x00\x1c\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x0a\x00\x13\x00\x03\x00\x07\x00\x16\x00\x10\x00\x11\x00\x0a\x00\x1a\x00\x1b\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x00\x00\x01\x00\x02\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x00\x00\x01\x00\x02\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x16\x00\x00\x00\x20\x00\x0a\x00\x0b\x00\x1b\x00\x00\x00\x0a\x00\x0b\x00\x00\x00\x18\x00\x19\x00\x23\x00\x1b\x00\x0d\x00\x00\x00\x19\x00\x18\x00\x0c\x00\x0d\x00\x1b\x00\x18\x00\x0d\x00\x0e\x00\x1b\x00\x00\x00\x01\x00\x02\x00\x16\x00\x00\x00\x01\x00\x02\x00\x00\x00\x12\x00\x13\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x23\x00\x0c\x00\x0d\x00\x0a\x00\x03\x00\x00\x00\x18\x00\x0a\x00\x07\x00\x1b\x00\x18\x00\x16\x00\x03\x00\x1b\x00\x05\x00\x03\x00\x00\x00\x18\x00\x0d\x00\x0e\x00\x1b\x00\x18\x00\x0b\x00\x23\x00\x1b\x00\x00\x00\x01\x00\x02\x00\x00\x00\x00\x00\x01\x00\x02\x00\x10\x00\x11\x00\x00\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x1e\x00\x10\x00\x11\x00\x0a\x00\x0d\x00\x0e\x00\x18\x00\x0a\x00\x07\x00\x1b\x00\x18\x00\x03\x00\x07\x00\x1b\x00\x09\x00\x00\x00\x23\x00\x18\x00\x23\x00\x00\x00\x1b\x00\x18\x00\x20\x00\x07\x00\x1b\x00\x00\x00\x01\x00\x02\x00\x0d\x00\x00\x00\x01\x00\x02\x00\x0d\x00\x0e\x00\x00\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x04\x00\x00\x00\x06\x00\x0a\x00\x0d\x00\x23\x00\x18\x00\x0a\x00\x0c\x00\x1b\x00\x18\x00\x0f\x00\x07\x00\x1b\x00\x0d\x00\x23\x00\x03\x00\x18\x00\x23\x00\x23\x00\x1b\x00\x18\x00\x16\x00\x03\x00\x1b\x00\x00\x00\x01\x00\x02\x00\x04\x00\x00\x00\x01\x00\x02\x00\x04\x00\x0f\x00\x20\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x04\x00\x20\x00\x06\x00\x0a\x00\x20\x00\x04\x00\x18\x00\x0a\x00\x0c\x00\x1b\x00\x18\x00\x0f\x00\x09\x00\x1b\x00\x04\x00\x04\x00\x04\x00\x18\x00\x04\x00\x20\x00\x1b\x00\x18\x00\x03\x00\x20\x00\x1b\x00\x00\x00\x01\x00\x02\x00\x04\x00\x00\x00\x01\x00\x02\x00\x04\x00\x03\x00\x20\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x04\x00\x06\x00\x04\x00\x0a\x00\x0f\x00\x0f\x00\x18\x00\x0a\x00\x05\x00\x1b\x00\x18\x00\x0c\x00\x1c\x00\x1b\x00\x09\x00\x0f\x00\x0c\x00\x18\x00\x1b\x00\x00\x00\x1b\x00\x18\x00\x00\x00\x01\x00\x1b\x00\x00\x00\x01\x00\x02\x00\x0f\x00\x00\x00\x01\x00\x02\x00\x17\x00\x17\x00\x1c\x00\x0a\x00\x00\x00\x00\x00\x01\x00\x0a\x00\x00\x00\x01\x00\x14\x00\x15\x00\x00\x00\x01\x00\x16\x00\x00\x00\x01\x00\x18\x00\x00\x00\x1b\x00\x1b\x00\x18\x00\x07\x00\x08\x00\x1b\x00\x14\x00\x15\x00\x23\x00\x14\x00\x15\x00\x00\x00\x01\x00\x14\x00\x15\x00\x1c\x00\x14\x00\x00\x00\x07\x00\x08\x00\x00\x00\x01\x00\x00\x00\x01\x00\x06\x00\x1c\x00\x17\x00\x07\x00\x08\x00\x07\x00\x1c\x00\x14\x00\x01\x00\x02\x00\x00\x00\x1c\x00\x05\x00\x06\x00\xff\xff\x08\x00\x14\x00\x0a\x00\x14\x00\xff\xff\x0d\x00\x0e\x00\xff\xff\x10\x00\x11\x00\x12\x00\xff\xff\x14\x00\x01\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\x07\x00\x08\x00\xff\xff\x0a\x00\xff\xff\xff\xff\x0d\x00\x0e\x00\xff\xff\x10\x00\x11\x00\x12\x00\x01\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\x08\x00\xff\xff\x0a\x00\xff\xff\x0c\x00\x0d\x00\x0e\x00\xff\xff\x10\x00\x11\x00\x12\x00\x01\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\x08\x00\xff\xff\x0a\x00\xff\xff\x0c\x00\x0d\x00\x0e\x00\xff\xff\x10\x00\x11\x00\x12\x00\x01\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\x08\x00\xff\xff\x0a\x00\xff\xff\x0c\x00\x0d\x00\x0e\x00\xff\xff\x10\x00\x11\x00\x12\x00\x01\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\x08\x00\xff\xff\x0a\x00\xff\xff\x0c\x00\x0d\x00\x0e\x00\xff\xff\x10\x00\x11\x00\x12\x00\x01\x00\x02\x00\xff\xff\x13\x00\x05\x00\x06\x00\x16\x00\x08\x00\xff\xff\x0a\x00\x1a\x00\x1b\x00\x0d\x00\x0e\x00\xff\xff\x10\x00\x11\x00\x12\x00\xff\xff\x23\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x40\x00\x1e\x00\x1f\x00\x2a\x00\x6b\x00\x20\x00\x21\x00\x47\x00\x22\x00\x40\x00\x23\x00\x40\x00\x4b\x00\x24\x00\x25\x00\x47\x00\x26\x00\x27\x00\x28\x00\x2a\x00\x2a\x00\x2b\x00\x2c\x00\x41\x00\x46\x00\x48\x00\x49\x00\x39\x00\x4e\x00\x1c\x00\x55\x00\x56\x00\xff\xff\x4a\x00\x41\x00\xff\xff\xff\xff\x33\x00\x1c\x00\x34\x00\x1c\x00\x35\x00\x41\x00\x42\x00\x2e\x00\x36\x00\x3b\x00\x31\x00\x1c\x00\x37\x00\x38\x00\x1e\x00\x1f\x00\x1c\x00\x37\x00\x20\x00\x21\x00\x40\x00\x22\x00\x53\x00\x23\x00\x40\x00\x46\x00\x24\x00\x25\x00\x41\x00\x26\x00\x27\x00\x28\x00\x41\x00\x42\x00\x6b\x00\x3b\x00\x41\x00\x42\x00\x51\x00\x59\x00\x5a\x00\x94\x00\x47\x00\x41\x00\xb7\x00\xb3\x00\xae\x00\x41\x00\x76\x00\x1c\x00\x1e\x00\x1f\x00\x5b\x00\x1c\x00\x20\x00\x21\x00\x96\x00\x22\x00\x1c\x00\x23\x00\x48\x00\x9d\x00\x24\x00\x25\x00\x2a\x00\x26\x00\x27\x00\x28\x00\x2a\x00\x2b\x00\x2c\x00\x4b\x00\x2a\x00\x2b\x00\x2c\x00\x1c\x00\xff\xff\x93\x00\x2d\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x51\x00\x82\x00\x81\x00\x5f\x00\x4c\x00\x4d\x00\x2d\x00\x41\x00\x60\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x2e\x00\x2f\x00\x90\x00\x31\x00\x2a\x00\x2b\x00\x2c\x00\x2e\x00\x2f\x00\x8f\x00\x31\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2a\x00\x2b\x00\x2c\x00\x5f\x00\x3b\x00\x1c\x00\x55\x00\x82\x00\x60\x00\x3b\x00\x55\x00\xa1\x00\x3b\x00\x2e\x00\x38\x00\xff\xff\x31\x00\x69\x00\x47\x00\x80\x00\x2e\x00\x3c\x00\x3d\x00\x31\x00\x2e\x00\x51\x00\x52\x00\x31\x00\x2a\x00\x2b\x00\x2c\x00\x3e\x00\x2a\x00\x2b\x00\x2c\x00\x3b\x00\x48\x00\xc3\x00\x57\x00\x2a\x00\x2b\x00\x2c\x00\x8d\x00\x2a\x00\x2b\x00\x2c\x00\xff\xff\x3c\x00\x3d\x00\x8c\x00\x6b\x00\x3b\x00\x2e\x00\x8b\x00\x76\x00\x31\x00\x2e\x00\x7b\x00\x65\x00\x31\x00\x66\x00\x7b\x00\x4b\x00\x2e\x00\x77\x00\x78\x00\x31\x00\x2e\x00\x7a\x00\xff\xff\x31\x00\x2a\x00\x2b\x00\x2c\x00\x4b\x00\x2a\x00\x2b\x00\x2c\x00\x4c\x00\x9e\x00\x3b\x00\x8a\x00\x2a\x00\x2b\x00\x2c\x00\x89\x00\x2a\x00\x2b\x00\x2c\x00\x73\x00\x4c\x00\xa9\x00\x88\x00\x51\x00\xa0\x00\x2e\x00\x87\x00\x72\x00\x31\x00\x2e\x00\x74\x00\x70\x00\x31\x00\x99\x00\x3b\x00\xff\xff\x2e\x00\xff\xff\x3b\x00\x31\x00\x2e\x00\x1c\x00\x70\x00\x31\x00\x2a\x00\x2b\x00\x2c\x00\xa5\x00\x2a\x00\x2b\x00\x2c\x00\x51\x00\x95\x00\x3b\x00\x86\x00\x2a\x00\x2b\x00\x2c\x00\x85\x00\x2a\x00\x2b\x00\x2c\x00\x61\x00\x3b\x00\x62\x00\x84\x00\x9c\x00\xff\xff\x2e\x00\x83\x00\x63\x00\x31\x00\x2e\x00\x64\x00\x6c\x00\x31\x00\xac\x00\xff\xff\x6b\x00\x2e\x00\xff\xff\xff\xff\x31\x00\x2e\x00\x69\x00\xa0\x00\x31\x00\x2a\x00\x2b\x00\x2c\x00\x9c\x00\x2a\x00\x2b\x00\x2c\x00\x98\x00\xac\x00\x1c\x00\x7e\x00\x2a\x00\x2b\x00\x2c\x00\xb1\x00\x2a\x00\x2b\x00\x2c\x00\x67\x00\x1c\x00\x62\x00\xb0\x00\x1c\x00\x8f\x00\x2e\x00\xaf\x00\x63\x00\x31\x00\x2e\x00\x64\x00\x95\x00\x31\x00\xb4\x00\xaf\x00\xab\x00\x2e\x00\xa9\x00\x1c\x00\x31\x00\x2e\x00\xa8\x00\x1c\x00\x31\x00\x2a\x00\x2b\x00\x2c\x00\xbb\x00\x2a\x00\x2b\x00\x2c\x00\xb9\x00\xbd\x00\x1c\x00\xb9\x00\x2a\x00\x2b\x00\x2c\x00\xb7\x00\x2a\x00\x2b\x00\x2c\x00\xbf\x00\x5d\x00\xc6\x00\xb4\x00\xc0\x00\xc5\x00\x2e\x00\xc2\x00\x60\x00\x31\x00\x2e\x00\xc9\x00\x1c\x00\x31\x00\x58\x00\xc7\x00\x54\x00\x2e\x00\x28\x00\x7c\x00\x31\x00\x2e\x00\x41\x00\x42\x00\x31\x00\x2a\x00\x2b\x00\x2c\x00\x4f\x00\x2a\x00\x2b\x00\x2c\x00\x3a\x00\x7d\x00\x6d\x00\xc7\x00\x70\x00\x41\x00\x42\x00\xca\x00\x41\x00\x42\x00\x43\x00\x44\x00\x41\x00\x42\x00\x5f\x00\x41\x00\x42\x00\x2e\x00\xa4\x00\x60\x00\x31\x00\x2e\x00\x59\x00\xa2\x00\x31\x00\x43\x00\x74\x00\xff\xff\x43\x00\xa3\x00\x41\x00\x42\x00\x43\x00\x9a\x00\x6d\x00\x5b\x00\x99\x00\x59\x00\xbb\x00\x41\x00\x42\x00\x41\x00\x42\x00\x6c\x00\x6d\x00\x91\x00\x59\x00\xbd\x00\x5c\x00\x6d\x00\x5b\x00\x1e\x00\x1f\x00\xa6\x00\x6d\x00\x20\x00\x21\x00\x00\x00\x22\x00\x5b\x00\x23\x00\x5b\x00\x00\x00\x24\x00\x25\x00\x00\x00\x26\x00\x27\x00\x28\x00\x00\x00\xb6\x00\x1e\x00\x1f\x00\x00\x00\x00\x00\x20\x00\x21\x00\x6f\x00\x22\x00\x00\x00\x23\x00\x00\x00\x00\x00\x24\x00\x25\x00\x00\x00\x26\x00\x27\x00\x28\x00\x1e\x00\x1f\x00\x00\x00\x00\x00\x20\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\xc2\x00\x24\x00\x25\x00\x00\x00\x26\x00\x27\x00\x28\x00\x1e\x00\x1f\x00\x00\x00\x00\x00\x20\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\xc1\x00\x24\x00\x25\x00\x00\x00\x26\x00\x27\x00\x28\x00\x1e\x00\x1f\x00\x00\x00\x00\x00\x20\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\xca\x00\x24\x00\x25\x00\x00\x00\x26\x00\x27\x00\x28\x00\x1e\x00\x1f\x00\x00\x00\x00\x00\x20\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\xcc\x00\x24\x00\x25\x00\x00\x00\x26\x00\x27\x00\x28\x00\x1e\x00\x1f\x00\x00\x00\x51\x00\x20\x00\x21\x00\x5f\x00\x22\x00\x00\x00\x23\x00\x41\x00\x60\x00\x24\x00\x25\x00\x00\x00\x26\x00\x27\x00\x28\x00\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (26, 98) [
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98)
	]

happy_n_terms = 36 :: Int
happy_n_nonterms = 29 :: Int

happyReduce_26 = happySpecReduce_1  0# happyReduction_26
happyReduction_26 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn29
		 ((Just (tokenLineCol happy_var_1), Ident (prToken happy_var_1))
	)}

happyReduce_27 = happySpecReduce_1  1# happyReduction_27
happyReduction_27 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn30
		 ((Just (tokenLineCol happy_var_1), read (prToken happy_var_1))
	)}

happyReduce_28 = happySpecReduce_1  2# happyReduction_28
happyReduction_28 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn31
		 ((Just (tokenLineCol happy_var_1), prToken happy_var_1)
	)}

happyReduce_29 = happySpecReduce_1  3# happyReduction_29
happyReduction_29 happy_x_1
	 =  case happyOut34 happy_x_1 of { (HappyWrap34 happy_var_1) -> 
	happyIn32
		 ((fst happy_var_1, AbsHpl.ProgramB (fst happy_var_1)(reverse (snd happy_var_1)))
	)}

happyReduce_30 = happySpecReduce_1  4# happyReduction_30
happyReduction_30 happy_x_1
	 =  case happyOut35 happy_x_1 of { (HappyWrap35 happy_var_1) -> 
	happyIn33
		 ((fst happy_var_1, AbsHpl.FunctionOrRefOrTypeF (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_31 = happySpecReduce_1  4# happyReduction_31
happyReduction_31 happy_x_1
	 =  case happyOut44 happy_x_1 of { (HappyWrap44 happy_var_1) -> 
	happyIn33
		 ((fst happy_var_1, AbsHpl.FunctionOrRefOrTypeT (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_32 = happySpecReduce_1  4# happyReduction_32
happyReduction_32 happy_x_1
	 =  case happyOut41 happy_x_1 of { (HappyWrap41 happy_var_1) -> 
	happyIn33
		 ((fst happy_var_1, AbsHpl.FunctionOrRefOrTypeR (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_33 = happySpecReduce_0  5# happyReduction_33
happyReduction_33  =  happyIn34
		 ((Nothing, [])
	)

happyReduce_34 = happySpecReduce_2  5# happyReduction_34
happyReduction_34 happy_x_2
	happy_x_1
	 =  case happyOut34 happy_x_1 of { (HappyWrap34 happy_var_1) -> 
	case happyOut33 happy_x_2 of { (HappyWrap33 happy_var_2) -> 
	happyIn34
		 ((fst happy_var_1, flip (:) (snd happy_var_1)(snd happy_var_2))
	)}}

happyReduce_35 = happyReduce 9# 6# happyReduction_35
happyReduction_35 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut42 happy_x_2 of { (HappyWrap42 happy_var_2) -> 
	case happyOut29 happy_x_3 of { (HappyWrap29 happy_var_3) -> 
	case happyOut37 happy_x_5 of { (HappyWrap37 happy_var_5) -> 
	case happyOut39 happy_x_8 of { (HappyWrap39 happy_var_8) -> 
	happyIn35
		 ((Just (tokenLineCol happy_var_1), AbsHpl.FunctionDefB (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_3)(snd happy_var_5)(snd happy_var_8))
	) `HappyStk` happyRest}}}}}

happyReduce_36 = happyReduce 10# 6# happyReduction_36
happyReduction_36 (happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut42 happy_x_3 of { (HappyWrap42 happy_var_3) -> 
	case happyOut29 happy_x_4 of { (HappyWrap29 happy_var_4) -> 
	case happyOut37 happy_x_6 of { (HappyWrap37 happy_var_6) -> 
	case happyOut39 happy_x_9 of { (HappyWrap39 happy_var_9) -> 
	happyIn35
		 ((Just (tokenLineCol happy_var_1), AbsHpl.SusFunctionDef (Just (tokenLineCol happy_var_1)) (snd happy_var_3)(snd happy_var_4)(snd happy_var_6)(snd happy_var_9))
	) `HappyStk` happyRest}}}}}

happyReduce_37 = happySpecReduce_1  7# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	happyIn36
		 ((fst happy_var_1, AbsHpl.FunctionArgB (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_38 = happySpecReduce_0  8# happyReduction_38
happyReduction_38  =  happyIn37
		 ((Nothing, [])
	)

happyReduce_39 = happySpecReduce_1  8# happyReduction_39
happyReduction_39 happy_x_1
	 =  case happyOut36 happy_x_1 of { (HappyWrap36 happy_var_1) -> 
	happyIn37
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_40 = happySpecReduce_3  8# happyReduction_40
happyReduction_40 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { (HappyWrap36 happy_var_1) -> 
	case happyOut37 happy_x_3 of { (HappyWrap37 happy_var_3) -> 
	happyIn37
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_41 = happySpecReduce_0  9# happyReduction_41
happyReduction_41  =  happyIn38
		 ((Nothing, [])
	)

happyReduce_42 = happySpecReduce_2  9# happyReduction_42
happyReduction_42 happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { (HappyWrap38 happy_var_1) -> 
	case happyOut35 happy_x_2 of { (HappyWrap35 happy_var_2) -> 
	happyIn38
		 ((fst happy_var_1, flip (:) (snd happy_var_1)(snd happy_var_2))
	)}}

happyReduce_43 = happyReduce 4# 10# happyReduction_43
happyReduction_43 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut52 happy_x_2 of { (HappyWrap52 happy_var_2) -> 
	case happyOut39 happy_x_4 of { (HappyWrap39 happy_var_4) -> 
	happyIn39
		 ((Just (tokenLineCol happy_var_1), AbsHpl.ValueStatementB (Just (tokenLineCol happy_var_1)) (reverse (snd happy_var_2)) (snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_44 = happyReduce 5# 10# happyReduction_44
happyReduction_44 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut52 happy_x_3 of { (HappyWrap52 happy_var_3) -> 
	case happyOut39 happy_x_5 of { (HappyWrap39 happy_var_5) -> 
	happyIn39
		 ((Just (tokenLineCol happy_var_1), AbsHpl.ForceValueStatement (Just (tokenLineCol happy_var_1)) (reverse (snd happy_var_3)) (snd happy_var_5))
	) `HappyStk` happyRest}}}

happyReduce_45 = happyReduce 6# 10# happyReduction_45
happyReduction_45 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_2 of { (HappyWrap39 happy_var_2) -> 
	case happyOut39 happy_x_4 of { (HappyWrap39 happy_var_4) -> 
	case happyOut39 happy_x_6 of { (HappyWrap39 happy_var_6) -> 
	happyIn39
		 ((Just (tokenLineCol happy_var_1), AbsHpl.IfValueStatement (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_4)(snd happy_var_6))
	) `HappyStk` happyRest}}}}

happyReduce_46 = happySpecReduce_1  10# happyReduction_46
happyReduction_46 happy_x_1
	 =  case happyOut56 happy_x_1 of { (HappyWrap56 happy_var_1) -> 
	happyIn39
		 ((fst happy_var_1, AbsHpl.TValueStatement (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_47 = happySpecReduce_1  10# happyReduction_47
happyReduction_47 happy_x_1
	 =  case happyOut53 happy_x_1 of { (HappyWrap53 happy_var_1) -> 
	happyIn39
		 ((fst happy_var_1, AbsHpl.AValueStatement (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_48 = happySpecReduce_1  10# happyReduction_48
happyReduction_48 happy_x_1
	 =  case happyOut30 happy_x_1 of { (HappyWrap30 happy_var_1) -> 
	happyIn39
		 ((fst happy_var_1, AbsHpl.IValueStatement (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_49 = happySpecReduce_1  10# happyReduction_49
happyReduction_49 happy_x_1
	 =  case happyOut31 happy_x_1 of { (HappyWrap31 happy_var_1) -> 
	happyIn39
		 ((fst happy_var_1, AbsHpl.LitStrValueStatement (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_50 = happyReduce 5# 10# happyReduction_50
happyReduction_50 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut29 happy_x_2 of { (HappyWrap29 happy_var_2) -> 
	case happyOut39 happy_x_4 of { (HappyWrap39 happy_var_4) -> 
	happyIn39
		 ((Just (tokenLineCol happy_var_1), AbsHpl.FValueStatement (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_51 = happySpecReduce_2  10# happyReduction_51
happyReduction_51 happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { (HappyWrap39 happy_var_1) -> 
	case happyOut57 happy_x_2 of { (HappyWrap57 happy_var_2) -> 
	happyIn39
		 ((fst happy_var_1, AbsHpl.Expr (fst happy_var_1)(snd happy_var_1)(snd happy_var_2))
	)}}

happyReduce_52 = happySpecReduce_0  11# happyReduction_52
happyReduction_52  =  happyIn40
		 ((Nothing, [])
	)

happyReduce_53 = happySpecReduce_1  11# happyReduction_53
happyReduction_53 happy_x_1
	 =  case happyOut39 happy_x_1 of { (HappyWrap39 happy_var_1) -> 
	happyIn40
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_54 = happySpecReduce_3  11# happyReduction_54
happyReduction_54 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { (HappyWrap39 happy_var_1) -> 
	case happyOut40 happy_x_3 of { (HappyWrap40 happy_var_3) -> 
	happyIn40
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_55 = happyReduce 6# 12# happyReduction_55
happyReduction_55 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut42 happy_x_2 of { (HappyWrap42 happy_var_2) -> 
	case happyOut29 happy_x_3 of { (HappyWrap29 happy_var_3) -> 
	case happyOut39 happy_x_5 of { (HappyWrap39 happy_var_5) -> 
	happyIn41
		 ((Just (tokenLineCol happy_var_1), AbsHpl.RefDefB (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_3)(snd happy_var_5))
	) `HappyStk` happyRest}}}}

happyReduce_56 = happyReduce 4# 13# happyReduction_56
happyReduction_56 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	case happyOut43 happy_x_3 of { (HappyWrap43 happy_var_3) -> 
	happyIn42
		 ((fst happy_var_1, AbsHpl.TypeB (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_57 = happyReduce 5# 13# happyReduction_57
happyReduction_57 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut42 happy_x_2 of { (HappyWrap42 happy_var_2) -> 
	case happyOut42 happy_x_4 of { (HappyWrap42 happy_var_4) -> 
	happyIn42
		 ((Just (tokenLineCol happy_var_1), AbsHpl.FunType (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_58 = happySpecReduce_3  13# happyReduction_58
happyReduction_58 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { (HappyWrap43 happy_var_2) -> 
	happyIn42
		 ((Just (tokenLineCol happy_var_1), AbsHpl.TType (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_59 = happySpecReduce_0  14# happyReduction_59
happyReduction_59  =  happyIn43
		 ((Nothing, [])
	)

happyReduce_60 = happySpecReduce_1  14# happyReduction_60
happyReduction_60 happy_x_1
	 =  case happyOut42 happy_x_1 of { (HappyWrap42 happy_var_1) -> 
	happyIn43
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_61 = happySpecReduce_3  14# happyReduction_61
happyReduction_61 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { (HappyWrap42 happy_var_1) -> 
	case happyOut43 happy_x_3 of { (HappyWrap43 happy_var_3) -> 
	happyIn43
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_62 = happyReduce 8# 15# happyReduction_62
happyReduction_62 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut29 happy_x_2 of { (HappyWrap29 happy_var_2) -> 
	case happyOut46 happy_x_4 of { (HappyWrap46 happy_var_4) -> 
	case happyOut48 happy_x_7 of { (HappyWrap48 happy_var_7) -> 
	happyIn44
		 ((Just (tokenLineCol happy_var_1), AbsHpl.AlgTypeB (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_4)(snd happy_var_7))
	) `HappyStk` happyRest}}}}

happyReduce_63 = happySpecReduce_1  16# happyReduction_63
happyReduction_63 happy_x_1
	 =  case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	happyIn45
		 ((fst happy_var_1, AbsHpl.TypeArgB (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_64 = happySpecReduce_0  17# happyReduction_64
happyReduction_64  =  happyIn46
		 ((Nothing, [])
	)

happyReduce_65 = happySpecReduce_1  17# happyReduction_65
happyReduction_65 happy_x_1
	 =  case happyOut45 happy_x_1 of { (HappyWrap45 happy_var_1) -> 
	happyIn46
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_66 = happySpecReduce_3  17# happyReduction_66
happyReduction_66 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_1 of { (HappyWrap45 happy_var_1) -> 
	case happyOut46 happy_x_3 of { (HappyWrap46 happy_var_3) -> 
	happyIn46
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_67 = happyReduce 4# 18# happyReduction_67
happyReduction_67 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	case happyOut42 happy_x_3 of { (HappyWrap42 happy_var_3) -> 
	happyIn47
		 ((fst happy_var_1, AbsHpl.AlgTypeValB (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_68 = happySpecReduce_0  19# happyReduction_68
happyReduction_68  =  happyIn48
		 ((Nothing, [])
	)

happyReduce_69 = happySpecReduce_1  19# happyReduction_69
happyReduction_69 happy_x_1
	 =  case happyOut47 happy_x_1 of { (HappyWrap47 happy_var_1) -> 
	happyIn48
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_70 = happySpecReduce_3  19# happyReduction_70
happyReduction_70 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut47 happy_x_1 of { (HappyWrap47 happy_var_1) -> 
	case happyOut48 happy_x_3 of { (HappyWrap48 happy_var_3) -> 
	happyIn48
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_71 = happySpecReduce_1  20# happyReduction_71
happyReduction_71 happy_x_1
	 =  case happyOut30 happy_x_1 of { (HappyWrap30 happy_var_1) -> 
	happyIn49
		 ((fst happy_var_1, AbsHpl.PatternMatchI (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_72 = happySpecReduce_1  20# happyReduction_72
happyReduction_72 happy_x_1
	 =  case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	happyIn49
		 ((fst happy_var_1, AbsHpl.PatternMatchB (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_73 = happySpecReduce_3  20# happyReduction_73
happyReduction_73 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut50 happy_x_2 of { (HappyWrap50 happy_var_2) -> 
	happyIn49
		 ((Just (tokenLineCol happy_var_1), AbsHpl.TPatternMatch (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_74 = happyReduce 4# 20# happyReduction_74
happyReduction_74 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	case happyOut50 happy_x_3 of { (HappyWrap50 happy_var_3) -> 
	happyIn49
		 ((fst happy_var_1, AbsHpl.CPatternMatch (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_75 = happySpecReduce_0  21# happyReduction_75
happyReduction_75  =  happyIn50
		 ((Nothing, [])
	)

happyReduce_76 = happySpecReduce_1  21# happyReduction_76
happyReduction_76 happy_x_1
	 =  case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	happyIn50
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_77 = happySpecReduce_3  21# happyReduction_77
happyReduction_77 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	case happyOut50 happy_x_3 of { (HappyWrap50 happy_var_3) -> 
	happyIn50
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_78 = happyReduce 6# 22# happyReduction_78
happyReduction_78 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut42 happy_x_1 of { (HappyWrap42 happy_var_1) -> 
	case happyOut49 happy_x_3 of { (HappyWrap49 happy_var_3) -> 
	case happyOut39 happy_x_5 of { (HappyWrap39 happy_var_5) -> 
	happyIn51
		 ((fst happy_var_1, AbsHpl.AssignmentB (fst happy_var_1)(snd happy_var_1)(snd happy_var_3)(snd happy_var_5))
	) `HappyStk` happyRest}}}

happyReduce_79 = happySpecReduce_1  22# happyReduction_79
happyReduction_79 happy_x_1
	 =  case happyOut41 happy_x_1 of { (HappyWrap41 happy_var_1) -> 
	happyIn51
		 ((fst happy_var_1, AbsHpl.RefAssignment (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_80 = happySpecReduce_0  23# happyReduction_80
happyReduction_80  =  happyIn52
		 ((Nothing, [])
	)

happyReduce_81 = happySpecReduce_2  23# happyReduction_81
happyReduction_81 happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_1 of { (HappyWrap52 happy_var_1) -> 
	case happyOut51 happy_x_2 of { (HappyWrap51 happy_var_2) -> 
	happyIn52
		 ((fst happy_var_1, flip (:) (snd happy_var_1)(snd happy_var_2))
	)}}

happyReduce_82 = happyReduce 4# 24# happyReduction_82
happyReduction_82 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	case happyOut55 happy_x_3 of { (HappyWrap55 happy_var_3) -> 
	happyIn53
		 ((fst happy_var_1, AbsHpl.FunApplicationB (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_83 = happySpecReduce_1  25# happyReduction_83
happyReduction_83 happy_x_1
	 =  case happyOut39 happy_x_1 of { (HappyWrap39 happy_var_1) -> 
	happyIn54
		 ((fst happy_var_1, AbsHpl.FunctionArgApplB (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_84 = happySpecReduce_0  26# happyReduction_84
happyReduction_84  =  happyIn55
		 ((Nothing, [])
	)

happyReduce_85 = happySpecReduce_1  26# happyReduction_85
happyReduction_85 happy_x_1
	 =  case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	happyIn55
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_86 = happySpecReduce_3  26# happyReduction_86
happyReduction_86 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOut55 happy_x_3 of { (HappyWrap55 happy_var_3) -> 
	happyIn55
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_87 = happySpecReduce_3  27# happyReduction_87
happyReduction_87 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut40 happy_x_2 of { (HappyWrap40 happy_var_2) -> 
	happyIn56
		 ((Just (tokenLineCol happy_var_1), AbsHpl.TupleValueStatementB (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_88 = happySpecReduce_2  28# happyReduction_88
happyReduction_88 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_2 of { (HappyWrap39 happy_var_2) -> 
	happyIn57
		 ((Just (tokenLineCol happy_var_1), AbsHpl.EAdd (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_89 = happySpecReduce_2  28# happyReduction_89
happyReduction_89 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_2 of { (HappyWrap39 happy_var_2) -> 
	happyIn57
		 ((Just (tokenLineCol happy_var_1), AbsHpl.ESub (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_90 = happySpecReduce_2  28# happyReduction_90
happyReduction_90 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_2 of { (HappyWrap39 happy_var_2) -> 
	happyIn57
		 ((Just (tokenLineCol happy_var_1), AbsHpl.EMod (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_91 = happySpecReduce_2  28# happyReduction_91
happyReduction_91 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_2 of { (HappyWrap39 happy_var_2) -> 
	happyIn57
		 ((Just (tokenLineCol happy_var_1), AbsHpl.EMul (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_92 = happySpecReduce_2  28# happyReduction_92
happyReduction_92 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_2 of { (HappyWrap39 happy_var_2) -> 
	happyIn57
		 ((Just (tokenLineCol happy_var_1), AbsHpl.EDiv (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_93 = happySpecReduce_2  28# happyReduction_93
happyReduction_93 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_2 of { (HappyWrap39 happy_var_2) -> 
	happyIn57
		 ((Just (tokenLineCol happy_var_1), AbsHpl.EL (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_94 = happySpecReduce_2  28# happyReduction_94
happyReduction_94 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_2 of { (HappyWrap39 happy_var_2) -> 
	happyIn57
		 ((Just (tokenLineCol happy_var_1), AbsHpl.ELQ (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_95 = happySpecReduce_2  28# happyReduction_95
happyReduction_95 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_2 of { (HappyWrap39 happy_var_2) -> 
	happyIn57
		 ((Just (tokenLineCol happy_var_1), AbsHpl.EG (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_96 = happySpecReduce_2  28# happyReduction_96
happyReduction_96 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_2 of { (HappyWrap39 happy_var_2) -> 
	happyIn57
		 ((Just (tokenLineCol happy_var_1), AbsHpl.EGQ (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_97 = happySpecReduce_2  28# happyReduction_97
happyReduction_97 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_2 of { (HappyWrap39 happy_var_2) -> 
	happyIn57
		 ((Just (tokenLineCol happy_var_1), AbsHpl.EEQ (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_98 = happySpecReduce_2  28# happyReduction_98
happyReduction_98 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_2 of { (HappyWrap39 happy_var_2) -> 
	happyIn57
		 ((Just (tokenLineCol happy_var_1), AbsHpl.ENE (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyNewToken action sts stk [] =
	happyDoAction 35# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TS _ 31) -> cont 31#;
	PT _ (TV _) -> cont 32#;
	PT _ (TI _) -> cont 33#;
	PT _ (TL _) -> cont 34#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 35# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => ([(Token)], [String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pProgram_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap32 x') = happyOut32 x} in x'))

pFunctionOrRefOrType_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (let {(HappyWrap33 x') = happyOut33 x} in x'))

pListFunctionOrRefOrType_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (let {(HappyWrap34 x') = happyOut34 x} in x'))

pFunctionDef_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (let {(HappyWrap35 x') = happyOut35 x} in x'))

pFunctionArg_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (let {(HappyWrap36 x') = happyOut36 x} in x'))

pListFunctionArg_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (let {(HappyWrap37 x') = happyOut37 x} in x'))

pListFunctionDef_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (let {(HappyWrap38 x') = happyOut38 x} in x'))

pValueStatement_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (let {(HappyWrap39 x') = happyOut39 x} in x'))

pListValueStatement_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (let {(HappyWrap40 x') = happyOut40 x} in x'))

pRefDef_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (let {(HappyWrap41 x') = happyOut41 x} in x'))

pType_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (let {(HappyWrap42 x') = happyOut42 x} in x'))

pListType_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (let {(HappyWrap43 x') = happyOut43 x} in x'))

pAlgType_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 12# tks) (\x -> happyReturn (let {(HappyWrap44 x') = happyOut44 x} in x'))

pTypeArg_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 13# tks) (\x -> happyReturn (let {(HappyWrap45 x') = happyOut45 x} in x'))

pListTypeArg_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 14# tks) (\x -> happyReturn (let {(HappyWrap46 x') = happyOut46 x} in x'))

pAlgTypeVal_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 15# tks) (\x -> happyReturn (let {(HappyWrap47 x') = happyOut47 x} in x'))

pListAlgTypeVal_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 16# tks) (\x -> happyReturn (let {(HappyWrap48 x') = happyOut48 x} in x'))

pPatternMatch_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 17# tks) (\x -> happyReturn (let {(HappyWrap49 x') = happyOut49 x} in x'))

pListPatternMatch_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 18# tks) (\x -> happyReturn (let {(HappyWrap50 x') = happyOut50 x} in x'))

pAssignment_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 19# tks) (\x -> happyReturn (let {(HappyWrap51 x') = happyOut51 x} in x'))

pListAssignment_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 20# tks) (\x -> happyReturn (let {(HappyWrap52 x') = happyOut52 x} in x'))

pFunApplication_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 21# tks) (\x -> happyReturn (let {(HappyWrap53 x') = happyOut53 x} in x'))

pFunctionArgAppl_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 22# tks) (\x -> happyReturn (let {(HappyWrap54 x') = happyOut54 x} in x'))

pListFunctionArgAppl_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 23# tks) (\x -> happyReturn (let {(HappyWrap55 x') = happyOut55 x} in x'))

pTupleValueStatementr_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 24# tks) (\x -> happyReturn (let {(HappyWrap56 x') = happyOut56 x} in x'))

pValueStatementExpr_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 25# tks) (\x -> happyReturn (let {(HappyWrap57 x') = happyOut57 x} in x'))

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    t:_ -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens

pProgram = (>>= return . snd) . pProgram_internal
pFunctionOrRefOrType = (>>= return . snd) . pFunctionOrRefOrType_internal
pListFunctionOrRefOrType = (>>= return . snd) . pListFunctionOrRefOrType_internal
pFunctionDef = (>>= return . snd) . pFunctionDef_internal
pFunctionArg = (>>= return . snd) . pFunctionArg_internal
pListFunctionArg = (>>= return . snd) . pListFunctionArg_internal
pListFunctionDef = (>>= return . snd) . pListFunctionDef_internal
pValueStatement = (>>= return . snd) . pValueStatement_internal
pListValueStatement = (>>= return . snd) . pListValueStatement_internal
pRefDef = (>>= return . snd) . pRefDef_internal
pType = (>>= return . snd) . pType_internal
pListType = (>>= return . snd) . pListType_internal
pAlgType = (>>= return . snd) . pAlgType_internal
pTypeArg = (>>= return . snd) . pTypeArg_internal
pListTypeArg = (>>= return . snd) . pListTypeArg_internal
pAlgTypeVal = (>>= return . snd) . pAlgTypeVal_internal
pListAlgTypeVal = (>>= return . snd) . pListAlgTypeVal_internal
pPatternMatch = (>>= return . snd) . pPatternMatch_internal
pListPatternMatch = (>>= return . snd) . pListPatternMatch_internal
pAssignment = (>>= return . snd) . pAssignment_internal
pListAssignment = (>>= return . snd) . pListAssignment_internal
pFunApplication = (>>= return . snd) . pFunApplication_internal
pFunctionArgAppl = (>>= return . snd) . pFunctionArgAppl_internal
pListFunctionArgAppl = (>>= return . snd) . pListFunctionArgAppl_internal
pTupleValueStatementr = (>>= return . snd) . pTupleValueStatementr_internal
pValueStatementExpr = (>>= return . snd) . pValueStatementExpr_internal
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif



















data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








































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
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)













-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
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
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ((Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

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
