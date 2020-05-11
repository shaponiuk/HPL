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

-- parser produced by Happy Version 1.19.8

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn30 :: (Ident) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> (Ident)
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: (Integer) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> (Integer)
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: (String) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> (String)
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: (Program) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> (Program)
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: (FunctionOrRefOrType) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> (FunctionOrRefOrType)
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: ([FunctionOrRefOrType]) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> ([FunctionOrRefOrType])
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: (FunctionDef) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> (FunctionDef)
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: (FunctionArg) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> (FunctionArg)
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: ([FunctionArg]) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> ([FunctionArg])
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyIn39 :: ([FunctionDef]) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> ([FunctionDef])
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: (ValueStatement) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> (ValueStatement)
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: ([ValueStatement]) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> ([ValueStatement])
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyIn42 :: (RefDef) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> (RefDef)
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
happyIn43 :: (Type) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> (Type)
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
happyIn44 :: ([Type]) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> ([Type])
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
happyIn45 :: (AlgType) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> (AlgType)
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
happyIn46 :: (TypeArg) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> (TypeArg)
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
happyIn47 :: ([TypeArg]) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> ([TypeArg])
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
happyIn48 :: (AlgTypeVal) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> (AlgTypeVal)
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
happyIn49 :: ([AlgTypeVal]) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> ([AlgTypeVal])
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
happyIn50 :: (PatternMatch) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> (PatternMatch)
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
happyIn51 :: ([PatternMatch]) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> ([PatternMatch])
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
happyIn52 :: (Assignment) -> (HappyAbsSyn )
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> (Assignment)
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
happyIn53 :: ([Assignment]) -> (HappyAbsSyn )
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> ([Assignment])
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
happyIn54 :: (FunApplication) -> (HappyAbsSyn )
happyIn54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> (FunApplication)
happyOut54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut54 #-}
happyIn55 :: (FunctionArgAppl) -> (HappyAbsSyn )
happyIn55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn ) -> (FunctionArgAppl)
happyOut55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut55 #-}
happyIn56 :: ([FunctionArgAppl]) -> (HappyAbsSyn )
happyIn56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn ) -> ([FunctionArgAppl])
happyOut56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut56 #-}
happyIn57 :: (ListValueStatementr) -> (HappyAbsSyn )
happyIn57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn57 #-}
happyOut57 :: (HappyAbsSyn ) -> (ListValueStatementr)
happyOut57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut57 #-}
happyIn58 :: (TupleValueStatementr) -> (HappyAbsSyn )
happyIn58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn58 #-}
happyOut58 :: (HappyAbsSyn ) -> (TupleValueStatementr)
happyOut58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut58 #-}
happyIn59 :: (ValueStatementExpr) -> (HappyAbsSyn )
happyIn59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn59 #-}
happyOut59 :: (HappyAbsSyn ) -> (ValueStatementExpr)
happyOut59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut59 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x12\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x08\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x20\x2a\x72\x00\x00\x00\x00\x00\x00\x00\x20\x00\x40\x54\xe4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\xc0\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x80\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x04\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x08\x00\x10\x15\x39\x00\x00\x00\x00\x00\x00\x00\x10\x00\x20\x2a\x72\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x96\xec\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x10\x15\x39\x00\x00\x00\x00\x00\x00\x00\x10\x00\x20\x2a\x72\x00\x00\x00\x00\x00\x00\x00\x20\x00\x40\x54\xe4\x00\x00\x00\x00\x00\x00\x00\x40\x00\x80\xa8\xc8\x01\x00\x00\x00\x00\x00\x00\x80\x00\x00\x51\x91\x03\x00\x00\x00\x00\x00\x00\x00\x01\x00\xa2\x22\x07\x00\x00\x00\x00\x00\x00\x00\x02\x00\x44\x45\x0e\x00\x00\x00\x00\x00\x00\x00\x04\x00\x88\x8a\x1c\x00\x00\x00\x00\x00\x00\x00\x08\x00\x10\x15\x39\x00\x00\x00\x00\x00\x00\x00\x10\x00\x20\x2a\x72\x00\x00\x00\x00\x00\x00\x00\x20\x00\x40\x54\xe4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x51\x91\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x44\x45\x0e\x00\x00\x00\x00\x00\x00\x00\x04\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x98\x25\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x20\x2a\x72\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x20\x08\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\xc0\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x4f\x76\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x2c\xd9\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x10\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x90\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x24\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x51\x91\x03\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\xc0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x40\x41\x00\x00\x00\x00\x00\x00\x00\x60\x96\xec\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x44\x45\x0e\x00\x00\x00\x00\x00\x00\x00\x04\x00\x88\x8a\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x40\x41\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x51\x91\x03\x00\x00\x00\x00\x00\x00\x00\x01\x00\xa2\x22\x07\x00\x00\x00\x00\x00\x00\x00\x02\x00\x44\x45\x0e\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\xa2\x22\x07\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x88\x8a\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcc\x92\x1d\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x4b\x76\x04\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x51\x91\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x88\x8a\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcc\xd2\x1d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x4b\x77\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x44\x45\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x10\x15\x39\x00\x00\x00\x00\x00\x00\x00\xcc\xd2\x1d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x96\xee\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram","%start_pFunctionOrRefOrType","%start_pListFunctionOrRefOrType","%start_pFunctionDef","%start_pFunctionArg","%start_pListFunctionArg","%start_pListFunctionDef","%start_pValueStatement","%start_pListValueStatement","%start_pRefDef","%start_pType","%start_pListType","%start_pAlgType","%start_pTypeArg","%start_pListTypeArg","%start_pAlgTypeVal","%start_pListAlgTypeVal","%start_pPatternMatch","%start_pListPatternMatch","%start_pAssignment","%start_pListAssignment","%start_pFunApplication","%start_pFunctionArgAppl","%start_pListFunctionArgAppl","%start_pListValueStatementr","%start_pTupleValueStatementr","%start_pValueStatementExpr","Ident","Integer","String","Program","FunctionOrRefOrType","ListFunctionOrRefOrType","FunctionDef","FunctionArg","ListFunctionArg","ListFunctionDef","ValueStatement","ListValueStatement","RefDef","Type","ListType","AlgType","TypeArg","ListTypeArg","AlgTypeVal","ListAlgTypeVal","PatternMatch","ListPatternMatch","Assignment","ListAssignment","FunApplication","FunctionArgAppl","ListFunctionArgAppl","ListValueStatementr","TupleValueStatementr","ValueStatementExpr","'!='","'%'","'('","')'","'*'","'+'","','","'-'","'->'","'.'","'/'","'::'","';'","'<'","'<='","'='","'=='","'>'","'>='","'['","']'","'data'","'else'","'force'","'fun'","'if'","'in'","'let'","'ref'","'sus'","'then'","'{'","'|'","'}'","L_ident","L_integ","L_quoted","%eof"]
        bit_start = st * 97
        bit_end = (st + 1) * 97
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..96]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\xeb\x00\x00\x00\x77\x00\x17\x00\x17\x00\x00\x00\x12\x00\x12\x00\xeb\xff\x0a\x00\x0a\x00\x03\x00\xf3\xff\xf3\xff\xf3\xff\xf3\xff\x17\x00\x17\x00\x31\x00\x00\x00\xf3\xff\x12\x00\x12\x00\x2a\x00\x1f\x00\x5c\x02\x1e\x00\x00\x00\x21\x00\x12\x00\x12\x00\x12\x00\x12\x00\x12\x00\x12\x00\x12\x00\x12\x00\x12\x00\x12\x00\x12\x00\x21\x00\x12\x00\x21\x00\x12\x00\x15\x00\x00\x00\x00\x00\x5c\x02\x00\x00\x48\x00\x34\x00\x00\x00\x00\x00\x3b\x00\x12\x00\x00\x00\x54\x00\x00\x00\x00\x00\x52\x00\x52\x00\xfe\xff\x93\x00\x00\x00\x95\x00\x8c\x00\x0a\x00\x0a\x00\x00\x00\x00\x00\x72\x00\x8c\x00\x17\x00\x02\x00\xb7\x00\xb3\x00\xaf\x00\xaf\x00\x00\x00\xe0\x00\xcc\x00\xcc\x00\xcc\x00\xd5\x00\xec\x00\xe9\x00\xe9\x00\xe9\x00\xfd\x01\xe9\x00\x01\x00\x29\x01\x0d\x01\xf4\x00\x1d\x01\xf9\x00\xf9\x00\x0a\x00\x12\x01\x53\x02\xff\x00\x00\x00\x00\x00\x00\x00\xff\x00\xeb\x00\x00\x00\x0a\x00\x07\x01\x17\x00\x17\x00\x00\x00\x00\x00\x12\x00\x0a\x00\x2b\x01\x0e\x01\x0e\x01\x0a\x00\x31\x01\x17\x00\x13\x01\xc6\x00\x38\x01\x17\x00\x0a\x00\x00\x00\x43\x01\x08\x00\x50\x00\x00\x00\x12\x00\x12\x00\x1e\x01\x3d\x01\x53\x01\x5c\x02\x5c\x02\x5c\x02\x5c\x02\x5c\x02\x5c\x02\x5c\x02\x5c\x02\x5c\x02\x5c\x02\x5c\x02\x00\x00\x00\x00\x00\x00\x54\x01\x00\x00\x30\x00\x12\x00\x12\x00\x12\x00\x57\x01\x07\x00\x00\x00\x0a\x00\x4e\x01\x00\x00\x00\x00\x5f\x01\x00\x00\x00\x00\x2a\x01\x00\x00\x00\x00\x00\x00\x65\x01\x67\x01\x4b\x01\x7c\x01\x17\x00\x00\x00\x70\x01\x00\x00\x12\x00\x75\x01\x12\x00\x00\x00\x37\x00\x5c\x02\xc3\x01\x12\x00\x00\x00\x5c\x02\x12\x00\x00\x00\x10\x02\x00\x00\x23\x02\x74\x01\x81\x01\x17\x00\x84\x01\x7b\x01\x6d\x01\x00\x00\x00\x00\x5c\x02\x89\x01\x12\x00\x87\x01\x12\x00\x36\x02\x00\x00\x00\x00\x49\x02\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x77\x01\xa6\x01\x96\x01\x9b\x01\x9f\x00\xde\x01\x9d\x01\xc2\x00\x8d\x00\xa5\x01\x03\x01\xa2\x00\x9a\x01\x40\x00\x4c\x00\x0e\x00\x74\x00\x1d\x00\x2f\x00\x60\x00\x90\x01\x04\x00\x92\x00\x64\x00\x9c\x01\xa0\x01\xa9\x01\x00\x00\x00\x00\x00\x00\xdf\x00\xe4\x00\xea\x00\xef\x00\x0c\x01\x11\x01\x17\x01\x1c\x01\x39\x01\x3e\x01\x44\x01\x00\x00\x99\x00\x00\x00\xb6\x00\x00\x00\x00\x00\x00\x00\xaa\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x49\x01\xa1\x01\xbd\x01\x00\x00\x00\x00\x00\x00\x00\x00\x91\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcb\x00\x30\x01\x00\x00\x00\x00\x00\x00\x00\x00\xc7\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd3\x01\x00\x00\x00\x00\x00\x00\x00\x00\xba\x01\x00\x00\xba\x01\xd7\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x01\x00\x00\x6e\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6e\x02\x00\x00\x62\x01\xd9\x01\xcc\x01\xe7\x01\x00\x00\x00\x00\xbb\x00\xe8\x00\x00\x00\xb9\x00\xac\x00\x7d\x01\x00\x00\xcf\x01\xe2\x01\x00\x00\x00\x00\x67\x00\x15\x01\x00\x00\x00\x00\x91\x00\xce\x01\xac\x01\x69\x00\x70\x00\x11\x00\x00\x00\x00\x00\xd8\x01\xd8\x01\xd8\x01\xd8\x01\xd8\x01\xd8\x01\xd8\x01\xd8\x01\xd8\x01\xd8\x01\xd8\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x91\x00\x66\x01\x6b\x01\x71\x01\x00\x00\x00\x00\x00\x00\x8f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xed\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf6\x01\x00\x00\xe9\x01\x00\x00\x00\x00\x00\x00\x76\x01\x00\x00\x93\x01\x00\x00\xda\x01\xda\x01\xda\x01\x98\x01\x00\x00\xda\x01\x9e\x01\x00\x00\xda\x01\x00\x00\xda\x01\x00\x00\x00\x00\xec\x01\x00\x00\x00\x00\xd0\x00\x00\x00\x00\x00\xda\x01\x00\x00\xa3\x01\x00\x00\xc0\x01\xda\x01\x00\x00\x00\x00\xda\x01\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xdd\xff\x00\x00\xdd\xff\x00\x00\x00\x00\xd8\xff\xd5\xff\x00\x00\xc9\xff\x00\x00\x00\x00\xc2\xff\x00\x00\x00\x00\xbd\xff\x00\x00\xb9\xff\x00\x00\xb2\xff\x00\x00\xad\xff\x00\x00\x00\x00\xa8\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe4\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc9\xff\x00\x00\xc9\xff\x00\x00\xcd\xff\xcc\xff\xa9\xff\xce\xff\xa7\xff\x00\x00\xd0\xff\xcf\xff\x00\x00\x00\x00\xad\xff\x00\x00\xe3\xff\xe2\xff\x00\x00\x00\x00\x00\x00\x00\x00\xae\xff\x00\x00\x00\x00\xc2\xff\x00\x00\xb5\xff\xb6\xff\xb1\xff\x00\x00\xb2\xff\x00\x00\x00\x00\xb8\xff\x00\x00\x00\x00\xbe\xff\xbc\xff\x00\x00\x00\x00\x00\x00\x00\x00\xc1\xff\x00\x00\x00\x00\x00\x00\xc8\xff\x00\x00\x00\x00\x00\x00\xd7\xff\x00\x00\xd9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe0\xff\xde\xff\xdf\xff\x00\x00\xe1\xff\xdc\xff\x00\x00\x00\x00\xb2\xff\xd8\xff\xd4\xff\xca\xff\xc9\xff\xc2\xff\x00\x00\xbd\xff\xb9\xff\x00\x00\x00\x00\xb2\xff\x00\x00\xc1\xff\x00\x00\x00\x00\xc2\xff\xac\xff\x00\x00\x00\x00\x00\x00\xad\xff\xa8\xff\xa8\xff\x00\x00\x00\x00\x00\x00\x9b\xff\x9c\xff\x9a\xff\x9d\xff\x9e\xff\x9f\xff\xa2\xff\xa3\xff\xa0\xff\xa1\xff\x99\xff\xa4\xff\xa5\xff\xab\xff\x00\x00\xa6\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc3\xff\x00\x00\x00\x00\xb0\xff\xb4\xff\x00\x00\xb7\xff\xbb\xff\xbd\xff\xc0\xff\xc7\xff\xd6\xff\x00\x00\x00\x00\x00\x00\x00\x00\xd8\xff\xb3\xff\x00\x00\xba\xff\x00\x00\x00\x00\x00\x00\xc5\xff\x00\x00\xd3\xff\x00\x00\x00\x00\xaa\xff\xd2\xff\x00\x00\xcb\xff\x00\x00\xc4\xff\x00\x00\x00\x00\x00\x00\xd8\xff\x00\x00\x00\x00\xb9\xff\xc6\xff\xaf\xff\xd1\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbf\xff\xdb\xff\x00\x00\xda\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x03\x00\x01\x00\x02\x00\x00\x00\x03\x00\x05\x00\x06\x00\x1d\x00\x08\x00\x03\x00\x03\x00\x0b\x00\x03\x00\x00\x00\x0e\x00\x0f\x00\x00\x00\x11\x00\x12\x00\x13\x00\x03\x00\x23\x00\x10\x00\x03\x00\x16\x00\x03\x00\x1d\x00\x18\x00\x00\x00\x01\x00\x0a\x00\x12\x00\x23\x00\x03\x00\x1b\x00\x26\x00\x1d\x00\x14\x00\x26\x00\x26\x00\x18\x00\x18\x00\x23\x00\x1a\x00\x23\x00\x1c\x00\x00\x00\x01\x00\x14\x00\x20\x00\x03\x00\x03\x00\x23\x00\x24\x00\x25\x00\x01\x00\x02\x00\x23\x00\x24\x00\x05\x00\x06\x00\x14\x00\x08\x00\x00\x00\x23\x00\x0b\x00\x14\x00\x15\x00\x0e\x00\x0f\x00\x26\x00\x11\x00\x12\x00\x13\x00\x1b\x00\x00\x00\x1d\x00\x1d\x00\x07\x00\x10\x00\x01\x00\x02\x00\x23\x00\x23\x00\x05\x00\x06\x00\x1c\x00\x08\x00\x22\x00\x26\x00\x0b\x00\x10\x00\x11\x00\x0e\x00\x0f\x00\x00\x00\x11\x00\x12\x00\x13\x00\x00\x00\x01\x00\x02\x00\x00\x00\x01\x00\x00\x00\x01\x00\x02\x00\x0c\x00\x0d\x00\x0a\x00\x1f\x00\x00\x00\x01\x00\x02\x00\x0a\x00\x00\x00\x03\x00\x16\x00\x23\x00\x26\x00\x07\x00\x0a\x00\x14\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x12\x00\x13\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x00\x00\x01\x00\x02\x00\x19\x00\x00\x00\x00\x00\x01\x00\x02\x00\x1e\x00\x03\x00\x0a\x00\x0b\x00\x00\x00\x01\x00\x02\x00\x0a\x00\x0c\x00\x0d\x00\x00\x00\x01\x00\x0c\x00\x00\x00\x0a\x00\x0b\x00\x18\x00\x07\x00\x16\x00\x1b\x00\x1c\x00\x18\x00\x19\x00\x00\x00\x1b\x00\x1c\x00\x0d\x00\x0e\x00\x18\x00\x26\x00\x14\x00\x1b\x00\x1c\x00\x00\x00\x01\x00\x02\x00\x00\x00\x03\x00\x00\x00\x01\x00\x02\x00\x12\x00\x13\x00\x0a\x00\x0b\x00\x00\x00\x01\x00\x02\x00\x0a\x00\x0b\x00\x00\x00\x01\x00\x10\x00\x11\x00\x00\x00\x0a\x00\x07\x00\x18\x00\x09\x00\x00\x00\x1b\x00\x1c\x00\x18\x00\x21\x00\x26\x00\x1b\x00\x1c\x00\x0d\x00\x0e\x00\x18\x00\x14\x00\x15\x00\x1b\x00\x1c\x00\x00\x00\x01\x00\x02\x00\x12\x00\x13\x00\x00\x00\x01\x00\x02\x00\x07\x00\x00\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x00\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x26\x00\x07\x00\x0a\x00\x0d\x00\x0e\x00\x18\x00\x23\x00\x0a\x00\x1b\x00\x1c\x00\x18\x00\x10\x00\x11\x00\x1b\x00\x1c\x00\x16\x00\x18\x00\x00\x00\x19\x00\x1b\x00\x1c\x00\x18\x00\x1d\x00\x1e\x00\x1b\x00\x1c\x00\x00\x00\x01\x00\x02\x00\x26\x00\x0d\x00\x00\x00\x01\x00\x02\x00\x07\x00\x00\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x26\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x26\x00\x03\x00\x0a\x00\x0d\x00\x0e\x00\x18\x00\x26\x00\x0a\x00\x1b\x00\x1c\x00\x18\x00\x23\x00\x19\x00\x1b\x00\x1c\x00\x03\x00\x18\x00\x00\x00\x23\x00\x1b\x00\x1c\x00\x18\x00\x04\x00\x23\x00\x1b\x00\x1c\x00\x00\x00\x01\x00\x02\x00\x04\x00\x0d\x00\x00\x00\x01\x00\x02\x00\x23\x00\x19\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x1e\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x09\x00\x23\x00\x0a\x00\x26\x00\x00\x00\x18\x00\x15\x00\x0a\x00\x1b\x00\x1c\x00\x18\x00\x04\x00\x04\x00\x1b\x00\x1c\x00\x04\x00\x18\x00\x0d\x00\x10\x00\x1b\x00\x1c\x00\x18\x00\x00\x00\x04\x00\x1b\x00\x1c\x00\x00\x00\x01\x00\x02\x00\x04\x00\x03\x00\x00\x00\x01\x00\x02\x00\x23\x00\x0d\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x04\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x04\x00\x03\x00\x0a\x00\x05\x00\x00\x00\x18\x00\x03\x00\x0a\x00\x1b\x00\x1c\x00\x18\x00\x10\x00\x04\x00\x1b\x00\x1c\x00\x04\x00\x18\x00\x0d\x00\x10\x00\x1b\x00\x1c\x00\x18\x00\x00\x00\x23\x00\x1b\x00\x1c\x00\x00\x00\x01\x00\x02\x00\x0d\x00\x10\x00\x00\x00\x01\x00\x02\x00\x05\x00\x0d\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x06\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x09\x00\x17\x00\x0a\x00\x0f\x00\x04\x00\x18\x00\x06\x00\x0a\x00\x1b\x00\x1c\x00\x18\x00\x0c\x00\x0c\x00\x1b\x00\x1c\x00\x0f\x00\x18\x00\x1b\x00\x17\x00\x1b\x00\x1c\x00\x18\x00\x1c\x00\x00\x00\x1b\x00\x1c\x00\x00\x00\x01\x00\x02\x00\x17\x00\x01\x00\x02\x00\x1d\x00\x1d\x00\x05\x00\x06\x00\x0a\x00\x08\x00\x00\x00\x01\x00\x0b\x00\x00\x00\x01\x00\x0e\x00\x0f\x00\x00\x00\x11\x00\x12\x00\x13\x00\x1d\x00\x18\x00\x00\x00\x17\x00\x1b\x00\x1c\x00\x06\x00\x00\x00\x01\x00\x14\x00\x15\x00\x00\x00\x14\x00\x15\x00\x07\x00\x08\x00\x00\x00\x01\x00\x00\x00\x01\x00\x1d\x00\x00\x00\x01\x00\x07\x00\x08\x00\x07\x00\x08\x00\x14\x00\x07\x00\x08\x00\x1d\x00\x00\x00\x1d\x00\xff\xff\xff\xff\xff\xff\x14\x00\xff\xff\x14\x00\x01\x00\x02\x00\x14\x00\xff\xff\x05\x00\x06\x00\x07\x00\x08\x00\xff\xff\xff\xff\x0b\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\xff\xff\x11\x00\x12\x00\x13\x00\x01\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\x08\x00\xff\xff\xff\xff\x0b\x00\xff\xff\x0d\x00\x0e\x00\x0f\x00\xff\xff\x11\x00\x12\x00\x13\x00\x01\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\x08\x00\xff\xff\xff\xff\x0b\x00\xff\xff\x0d\x00\x0e\x00\x0f\x00\xff\xff\x11\x00\x12\x00\x13\x00\x01\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\x08\x00\xff\xff\xff\xff\x0b\x00\xff\xff\x0d\x00\x0e\x00\x0f\x00\xff\xff\x11\x00\x12\x00\x13\x00\x01\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\x08\x00\xff\xff\xff\xff\x0b\x00\xff\xff\x0d\x00\x0e\x00\x0f\x00\xff\xff\x11\x00\x12\x00\x13\x00\x01\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\x08\x00\xff\xff\xff\xff\x0b\x00\xff\xff\x16\x00\x0e\x00\x0f\x00\x19\x00\x11\x00\x12\x00\x13\x00\x1d\x00\x1e\x00\x04\x00\xff\xff\x06\x00\xff\xff\xff\xff\xff\xff\xff\xff\x26\x00\x0c\x00\xff\xff\xff\xff\x0f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x44\x00\x1f\x00\x20\x00\x2d\x00\x6f\x00\x21\x00\x22\x00\x45\x00\x23\x00\x6f\x00\x44\x00\x24\x00\x44\x00\x4b\x00\x25\x00\x26\x00\x2d\x00\x27\x00\x28\x00\x29\x00\x2b\x00\x1d\x00\xb6\x00\x86\x00\x55\x00\x4a\x00\x45\x00\x3d\x00\x45\x00\x46\x00\x87\x00\x4e\x00\x1d\x00\x2b\x00\x9c\x00\xff\xff\x45\x00\x2d\x00\xff\xff\xff\xff\x96\x00\x37\x00\x1d\x00\x38\x00\x1d\x00\x39\x00\x45\x00\x46\x00\x4a\x00\x3a\x00\x44\x00\x44\x00\x1d\x00\x3b\x00\x3c\x00\x1f\x00\x20\x00\x1d\x00\x3b\x00\x21\x00\x22\x00\x2d\x00\x23\x00\x4f\x00\x1d\x00\x24\x00\x47\x00\x48\x00\x25\x00\x26\x00\xff\xff\x27\x00\x28\x00\x29\x00\xbb\x00\x4f\x00\x45\x00\x45\x00\x85\x00\x52\x00\x1f\x00\x20\x00\x1d\x00\x1d\x00\x21\x00\x22\x00\x84\x00\x23\x00\xbf\x00\xff\xff\x24\x00\x50\x00\x51\x00\x25\x00\x26\x00\x3f\x00\x27\x00\x28\x00\x29\x00\x2d\x00\x2e\x00\x2f\x00\x45\x00\x46\x00\x2d\x00\x2e\x00\x2f\x00\x40\x00\x41\x00\x30\x00\x9b\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x4b\x00\x6f\x00\x42\x00\x1d\x00\xff\xff\x7a\x00\x30\x00\x9e\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x31\x00\x32\x00\x98\x00\x34\x00\x35\x00\x4c\x00\x4d\x00\x31\x00\x32\x00\x97\x00\x34\x00\x35\x00\x2d\x00\x2e\x00\x2f\x00\x63\x00\x3f\x00\x2d\x00\x2e\x00\x2f\x00\x64\x00\x7f\x00\x59\x00\x5a\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x40\x00\x41\x00\x45\x00\x46\x00\x7e\x00\x3f\x00\x59\x00\x88\x00\x31\x00\x60\x00\x7f\x00\x34\x00\x35\x00\x31\x00\x3c\x00\x4b\x00\x34\x00\x35\x00\x55\x00\x56\x00\x31\x00\xff\xff\x5f\x00\x34\x00\x35\x00\x2d\x00\x2e\x00\x2f\x00\x4f\x00\x78\x00\x2d\x00\x2e\x00\x2f\x00\x4c\x00\xa5\x00\x59\x00\x87\x00\x2d\x00\x2e\x00\x2f\x00\x59\x00\xa9\x00\x45\x00\x46\x00\x50\x00\xa6\x00\x3f\x00\x5b\x00\x74\x00\x31\x00\xa1\x00\x4b\x00\x34\x00\x35\x00\x31\x00\x77\x00\xff\xff\x34\x00\x35\x00\x7b\x00\x7c\x00\x31\x00\x47\x00\x78\x00\x34\x00\x35\x00\x2d\x00\x2e\x00\x2f\x00\x4c\x00\xcb\x00\x2d\x00\x2e\x00\x2f\x00\x76\x00\x3f\x00\x93\x00\x2d\x00\x2e\x00\x2f\x00\x4f\x00\x92\x00\x2d\x00\x2e\x00\x2f\x00\xff\xff\x74\x00\x91\x00\x55\x00\xa8\x00\x31\x00\x1d\x00\x90\x00\x34\x00\x35\x00\x31\x00\x50\x00\xb1\x00\x34\x00\x35\x00\x55\x00\x31\x00\x3f\x00\x63\x00\x34\x00\x35\x00\x31\x00\x45\x00\x64\x00\x34\x00\x35\x00\x2d\x00\x2e\x00\x2f\x00\xff\xff\x57\x00\x2d\x00\x2e\x00\x2f\x00\x70\x00\x3f\x00\x8f\x00\x2d\x00\x2e\x00\x2f\x00\xff\xff\x8e\x00\x2d\x00\x2e\x00\x2f\x00\xff\xff\x6f\x00\x8d\x00\x55\x00\x9d\x00\x31\x00\xff\xff\x8c\x00\x34\x00\x35\x00\x31\x00\x1d\x00\x6d\x00\x34\x00\x35\x00\xa8\x00\x31\x00\x3f\x00\x1d\x00\x34\x00\x35\x00\x31\x00\xa4\x00\x1d\x00\x34\x00\x35\x00\x2d\x00\x2e\x00\x2f\x00\xa0\x00\x7a\x00\x2d\x00\x2e\x00\x2f\x00\x1d\x00\x63\x00\x8b\x00\x2d\x00\x2e\x00\x2f\x00\x64\x00\x8a\x00\x2d\x00\x2e\x00\x2f\x00\x9d\x00\x1d\x00\x89\x00\xff\xff\x3f\x00\x31\x00\x96\x00\x82\x00\x34\x00\x35\x00\x31\x00\x95\x00\xbc\x00\x34\x00\x35\x00\xb7\x00\x31\x00\x6d\x00\xb4\x00\x34\x00\x35\x00\x31\x00\x3f\x00\xb3\x00\x34\x00\x35\x00\x2d\x00\x2e\x00\x2f\x00\xb1\x00\xb0\x00\x2d\x00\x2e\x00\x2f\x00\x1d\x00\xad\x00\xb9\x00\x2d\x00\x2e\x00\x2f\x00\xc3\x00\xb8\x00\x2d\x00\x2e\x00\x2f\x00\xc1\x00\x69\x00\xb7\x00\x6a\x00\x3f\x00\x31\x00\xc5\x00\xc1\x00\x34\x00\x35\x00\x31\x00\xc8\x00\xc7\x00\x34\x00\x35\x00\xce\x00\x31\x00\xa4\x00\xcd\x00\x34\x00\x35\x00\x31\x00\x3f\x00\x1d\x00\x34\x00\x35\x00\x2d\x00\x2e\x00\x2f\x00\xd1\x00\xcf\x00\x2d\x00\x2e\x00\x2f\x00\x64\x00\xb4\x00\xbf\x00\x2d\x00\x2e\x00\x2f\x00\x61\x00\xbc\x00\x2d\x00\x2e\x00\x2f\x00\x5c\x00\x3e\x00\xca\x00\x53\x00\x65\x00\x31\x00\x66\x00\xcf\x00\x34\x00\x35\x00\x31\x00\x58\x00\x67\x00\x34\x00\x35\x00\x68\x00\x31\x00\x2b\x00\x81\x00\x34\x00\x35\x00\x31\x00\x29\x00\x80\x00\x34\x00\x35\x00\x2d\x00\x2e\x00\x2f\x00\x99\x00\x1f\x00\x20\x00\x1d\x00\x71\x00\x21\x00\x22\x00\xd2\x00\x23\x00\x45\x00\x46\x00\x24\x00\x45\x00\x46\x00\x25\x00\x26\x00\x74\x00\x27\x00\x28\x00\x29\x00\x71\x00\x31\x00\xac\x00\xbe\x00\x34\x00\x35\x00\x70\x00\x45\x00\x46\x00\x47\x00\xab\x00\xa1\x00\x47\x00\xa2\x00\x5d\x00\x5e\x00\x45\x00\x46\x00\x45\x00\x46\x00\x71\x00\x45\x00\x46\x00\x5d\x00\xaa\x00\x5d\x00\xc3\x00\x5f\x00\x5d\x00\xc5\x00\x71\x00\xae\x00\x71\x00\x00\x00\x00\x00\x00\x00\x5f\x00\x00\x00\x5f\x00\x1f\x00\x20\x00\x5f\x00\x00\x00\x21\x00\x22\x00\x73\x00\x23\x00\x00\x00\x00\x00\x24\x00\x00\x00\x00\x00\x25\x00\x26\x00\x00\x00\x27\x00\x28\x00\x29\x00\x1f\x00\x20\x00\x00\x00\x00\x00\x21\x00\x22\x00\x00\x00\x23\x00\x00\x00\x00\x00\x24\x00\x00\x00\xca\x00\x25\x00\x26\x00\x00\x00\x27\x00\x28\x00\x29\x00\x1f\x00\x20\x00\x00\x00\x00\x00\x21\x00\x22\x00\x00\x00\x23\x00\x00\x00\x00\x00\x24\x00\x00\x00\xc9\x00\x25\x00\x26\x00\x00\x00\x27\x00\x28\x00\x29\x00\x1f\x00\x20\x00\x00\x00\x00\x00\x21\x00\x22\x00\x00\x00\x23\x00\x00\x00\x00\x00\x24\x00\x00\x00\xd2\x00\x25\x00\x26\x00\x00\x00\x27\x00\x28\x00\x29\x00\x1f\x00\x20\x00\x00\x00\x00\x00\x21\x00\x22\x00\x00\x00\x23\x00\x00\x00\x00\x00\x24\x00\x00\x00\xd4\x00\x25\x00\x26\x00\x00\x00\x27\x00\x28\x00\x29\x00\x1f\x00\x20\x00\x00\x00\x00\x00\x21\x00\x22\x00\x00\x00\x23\x00\x00\x00\x00\x00\x24\x00\x00\x00\x55\x00\x25\x00\x26\x00\x63\x00\x27\x00\x28\x00\x29\x00\x45\x00\x64\x00\x6b\x00\x00\x00\x66\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\xff\x67\x00\x00\x00\x00\x00\x68\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (27, 102) [
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
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102)
	]

happy_n_terms = 39 :: Int
happy_n_nonterms = 30 :: Int

happyReduce_27 = happySpecReduce_1  0# happyReduction_27
happyReduction_27 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TV happy_var_1)) -> 
	happyIn30
		 (Ident happy_var_1
	)}

happyReduce_28 = happySpecReduce_1  1# happyReduction_28
happyReduction_28 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TI happy_var_1)) -> 
	happyIn31
		 ((read ( happy_var_1)) :: Integer
	)}

happyReduce_29 = happySpecReduce_1  2# happyReduction_29
happyReduction_29 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TL happy_var_1)) -> 
	happyIn32
		 (happy_var_1
	)}

happyReduce_30 = happySpecReduce_1  3# happyReduction_30
happyReduction_30 happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	happyIn33
		 (AbsHpl.ProgramB (reverse happy_var_1)
	)}

happyReduce_31 = happySpecReduce_1  4# happyReduction_31
happyReduction_31 happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	happyIn34
		 (AbsHpl.FunctionOrRefOrTypeF happy_var_1
	)}

happyReduce_32 = happySpecReduce_1  4# happyReduction_32
happyReduction_32 happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	happyIn34
		 (AbsHpl.FunctionOrRefOrTypeT happy_var_1
	)}

happyReduce_33 = happySpecReduce_1  4# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	happyIn34
		 (AbsHpl.FunctionOrRefOrTypeR happy_var_1
	)}

happyReduce_34 = happySpecReduce_0  5# happyReduction_34
happyReduction_34  =  happyIn35
		 ([]
	)

happyReduce_35 = happySpecReduce_2  5# happyReduction_35
happyReduction_35 happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	case happyOut34 happy_x_2 of { happy_var_2 -> 
	happyIn35
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_36 = happyReduce 9# 6# happyReduction_36
happyReduction_36 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut43 happy_x_2 of { happy_var_2 -> 
	case happyOut30 happy_x_3 of { happy_var_3 -> 
	case happyOut38 happy_x_5 of { happy_var_5 -> 
	case happyOut40 happy_x_8 of { happy_var_8 -> 
	happyIn36
		 (AbsHpl.FunctionDefB happy_var_2 happy_var_3 happy_var_5 happy_var_8
	) `HappyStk` happyRest}}}}

happyReduce_37 = happyReduce 10# 6# happyReduction_37
happyReduction_37 (happy_x_10 `HappyStk`
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
	 = case happyOut43 happy_x_3 of { happy_var_3 -> 
	case happyOut30 happy_x_4 of { happy_var_4 -> 
	case happyOut38 happy_x_6 of { happy_var_6 -> 
	case happyOut40 happy_x_9 of { happy_var_9 -> 
	happyIn36
		 (AbsHpl.SusFunctionDef happy_var_3 happy_var_4 happy_var_6 happy_var_9
	) `HappyStk` happyRest}}}}

happyReduce_38 = happySpecReduce_1  7# happyReduction_38
happyReduction_38 happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	happyIn37
		 (AbsHpl.FunctionArgB happy_var_1
	)}

happyReduce_39 = happySpecReduce_0  8# happyReduction_39
happyReduction_39  =  happyIn38
		 ([]
	)

happyReduce_40 = happySpecReduce_1  8# happyReduction_40
happyReduction_40 happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	happyIn38
		 ((:[]) happy_var_1
	)}

happyReduce_41 = happySpecReduce_3  8# happyReduction_41
happyReduction_41 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut38 happy_x_3 of { happy_var_3 -> 
	happyIn38
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_42 = happySpecReduce_0  9# happyReduction_42
happyReduction_42  =  happyIn39
		 ([]
	)

happyReduce_43 = happySpecReduce_2  9# happyReduction_43
happyReduction_43 happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut36 happy_x_2 of { happy_var_2 -> 
	happyIn39
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_44 = happyReduce 4# 10# happyReduction_44
happyReduction_44 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut53 happy_x_2 of { happy_var_2 -> 
	case happyOut40 happy_x_4 of { happy_var_4 -> 
	happyIn40
		 (AbsHpl.ValueStatementB (reverse happy_var_2) happy_var_4
	) `HappyStk` happyRest}}

happyReduce_45 = happyReduce 5# 10# happyReduction_45
happyReduction_45 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut53 happy_x_3 of { happy_var_3 -> 
	case happyOut40 happy_x_5 of { happy_var_5 -> 
	happyIn40
		 (AbsHpl.ForceValueStatement (reverse happy_var_3) happy_var_5
	) `HappyStk` happyRest}}

happyReduce_46 = happyReduce 6# 10# happyReduction_46
happyReduction_46 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut40 happy_x_2 of { happy_var_2 -> 
	case happyOut40 happy_x_4 of { happy_var_4 -> 
	case happyOut40 happy_x_6 of { happy_var_6 -> 
	happyIn40
		 (AbsHpl.IfValueStatement happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_47 = happySpecReduce_1  10# happyReduction_47
happyReduction_47 happy_x_1
	 =  case happyOut57 happy_x_1 of { happy_var_1 -> 
	happyIn40
		 (AbsHpl.LValueStatement happy_var_1
	)}

happyReduce_48 = happySpecReduce_1  10# happyReduction_48
happyReduction_48 happy_x_1
	 =  case happyOut58 happy_x_1 of { happy_var_1 -> 
	happyIn40
		 (AbsHpl.TValueStatement happy_var_1
	)}

happyReduce_49 = happySpecReduce_1  10# happyReduction_49
happyReduction_49 happy_x_1
	 =  case happyOut54 happy_x_1 of { happy_var_1 -> 
	happyIn40
		 (AbsHpl.AValueStatement happy_var_1
	)}

happyReduce_50 = happySpecReduce_1  10# happyReduction_50
happyReduction_50 happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn40
		 (AbsHpl.IValueStatement happy_var_1
	)}

happyReduce_51 = happySpecReduce_1  10# happyReduction_51
happyReduction_51 happy_x_1
	 =  case happyOut32 happy_x_1 of { happy_var_1 -> 
	happyIn40
		 (AbsHpl.LitStrValueStatement happy_var_1
	)}

happyReduce_52 = happyReduce 5# 10# happyReduction_52
happyReduction_52 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut30 happy_x_2 of { happy_var_2 -> 
	case happyOut40 happy_x_4 of { happy_var_4 -> 
	happyIn40
		 (AbsHpl.FValueStatement happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_53 = happySpecReduce_2  10# happyReduction_53
happyReduction_53 happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	case happyOut59 happy_x_2 of { happy_var_2 -> 
	happyIn40
		 (AbsHpl.Expr happy_var_1 happy_var_2
	)}}

happyReduce_54 = happySpecReduce_0  11# happyReduction_54
happyReduction_54  =  happyIn41
		 ([]
	)

happyReduce_55 = happySpecReduce_1  11# happyReduction_55
happyReduction_55 happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn41
		 ((:[]) happy_var_1
	)}

happyReduce_56 = happySpecReduce_3  11# happyReduction_56
happyReduction_56 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	case happyOut41 happy_x_3 of { happy_var_3 -> 
	happyIn41
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_57 = happyReduce 6# 12# happyReduction_57
happyReduction_57 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut43 happy_x_2 of { happy_var_2 -> 
	case happyOut30 happy_x_3 of { happy_var_3 -> 
	case happyOut40 happy_x_5 of { happy_var_5 -> 
	happyIn42
		 (AbsHpl.RefDefB happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_58 = happyReduce 4# 13# happyReduction_58
happyReduction_58 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut30 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn43
		 (AbsHpl.TypeB happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_59 = happyReduce 5# 13# happyReduction_59
happyReduction_59 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut43 happy_x_2 of { happy_var_2 -> 
	case happyOut43 happy_x_4 of { happy_var_4 -> 
	happyIn43
		 (AbsHpl.FunType happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_60 = happySpecReduce_3  13# happyReduction_60
happyReduction_60 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_2 of { happy_var_2 -> 
	happyIn43
		 (AbsHpl.TType happy_var_2
	)}

happyReduce_61 = happySpecReduce_0  14# happyReduction_61
happyReduction_61  =  happyIn44
		 ([]
	)

happyReduce_62 = happySpecReduce_1  14# happyReduction_62
happyReduction_62 happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	happyIn44
		 ((:[]) happy_var_1
	)}

happyReduce_63 = happySpecReduce_3  14# happyReduction_63
happyReduction_63 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_64 = happyReduce 8# 15# happyReduction_64
happyReduction_64 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut30 happy_x_2 of { happy_var_2 -> 
	case happyOut47 happy_x_4 of { happy_var_4 -> 
	case happyOut49 happy_x_7 of { happy_var_7 -> 
	happyIn45
		 (AbsHpl.AlgTypeB happy_var_2 happy_var_4 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_65 = happySpecReduce_1  16# happyReduction_65
happyReduction_65 happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	happyIn46
		 (AbsHpl.TypeArgB happy_var_1
	)}

happyReduce_66 = happySpecReduce_0  17# happyReduction_66
happyReduction_66  =  happyIn47
		 ([]
	)

happyReduce_67 = happySpecReduce_1  17# happyReduction_67
happyReduction_67 happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 ((:[]) happy_var_1
	)}

happyReduce_68 = happySpecReduce_3  17# happyReduction_68
happyReduction_68 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	case happyOut47 happy_x_3 of { happy_var_3 -> 
	happyIn47
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_69 = happyReduce 4# 18# happyReduction_69
happyReduction_69 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut30 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn48
		 (AbsHpl.AlgTypeValB happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_70 = happySpecReduce_0  19# happyReduction_70
happyReduction_70  =  happyIn49
		 ([]
	)

happyReduce_71 = happySpecReduce_1  19# happyReduction_71
happyReduction_71 happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	happyIn49
		 ((:[]) happy_var_1
	)}

happyReduce_72 = happySpecReduce_3  19# happyReduction_72
happyReduction_72 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	case happyOut49 happy_x_3 of { happy_var_3 -> 
	happyIn49
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_73 = happySpecReduce_1  20# happyReduction_73
happyReduction_73 happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn50
		 (AbsHpl.PatternMatchI happy_var_1
	)}

happyReduce_74 = happySpecReduce_1  20# happyReduction_74
happyReduction_74 happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	happyIn50
		 (AbsHpl.PatternMatchB happy_var_1
	)}

happyReduce_75 = happySpecReduce_3  20# happyReduction_75
happyReduction_75 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut51 happy_x_2 of { happy_var_2 -> 
	happyIn50
		 (AbsHpl.TPatternMatch happy_var_2
	)}

happyReduce_76 = happyReduce 4# 20# happyReduction_76
happyReduction_76 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut50 happy_x_1 of { happy_var_1 -> 
	case happyOut51 happy_x_3 of { happy_var_3 -> 
	happyIn50
		 (AbsHpl.CPatternMatch happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_77 = happySpecReduce_0  21# happyReduction_77
happyReduction_77  =  happyIn51
		 ([]
	)

happyReduce_78 = happySpecReduce_1  21# happyReduction_78
happyReduction_78 happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	happyIn51
		 ((:[]) happy_var_1
	)}

happyReduce_79 = happySpecReduce_3  21# happyReduction_79
happyReduction_79 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	case happyOut51 happy_x_3 of { happy_var_3 -> 
	happyIn51
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_80 = happyReduce 6# 22# happyReduction_80
happyReduction_80 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut50 happy_x_3 of { happy_var_3 -> 
	case happyOut40 happy_x_5 of { happy_var_5 -> 
	happyIn52
		 (AbsHpl.AssignmentB happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_81 = happySpecReduce_1  22# happyReduction_81
happyReduction_81 happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	happyIn52
		 (AbsHpl.RefAssignment happy_var_1
	)}

happyReduce_82 = happySpecReduce_0  23# happyReduction_82
happyReduction_82  =  happyIn53
		 ([]
	)

happyReduce_83 = happySpecReduce_2  23# happyReduction_83
happyReduction_83 happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_1 of { happy_var_1 -> 
	case happyOut52 happy_x_2 of { happy_var_2 -> 
	happyIn53
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_84 = happySpecReduce_3  24# happyReduction_84
happyReduction_84 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	case happyOut54 happy_x_3 of { happy_var_3 -> 
	happyIn54
		 (AbsHpl.SFunApplication happy_var_1 happy_var_3
	)}}

happyReduce_85 = happyReduce 4# 24# happyReduction_85
happyReduction_85 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut30 happy_x_1 of { happy_var_1 -> 
	case happyOut56 happy_x_3 of { happy_var_3 -> 
	happyIn54
		 (AbsHpl.FunApplicationB happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_86 = happySpecReduce_1  25# happyReduction_86
happyReduction_86 happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn55
		 (AbsHpl.FunctionArgApplB happy_var_1
	)}

happyReduce_87 = happySpecReduce_0  26# happyReduction_87
happyReduction_87  =  happyIn56
		 ([]
	)

happyReduce_88 = happySpecReduce_1  26# happyReduction_88
happyReduction_88 happy_x_1
	 =  case happyOut55 happy_x_1 of { happy_var_1 -> 
	happyIn56
		 ((:[]) happy_var_1
	)}

happyReduce_89 = happySpecReduce_3  26# happyReduction_89
happyReduction_89 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut55 happy_x_1 of { happy_var_1 -> 
	case happyOut56 happy_x_3 of { happy_var_3 -> 
	happyIn56
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_90 = happySpecReduce_3  27# happyReduction_90
happyReduction_90 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut41 happy_x_2 of { happy_var_2 -> 
	happyIn57
		 (AbsHpl.ListValueStatementB happy_var_2
	)}

happyReduce_91 = happySpecReduce_3  28# happyReduction_91
happyReduction_91 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut41 happy_x_2 of { happy_var_2 -> 
	happyIn58
		 (AbsHpl.TupleValueStatementB happy_var_2
	)}

happyReduce_92 = happySpecReduce_2  29# happyReduction_92
happyReduction_92 happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_2 of { happy_var_2 -> 
	happyIn59
		 (AbsHpl.EAdd happy_var_2
	)}

happyReduce_93 = happySpecReduce_2  29# happyReduction_93
happyReduction_93 happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_2 of { happy_var_2 -> 
	happyIn59
		 (AbsHpl.ESub happy_var_2
	)}

happyReduce_94 = happySpecReduce_2  29# happyReduction_94
happyReduction_94 happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_2 of { happy_var_2 -> 
	happyIn59
		 (AbsHpl.EMod happy_var_2
	)}

happyReduce_95 = happySpecReduce_2  29# happyReduction_95
happyReduction_95 happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_2 of { happy_var_2 -> 
	happyIn59
		 (AbsHpl.EMul happy_var_2
	)}

happyReduce_96 = happySpecReduce_2  29# happyReduction_96
happyReduction_96 happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_2 of { happy_var_2 -> 
	happyIn59
		 (AbsHpl.EDiv happy_var_2
	)}

happyReduce_97 = happySpecReduce_2  29# happyReduction_97
happyReduction_97 happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_2 of { happy_var_2 -> 
	happyIn59
		 (AbsHpl.EL happy_var_2
	)}

happyReduce_98 = happySpecReduce_2  29# happyReduction_98
happyReduction_98 happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_2 of { happy_var_2 -> 
	happyIn59
		 (AbsHpl.ELQ happy_var_2
	)}

happyReduce_99 = happySpecReduce_2  29# happyReduction_99
happyReduction_99 happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_2 of { happy_var_2 -> 
	happyIn59
		 (AbsHpl.EG happy_var_2
	)}

happyReduce_100 = happySpecReduce_2  29# happyReduction_100
happyReduction_100 happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_2 of { happy_var_2 -> 
	happyIn59
		 (AbsHpl.EGQ happy_var_2
	)}

happyReduce_101 = happySpecReduce_2  29# happyReduction_101
happyReduction_101 happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_2 of { happy_var_2 -> 
	happyIn59
		 (AbsHpl.EEQ happy_var_2
	)}

happyReduce_102 = happySpecReduce_2  29# happyReduction_102
happyReduction_102 happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_2 of { happy_var_2 -> 
	happyIn59
		 (AbsHpl.ENE happy_var_2
	)}

happyNewToken action sts stk [] =
	happyDoAction 38# notHappyAtAll action sts stk []

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
	PT _ (TS _ 32) -> cont 32#;
	PT _ (TS _ 33) -> cont 33#;
	PT _ (TS _ 34) -> cont 34#;
	PT _ (TV happy_dollar_dollar) -> cont 35#;
	PT _ (TI happy_dollar_dollar) -> cont 36#;
	PT _ (TL happy_dollar_dollar) -> cont 37#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 38# tk tks = happyError' (tks, explist)
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
pProgram tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut33 x))

pFunctionOrRefOrType tks = happySomeParser where
 happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (happyOut34 x))

pListFunctionOrRefOrType tks = happySomeParser where
 happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (happyOut35 x))

pFunctionDef tks = happySomeParser where
 happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (happyOut36 x))

pFunctionArg tks = happySomeParser where
 happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (happyOut37 x))

pListFunctionArg tks = happySomeParser where
 happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (happyOut38 x))

pListFunctionDef tks = happySomeParser where
 happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (happyOut39 x))

pValueStatement tks = happySomeParser where
 happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (happyOut40 x))

pListValueStatement tks = happySomeParser where
 happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (happyOut41 x))

pRefDef tks = happySomeParser where
 happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (happyOut42 x))

pType tks = happySomeParser where
 happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (happyOut43 x))

pListType tks = happySomeParser where
 happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (happyOut44 x))

pAlgType tks = happySomeParser where
 happySomeParser = happyThen (happyParse 12# tks) (\x -> happyReturn (happyOut45 x))

pTypeArg tks = happySomeParser where
 happySomeParser = happyThen (happyParse 13# tks) (\x -> happyReturn (happyOut46 x))

pListTypeArg tks = happySomeParser where
 happySomeParser = happyThen (happyParse 14# tks) (\x -> happyReturn (happyOut47 x))

pAlgTypeVal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 15# tks) (\x -> happyReturn (happyOut48 x))

pListAlgTypeVal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 16# tks) (\x -> happyReturn (happyOut49 x))

pPatternMatch tks = happySomeParser where
 happySomeParser = happyThen (happyParse 17# tks) (\x -> happyReturn (happyOut50 x))

pListPatternMatch tks = happySomeParser where
 happySomeParser = happyThen (happyParse 18# tks) (\x -> happyReturn (happyOut51 x))

pAssignment tks = happySomeParser where
 happySomeParser = happyThen (happyParse 19# tks) (\x -> happyReturn (happyOut52 x))

pListAssignment tks = happySomeParser where
 happySomeParser = happyThen (happyParse 20# tks) (\x -> happyReturn (happyOut53 x))

pFunApplication tks = happySomeParser where
 happySomeParser = happyThen (happyParse 21# tks) (\x -> happyReturn (happyOut54 x))

pFunctionArgAppl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 22# tks) (\x -> happyReturn (happyOut55 x))

pListFunctionArgAppl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 23# tks) (\x -> happyReturn (happyOut56 x))

pListValueStatementr tks = happySomeParser where
 happySomeParser = happyThen (happyParse 24# tks) (\x -> happyReturn (happyOut57 x))

pTupleValueStatementr tks = happySomeParser where
 happySomeParser = happyThen (happyParse 25# tks) (\x -> happyReturn (happyOut58 x))

pValueStatementExpr tks = happySomeParser where
 happySomeParser = happyThen (happyParse 26# tks) (\x -> happyReturn (happyOut59 x))

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
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 11 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "/tmp/ghcb5f8_0/ghc_2.h" #-}




























































































































































{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 













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
{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
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
         off_i  = (off Happy_GHC_Exts.+#  i)
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

{-# LINE 180 "templates/GenericTemplate.hs" #-}

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
             off_i = (off Happy_GHC_Exts.+#  nt)
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
         off_i = (off Happy_GHC_Exts.+#  nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

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
