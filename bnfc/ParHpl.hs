{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# OPTIONS -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParHpl where
import AbsHpl
import LexHpl
import ErrM
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn39 :: (Ident) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> (Ident)
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: (Integer) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> (Integer)
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: (String) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> (String)
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyIn42 :: (Program) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> (Program)
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
happyIn43 :: (StructOrInterfaceOrType) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> (StructOrInterfaceOrType)
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
happyIn44 :: ([StructOrInterfaceOrType]) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> ([StructOrInterfaceOrType])
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
happyIn45 :: (Struct) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> (Struct)
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
happyIn46 :: (StructBody) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> (StructBody)
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
happyIn47 :: (StructField) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> (StructField)
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
happyIn48 :: ([StructField]) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> ([StructField])
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
happyIn49 :: (InterfaceId) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> (InterfaceId)
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
happyIn50 :: ([InterfaceId]) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> ([InterfaceId])
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
happyIn51 :: (Interface) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> (Interface)
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
happyIn52 :: (InterfaceBody) -> (HappyAbsSyn )
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> (InterfaceBody)
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
happyIn53 :: (FunOrRefDecl) -> (HappyAbsSyn )
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> (FunOrRefDecl)
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
happyIn54 :: ([FunOrRefDecl]) -> (HappyAbsSyn )
happyIn54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> ([FunOrRefDecl])
happyOut54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut54 #-}
happyIn55 :: (FunctionDef) -> (HappyAbsSyn )
happyIn55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn ) -> (FunctionDef)
happyOut55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut55 #-}
happyIn56 :: (FunctionArg) -> (HappyAbsSyn )
happyIn56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn ) -> (FunctionArg)
happyOut56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut56 #-}
happyIn57 :: ([FunctionArg]) -> (HappyAbsSyn )
happyIn57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn57 #-}
happyOut57 :: (HappyAbsSyn ) -> ([FunctionArg])
happyOut57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut57 #-}
happyIn58 :: (ValueStatement) -> (HappyAbsSyn )
happyIn58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn58 #-}
happyOut58 :: (HappyAbsSyn ) -> (ValueStatement)
happyOut58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut58 #-}
happyIn59 :: ([ValueStatement]) -> (HappyAbsSyn )
happyIn59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn59 #-}
happyOut59 :: (HappyAbsSyn ) -> ([ValueStatement])
happyOut59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut59 #-}
happyIn60 :: (RefDef) -> (HappyAbsSyn )
happyIn60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn60 #-}
happyOut60 :: (HappyAbsSyn ) -> (RefDef)
happyOut60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut60 #-}
happyIn61 :: (Type) -> (HappyAbsSyn )
happyIn61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn61 #-}
happyOut61 :: (HappyAbsSyn ) -> (Type)
happyOut61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut61 #-}
happyIn62 :: ([Type]) -> (HappyAbsSyn )
happyIn62 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn62 #-}
happyOut62 :: (HappyAbsSyn ) -> ([Type])
happyOut62 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut62 #-}
happyIn63 :: (AlgType) -> (HappyAbsSyn )
happyIn63 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn63 #-}
happyOut63 :: (HappyAbsSyn ) -> (AlgType)
happyOut63 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut63 #-}
happyIn64 :: (TypeArg) -> (HappyAbsSyn )
happyIn64 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn64 #-}
happyOut64 :: (HappyAbsSyn ) -> (TypeArg)
happyOut64 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut64 #-}
happyIn65 :: ([TypeArg]) -> (HappyAbsSyn )
happyIn65 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn65 #-}
happyOut65 :: (HappyAbsSyn ) -> ([TypeArg])
happyOut65 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut65 #-}
happyIn66 :: (AlgTypeVal) -> (HappyAbsSyn )
happyIn66 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn66 #-}
happyOut66 :: (HappyAbsSyn ) -> (AlgTypeVal)
happyOut66 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut66 #-}
happyIn67 :: ([AlgTypeVal]) -> (HappyAbsSyn )
happyIn67 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn67 #-}
happyOut67 :: (HappyAbsSyn ) -> ([AlgTypeVal])
happyOut67 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut67 #-}
happyIn68 :: (PatternMatch) -> (HappyAbsSyn )
happyIn68 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn68 #-}
happyOut68 :: (HappyAbsSyn ) -> (PatternMatch)
happyOut68 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut68 #-}
happyIn69 :: ([PatternMatch]) -> (HappyAbsSyn )
happyIn69 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn69 #-}
happyOut69 :: (HappyAbsSyn ) -> ([PatternMatch])
happyOut69 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut69 #-}
happyIn70 :: (Assignment) -> (HappyAbsSyn )
happyIn70 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn70 #-}
happyOut70 :: (HappyAbsSyn ) -> (Assignment)
happyOut70 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut70 #-}
happyIn71 :: ([Assignment]) -> (HappyAbsSyn )
happyIn71 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn71 #-}
happyOut71 :: (HappyAbsSyn ) -> ([Assignment])
happyOut71 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut71 #-}
happyIn72 :: (FunApplication) -> (HappyAbsSyn )
happyIn72 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn72 #-}
happyOut72 :: (HappyAbsSyn ) -> (FunApplication)
happyOut72 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut72 #-}
happyIn73 :: (FunctionArgAppl) -> (HappyAbsSyn )
happyIn73 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn73 #-}
happyOut73 :: (HappyAbsSyn ) -> (FunctionArgAppl)
happyOut73 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut73 #-}
happyIn74 :: ([FunctionArgAppl]) -> (HappyAbsSyn )
happyIn74 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn74 #-}
happyOut74 :: (HappyAbsSyn ) -> ([FunctionArgAppl])
happyOut74 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut74 #-}
happyIn75 :: (ListValueStatementr) -> (HappyAbsSyn )
happyIn75 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn75 #-}
happyOut75 :: (HappyAbsSyn ) -> (ListValueStatementr)
happyOut75 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut75 #-}
happyIn76 :: (TupleValueStatementr) -> (HappyAbsSyn )
happyIn76 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn76 #-}
happyOut76 :: (HappyAbsSyn ) -> (TupleValueStatementr)
happyOut76 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut76 #-}
happyIn77 :: (ValueStatementExpr) -> (HappyAbsSyn )
happyIn77 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn77 #-}
happyOut77 :: (HappyAbsSyn ) -> (ValueStatementExpr)
happyOut77 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut77 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x58\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xa2\x04\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xa2\x04\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x10\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xa2\x04\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xa2\x04\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x96\xd8\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xa2\x04\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xa2\x04\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xa2\x04\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xa2\x04\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xa2\x04\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xa2\x04\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xa2\x04\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xa2\x04\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xa2\x04\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xa2\x04\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xa2\x04\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xa2\x04\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xa2\x04\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x96\xd8\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xa2\x04\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x10\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x9e\xd8\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x96\xd8\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x58\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x58\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xa2\x04\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x11\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x96\xd8\x01\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xa2\x04\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xa2\x04\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x11\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xa2\x04\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xa2\x04\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xa2\x04\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xa2\x04\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xa2\x04\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x96\xd8\x01\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x96\xd8\x11\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xa2\x04\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xa2\x04\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x96\xdc\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x96\xdc\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xa2\x04\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x96\xdc\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram","%start_pStructOrInterfaceOrType","%start_pListStructOrInterfaceOrType","%start_pStruct","%start_pStructBody","%start_pStructField","%start_pListStructField","%start_pInterfaceId","%start_pListInterfaceId","%start_pInterface","%start_pInterfaceBody","%start_pFunOrRefDecl","%start_pListFunOrRefDecl","%start_pFunctionDef","%start_pFunctionArg","%start_pListFunctionArg","%start_pValueStatement","%start_pListValueStatement","%start_pRefDef","%start_pType","%start_pListType","%start_pAlgType","%start_pTypeArg","%start_pListTypeArg","%start_pAlgTypeVal","%start_pListAlgTypeVal","%start_pPatternMatch","%start_pListPatternMatch","%start_pAssignment","%start_pListAssignment","%start_pFunApplication","%start_pFunctionArgAppl","%start_pListFunctionArgAppl","%start_pListValueStatementr","%start_pTupleValueStatementr","%start_pValueStatementExpr","Ident","Integer","String","Program","StructOrInterfaceOrType","ListStructOrInterfaceOrType","Struct","StructBody","StructField","ListStructField","InterfaceId","ListInterfaceId","Interface","InterfaceBody","FunOrRefDecl","ListFunOrRefDecl","FunctionDef","FunctionArg","ListFunctionArg","ValueStatement","ListValueStatement","RefDef","Type","ListType","AlgType","TypeArg","ListTypeArg","AlgTypeVal","ListAlgTypeVal","PatternMatch","ListPatternMatch","Assignment","ListAssignment","FunApplication","FunctionArgAppl","ListFunctionArgAppl","ListValueStatementr","TupleValueStatementr","ValueStatementExpr","'!='","'%'","'('","')'","'*'","'+'","','","'-'","'->'","'.'","'/'","':'","'::'","';'","'<'","'<='","'='","'=='","'>'","'>='","'['","']'","'data'","'else'","'force'","'fun'","'if'","'in'","'interface'","'let'","'private'","'ref'","'struct'","'sus'","'then'","'{'","'|'","'}'","L_ident","L_integ","L_quoted","L_err","%eof"]
        bit_start = st * 120
        bit_end = (st + 1) * 120
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..119]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\xc8\x01\x00\x00\xee\xff\x00\x00\xe4\x01\x00\x00\xeb\xff\xeb\xff\x35\x00\x00\x00\xf8\x01\x00\x00\x0f\x01\x21\x00\x21\x00\x15\x00\x15\x00\x6f\x00\x1a\x00\x1a\x00\x3e\x00\x5c\x00\x5c\x00\x5c\x00\x5c\x00\x21\x00\x21\x00\x08\x00\x00\x00\x5c\x00\x15\x00\x15\x00\x80\x00\xc1\x00\x36\x02\x8b\x00\x00\x00\x93\x00\x15\x00\x15\x00\x15\x00\x15\x00\x15\x00\x15\x00\x15\x00\x15\x00\x15\x00\x15\x00\x15\x00\x93\x00\x15\x00\x93\x00\x15\x00\x7f\x00\x00\x00\x00\x00\x36\x02\x00\x00\xd0\x00\xb2\x00\x00\x00\x00\x00\xe1\x00\x15\x00\x00\x00\xb7\x00\x00\x00\x00\x00\xc7\x00\xc7\x00\xfe\xff\xe8\x00\x00\x00\xfd\x00\xf2\x00\x1a\x00\x1a\x00\x00\x00\x00\x00\x18\x00\xf2\x00\x21\x00\x02\x00\x21\x01\xfe\x00\x0d\x01\x0d\x01\x00\x00\x35\x01\x17\x01\x17\x01\x17\x01\x23\x01\x4e\x01\x2b\x01\x2b\x01\x2b\x01\xe6\x01\x2b\x01\x01\x00\x5c\x01\x3e\x01\x67\x01\x49\x01\x49\x01\x1a\x00\x0f\x01\x2d\x02\x49\x01\x1a\x00\x1a\x00\x64\x01\x6c\x01\xf8\x01\x6c\x01\xa6\x01\x00\x00\xd2\x01\xaf\x01\xaf\x01\x20\x02\xaf\x01\x00\x00\x00\x00\x0c\x02\xaf\x01\xe4\x01\xaf\x01\xbd\x01\x20\x01\xc5\x01\x00\x00\x00\x00\x00\x00\xc5\x01\xc8\x01\x00\x00\xfc\xff\x00\x00\x00\x00\x00\x00\xc3\x01\x74\x00\x00\x00\x1a\x00\xc3\x01\xc3\x01\x00\x00\xc3\x01\x21\x00\x21\x00\x00\x00\x15\x00\x1a\x00\xf0\x01\xcd\x01\xcd\x01\x1a\x00\xf3\x01\x21\x00\xd6\x01\x52\x00\xfd\x01\x21\x00\x1a\x00\x00\x00\xfe\x01\x37\x00\x86\x01\x00\x00\x15\x00\x15\x00\xea\x01\xf5\x01\x17\x02\x36\x02\x36\x02\x36\x02\x36\x02\x36\x02\x36\x02\x36\x02\x36\x02\x36\x02\x36\x02\x36\x02\x00\x00\x00\x00\x00\x00\x1b\x02\x00\x00\x98\x00\x15\x00\x15\x00\x15\x00\x21\x02\x0b\x00\x00\x00\x1a\x00\x1e\x02\x00\x00\x00\x00\x2f\x02\x00\x00\x00\x00\x12\x02\x00\x00\x00\x00\x00\x00\x39\x02\x40\x02\x46\x02\x47\x02\x1d\x02\x1d\x02\x00\x00\x00\x00\x1d\x02\x00\x00\x30\x02\x33\x02\x34\x02\x35\x02\x4d\x02\x00\x00\x00\x00\x21\x00\x00\x00\x58\x02\x00\x00\x15\x00\x59\x02\x15\x00\x00\x00\x30\x00\x36\x02\xc4\x01\x15\x00\x00\x00\x36\x02\x15\x00\x00\x00\xfa\x01\x00\x00\x0e\x02\x4e\x02\x5a\x02\x00\x00\x00\x00\x52\x02\x00\x00\x53\x02\x00\x00\x3c\x02\x00\x00\x3d\x02\x54\x02\x3f\x02\x00\x00\x00\x00\x36\x02\x56\x02\x15\x00\x5b\x02\x5c\x02\x00\x00\x00\x00\x22\x02\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x78\x01\xd7\x01\x62\x02\x65\x02\xc7\x01\x4c\x00\x5f\x02\xaf\x00\x17\x00\x60\x02\x08\x02\x61\x02\x5e\x02\x63\x02\x9d\x01\xa1\x01\xa7\x00\x63\x00\x5d\x02\x81\x00\x94\x01\x64\x02\xaa\x00\x0d\x00\x5a\x00\xae\x00\x5f\x00\x82\x01\x04\x00\x50\x02\x0a\x00\x78\x00\x45\x00\x51\x02\x49\x02\x4b\x02\x00\x00\x00\x00\x00\x00\xbb\x00\xc0\x00\xc5\x00\xcf\x00\xd4\x00\xd9\x00\xed\x00\x01\x01\x06\x01\x0b\x01\x15\x01\x00\x00\x7d\x00\x00\x00\x8c\x00\x00\x00\x00\x00\x00\x00\x55\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1a\x01\x57\x02\x74\x02\x00\x00\x00\x00\x00\x00\x00\x00\x96\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb1\x01\xcd\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x76\x02\x00\x00\x00\x00\x00\x00\x00\x00\x66\x02\x00\x00\x66\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd2\x00\x68\x02\x6b\x02\x00\x00\x18\x01\x59\x01\x00\x00\x00\x00\x6b\x02\x00\x00\xf1\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf4\x00\x00\x00\x00\x00\x00\x00\xac\x01\x00\x00\xf4\x00\x00\x00\x7a\x02\xda\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xda\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7b\x00\x00\x00\x00\x00\x77\x01\x7d\x02\x7e\x02\x00\x00\x7f\x02\x92\x01\xa3\x01\x00\x00\x92\x00\xbb\x01\x00\x00\x34\x00\x5e\x01\xa7\x01\x00\x00\x9b\x01\x80\x02\x00\x00\x00\x00\xf6\x00\xcb\x01\x00\x00\x00\x00\x96\x00\x67\x02\x69\x02\x4a\x00\x4f\x00\x16\x00\x00\x00\x00\x00\x6a\x02\x6a\x02\x6a\x02\x6a\x02\x6a\x02\x6a\x02\x6a\x02\x6a\x02\x6a\x02\x6a\x02\x6a\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x96\x00\x1f\x01\x33\x01\x47\x01\x00\x00\x00\x00\x00\x00\xbf\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x81\x02\x05\x01\x1c\x02\x00\x00\x45\x01\x45\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa5\x01\x00\x00\x00\x00\x00\x00\x4c\x01\x00\x00\x51\x01\x00\x00\x6c\x02\x6c\x02\x6c\x02\x5b\x01\x00\x00\x6c\x02\x60\x01\x00\x00\x6c\x02\x00\x00\x6c\x02\x00\x00\x00\x00\x00\x00\x43\x02\x00\x00\x4a\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa8\x01\x00\x00\x00\x00\x6c\x02\x00\x00\x65\x01\x00\x00\x00\x00\x00\x00\x00\x00\x6c\x02\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xd4\xff\x00\x00\xd4\xff\x00\x00\xcb\xff\x00\x00\xcb\xff\x00\x00\x00\x00\x00\x00\xc0\xff\x00\x00\xc0\xff\x00\x00\x00\x00\xbb\xff\x00\x00\xae\xff\x00\x00\x00\x00\xa7\xff\x00\x00\x00\x00\xa2\xff\x00\x00\x9e\xff\x00\x00\x97\xff\x00\x00\x92\xff\x00\x00\x00\x00\x8d\xff\x00\x00\x00\x00\x00\x00\x00\x00\xdb\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xae\xff\x00\x00\xae\xff\x00\x00\xb2\xff\xb1\xff\x8e\xff\xb3\xff\x8c\xff\x00\x00\xb5\xff\xb4\xff\x00\x00\x00\x00\x92\xff\x00\x00\xda\xff\xd9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x93\xff\x00\x00\x00\x00\xa7\xff\x00\x00\x9a\xff\x9b\xff\x96\xff\x00\x00\x97\xff\x00\x00\x00\x00\x9d\xff\x00\x00\x00\x00\xa3\xff\xa1\xff\x00\x00\x00\x00\x00\x00\x00\x00\xa6\xff\x00\x00\x00\x00\x00\x00\xad\xff\x00\x00\x00\x00\xba\xff\x00\x00\xbc\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc4\xff\x00\x00\x00\x00\xc9\xff\xc8\xff\x00\x00\x00\x00\x00\x00\x00\x00\xce\xff\xcc\xff\x00\x00\x00\x00\xd0\xff\x00\x00\x00\x00\x00\x00\x00\x00\xd7\xff\xd6\xff\xd5\xff\x00\x00\xd8\xff\xd3\xff\x00\x00\xca\xff\xcf\xff\xcd\xff\x00\x00\x00\x00\xbf\xff\x00\x00\x00\x00\x00\x00\xbd\xff\x00\x00\x97\xff\xbb\xff\xaf\xff\xae\xff\xa7\xff\x00\x00\xa2\xff\x9e\xff\x00\x00\x00\x00\x97\xff\x00\x00\xa6\xff\x00\x00\x00\x00\xa7\xff\x91\xff\x00\x00\x00\x00\x00\x00\x92\xff\x8d\xff\x8d\xff\x00\x00\x00\x00\x00\x00\x80\xff\x81\xff\x7f\xff\x82\xff\x83\xff\x84\xff\x87\xff\x88\xff\x85\xff\x86\xff\x7e\xff\x89\xff\x8a\xff\x90\xff\x00\x00\x8b\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa8\xff\x00\x00\x00\x00\x95\xff\x99\xff\x00\x00\x9c\xff\xa0\xff\xa2\xff\xa5\xff\xac\xff\xb9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\xff\xc7\xff\x00\x00\xcb\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc1\xff\xc3\xff\xbb\xff\x98\xff\x00\x00\x9f\xff\x00\x00\x00\x00\x00\x00\xaa\xff\x00\x00\xb8\xff\x00\x00\x00\x00\x8f\xff\xb7\xff\x00\x00\xb0\xff\x00\x00\xa9\xff\x00\x00\x00\x00\x00\x00\xc2\xff\xc0\xff\x00\x00\xcb\xff\x00\x00\xd2\xff\x00\x00\xc6\xff\x00\x00\x00\x00\x9e\xff\xab\xff\x94\xff\xb6\xff\x00\x00\x00\x00\x00\x00\x00\x00\xd1\xff\xc5\xff\x00\x00\xa4\xff\xbe\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x03\x00\x01\x00\x02\x00\x00\x00\x03\x00\x05\x00\x06\x00\x0c\x00\x08\x00\x00\x00\x03\x00\x0b\x00\x00\x00\x03\x00\x21\x00\x0f\x00\x10\x00\x27\x00\x12\x00\x13\x00\x14\x00\x00\x00\x00\x00\x03\x00\x15\x00\x16\x00\x03\x00\x11\x00\x03\x00\x20\x00\x07\x00\x24\x00\x0a\x00\x0b\x00\x1f\x00\x03\x00\x27\x00\x19\x00\x1a\x00\x20\x00\x2b\x00\x15\x00\x21\x00\x2b\x00\x2b\x00\x19\x00\x27\x00\x1b\x00\x01\x00\x02\x00\x1e\x00\x00\x00\x05\x00\x06\x00\x21\x00\x08\x00\x24\x00\x03\x00\x0b\x00\x27\x00\x28\x00\x29\x00\x0f\x00\x10\x00\x27\x00\x12\x00\x13\x00\x14\x00\x00\x00\x01\x00\x02\x00\x27\x00\x28\x00\x00\x00\x01\x00\x02\x00\x19\x00\x1a\x00\x00\x00\x01\x00\x02\x00\x1d\x00\x1c\x00\x08\x00\x17\x00\x26\x00\x20\x00\x13\x00\x07\x00\x00\x00\x09\x00\x10\x00\x13\x00\x27\x00\x00\x00\x01\x00\x15\x00\x13\x00\x00\x00\x01\x00\x02\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x1b\x00\x13\x00\x14\x00\x00\x00\x01\x00\x02\x00\x00\x00\x1d\x00\x00\x00\x01\x00\x02\x00\x0c\x00\x00\x00\x03\x00\x27\x00\x21\x00\x0a\x00\x0b\x00\x24\x00\x25\x00\x0a\x00\x00\x00\x13\x00\x00\x00\x01\x00\x02\x00\x20\x00\x13\x00\x14\x00\x00\x00\x01\x00\x02\x00\x15\x00\x00\x00\x16\x00\x24\x00\x21\x00\x22\x00\x03\x00\x24\x00\x25\x00\x21\x00\x13\x00\x14\x00\x24\x00\x25\x00\x19\x00\x1a\x00\x13\x00\x14\x00\x00\x00\x01\x00\x02\x00\x00\x00\x15\x00\x16\x00\x21\x00\x00\x00\x00\x00\x24\x00\x25\x00\x27\x00\x21\x00\x1c\x00\x1f\x00\x24\x00\x25\x00\x20\x00\x0a\x00\x13\x00\x00\x00\x01\x00\x02\x00\x2b\x00\x27\x00\x00\x00\x01\x00\x02\x00\x19\x00\x03\x00\x00\x00\x01\x00\x02\x00\x21\x00\x1b\x00\x1c\x00\x24\x00\x25\x00\x00\x00\x13\x00\x00\x00\x01\x00\x02\x00\x00\x00\x13\x00\x00\x00\x01\x00\x02\x00\x07\x00\x13\x00\x00\x00\x01\x00\x02\x00\x21\x00\x2b\x00\x27\x00\x24\x00\x25\x00\x21\x00\x13\x00\x16\x00\x24\x00\x25\x00\x21\x00\x13\x00\x16\x00\x24\x00\x25\x00\x03\x00\x13\x00\x00\x00\x01\x00\x02\x00\x21\x00\x00\x00\x2b\x00\x24\x00\x25\x00\x21\x00\x00\x00\x01\x00\x24\x00\x25\x00\x21\x00\x0a\x00\x08\x00\x24\x00\x25\x00\x1e\x00\x13\x00\x00\x00\x01\x00\x02\x00\x10\x00\x00\x00\x00\x00\x01\x00\x02\x00\x15\x00\x0d\x00\x00\x00\x01\x00\x02\x00\x21\x00\x0a\x00\x0b\x00\x24\x00\x25\x00\x1d\x00\x13\x00\x00\x00\x01\x00\x02\x00\x00\x00\x13\x00\x00\x00\x01\x00\x02\x00\x2b\x00\x13\x00\x00\x00\x01\x00\x02\x00\x21\x00\x25\x00\x03\x00\x24\x00\x25\x00\x21\x00\x13\x00\x1a\x00\x24\x00\x25\x00\x21\x00\x13\x00\x16\x00\x24\x00\x25\x00\x22\x00\x13\x00\x00\x00\x01\x00\x02\x00\x21\x00\x17\x00\x2b\x00\x24\x00\x25\x00\x21\x00\x07\x00\x1d\x00\x24\x00\x25\x00\x21\x00\x21\x00\x2b\x00\x24\x00\x25\x00\x00\x00\x13\x00\x00\x00\x01\x00\x02\x00\x27\x00\x2b\x00\x00\x00\x01\x00\x02\x00\x0a\x00\x0b\x00\x00\x00\x01\x00\x02\x00\x21\x00\x07\x00\x2b\x00\x24\x00\x25\x00\x00\x00\x13\x00\x00\x00\x01\x00\x02\x00\x00\x00\x13\x00\x00\x00\x01\x00\x02\x00\x07\x00\x13\x00\x00\x00\x01\x00\x02\x00\x21\x00\x2b\x00\x03\x00\x24\x00\x25\x00\x21\x00\x13\x00\x16\x00\x24\x00\x25\x00\x21\x00\x13\x00\x2b\x00\x24\x00\x25\x00\x00\x00\x13\x00\x1b\x00\x1c\x00\x03\x00\x21\x00\x05\x00\x1a\x00\x24\x00\x25\x00\x21\x00\x00\x00\x01\x00\x24\x00\x25\x00\x21\x00\x01\x00\x02\x00\x24\x00\x25\x00\x05\x00\x06\x00\x16\x00\x08\x00\x00\x00\x01\x00\x0b\x00\x00\x00\x01\x00\x00\x00\x0f\x00\x10\x00\x2b\x00\x12\x00\x13\x00\x14\x00\x00\x00\x01\x00\x00\x00\x01\x00\x1d\x00\x1e\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x00\x00\x23\x00\x16\x00\x17\x00\x1d\x00\x1e\x00\x11\x00\x1d\x00\x1e\x00\x00\x00\x11\x00\x12\x00\x11\x00\x12\x00\x11\x00\x12\x00\x1d\x00\x1e\x00\x1d\x00\x00\x00\x10\x00\x16\x00\x1d\x00\x00\x00\x1d\x00\x15\x00\x1d\x00\x1b\x00\x1c\x00\x01\x00\x02\x00\x16\x00\x17\x00\x05\x00\x06\x00\x00\x00\x08\x00\x27\x00\x07\x00\x0b\x00\x09\x00\x16\x00\x17\x00\x0f\x00\x10\x00\x16\x00\x12\x00\x13\x00\x14\x00\x07\x00\x2b\x00\x04\x00\x18\x00\x06\x00\x04\x00\x17\x00\x06\x00\x16\x00\x17\x00\x0c\x00\x27\x00\x1d\x00\x0c\x00\x01\x00\x02\x00\x21\x00\x27\x00\x05\x00\x06\x00\x07\x00\x08\x00\x18\x00\x2b\x00\x0b\x00\x18\x00\x03\x00\x27\x00\x0f\x00\x10\x00\x04\x00\x12\x00\x13\x00\x14\x00\x01\x00\x02\x00\x27\x00\x1a\x00\x05\x00\x06\x00\x04\x00\x08\x00\x1f\x00\x20\x00\x0b\x00\x22\x00\x09\x00\x0e\x00\x0f\x00\x10\x00\x16\x00\x12\x00\x13\x00\x14\x00\x01\x00\x02\x00\x27\x00\x1a\x00\x05\x00\x06\x00\x0d\x00\x08\x00\x0f\x00\x20\x00\x0b\x00\x22\x00\x04\x00\x0e\x00\x0f\x00\x10\x00\x04\x00\x12\x00\x13\x00\x14\x00\x01\x00\x02\x00\x04\x00\x1a\x00\x05\x00\x06\x00\x0d\x00\x08\x00\x0f\x00\x20\x00\x0b\x00\x22\x00\x11\x00\x0e\x00\x0f\x00\x10\x00\x04\x00\x12\x00\x13\x00\x14\x00\x01\x00\x02\x00\x27\x00\x1a\x00\x05\x00\x06\x00\x04\x00\x08\x00\x1f\x00\x20\x00\x0b\x00\x22\x00\x03\x00\x27\x00\x0f\x00\x10\x00\x1a\x00\x12\x00\x13\x00\x14\x00\x2b\x00\x07\x00\x20\x00\x09\x00\x22\x00\x0d\x00\x07\x00\x0f\x00\x09\x00\x0e\x00\x0e\x00\x26\x00\x24\x00\x2b\x00\x24\x00\x26\x00\x0e\x00\x04\x00\x04\x00\x04\x00\x11\x00\x0e\x00\x0e\x00\x26\x00\x26\x00\x0e\x00\x11\x00\x27\x00\x05\x00\x09\x00\x0e\x00\x0e\x00\x06\x00\x0c\x00\x0f\x00\x25\x00\x0e\x00\x20\x00\x26\x00\x15\x00\x10\x00\x00\x00\x24\x00\x00\x00\x20\x00\x10\x00\x0e\x00\x00\x00\x26\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x20\x00\xff\xff\xff\xff\x26\x00\x26\x00\xff\xff\xff\xff\x26\x00\xff\xff\x26\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x4d\x00\x28\x00\x29\x00\x48\x00\x97\x00\x2a\x00\x2b\x00\xdb\x00\x2c\x00\x36\x00\x4d\x00\x2d\x00\x58\x00\x97\x00\x82\x00\x2e\x00\x2f\x00\x26\x00\x30\x00\x31\x00\x32\x00\x36\x00\x75\x00\x34\x00\x49\x00\x4a\x00\x97\x00\xea\x00\x4d\x00\x4e\x00\xa1\x00\xdc\x00\x76\x00\x77\x00\x4b\x00\x53\x00\x26\x00\x59\x00\x5a\x00\x4e\x00\xff\xff\x36\x00\x46\x00\xff\xff\xff\xff\x40\x00\x26\x00\x41\x00\x28\x00\x29\x00\x42\x00\x58\x00\x2a\x00\x2b\x00\xbd\x00\x2c\x00\x43\x00\x4d\x00\x2d\x00\x26\x00\x44\x00\x45\x00\x2e\x00\x2f\x00\x26\x00\x30\x00\x31\x00\x32\x00\x36\x00\x37\x00\x38\x00\x26\x00\x44\x00\x36\x00\x37\x00\x38\x00\x59\x00\xcd\x00\x36\x00\x37\x00\x38\x00\x75\x00\xc3\x00\x7a\x00\x5e\x00\xf3\x00\x4e\x00\x39\x00\x9b\x00\x54\x00\xc8\x00\x7b\x00\x39\x00\x26\x00\x4e\x00\x4f\x00\x7c\x00\x39\x00\x36\x00\x37\x00\x38\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3a\x00\x3b\x00\xbf\x00\x3d\x00\x3e\x00\x3a\x00\x3b\x00\xbe\x00\x3d\x00\x3e\x00\x57\x00\x62\x00\x63\x00\x36\x00\x37\x00\x38\x00\x75\x00\x53\x00\x36\x00\x37\x00\x38\x00\xd8\x00\x48\x00\xad\x00\x26\x00\x3a\x00\x76\x00\xd9\x00\x3d\x00\x3e\x00\xae\x00\x58\x00\x39\x00\x36\x00\x37\x00\x38\x00\x4e\x00\x62\x00\xaf\x00\x36\x00\x37\x00\x38\x00\x36\x00\x48\x00\x60\x00\xd9\x00\x3a\x00\x45\x00\x4d\x00\x3d\x00\x3e\x00\x3a\x00\x62\x00\xae\x00\x3d\x00\x3e\x00\x59\x00\xe5\x00\x62\x00\xd0\x00\x36\x00\x37\x00\x38\x00\x58\x00\x49\x00\x4a\x00\x3a\x00\x54\x00\x75\x00\x3d\x00\x3e\x00\x26\x00\x3a\x00\xef\x00\xa6\x00\x3d\x00\x3e\x00\x4e\x00\x78\x00\x64\x00\x36\x00\x37\x00\x38\x00\xff\xff\x26\x00\x36\x00\x37\x00\x38\x00\x5b\x00\x34\x00\x36\x00\x37\x00\x38\x00\x3a\x00\x55\x00\x56\x00\x3d\x00\x3e\x00\x48\x00\xba\x00\x36\x00\x37\x00\x38\x00\x48\x00\xb9\x00\x36\x00\x37\x00\x38\x00\xac\x00\xb8\x00\x36\x00\x37\x00\x38\x00\x3a\x00\xff\xff\x26\x00\x3d\x00\x3e\x00\x3a\x00\xb7\x00\xa1\x00\x3d\x00\x3e\x00\x3a\x00\xb6\x00\x95\x00\x3d\x00\x3e\x00\xa6\x00\xb5\x00\x36\x00\x37\x00\x38\x00\x3a\x00\x75\x00\xff\xff\x3d\x00\x3e\x00\x3a\x00\x4e\x00\x4f\x00\x3d\x00\x3e\x00\x3a\x00\x8f\x00\x8b\x00\x3d\x00\x3e\x00\xab\x00\xb4\x00\x36\x00\x37\x00\x38\x00\x7b\x00\x75\x00\x36\x00\x37\x00\x38\x00\x7c\x00\xa5\x00\x36\x00\x37\x00\x38\x00\x3a\x00\x76\x00\xdf\x00\x3d\x00\x3e\x00\xc5\x00\xb3\x00\x36\x00\x37\x00\x38\x00\x48\x00\xb2\x00\x36\x00\x37\x00\x38\x00\xff\xff\xb1\x00\x36\x00\x37\x00\x38\x00\x3a\x00\x9e\x00\x9f\x00\x3d\x00\x3e\x00\x3a\x00\xb0\x00\x6b\x00\x3d\x00\x3e\x00\x3a\x00\xa9\x00\x93\x00\x3d\x00\x3e\x00\x6c\x00\xed\x00\x36\x00\x37\x00\x38\x00\x3a\x00\x5e\x00\xff\xff\x3d\x00\x3e\x00\x3a\x00\x9d\x00\x75\x00\x3d\x00\x3e\x00\x3a\x00\x82\x00\xff\xff\x3d\x00\x3e\x00\x75\x00\xec\x00\x36\x00\x37\x00\x38\x00\x26\x00\xff\xff\x36\x00\x37\x00\x38\x00\x76\x00\xdd\x00\x36\x00\x37\x00\x38\x00\x3a\x00\x9b\x00\xff\xff\x3d\x00\x3e\x00\x48\x00\xeb\x00\x36\x00\x37\x00\x38\x00\x54\x00\xf5\x00\x36\x00\x37\x00\x38\x00\x98\x00\xf3\x00\x36\x00\x37\x00\x38\x00\x3a\x00\xff\xff\x97\x00\x3d\x00\x3e\x00\x3a\x00\xf0\x00\x92\x00\x3d\x00\x3e\x00\x3a\x00\x05\x01\xff\xff\x3d\x00\x3e\x00\x48\x00\x0c\x01\x55\x00\xcc\x00\x87\x00\x3a\x00\x88\x00\x92\x00\x3d\x00\x3e\x00\x3a\x00\x4e\x00\x4f\x00\x3d\x00\x3e\x00\x3a\x00\x28\x00\x29\x00\x3d\x00\x3e\x00\x2a\x00\x2b\x00\xd6\x00\x2c\x00\x4e\x00\x4f\x00\x2d\x00\x4e\x00\x4f\x00\x48\x00\x2e\x00\x2f\x00\xff\xff\x30\x00\x31\x00\x32\x00\x4e\x00\x4f\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x4e\x00\x4f\x00\x4e\x00\x4f\x00\x4e\x00\x4f\x00\x48\x00\x54\x00\xc2\x00\x5e\x00\x5f\x00\x50\x00\x9f\x00\x68\x00\x50\x00\xd2\x00\x48\x00\x65\x00\x66\x00\x65\x00\xd1\x00\x65\x00\xf7\x00\x50\x00\xc9\x00\x67\x00\x48\x00\x8c\x00\xcb\x00\x67\x00\x48\x00\x67\x00\x8d\x00\x67\x00\x55\x00\x06\x01\x28\x00\x29\x00\xa2\x00\xa3\x00\x2a\x00\x2b\x00\x48\x00\x2c\x00\x26\x00\x7e\x00\x2d\x00\x7f\x00\x5e\x00\xcf\x00\x2e\x00\x2f\x00\xe8\x00\x30\x00\x31\x00\x32\x00\x8f\x00\xff\xff\x83\x00\xf2\x00\x84\x00\x89\x00\x5e\x00\x84\x00\x5e\x00\xc4\x00\x85\x00\x26\x00\x75\x00\x85\x00\x28\x00\x29\x00\x82\x00\x26\x00\x2a\x00\x2b\x00\x9a\x00\x2c\x00\x86\x00\xff\xff\x2d\x00\x86\x00\xcf\x00\x26\x00\x2e\x00\x2f\x00\xcb\x00\x30\x00\x31\x00\x32\x00\x28\x00\x29\x00\x26\x00\x6b\x00\x2a\x00\x2b\x00\xc7\x00\x2c\x00\x7e\x00\x4e\x00\x2d\x00\x6c\x00\xc4\x00\x05\x01\x2e\x00\x2f\x00\xbd\x00\x30\x00\x31\x00\x32\x00\x28\x00\x29\x00\x26\x00\x6f\x00\x2a\x00\x2b\x00\x71\x00\x2c\x00\x72\x00\x70\x00\x2d\x00\x71\x00\xbc\x00\x04\x01\x2e\x00\x2f\x00\xf0\x00\x30\x00\x31\x00\x32\x00\x28\x00\x29\x00\xeb\x00\x6b\x00\x2a\x00\x2b\x00\xde\x00\x2c\x00\x72\x00\x4e\x00\x2d\x00\x6c\x00\xe8\x00\x0f\x01\x2e\x00\x2f\x00\xe7\x00\x30\x00\x31\x00\x32\x00\x28\x00\x29\x00\x26\x00\x6b\x00\x2a\x00\x2b\x00\xe5\x00\x2c\x00\x7e\x00\x4e\x00\x2d\x00\x6c\x00\xe4\x00\x26\x00\x2e\x00\x2f\x00\x6f\x00\x30\x00\x31\x00\x32\x00\xff\xff\xdc\x00\x70\x00\x7f\x00\x71\x00\x00\x01\xfe\x00\x72\x00\x7f\x00\xe3\x00\xe2\x00\xfd\x00\xfc\x00\xff\xff\xfa\x00\xfb\x00\xf9\x00\xf7\x00\xf5\x00\x02\x01\x03\x01\x00\x01\xfe\x00\x0a\x01\x09\x01\x0e\x01\x08\x01\x26\x00\x82\x00\x79\x00\x0c\x01\x0b\x01\x80\x00\x73\x00\x6c\x00\x32\x00\x6d\x00\x47\x00\x26\x00\x61\x00\x69\x00\xa7\x00\x34\x00\x9b\x00\xa8\x00\x94\x00\x90\x00\x8a\x00\x98\x00\x5c\x00\xd5\x00\xd4\x00\xd3\x00\xc8\x00\xe0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x00\x00\x00\x00\x98\x00\x98\x00\x00\x00\x00\x00\x98\x00\x00\x00\x98\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (36, 129) [
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
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113),
	(114 , happyReduce_114),
	(115 , happyReduce_115),
	(116 , happyReduce_116),
	(117 , happyReduce_117),
	(118 , happyReduce_118),
	(119 , happyReduce_119),
	(120 , happyReduce_120),
	(121 , happyReduce_121),
	(122 , happyReduce_122),
	(123 , happyReduce_123),
	(124 , happyReduce_124),
	(125 , happyReduce_125),
	(126 , happyReduce_126),
	(127 , happyReduce_127),
	(128 , happyReduce_128),
	(129 , happyReduce_129)
	]

happy_n_terms = 44 :: Int
happy_n_nonterms = 39 :: Int

happyReduce_36 = happySpecReduce_1  0# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TV happy_var_1)) -> 
	happyIn39
		 (Ident happy_var_1
	)}

happyReduce_37 = happySpecReduce_1  1# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TI happy_var_1)) -> 
	happyIn40
		 ((read ( happy_var_1)) :: Integer
	)}

happyReduce_38 = happySpecReduce_1  2# happyReduction_38
happyReduction_38 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TL happy_var_1)) -> 
	happyIn41
		 (happy_var_1
	)}

happyReduce_39 = happySpecReduce_1  3# happyReduction_39
happyReduction_39 happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	happyIn42
		 (ProgramB (reverse happy_var_1)
	)}

happyReduce_40 = happySpecReduce_1  4# happyReduction_40
happyReduction_40 happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	happyIn43
		 (StructOrInterfaceOrTypeS happy_var_1
	)}

happyReduce_41 = happySpecReduce_1  4# happyReduction_41
happyReduction_41 happy_x_1
	 =  case happyOut51 happy_x_1 of { happy_var_1 -> 
	happyIn43
		 (StructOrInterfaceOrTypeI happy_var_1
	)}

happyReduce_42 = happySpecReduce_1  4# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOut63 happy_x_1 of { happy_var_1 -> 
	happyIn43
		 (StructOrInterfaceOrTypeT happy_var_1
	)}

happyReduce_43 = happySpecReduce_0  5# happyReduction_43
happyReduction_43  =  happyIn44
		 ([]
	)

happyReduce_44 = happySpecReduce_2  5# happyReduction_44
happyReduction_44 happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	happyIn44
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_45 = happyReduce 6# 6# happyReduction_45
happyReduction_45 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut39 happy_x_2 of { happy_var_2 -> 
	case happyOut46 happy_x_4 of { happy_var_4 -> 
	happyIn45
		 (StructB happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_46 = happyReduce 8# 6# happyReduction_46
happyReduction_46 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut39 happy_x_2 of { happy_var_2 -> 
	case happyOut50 happy_x_4 of { happy_var_4 -> 
	case happyOut46 happy_x_6 of { happy_var_6 -> 
	happyIn45
		 (StructI happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_47 = happySpecReduce_1  7# happyReduction_47
happyReduction_47 happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	happyIn46
		 (StructBodyB (reverse happy_var_1)
	)}

happyReduce_48 = happySpecReduce_2  8# happyReduction_48
happyReduction_48 happy_x_2
	happy_x_1
	 =  case happyOut55 happy_x_2 of { happy_var_2 -> 
	happyIn47
		 (StructFieldFunPr happy_var_2
	)}

happyReduce_49 = happySpecReduce_1  8# happyReduction_49
happyReduction_49 happy_x_1
	 =  case happyOut55 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (StructFieldFunPu happy_var_1
	)}

happyReduce_50 = happySpecReduce_2  8# happyReduction_50
happyReduction_50 happy_x_2
	happy_x_1
	 =  case happyOut60 happy_x_2 of { happy_var_2 -> 
	happyIn47
		 (StructFieldRefPr happy_var_2
	)}

happyReduce_51 = happySpecReduce_1  8# happyReduction_51
happyReduction_51 happy_x_1
	 =  case happyOut60 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (StructFieldRefPu happy_var_1
	)}

happyReduce_52 = happySpecReduce_0  9# happyReduction_52
happyReduction_52  =  happyIn48
		 ([]
	)

happyReduce_53 = happySpecReduce_2  9# happyReduction_53
happyReduction_53 happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	case happyOut47 happy_x_2 of { happy_var_2 -> 
	happyIn48
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_54 = happySpecReduce_1  10# happyReduction_54
happyReduction_54 happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	happyIn49
		 (InterfaceIdB happy_var_1
	)}

happyReduce_55 = happySpecReduce_1  11# happyReduction_55
happyReduction_55 happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	happyIn50
		 ((:[]) happy_var_1
	)}

happyReduce_56 = happySpecReduce_3  11# happyReduction_56
happyReduction_56 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	case happyOut50 happy_x_3 of { happy_var_3 -> 
	happyIn50
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
	 = case happyOut49 happy_x_2 of { happy_var_2 -> 
	case happyOut52 happy_x_4 of { happy_var_4 -> 
	happyIn51
		 (InterfaceB happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_58 = happyReduce 8# 12# happyReduction_58
happyReduction_58 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut49 happy_x_2 of { happy_var_2 -> 
	case happyOut50 happy_x_4 of { happy_var_4 -> 
	case happyOut52 happy_x_6 of { happy_var_6 -> 
	happyIn51
		 (InterfaceBInh happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_59 = happySpecReduce_1  13# happyReduction_59
happyReduction_59 happy_x_1
	 =  case happyOut54 happy_x_1 of { happy_var_1 -> 
	happyIn52
		 (InterfaceBodyB (reverse happy_var_1)
	)}

happyReduce_60 = happyReduce 4# 14# happyReduction_60
happyReduction_60 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut61 happy_x_2 of { happy_var_2 -> 
	case happyOut39 happy_x_3 of { happy_var_3 -> 
	happyIn53
		 (FunOrRefDeclF happy_var_2 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_61 = happyReduce 5# 14# happyReduction_61
happyReduction_61 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut61 happy_x_3 of { happy_var_3 -> 
	case happyOut39 happy_x_4 of { happy_var_4 -> 
	happyIn53
		 (FunOrRefDeclSF happy_var_3 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_62 = happyReduce 4# 14# happyReduction_62
happyReduction_62 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut61 happy_x_2 of { happy_var_2 -> 
	case happyOut39 happy_x_3 of { happy_var_3 -> 
	happyIn53
		 (FunOrRefDeclR happy_var_2 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_63 = happySpecReduce_0  15# happyReduction_63
happyReduction_63  =  happyIn54
		 ([]
	)

happyReduce_64 = happySpecReduce_2  15# happyReduction_64
happyReduction_64 happy_x_2
	happy_x_1
	 =  case happyOut54 happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_2 of { happy_var_2 -> 
	happyIn54
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_65 = happyReduce 9# 16# happyReduction_65
happyReduction_65 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut61 happy_x_2 of { happy_var_2 -> 
	case happyOut39 happy_x_3 of { happy_var_3 -> 
	case happyOut57 happy_x_5 of { happy_var_5 -> 
	case happyOut58 happy_x_8 of { happy_var_8 -> 
	happyIn55
		 (FunctionDefB happy_var_2 happy_var_3 happy_var_5 happy_var_8
	) `HappyStk` happyRest}}}}

happyReduce_66 = happySpecReduce_2  16# happyReduction_66
happyReduction_66 happy_x_2
	happy_x_1
	 =  case happyOut55 happy_x_2 of { happy_var_2 -> 
	happyIn55
		 (SusFunctionDef happy_var_2
	)}

happyReduce_67 = happySpecReduce_1  17# happyReduction_67
happyReduction_67 happy_x_1
	 =  case happyOut68 happy_x_1 of { happy_var_1 -> 
	happyIn56
		 (FunctionArgB happy_var_1
	)}

happyReduce_68 = happySpecReduce_0  18# happyReduction_68
happyReduction_68  =  happyIn57
		 ([]
	)

happyReduce_69 = happySpecReduce_1  18# happyReduction_69
happyReduction_69 happy_x_1
	 =  case happyOut56 happy_x_1 of { happy_var_1 -> 
	happyIn57
		 ((:[]) happy_var_1
	)}

happyReduce_70 = happySpecReduce_3  18# happyReduction_70
happyReduction_70 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut56 happy_x_1 of { happy_var_1 -> 
	case happyOut57 happy_x_3 of { happy_var_3 -> 
	happyIn57
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_71 = happyReduce 4# 19# happyReduction_71
happyReduction_71 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut71 happy_x_2 of { happy_var_2 -> 
	case happyOut58 happy_x_4 of { happy_var_4 -> 
	happyIn58
		 (ValueStatementB (reverse happy_var_2) happy_var_4
	) `HappyStk` happyRest}}

happyReduce_72 = happyReduce 5# 19# happyReduction_72
happyReduction_72 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut71 happy_x_3 of { happy_var_3 -> 
	case happyOut58 happy_x_5 of { happy_var_5 -> 
	happyIn58
		 (ForceValueStatement (reverse happy_var_3) happy_var_5
	) `HappyStk` happyRest}}

happyReduce_73 = happyReduce 6# 19# happyReduction_73
happyReduction_73 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut58 happy_x_2 of { happy_var_2 -> 
	case happyOut58 happy_x_4 of { happy_var_4 -> 
	case happyOut58 happy_x_6 of { happy_var_6 -> 
	happyIn58
		 (IfValueStatement happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_74 = happySpecReduce_1  19# happyReduction_74
happyReduction_74 happy_x_1
	 =  case happyOut75 happy_x_1 of { happy_var_1 -> 
	happyIn58
		 (LValueStatement happy_var_1
	)}

happyReduce_75 = happySpecReduce_1  19# happyReduction_75
happyReduction_75 happy_x_1
	 =  case happyOut76 happy_x_1 of { happy_var_1 -> 
	happyIn58
		 (TValueStatement happy_var_1
	)}

happyReduce_76 = happySpecReduce_1  19# happyReduction_76
happyReduction_76 happy_x_1
	 =  case happyOut72 happy_x_1 of { happy_var_1 -> 
	happyIn58
		 (AValueStatement happy_var_1
	)}

happyReduce_77 = happySpecReduce_1  19# happyReduction_77
happyReduction_77 happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn58
		 (IValueStatement happy_var_1
	)}

happyReduce_78 = happySpecReduce_1  19# happyReduction_78
happyReduction_78 happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	happyIn58
		 (LitStrValueStatement happy_var_1
	)}

happyReduce_79 = happyReduce 5# 19# happyReduction_79
happyReduction_79 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut39 happy_x_2 of { happy_var_2 -> 
	case happyOut58 happy_x_4 of { happy_var_4 -> 
	happyIn58
		 (FValueStatement happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_80 = happySpecReduce_2  19# happyReduction_80
happyReduction_80 happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_1 of { happy_var_1 -> 
	case happyOut77 happy_x_2 of { happy_var_2 -> 
	happyIn58
		 (Expr happy_var_1 happy_var_2
	)}}

happyReduce_81 = happySpecReduce_0  20# happyReduction_81
happyReduction_81  =  happyIn59
		 ([]
	)

happyReduce_82 = happySpecReduce_1  20# happyReduction_82
happyReduction_82 happy_x_1
	 =  case happyOut58 happy_x_1 of { happy_var_1 -> 
	happyIn59
		 ((:[]) happy_var_1
	)}

happyReduce_83 = happySpecReduce_3  20# happyReduction_83
happyReduction_83 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_1 of { happy_var_1 -> 
	case happyOut59 happy_x_3 of { happy_var_3 -> 
	happyIn59
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_84 = happyReduce 6# 21# happyReduction_84
happyReduction_84 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut61 happy_x_2 of { happy_var_2 -> 
	case happyOut39 happy_x_3 of { happy_var_3 -> 
	case happyOut58 happy_x_5 of { happy_var_5 -> 
	happyIn60
		 (RefDefB happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_85 = happyReduce 4# 22# happyReduction_85
happyReduction_85 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut62 happy_x_3 of { happy_var_3 -> 
	happyIn61
		 (TypeB happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_86 = happyReduce 5# 22# happyReduction_86
happyReduction_86 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut61 happy_x_2 of { happy_var_2 -> 
	case happyOut61 happy_x_4 of { happy_var_4 -> 
	happyIn61
		 (FunType happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_87 = happySpecReduce_3  22# happyReduction_87
happyReduction_87 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut62 happy_x_2 of { happy_var_2 -> 
	happyIn61
		 (TType happy_var_2
	)}

happyReduce_88 = happySpecReduce_0  23# happyReduction_88
happyReduction_88  =  happyIn62
		 ([]
	)

happyReduce_89 = happySpecReduce_1  23# happyReduction_89
happyReduction_89 happy_x_1
	 =  case happyOut61 happy_x_1 of { happy_var_1 -> 
	happyIn62
		 ((:[]) happy_var_1
	)}

happyReduce_90 = happySpecReduce_3  23# happyReduction_90
happyReduction_90 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut61 happy_x_1 of { happy_var_1 -> 
	case happyOut62 happy_x_3 of { happy_var_3 -> 
	happyIn62
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_91 = happyReduce 8# 24# happyReduction_91
happyReduction_91 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut39 happy_x_2 of { happy_var_2 -> 
	case happyOut65 happy_x_4 of { happy_var_4 -> 
	case happyOut67 happy_x_7 of { happy_var_7 -> 
	happyIn63
		 (AlgTypeB happy_var_2 happy_var_4 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_92 = happySpecReduce_1  25# happyReduction_92
happyReduction_92 happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	happyIn64
		 (TypeArgB happy_var_1
	)}

happyReduce_93 = happySpecReduce_0  26# happyReduction_93
happyReduction_93  =  happyIn65
		 ([]
	)

happyReduce_94 = happySpecReduce_1  26# happyReduction_94
happyReduction_94 happy_x_1
	 =  case happyOut64 happy_x_1 of { happy_var_1 -> 
	happyIn65
		 ((:[]) happy_var_1
	)}

happyReduce_95 = happySpecReduce_3  26# happyReduction_95
happyReduction_95 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut64 happy_x_1 of { happy_var_1 -> 
	case happyOut65 happy_x_3 of { happy_var_3 -> 
	happyIn65
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_96 = happyReduce 4# 27# happyReduction_96
happyReduction_96 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut61 happy_x_3 of { happy_var_3 -> 
	happyIn66
		 (AlgTypeValB happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_97 = happySpecReduce_0  28# happyReduction_97
happyReduction_97  =  happyIn67
		 ([]
	)

happyReduce_98 = happySpecReduce_1  28# happyReduction_98
happyReduction_98 happy_x_1
	 =  case happyOut66 happy_x_1 of { happy_var_1 -> 
	happyIn67
		 ((:[]) happy_var_1
	)}

happyReduce_99 = happySpecReduce_3  28# happyReduction_99
happyReduction_99 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut66 happy_x_1 of { happy_var_1 -> 
	case happyOut67 happy_x_3 of { happy_var_3 -> 
	happyIn67
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_100 = happySpecReduce_1  29# happyReduction_100
happyReduction_100 happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn68
		 (PatternMatchI happy_var_1
	)}

happyReduce_101 = happySpecReduce_1  29# happyReduction_101
happyReduction_101 happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	happyIn68
		 (PatternMatchB happy_var_1
	)}

happyReduce_102 = happySpecReduce_3  29# happyReduction_102
happyReduction_102 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut69 happy_x_2 of { happy_var_2 -> 
	happyIn68
		 (TPatternMatch happy_var_2
	)}

happyReduce_103 = happyReduce 4# 29# happyReduction_103
happyReduction_103 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut68 happy_x_1 of { happy_var_1 -> 
	case happyOut69 happy_x_3 of { happy_var_3 -> 
	happyIn68
		 (CPatternMatch happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_104 = happySpecReduce_0  30# happyReduction_104
happyReduction_104  =  happyIn69
		 ([]
	)

happyReduce_105 = happySpecReduce_1  30# happyReduction_105
happyReduction_105 happy_x_1
	 =  case happyOut68 happy_x_1 of { happy_var_1 -> 
	happyIn69
		 ((:[]) happy_var_1
	)}

happyReduce_106 = happySpecReduce_3  30# happyReduction_106
happyReduction_106 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut68 happy_x_1 of { happy_var_1 -> 
	case happyOut69 happy_x_3 of { happy_var_3 -> 
	happyIn69
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_107 = happyReduce 6# 31# happyReduction_107
happyReduction_107 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut61 happy_x_1 of { happy_var_1 -> 
	case happyOut68 happy_x_3 of { happy_var_3 -> 
	case happyOut58 happy_x_5 of { happy_var_5 -> 
	happyIn70
		 (AssignmentB happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_108 = happySpecReduce_1  31# happyReduction_108
happyReduction_108 happy_x_1
	 =  case happyOut60 happy_x_1 of { happy_var_1 -> 
	happyIn70
		 (RefAssignment happy_var_1
	)}

happyReduce_109 = happySpecReduce_0  32# happyReduction_109
happyReduction_109  =  happyIn71
		 ([]
	)

happyReduce_110 = happySpecReduce_2  32# happyReduction_110
happyReduction_110 happy_x_2
	happy_x_1
	 =  case happyOut71 happy_x_1 of { happy_var_1 -> 
	case happyOut70 happy_x_2 of { happy_var_2 -> 
	happyIn71
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_111 = happySpecReduce_3  33# happyReduction_111
happyReduction_111 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut72 happy_x_3 of { happy_var_3 -> 
	happyIn72
		 (SFunApplication happy_var_1 happy_var_3
	)}}

happyReduce_112 = happyReduce 4# 33# happyReduction_112
happyReduction_112 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut74 happy_x_3 of { happy_var_3 -> 
	happyIn72
		 (FunApplicationB happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_113 = happySpecReduce_1  34# happyReduction_113
happyReduction_113 happy_x_1
	 =  case happyOut58 happy_x_1 of { happy_var_1 -> 
	happyIn73
		 (FunctionArgApplB happy_var_1
	)}

happyReduce_114 = happySpecReduce_0  35# happyReduction_114
happyReduction_114  =  happyIn74
		 ([]
	)

happyReduce_115 = happySpecReduce_1  35# happyReduction_115
happyReduction_115 happy_x_1
	 =  case happyOut73 happy_x_1 of { happy_var_1 -> 
	happyIn74
		 ((:[]) happy_var_1
	)}

happyReduce_116 = happySpecReduce_3  35# happyReduction_116
happyReduction_116 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut73 happy_x_1 of { happy_var_1 -> 
	case happyOut74 happy_x_3 of { happy_var_3 -> 
	happyIn74
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_117 = happySpecReduce_3  36# happyReduction_117
happyReduction_117 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut59 happy_x_2 of { happy_var_2 -> 
	happyIn75
		 (ListValueStatementB happy_var_2
	)}

happyReduce_118 = happySpecReduce_3  37# happyReduction_118
happyReduction_118 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut59 happy_x_2 of { happy_var_2 -> 
	happyIn76
		 (TupleValueStatementB happy_var_2
	)}

happyReduce_119 = happySpecReduce_2  38# happyReduction_119
happyReduction_119 happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_2 of { happy_var_2 -> 
	happyIn77
		 (EAdd happy_var_2
	)}

happyReduce_120 = happySpecReduce_2  38# happyReduction_120
happyReduction_120 happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_2 of { happy_var_2 -> 
	happyIn77
		 (ESub happy_var_2
	)}

happyReduce_121 = happySpecReduce_2  38# happyReduction_121
happyReduction_121 happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_2 of { happy_var_2 -> 
	happyIn77
		 (EMod happy_var_2
	)}

happyReduce_122 = happySpecReduce_2  38# happyReduction_122
happyReduction_122 happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_2 of { happy_var_2 -> 
	happyIn77
		 (EMul happy_var_2
	)}

happyReduce_123 = happySpecReduce_2  38# happyReduction_123
happyReduction_123 happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_2 of { happy_var_2 -> 
	happyIn77
		 (EDiv happy_var_2
	)}

happyReduce_124 = happySpecReduce_2  38# happyReduction_124
happyReduction_124 happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_2 of { happy_var_2 -> 
	happyIn77
		 (EL happy_var_2
	)}

happyReduce_125 = happySpecReduce_2  38# happyReduction_125
happyReduction_125 happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_2 of { happy_var_2 -> 
	happyIn77
		 (ELQ happy_var_2
	)}

happyReduce_126 = happySpecReduce_2  38# happyReduction_126
happyReduction_126 happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_2 of { happy_var_2 -> 
	happyIn77
		 (EG happy_var_2
	)}

happyReduce_127 = happySpecReduce_2  38# happyReduction_127
happyReduction_127 happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_2 of { happy_var_2 -> 
	happyIn77
		 (EGQ happy_var_2
	)}

happyReduce_128 = happySpecReduce_2  38# happyReduction_128
happyReduction_128 happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_2 of { happy_var_2 -> 
	happyIn77
		 (EEQ happy_var_2
	)}

happyReduce_129 = happySpecReduce_2  38# happyReduction_129
happyReduction_129 happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_2 of { happy_var_2 -> 
	happyIn77
		 (ENE happy_var_2
	)}

happyNewToken action sts stk [] =
	happyDoAction 43# notHappyAtAll action sts stk []

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
	PT _ (TS _ 35) -> cont 35#;
	PT _ (TS _ 36) -> cont 36#;
	PT _ (TS _ 37) -> cont 37#;
	PT _ (TS _ 38) -> cont 38#;
	PT _ (TV happy_dollar_dollar) -> cont 39#;
	PT _ (TI happy_dollar_dollar) -> cont 40#;
	PT _ (TL happy_dollar_dollar) -> cont 41#;
	_ -> cont 42#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 43# tk tks = happyError' (tks, explist)
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
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut42 x))

pStructOrInterfaceOrType tks = happySomeParser where
 happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (happyOut43 x))

pListStructOrInterfaceOrType tks = happySomeParser where
 happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (happyOut44 x))

pStruct tks = happySomeParser where
 happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (happyOut45 x))

pStructBody tks = happySomeParser where
 happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (happyOut46 x))

pStructField tks = happySomeParser where
 happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (happyOut47 x))

pListStructField tks = happySomeParser where
 happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (happyOut48 x))

pInterfaceId tks = happySomeParser where
 happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (happyOut49 x))

pListInterfaceId tks = happySomeParser where
 happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (happyOut50 x))

pInterface tks = happySomeParser where
 happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (happyOut51 x))

pInterfaceBody tks = happySomeParser where
 happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (happyOut52 x))

pFunOrRefDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (happyOut53 x))

pListFunOrRefDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 12# tks) (\x -> happyReturn (happyOut54 x))

pFunctionDef tks = happySomeParser where
 happySomeParser = happyThen (happyParse 13# tks) (\x -> happyReturn (happyOut55 x))

pFunctionArg tks = happySomeParser where
 happySomeParser = happyThen (happyParse 14# tks) (\x -> happyReturn (happyOut56 x))

pListFunctionArg tks = happySomeParser where
 happySomeParser = happyThen (happyParse 15# tks) (\x -> happyReturn (happyOut57 x))

pValueStatement tks = happySomeParser where
 happySomeParser = happyThen (happyParse 16# tks) (\x -> happyReturn (happyOut58 x))

pListValueStatement tks = happySomeParser where
 happySomeParser = happyThen (happyParse 17# tks) (\x -> happyReturn (happyOut59 x))

pRefDef tks = happySomeParser where
 happySomeParser = happyThen (happyParse 18# tks) (\x -> happyReturn (happyOut60 x))

pType tks = happySomeParser where
 happySomeParser = happyThen (happyParse 19# tks) (\x -> happyReturn (happyOut61 x))

pListType tks = happySomeParser where
 happySomeParser = happyThen (happyParse 20# tks) (\x -> happyReturn (happyOut62 x))

pAlgType tks = happySomeParser where
 happySomeParser = happyThen (happyParse 21# tks) (\x -> happyReturn (happyOut63 x))

pTypeArg tks = happySomeParser where
 happySomeParser = happyThen (happyParse 22# tks) (\x -> happyReturn (happyOut64 x))

pListTypeArg tks = happySomeParser where
 happySomeParser = happyThen (happyParse 23# tks) (\x -> happyReturn (happyOut65 x))

pAlgTypeVal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 24# tks) (\x -> happyReturn (happyOut66 x))

pListAlgTypeVal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 25# tks) (\x -> happyReturn (happyOut67 x))

pPatternMatch tks = happySomeParser where
 happySomeParser = happyThen (happyParse 26# tks) (\x -> happyReturn (happyOut68 x))

pListPatternMatch tks = happySomeParser where
 happySomeParser = happyThen (happyParse 27# tks) (\x -> happyReturn (happyOut69 x))

pAssignment tks = happySomeParser where
 happySomeParser = happyThen (happyParse 28# tks) (\x -> happyReturn (happyOut70 x))

pListAssignment tks = happySomeParser where
 happySomeParser = happyThen (happyParse 29# tks) (\x -> happyReturn (happyOut71 x))

pFunApplication tks = happySomeParser where
 happySomeParser = happyThen (happyParse 30# tks) (\x -> happyReturn (happyOut72 x))

pFunctionArgAppl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 31# tks) (\x -> happyReturn (happyOut73 x))

pListFunctionArgAppl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 32# tks) (\x -> happyReturn (happyOut74 x))

pListValueStatementr tks = happySomeParser where
 happySomeParser = happyThen (happyParse 33# tks) (\x -> happyReturn (happyOut75 x))

pTupleValueStatementr tks = happySomeParser where
 happySomeParser = happyThen (happyParse 34# tks) (\x -> happyReturn (happyOut76 x))

pValueStatementExpr tks = happySomeParser where
 happySomeParser = happyThen (happyParse 35# tks) (\x -> happyReturn (happyOut77 x))

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
{-# LINE 10 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "/home/shaponiuk/.stack/programs/x86_64-linux/ghc-tinfo6-8.6.4/lib/ghc-8.6.4/include/ghcversion.h" #-}















{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc37637_0/ghc_2.h" #-}
































































































































































































{-# LINE 10 "<command-line>" #-}
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