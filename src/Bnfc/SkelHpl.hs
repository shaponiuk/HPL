module SkelHpl where

-- Haskell module generated by the BNF converter

import AbsHpl
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> failure x
transProgram :: Program -> Result
transProgram x = case x of
  ProgramB structorinterfaceortypes -> failure x
transStructOrInterfaceOrType :: StructOrInterfaceOrType -> Result
transStructOrInterfaceOrType x = case x of
  StructOrInterfaceOrTypeS struct -> failure x
  StructOrInterfaceOrTypeT algtype -> failure x
transStruct :: Struct -> Result
transStruct x = case x of
  StructB ident structbody -> failure x
transStructBody :: StructBody -> Result
transStructBody x = case x of
  StructBodyB structfields -> failure x
transStructField :: StructField -> Result
transStructField x = case x of
  StructFieldFunPu functiondef -> failure x
transFunctionDef :: FunctionDef -> Result
transFunctionDef x = case x of
  FunctionDefB type_ ident functionargs valuestatement -> failure x
  SusFunctionDef functiondef -> failure x
transFunctionArg :: FunctionArg -> Result
transFunctionArg x = case x of
  FunctionArgB patternmatch -> failure x
transValueStatement :: ValueStatement -> Result
transValueStatement x = case x of
  ValueStatementB assignments valuestatement -> failure x
  ForceValueStatement assignments valuestatement -> failure x
  IfValueStatement valuestatement1 valuestatement2 valuestatement3 -> failure x
  LValueStatement listvaluestatementr -> failure x
  TValueStatement tuplevaluestatementr -> failure x
  AValueStatement funapplication -> failure x
  IValueStatement integer -> failure x
  LitStrValueStatement string -> failure x
  FValueStatement ident valuestatement -> failure x
  Expr valuestatement valuestatementexpr -> failure x
transRefDef :: RefDef -> Result
transRefDef x = case x of
  RefDefB type_ ident valuestatement -> failure x
transType :: Type -> Result
transType x = case x of
  TypeB ident types -> failure x
  FunType type_1 type_2 -> failure x
  TType types -> failure x
transAlgType :: AlgType -> Result
transAlgType x = case x of
  AlgTypeB ident typeargs algtypevals -> failure x
transTypeArg :: TypeArg -> Result
transTypeArg x = case x of
  TypeArgB ident -> failure x
transAlgTypeVal :: AlgTypeVal -> Result
transAlgTypeVal x = case x of
  AlgTypeValB ident type_ -> failure x
transPatternMatch :: PatternMatch -> Result
transPatternMatch x = case x of
  PatternMatchI integer -> failure x
  PatternMatchB ident -> failure x
  TPatternMatch patternmatchs -> failure x
  CPatternMatch patternmatch patternmatchs -> failure x
transAssignment :: Assignment -> Result
transAssignment x = case x of
  AssignmentB type_ patternmatch valuestatement -> failure x
  RefAssignment refdef -> failure x
transFunApplication :: FunApplication -> Result
transFunApplication x = case x of
  SFunApplication ident funapplication -> failure x
  FunApplicationB ident functionargappls -> failure x
transFunctionArgAppl :: FunctionArgAppl -> Result
transFunctionArgAppl x = case x of
  FunctionArgApplB valuestatement -> failure x
transListValueStatementr :: ListValueStatementr -> Result
transListValueStatementr x = case x of
  ListValueStatementB valuestatements -> failure x
transTupleValueStatementr :: TupleValueStatementr -> Result
transTupleValueStatementr x = case x of
  TupleValueStatementB valuestatements -> failure x
transValueStatementExpr :: ValueStatementExpr -> Result
transValueStatementExpr x = case x of
  EAdd valuestatement -> failure x
  ESub valuestatement -> failure x
  EMod valuestatement -> failure x
  EMul valuestatement -> failure x
  EDiv valuestatement -> failure x
  EL valuestatement -> failure x
  ELQ valuestatement -> failure x
  EG valuestatement -> failure x
  EGQ valuestatement -> failure x
  EEQ valuestatement -> failure x
  ENE valuestatement -> failure x

