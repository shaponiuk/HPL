module SkelHpl where

-- Haskell module generated by the BNF converter

import Bnfc.AbsHpl
import Bnfc.ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> failure x
transProgram :: Show a => Program a -> Result
transProgram x = case x of
  ProgramB _ functionorrefortypes -> failure x
transFunctionOrRefOrType :: Show a => FunctionOrRefOrType a -> Result
transFunctionOrRefOrType x = case x of
  FunctionOrRefOrTypeF _ functiondef -> failure x
  FunctionOrRefOrTypeT _ algtype -> failure x
  FunctionOrRefOrTypeR _ refdef -> failure x
transFunctionDef :: Show a => FunctionDef a -> Result
transFunctionDef x = case x of
  FunctionDefB _ type_ ident functionargs valuestatement -> failure x
  SusFunctionDef _ type_ ident functionargs valuestatement -> failure x
transFunctionArg :: Show a => FunctionArg a -> Result
transFunctionArg x = case x of
  FunctionArgB _ patternmatch -> failure x
transValueStatement :: Show a => ValueStatement a -> Result
transValueStatement x = case x of
  ValueStatementB _ assignments valuestatement -> failure x
  ForceValueStatement _ assignments valuestatement -> failure x
  IfValueStatement _ valuestatement1 valuestatement2 valuestatement3 -> failure x
  TValueStatement _ tuplevaluestatementr -> failure x
  AValueStatement _ funapplication -> failure x
  IValueStatement _ integer -> failure x
  LitStrValueStatement _ string -> failure x
  FValueStatement _ ident valuestatement -> failure x
  Expr _ valuestatement valuestatementexpr -> failure x
transRefDef :: Show a => RefDef a -> Result
transRefDef x = case x of
  RefDefB _ type_ ident valuestatement -> failure x
transType :: Show a => Type a -> Result
transType x = case x of
  TypeB _ ident types -> failure x
  FunType _ type_1 type_2 -> failure x
  TType _ types -> failure x
transAlgType :: Show a => AlgType a -> Result
transAlgType x = case x of
  AlgTypeB _ ident typeargs algtypevals -> failure x
transTypeArg :: Show a => TypeArg a -> Result
transTypeArg x = case x of
  TypeArgB _ ident -> failure x
transAlgTypeVal :: Show a => AlgTypeVal a -> Result
transAlgTypeVal x = case x of
  AlgTypeValB _ ident type_ -> failure x
transPatternMatch :: Show a => PatternMatch a -> Result
transPatternMatch x = case x of
  PatternMatchI _ integer -> failure x
  PatternMatchB _ ident -> failure x
  TPatternMatch _ patternmatchs -> failure x
  CPatternMatch _ patternmatch patternmatchs -> failure x
transAssignment :: Show a => Assignment a -> Result
transAssignment x = case x of
  AssignmentB _ type_ patternmatch valuestatement -> failure x
  RefAssignment _ refdef -> failure x
transFunApplication :: Show a => FunApplication a -> Result
transFunApplication x = case x of
  FunApplicationB _ ident functionargappls -> failure x
transFunctionArgAppl :: Show a => FunctionArgAppl a -> Result
transFunctionArgAppl x = case x of
  FunctionArgApplB _ valuestatement -> failure x
transTupleValueStatementr :: Show a => TupleValueStatementr a -> Result
transTupleValueStatementr x = case x of
  TupleValueStatementB _ valuestatements -> failure x
transValueStatementExpr :: Show a => ValueStatementExpr a -> Result
transValueStatementExpr x = case x of
  EAdd _ valuestatement -> failure x
  ESub _ valuestatement -> failure x
  EMod _ valuestatement -> failure x
  EMul _ valuestatement -> failure x
  EDiv _ valuestatement -> failure x
  EL _ valuestatement -> failure x
  ELQ _ valuestatement -> failure x
  EG _ valuestatement -> failure x
  EGQ _ valuestatement -> failure x
  EEQ _ valuestatement -> failure x
  ENE _ valuestatement -> failure x

