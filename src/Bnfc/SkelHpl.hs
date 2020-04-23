module Bnfc.SkelHpl where

-- Haskell module generated by the BNF converter

import Bnfc.AbsHpl
import Bnfc.ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident str  -> failure x


transProgram :: Program -> Result
transProgram x = case x of
  ProgramB structorinterfaceortypes  -> failure x


transStructOrInterfaceOrType :: StructOrInterfaceOrType -> Result
transStructOrInterfaceOrType x = case x of
  StructOrInterfaceOrTypeS struct  -> failure x
  StructOrInterfaceOrTypeI interface  -> failure x
  StructOrInterfaceOrTypeT algtype  -> failure x


transStruct :: Struct -> Result
transStruct x = case x of
  StructB id structbody  -> failure x
  StructI id interfaceids structbody  -> failure x


transStructBody :: StructBody -> Result
transStructBody x = case x of
  StructBodyB structfields  -> failure x


transStructField :: StructField -> Result
transStructField x = case x of
  StructFieldFunPr functiondef  -> failure x
  StructFieldFunPu functiondef  -> failure x
  StructFieldRefPr refdef  -> failure x
  StructFieldRefPu refdef  -> failure x


transInterfaceId :: InterfaceId -> Result
transInterfaceId x = case x of
  InterfaceIdB id  -> failure x


transInterface :: Interface -> Result
transInterface x = case x of
  InterfaceB interfaceid interfacebody  -> failure x
  InterfaceBInh interfaceid interfaceids interfacebody  -> failure x


transInterfaceBody :: InterfaceBody -> Result
transInterfaceBody x = case x of
  InterfaceBodyB funorrefdecls  -> failure x


transFunOrRefDecl :: FunOrRefDecl -> Result
transFunOrRefDecl x = case x of
  FunOrRefDeclF type' id  -> failure x
  FunOrRefDeclSF type' id  -> failure x
  FunOrRefDeclR type' id  -> failure x


transFunctionDef :: FunctionDef -> Result
transFunctionDef x = case x of
  FunctionDefB type' id functionargs valuestatement  -> failure x
  SusFunctionDef functiondef  -> failure x


transFunctionArg :: FunctionArg -> Result
transFunctionArg x = case x of
  FunctionArgB patternmatch  -> failure x


transValueStatement :: ValueStatement -> Result
transValueStatement x = case x of
  ValueStatementB assignments valuestatement  -> failure x
  ForceValueStatement assignments valuestatement  -> failure x
  IfValueStatement valuestatement0 valuestatement1 valuestatement  -> failure x
  LValueStatement listvaluestatementr  -> failure x
  TValueStatement tuplevaluestatementr  -> failure x
  AValueStatement funapplication  -> failure x
  IValueStatement n  -> failure x
  LitStrValueStatement str  -> failure x
  FValueStatement id valuestatement  -> failure x
  Expr valuestatement valuestatementexpr  -> failure x


transRefDef :: RefDef -> Result
transRefDef x = case x of
  RefDefB type' id valuestatement  -> failure x


transType :: Type -> Result
transType x = case x of
  TypeB id types  -> failure x
  FunType type'0 type'  -> failure x
  TType types  -> failure x


transAlgType :: AlgType -> Result
transAlgType x = case x of
  AlgTypeB id typeargs algtypevals  -> failure x


transTypeArg :: TypeArg -> Result
transTypeArg x = case x of
  TypeArgB id  -> failure x


transAlgTypeVal :: AlgTypeVal -> Result
transAlgTypeVal x = case x of
  AlgTypeValB id type'  -> failure x


transPatternMatch :: PatternMatch -> Result
transPatternMatch x = case x of
  PatternMatchI n  -> failure x
  PatternMatchB id  -> failure x
  TPatternMatch patternmatchs  -> failure x
  CPatternMatch patternmatch patternmatchs  -> failure x


transAssignment :: Assignment -> Result
transAssignment x = case x of
  AssignmentB type' patternmatch valuestatement  -> failure x
  RefAssignment refdef  -> failure x


transFunApplication :: FunApplication -> Result
transFunApplication x = case x of
  SFunApplication id funapplication  -> failure x
  FunApplicationB id functionargappls  -> failure x


transFunctionArgAppl :: FunctionArgAppl -> Result
transFunctionArgAppl x = case x of
  FunctionArgApplB valuestatement  -> failure x


transListValueStatementr :: ListValueStatementr -> Result
transListValueStatementr x = case x of
  ListValueStatementB valuestatements  -> failure x


transTupleValueStatementr :: TupleValueStatementr -> Result
transTupleValueStatementr x = case x of
  TupleValueStatementB valuestatements  -> failure x


transValueStatementExpr :: ValueStatementExpr -> Result
transValueStatementExpr x = case x of
  EAdd valuestatement  -> failure x
  ESub valuestatement  -> failure x
  EMod valuestatement  -> failure x
  EMul valuestatement  -> failure x
  EDiv valuestatement  -> failure x
  EL valuestatement  -> failure x
  ELQ valuestatement  -> failure x
  EG valuestatement  -> failure x
  EGQ valuestatement  -> failure x
  EEQ valuestatement  -> failure x
  ENE valuestatement  -> failure x



