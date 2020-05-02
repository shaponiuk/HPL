

module Bnfc.AbsHpl where

-- Haskell module generated by the BNF converter




newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
data Program = ProgramB [StructOrInterfaceOrType]
  deriving (Eq, Ord, Show, Read)

data StructOrInterfaceOrType
    = StructOrInterfaceOrTypeS Struct
    | StructOrInterfaceOrTypeT AlgType
  deriving (Eq, Ord, Show, Read)

data Struct = StructB Ident StructBody
  deriving (Eq, Ord, Show, Read)

data StructBody = StructBodyB [StructField]
  deriving (Eq, Ord, Show, Read)

data StructField
    = StructFieldFunPr FunctionDef
    | StructFieldFunPu FunctionDef
    | StructFieldRefPr RefDef
    | StructFieldRefPu RefDef
  deriving (Eq, Ord, Show, Read)

data FunctionDef
    = FunctionDefB Type Ident [FunctionArg] ValueStatement
    | SusFunctionDef FunctionDef
  deriving (Eq, Ord, Show, Read)

data FunctionArg = FunctionArgB PatternMatch
  deriving (Eq, Ord, Show, Read)

data ValueStatement
    = ValueStatementB [Assignment] ValueStatement
    | ForceValueStatement [Assignment] ValueStatement
    | IfValueStatement ValueStatement ValueStatement ValueStatement
    | LValueStatement ListValueStatementr
    | TValueStatement TupleValueStatementr
    | AValueStatement FunApplication
    | IValueStatement Integer
    | LitStrValueStatement String
    | FValueStatement Ident ValueStatement
    | Expr ValueStatement ValueStatementExpr
  deriving (Eq, Ord, Show, Read)

data RefDef = RefDefB Type Ident ValueStatement
  deriving (Eq, Ord, Show, Read)

data Type = TypeB Ident [Type] | FunType Type Type | TType [Type]
  deriving (Eq, Ord, Show, Read)

data AlgType = AlgTypeB Ident [TypeArg] [AlgTypeVal]
  deriving (Eq, Ord, Show, Read)

data TypeArg = TypeArgB Ident
  deriving (Eq, Ord, Show, Read)

data AlgTypeVal = AlgTypeValB Ident Type
  deriving (Eq, Ord, Show, Read)

data PatternMatch
    = PatternMatchI Integer
    | PatternMatchB Ident
    | TPatternMatch [PatternMatch]
    | CPatternMatch PatternMatch [PatternMatch]
  deriving (Eq, Ord, Show, Read)

data Assignment
    = AssignmentB Type PatternMatch ValueStatement
    | RefAssignment RefDef
  deriving (Eq, Ord, Show, Read)

data FunApplication
    = SFunApplication Ident FunApplication
    | FunApplicationB Ident [FunctionArgAppl]
  deriving (Eq, Ord, Show, Read)

data FunctionArgAppl = FunctionArgApplB ValueStatement
  deriving (Eq, Ord, Show, Read)

data ListValueStatementr = ListValueStatementB [ValueStatement]
  deriving (Eq, Ord, Show, Read)

data TupleValueStatementr = TupleValueStatementB [ValueStatement]
  deriving (Eq, Ord, Show, Read)

data ValueStatementExpr
    = EAdd ValueStatement
    | ESub ValueStatement
    | EMod ValueStatement
    | EMul ValueStatement
    | EDiv ValueStatement
    | EL ValueStatement
    | ELQ ValueStatement
    | EG ValueStatement
    | EGQ ValueStatement
    | EEQ ValueStatement
    | ENE ValueStatement
  deriving (Eq, Ord, Show, Read)

