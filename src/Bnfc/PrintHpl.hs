{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Bnfc.PrintHpl where

-- pretty-printer generated by the BNF converter

import Bnfc.AbsHpl
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else ' ':s)

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString ( i))



instance Print (Program a) where
  prt i e = case e of
    ProgramB _ functionorrefortypes -> prPrec i 0 (concatD [prt 0 functionorrefortypes])

instance Print (FunctionOrRefOrType a) where
  prt i e = case e of
    FunctionOrRefOrTypeF _ functiondef -> prPrec i 0 (concatD [prt 0 functiondef])
    FunctionOrRefOrTypeT _ algtype -> prPrec i 0 (concatD [prt 0 algtype])
    FunctionOrRefOrTypeR _ refdef -> prPrec i 0 (concatD [prt 0 refdef])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print (FunctionDef a) where
  prt i e = case e of
    FunctionDefB _ type_ id functionargs valuestatement -> prPrec i 0 (concatD [doc (showString "fun"), prt 0 type_, prt 0 id, doc (showString "("), prt 0 functionargs, doc (showString ")"), doc (showString "="), prt 0 valuestatement, doc (showString ";")])
    SusFunctionDef _ type_ id functionargs valuestatement -> prPrec i 0 (concatD [doc (showString "sus"), doc (showString "fun"), prt 0 type_, prt 0 id, doc (showString "("), prt 0 functionargs, doc (showString ")"), doc (showString "="), prt 0 valuestatement, doc (showString ";")])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print (FunctionArg a) where
  prt i e = case e of
    FunctionArgB _ patternmatch -> prPrec i 0 (concatD [prt 0 patternmatch])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print (ValueStatement a) where
  prt i e = case e of
    ValueStatementB _ assignments valuestatement -> prPrec i 0 (concatD [doc (showString "let"), prt 0 assignments, doc (showString "in"), prt 0 valuestatement])
    ForceValueStatement _ assignments valuestatement -> prPrec i 0 (concatD [doc (showString "force"), doc (showString "let"), prt 0 assignments, doc (showString "in"), prt 0 valuestatement])
    IfValueStatement _ valuestatement1 valuestatement2 valuestatement3 -> prPrec i 0 (concatD [doc (showString "if"), prt 0 valuestatement1, doc (showString "then"), prt 0 valuestatement2, doc (showString "else"), prt 0 valuestatement3])
    TValueStatement _ tuplevaluestatementr -> prPrec i 0 (concatD [prt 0 tuplevaluestatementr])
    AValueStatement _ funapplication -> prPrec i 0 (concatD [prt 0 funapplication])
    IValueStatement _ n -> prPrec i 0 (concatD [prt 0 n])
    LitStrValueStatement _ str -> prPrec i 0 (concatD [prt 0 str])
    FValueStatement _ id valuestatement -> prPrec i 0 (concatD [doc (showString "{"), prt 0 id, doc (showString "->"), prt 0 valuestatement, doc (showString "}")])
    Expr _ valuestatement valuestatementexpr -> prPrec i 0 (concatD [prt 0 valuestatement, prt 0 valuestatementexpr])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print (RefDef a) where
  prt i e = case e of
    RefDefB _ type_ id valuestatement -> prPrec i 0 (concatD [doc (showString "ref"), prt 0 type_, prt 0 id, doc (showString "="), prt 0 valuestatement, doc (showString ";")])

instance Print (Type a) where
  prt i e = case e of
    TypeB _ id types -> prPrec i 0 (concatD [prt 0 id, doc (showString "("), prt 0 types, doc (showString ")")])
    FunType _ type_1 type_2 -> prPrec i 0 (concatD [doc (showString "("), prt 0 type_1, doc (showString "->"), prt 0 type_2, doc (showString ")")])
    TType _ types -> prPrec i 0 (concatD [doc (showString "("), prt 0 types, doc (showString ")")])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print (AlgType a) where
  prt i e = case e of
    AlgTypeB _ id typeargs algtypevals -> prPrec i 0 (concatD [doc (showString "data"), prt 0 id, doc (showString "("), prt 0 typeargs, doc (showString ")"), doc (showString "="), prt 0 algtypevals, doc (showString ";")])

instance Print (TypeArg a) where
  prt i e = case e of
    TypeArgB _ id -> prPrec i 0 (concatD [prt 0 id])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print (AlgTypeVal a) where
  prt i e = case e of
    AlgTypeValB _ id type_ -> prPrec i 0 (concatD [prt 0 id, doc (showString "("), prt 0 type_, doc (showString ")")])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString "|"), prt 0 xs])
instance Print (PatternMatch a) where
  prt i e = case e of
    PatternMatchI _ n -> prPrec i 0 (concatD [prt 0 n])
    PatternMatchB _ id -> prPrec i 0 (concatD [prt 0 id])
    TPatternMatch _ patternmatchs -> prPrec i 0 (concatD [doc (showString "("), prt 0 patternmatchs, doc (showString ")")])
    CPatternMatch _ patternmatch patternmatchs -> prPrec i 0 (concatD [prt 0 patternmatch, doc (showString "("), prt 0 patternmatchs, doc (showString ")")])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print (Assignment a) where
  prt i e = case e of
    AssignmentB _ type_ patternmatch valuestatement -> prPrec i 0 (concatD [prt 0 type_, doc (showString "::"), prt 0 patternmatch, doc (showString "="), prt 0 valuestatement, doc (showString ";")])
    RefAssignment _ refdef -> prPrec i 0 (concatD [prt 0 refdef])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print (FunApplication a) where
  prt i e = case e of
    FunApplicationB _ id functionargappls -> prPrec i 0 (concatD [prt 0 id, doc (showString "("), prt 0 functionargappls, doc (showString ")")])

instance Print (FunctionArgAppl a) where
  prt i e = case e of
    FunctionArgApplB _ valuestatement -> prPrec i 0 (concatD [prt 0 valuestatement])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print (TupleValueStatementr a) where
  prt i e = case e of
    TupleValueStatementB _ valuestatements -> prPrec i 0 (concatD [doc (showString "("), prt 0 valuestatements, doc (showString ")")])

instance Print (ValueStatementExpr a) where
  prt i e = case e of
    EAdd _ valuestatement -> prPrec i 0 (concatD [doc (showString "+"), prt 0 valuestatement])
    ESub _ valuestatement -> prPrec i 0 (concatD [doc (showString "-"), prt 0 valuestatement])
    EMod _ valuestatement -> prPrec i 0 (concatD [doc (showString "%"), prt 0 valuestatement])
    EMul _ valuestatement -> prPrec i 0 (concatD [doc (showString "*"), prt 0 valuestatement])
    EDiv _ valuestatement -> prPrec i 0 (concatD [doc (showString "/"), prt 0 valuestatement])
    EL _ valuestatement -> prPrec i 0 (concatD [doc (showString "<"), prt 0 valuestatement])
    ELQ _ valuestatement -> prPrec i 0 (concatD [doc (showString "<="), prt 0 valuestatement])
    EG _ valuestatement -> prPrec i 0 (concatD [doc (showString ">"), prt 0 valuestatement])
    EGQ _ valuestatement -> prPrec i 0 (concatD [doc (showString ">="), prt 0 valuestatement])
    EEQ _ valuestatement -> prPrec i 0 (concatD [doc (showString "=="), prt 0 valuestatement])
    ENE _ valuestatement -> prPrec i 0 (concatD [doc (showString "!="), prt 0 valuestatement])


