{-# OPTIONS -fno-warn-incomplete-patterns #-}
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
  space t = showString t . (\s -> if null s then "" else (' ':s))

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
  prtList :: [a] -> Doc
  prtList = concatD . map (prt 0)

instance Print a => Print [a] where
  prt _ = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

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



instance Print Program where
  prt i e = case e of
   ProgramB functionorrefortypes -> prPrec i 0 (concatD [prt 0 functionorrefortypes])


instance Print FunctionOrRefOrType where
  prt i e = case e of
   FunctionOrRefOrTypeF functiondef -> prPrec i 0 (concatD [prt 0 functiondef])
   FunctionOrRefOrTypeT algtype -> prPrec i 0 (concatD [prt 0 algtype])
   FunctionOrRefOrTypeR refdef -> prPrec i 0 (concatD [prt 0 refdef])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print FunctionDef where
  prt i e = case e of
   FunctionDefB type' id functionargs valuestatement -> prPrec i 0 (concatD [doc (showString "fun") , prt 0 type' , prt 0 id , doc (showString "(") , prt 0 functionargs , doc (showString ")") , doc (showString "=") , prt 0 valuestatement , doc (showString ";")])
   SusFunctionDef type' id functionargs valuestatement -> prPrec i 0 (concatD [doc (showString "sus") , doc (showString "fun") , prt 0 type' , prt 0 id , doc (showString "(") , prt 0 functionargs , doc (showString ")") , doc (showString "=") , prt 0 valuestatement , doc (showString ";")])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print FunctionArg where
  prt i e = case e of
   FunctionArgB patternmatch -> prPrec i 0 (concatD [prt 0 patternmatch])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print ValueStatement where
  prt i e = case e of
   ValueStatementB assignments valuestatement -> prPrec i 0 (concatD [doc (showString "let") , prt 0 assignments , doc (showString "in") , prt 0 valuestatement])
   ForceValueStatement assignments valuestatement -> prPrec i 0 (concatD [doc (showString "force") , doc (showString "let") , prt 0 assignments , doc (showString "in") , prt 0 valuestatement])
   IfValueStatement valuestatement0 valuestatement1 valuestatement -> prPrec i 0 (concatD [doc (showString "if") , prt 0 valuestatement0 , doc (showString "then") , prt 0 valuestatement1 , doc (showString "else") , prt 0 valuestatement])
   LValueStatement listvaluestatementr -> prPrec i 0 (concatD [prt 0 listvaluestatementr])
   TValueStatement tuplevaluestatementr -> prPrec i 0 (concatD [prt 0 tuplevaluestatementr])
   AValueStatement funapplication -> prPrec i 0 (concatD [prt 0 funapplication])
   IValueStatement n -> prPrec i 0 (concatD [prt 0 n])
   LitStrValueStatement str -> prPrec i 0 (concatD [prt 0 str])
   FValueStatement id valuestatement -> prPrec i 0 (concatD [doc (showString "{") , prt 0 id , doc (showString "->") , prt 0 valuestatement , doc (showString "}")])
   Expr valuestatement valuestatementexpr -> prPrec i 0 (concatD [prt 0 valuestatement , prt 0 valuestatementexpr])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print RefDef where
  prt i e = case e of
   RefDefB type' id valuestatement -> prPrec i 0 (concatD [doc (showString "ref") , prt 0 type' , prt 0 id , doc (showString "=") , prt 0 valuestatement , doc (showString ";")])


instance Print Type where
  prt i e = case e of
   TypeB id types -> prPrec i 0 (concatD [prt 0 id , doc (showString "(") , prt 0 types , doc (showString ")")])
   FunType type'0 type' -> prPrec i 0 (concatD [doc (showString "(") , prt 0 type'0 , doc (showString "->") , prt 0 type' , doc (showString ")")])
   TType types -> prPrec i 0 (concatD [doc (showString "(") , prt 0 types , doc (showString ")")])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print AlgType where
  prt i e = case e of
   AlgTypeB id typeargs algtypevals -> prPrec i 0 (concatD [doc (showString "data") , prt 0 id , doc (showString "(") , prt 0 typeargs , doc (showString ")") , doc (showString "=") , prt 0 algtypevals , doc (showString ";")])


instance Print TypeArg where
  prt i e = case e of
   TypeArgB id -> prPrec i 0 (concatD [prt 0 id])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print AlgTypeVal where
  prt i e = case e of
   AlgTypeValB id type' -> prPrec i 0 (concatD [prt 0 id , doc (showString "(") , prt 0 type' , doc (showString ")")])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString "|") , prt 0 xs])

instance Print PatternMatch where
  prt i e = case e of
   PatternMatchI n -> prPrec i 0 (concatD [prt 0 n])
   PatternMatchB id -> prPrec i 0 (concatD [prt 0 id])
   TPatternMatch patternmatchs -> prPrec i 0 (concatD [doc (showString "(") , prt 0 patternmatchs , doc (showString ")")])
   CPatternMatch patternmatch patternmatchs -> prPrec i 0 (concatD [prt 0 patternmatch , doc (showString "(") , prt 0 patternmatchs , doc (showString ")")])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print Assignment where
  prt i e = case e of
   AssignmentB type' patternmatch valuestatement -> prPrec i 0 (concatD [prt 0 type' , doc (showString "::") , prt 0 patternmatch , doc (showString "=") , prt 0 valuestatement , doc (showString ";")])
   RefAssignment refdef -> prPrec i 0 (concatD [prt 0 refdef])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print FunApplication where
  prt i e = case e of
   SFunApplication id funapplication -> prPrec i 0 (concatD [prt 0 id , doc (showString ".") , prt 0 funapplication])
   FunApplicationB id functionargappls -> prPrec i 0 (concatD [prt 0 id , doc (showString "(") , prt 0 functionargappls , doc (showString ")")])


instance Print FunctionArgAppl where
  prt i e = case e of
   FunctionArgApplB valuestatement -> prPrec i 0 (concatD [prt 0 valuestatement])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print ListValueStatementr where
  prt i e = case e of
   ListValueStatementB valuestatements -> prPrec i 0 (concatD [doc (showString "[") , prt 0 valuestatements , doc (showString "]")])


instance Print TupleValueStatementr where
  prt i e = case e of
   TupleValueStatementB valuestatements -> prPrec i 0 (concatD [doc (showString "(") , prt 0 valuestatements , doc (showString ")")])


instance Print ValueStatementExpr where
  prt i e = case e of
   EAdd valuestatement -> prPrec i 0 (concatD [doc (showString "+") , prt 0 valuestatement])
   ESub valuestatement -> prPrec i 0 (concatD [doc (showString "-") , prt 0 valuestatement])
   EMod valuestatement -> prPrec i 0 (concatD [doc (showString "%") , prt 0 valuestatement])
   EMul valuestatement -> prPrec i 0 (concatD [doc (showString "*") , prt 0 valuestatement])
   EDiv valuestatement -> prPrec i 0 (concatD [doc (showString "/") , prt 0 valuestatement])
   EL valuestatement -> prPrec i 0 (concatD [doc (showString "<") , prt 0 valuestatement])
   ELQ valuestatement -> prPrec i 0 (concatD [doc (showString "<=") , prt 0 valuestatement])
   EG valuestatement -> prPrec i 0 (concatD [doc (showString ">") , prt 0 valuestatement])
   EGQ valuestatement -> prPrec i 0 (concatD [doc (showString ">=") , prt 0 valuestatement])
   EEQ valuestatement -> prPrec i 0 (concatD [doc (showString "==") , prt 0 valuestatement])
   ENE valuestatement -> prPrec i 0 (concatD [doc (showString "!=") , prt 0 valuestatement])



