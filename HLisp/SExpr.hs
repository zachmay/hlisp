module HLisp.SExpr where

import qualified Data.Map as M
import Control.Monad.State
import Data.List (intercalate)

type Identifier = String
type Environment = M.Map Identifier [SExpr]
type Statement = State Environment SExpr
type BuiltinFunction = [SExpr] -> Statement

{- S-Expression data type.
 -
 - We distinguish integral and floating-point atoms for purposes of
 - arithmetic operators. -}
data SExpr = Nil
           | TrueAtom
           | Atom String
           | IntAtom Integer
           | FloatAtom Double
           | StringAtom String
           | Cons SExpr SExpr
           | Native String ([SExpr] -> State Environment SExpr)

{- Show instance for SExpr -}
instance Show SExpr where
    show Nil            = "NIL"
    show TrueAtom       = "T"
    show (Atom s)       = s
    show (IntAtom n)    = show n
    show (FloatAtom x)  = show x
    show (StringAtom s) = show s
    show list@(Cons x y)
        | isList list    = let shownComponents = map show $ fromLispList list in
                               '(' : (intercalate " " shownComponents) ++ ")"
        | otherwise      = '(' : (show x) ++ " . " ++ (show y) ++ ")"
    show (Native name _) = name ++ "[NATIVE FUNCTION]"

{- Eq instance for SExpr -}
instance Eq SExpr where
    Nil            == Nil            = True
    TrueAtom       == TrueAtom       = True
    (Atom s)       == (Atom t)       = s == t
    (IntAtom n)    == (IntAtom m)    = n == m
    (FloatAtom x)  == (FloatAtom y)  = x == y
    (StringAtom s) == (StringAtom t) = s == t
    (Cons a b)     == (Cons c d)     = (a == c) && (b == d)
    _              == _              = False


{- toLispList exps
 -
 - Transform a Haskell list of SExpr into a Lisp-style (SExpr) list -}
toLispList :: [SExpr] -> SExpr
toLispList []     = Nil
toLispList (x:xs) = Cons x (toLispList xs)

fromLispList :: SExpr -> [SExpr]
fromLispList Nil        = []
fromLispList (Cons x y) = x : (fromLispList y)

untilAtom (Cons x y) = untilAtom x
untilAtom a          = a

car (Cons x y) = x
car _          = error "CAR on non-list"
cdr (Cons x y) = y
cdr _          = error "CDR on non-list"
caar = car . car
cadr = car . cdr
cdar = cdr . car
cddr = cdr . cdr
cadar = cadr . car

fromAtom (Atom s) = s
fromAtom _        = error "fromAtom on non-atom."

isList :: SExpr -> Bool
isList (Cons s t) = isList t
isList Nil        = True
isList _          = False

intAtomToFloatAtom (IntAtom n) = FloatAtom (fromInteger n)
intAtomToFloatAtom _           = error "intAtomToFloatAtom on non-integer"
