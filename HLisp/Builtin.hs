module HLisp.Builtin where

import HLisp.SExpr hiding (car, cdr)

cons :: BuiltinFunction
cons [a, b] = return $ Cons a b
cons _      = fail "CONS requires exactly two arguments."

car :: BuiltinFunction
car [Cons a _] = return a
car _          = fail "CAR can only be called on a CONS cell."

cdr :: BuiltinFunction
cdr [Cons _ b] = return b
cdr _          = fail "CDR can only be called on a CONS cell."

equal :: BuiltinFunction
equal [s1, s2] = return $ if (s1 == s2) then TrueAtom else Nil
equal _       = fail "EQUAL: invalid arguments."

listP :: BuiltinFunction
listP [(Cons _ _)] = return TrueAtom
listP [Nil]        = return TrueAtom
listP [ _ ]        = return Nil
listP _            = fail "LISTP: invalid arguments."

list :: BuiltinFunction
list = return . toLispList 

null :: BuiltinFunction
null [Nil] = return TrueAtom
null [_]   = return Nil
null _     = fail "NULL: invalid arguments."

atom :: BuiltinFunction
atom [Cons _ _] = return Nil
atom [_]        = return TrueAtom
atom _          = fail "ATOM: invalid arguments."

numberP :: BuiltinFunction
numberP [IntAtom _]   = return TrueAtom
numberP [FloatAtom _] = return TrueAtom
numberP [_]           = return Nil
numberP _             = fail "NUMBERP: Only one argument expected."

stringP :: BuiltinFunction
stringP [StringAtom _] = return TrueAtom
stringP [_]            = return Nil
stringP _              = fail "STRINGP: Only one argument expected."

builtinError :: BuiltinFunction
builtinError [StringAtom s] = fail s
builtinError [sexpr]        = fail $ show sexpr
builtinError _              = fail "ERROR: Only one argument expected."

{- Arithmetic operations -}
plus :: BuiltinFunction
plus args = return $ foldl (withConversion iAdd fAdd) (IntAtom 0) args

minus :: BuiltinFunction
minus [IntAtom n]   = return $ IntAtom (-n)
minus [FloatAtom x] = return $ FloatAtom (-x)
minus (x:xs)        = return $ foldl (withConversion iSub fSub) x xs

times :: BuiltinFunction
times args = return $ foldl (withConversion iMul fMul) (IntAtom 1) args

divide :: BuiltinFunction
divide [IntAtom n]   = return $ FloatAtom (1.0 / fromInteger n)
divide [FloatAtom x] = return $ FloatAtom (1.0 / x)
divide (x:xs)        = return $ FloatAtom $ foldl (/) (forceFloat x) (map forceFloat xs)

{- Helper functions -}
withConversion iop fop (IntAtom n)   (IntAtom m)   = IntAtom   $ n `iop` m
withConversion iop fop (FloatAtom x) (FloatAtom y) = FloatAtom $ x `fop` y
withConversion iop fop (FloatAtom x) (IntAtom m)   = FloatAtom $ x `fop` (fromInteger m)
withConversion iop fop (IntAtom n)   (FloatAtom y) = FloatAtom $ (fromInteger n) `fop` y
withConversion _   _   _             _             = error "Non-numeric operands."

iAdd = (+) :: Integer -> Integer -> Integer
fAdd = (+) :: Double -> Double -> Double
iSub = (-) :: Integer -> Integer -> Integer
fSub = (-) :: Double -> Double -> Double
iMul = (*) :: Integer -> Integer -> Integer
fMul = (*) :: Double -> Double -> Double

forceFloat (IntAtom n)   = fromInteger n
forceFloat (FloatAtom x) = x
forceFloat _             = error "Cannot force non-numeric atom to float."

