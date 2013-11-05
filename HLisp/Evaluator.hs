module HLisp.Evaluator (eval, evalAll) where

import HLisp.SExpr
import HLisp.Environment
import Control.Monad

{- evalAll:
 - Evaluate a series of S-Expressions -}
evalAll = mapM eval

{- eval sexp
 -
 - Evaluate the S-Expression sexp, in the monadic environment. -}
eval :: SExpr -> Statement
eval Nil                                 = return Nil
eval TrueAtom                            = return TrueAtom
eval n@(IntAtom _)                       = return n
eval f@(FloatAtom _)                     = return f
eval s@(StringAtom _)                    = return s
eval f@(Native _ _)                      = return f
eval (Atom a)                            = envGet a
eval (Cons (Atom "QUOTE") sexpr)         = evalQuote sexpr
eval (Cons (Atom "COND") sexpr)          = evalCond sexpr
eval (Cons (Atom "SET") sexpr)           = evalSet sexpr
eval (Cons (Atom "SETQ") sexpr)          = evalSetQ sexpr
eval (Cons (Atom "DEFUN") sexpr)         = evalDefun sexpr
eval (Cons (Atom "AND") sexpr)           = evalAnd sexpr
eval (Cons (Atom "OR") sexpr)            = evalOr sexpr
eval (Cons (Atom "PRINT") sexpr)         = evalPrint sexpr
eval (Cons (Atom "APPLY") sexpr)         = evalApply sexpr
eval lambda@(Cons (Atom "LAMBDA") sexpr) = return lambda
eval list@(Cons _ _)                     = evalList list
    
{- evalQuote:
 - Evaluate the QUOTE form. -}
evalQuote :: SExpr -> Statement
evalQuote sexpr = return $ car sexpr

{- evalCond:
 - Evaluate the COND form. -}
evalCond :: SExpr -> Statement
evalCond (Cons thisCase rest) =
    case thisCase of
        (Cons test result) -> do
            evaluatedTest <- eval test
            if evaluatedTest /= Nil then
                eval $ car result
            else
                evalCond rest
        otherwise -> fail "Bad condition in COND"
evalCond _ = fail "Non-exhaustive cases in COND"

{- evalSet:
 - Evaluate the SET) form. -}
evalSet :: SExpr -> Statement
evalSet (Cons ident (Cons value Nil)) = do 
    symbol <- eval ident
    evaluated <- eval value
    case symbol of
        Atom s -> do
            envPut s evaluated
            return evaluated
        otherwise -> fail "SET: first argument must evaluate to symbol."
evalSet _ = fail "SET requires two arguments."


{- evalSetQ:
 - Evaluate the SETQ form. -}
evalSetQ :: SExpr -> Statement
evalSetQ (Cons (Atom s) (Cons value Nil)) = do 
    evaluated <- eval value
    envPut s evaluated
    return evaluated
evalSetQ _ = fail "SETQ requires two arguments, the first must be a symbol."

{- evalDefun:
 - Evaluate the DEFUN form. -}

evalDefun :: SExpr -> Statement
evalDefun (Cons (Atom s) definition@(Cons formals (Cons body Nil))) = do
    envPut s $ Cons (Atom "LAMBDA") definition
evalDefun sexpr = fail $ "DEFUN: Invalid form - " ++ (show sexpr)

{- evalAnd:
 - Evaluate the AND form. -}
evalAnd :: SExpr -> Statement
evalAnd Nil = return TrueAtom
evalAnd (Cons s rest) = do
    evaluated <- eval s
    case evaluated of
        Nil -> return Nil
        _   -> evalAnd rest

{- evalOr:
 - Evaluate the OR form. -}
evalOr :: SExpr -> Statement
evalOr Nil = return Nil
evalOr (Cons s rest) = do
    evaluated <- eval s
    case evaluated of
        Nil -> evalOr rest
        _   -> return TrueAtom

{- evalPrint:
 - Evaluate the PRINT form. -}
evalPrint :: SExpr -> Statement
evalPrint (Cons sexpr Nil) = do
    evaluated <- eval sexpr
    StringAtom outBuffer <- envGet "$OUTPUTBUFFER"
    envPut "$OUTPUTBUFFER" $ StringAtom $ outBuffer ++ show evaluated ++ "\n"
    return evaluated
evalPrint _ = fail "PRINT: Requires exactly one argument."

{- evalApply
 - Evaluate the APPLY form. -}
evalApply (Cons hd (Cons tl Nil)) = do
    func <- eval hd
    args <- eval tl
    eval $ Cons func args
evalApply _ = fail "APPLY: Requires exactly two arguments."

{- evalList:
 - Evaluate a list (as a function application) -}
evalList (Cons hd tl) = do
    func <- eval hd
    args <- mapM eval $ fromLispList tl
    case func of
        Native name f                      -> f args 
        Cons (Atom "LAMBDA") paramsAndBody -> applyLambda paramsAndBody args
        otherwise                          -> fail $ "Unable to apply a non-function: " ++ (show func)

{- applyLambda:
 - Apply an S-Expression representing an anonymous function to a list of
 - of already-evaluated arguments. -}
applyLambda (Cons paramList (Cons body _)) args =
    if length formals /= length args then
        fail "Formal parameter count doesn't match actual parameter count."
    else do
        bindParams formals args
        result <- eval body
        unbindParams formals 
        return result
    where formals = fromLispList paramList

bindParams formals actuals = mapM (\(f, a) -> bindParam f a) $ zip formals actuals

bindParam (Atom k) v = envPut k v
bindParam _ _        = fail "Can't bind value to non-symbol atom."

unbindParams = mapM unbindParam 

unbindParam (Atom k) = envUnbind k
unbindParam _        = fail "Can't unbind a non-symbol atom."



