module HLisp.Environment ( envEmpty
                         , envPut
                         , envGet
                         , envUnbind
                         , defaultEnvironment
                         , runFromDefault ) where

import HLisp.SExpr
import qualified HLisp.Builtin as Builtin
import qualified Data.Map as M
import Control.Monad.State

envEmpty :: Environment
envEmpty = M.empty

defaultEnvironment = M.fromList $ [
    ("*"      , [Native "(*)" Builtin.times]),
    ("+"      , [Native "(+)" Builtin.plus]),
    ("-"      , [Native "(-)" Builtin.minus]),
    ("/"      , [Native "(/)" Builtin.divide]),
    ("ATOM"   , [Native "ATOM" Builtin.atom]),
    ("CAR"    , [Native "CAR" Builtin.car]),
    ("CDR"    , [Native "CDR" Builtin.cdr]),
    ("CONS"   , [Native "CONS" Builtin.cons]),
    ("EQUAL"  , [Native "EQUAL" Builtin.equal]),
    ("EQ"     , [Native "EQ" Builtin.equal]),
    ("ERROR"  , [Native "ERROR" Builtin.builtinError]),
    ("LIST"   , [Native "LIST" Builtin.list]),
    ("LISTP"  , [Native "LISTP" Builtin.listP]),
    ("NULL"   , [Native "NULL" Builtin.null]),
    ("NUMBERP", [Native "NUMBERP" Builtin.numberP]),
    ("STRINGP", [Native "STRINGP" Builtin.stringP]),
    ("$OUTPUTBUFFER", [StringAtom ""])
    ]

runFromDefault x = runState x defaultEnvironment

envPut :: Identifier -> SExpr -> State Environment SExpr
envPut k v = do
    env <- get
    put $ M.insertWith (++) k [v] env
    return v

envGet :: Identifier -> State Environment SExpr
envGet k = do
    env <- get
    case M.lookup k env of
         Just (x:xs) -> return x
         _           -> fail $ "Unbound identifier " ++ k

envUnbind :: Identifier -> State Environment SExpr
envUnbind k = do
    env <- get
    case M.lookup k env of
        Just (x:xs) -> do 
            put $ M.insert k xs env
            return x
        _  -> fail $ "Unbound identifier " ++ k
    



