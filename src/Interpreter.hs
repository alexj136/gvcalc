module Interpreter where

import Util
import Syntax

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

newtype Machine =
    Machine (Heap, Queue)
    deriving (Show, Eq, Ord)

newtype Heap =
    Heap (M.Map Name HeapContents)
    deriving (Show, Eq, Ord)

data HeapContents
    = Empty
    | Vals (Val, [Val])
    | Exps (Exp, [Exp])
    deriving (Eq, Ord)

instance Show HeapContents where
    show Empty = "Empty"
    show (Vals (v, vs)) = "Vals " ++ show (v : vs)
    show (Exps (e, es)) = "Exps " ++ show (e : es)

newtype Queue =
    Queue [Exp]
    deriving (Show, Eq, Ord)

heapAppend, heapPrepend :: Heap -> Name -> Either Val Exp -> GVCalc Heap
heapAppend (Heap heapMap) n valOrExp = do
    valsOrExps <- mlookup heapMap n
    case valsOrExps of
        Vals  (firstVal, vals) -> case valOrExp of
            Left  val -> return $ Heap $
                M.insert n (Vals  (firstVal, vals ++ [val])) heapMap
            Right exp -> throwError "append of expression to list of values"
        Exps  (firstExp, exps) -> case valOrExp of
            Right exp -> return $ Heap $
                M.insert n (Exps  (firstExp, exps ++ [exp])) heapMap
            Left  val -> throwError "append of value to list of expressions"
        Empty -> case valOrExp of
            Left  val -> return $ Heap $ M.insert n (Vals (val, [])) heapMap
            Right exp -> return $ Heap $ M.insert n (Exps (exp, [])) heapMap
heapPrepend = undefined

fromConfig :: Config -> GVCalc Machine
fromConfig cfg = do
    (h, q) <- fc (M.empty, []) cfg
    return $ Machine (Heap h, Queue q)
    where
    fc :: (M.Map Name HeapContents, [Exp]) -> Config ->
        GVCalc (M.Map Name HeapContents, [Exp])
    fc (h, q) cfg = case cfg of
        Exe e            -> undefined
        ChanBuf c d i ms -> undefined
        Par p q          -> undefined
        New c d p        -> undefined

step :: Machine -> GVCalc (Maybe Machine)
step (Machine (_, Queue [])) = return Nothing
step machine@(Machine (heap@(Heap heapMap), q@(Queue (qlist@(qhead:qtail))))) =
    undefined
