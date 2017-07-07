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

emptyMachine :: Machine
emptyMachine = Machine (Heap M.empty, Queue [])

insertHeapContents :: Heap -> Name -> HeapContents -> Heap
insertHeapContents (Heap heapMap) n hc = Heap $ M.insert n hc heapMap

heapAppend :: Heap -> Name -> Either Val Exp -> GVCalc Heap
heapAppend heap@(Heap heapMap) n valOrExp = do
    let ins = return . (insertHeapContents heap n)
    valsOrExps <- mlookup heapMap n
    case valsOrExps of
        Vals (firstVal, vals) -> case valOrExp of
            Left  val -> ins $ Vals (firstVal, vals ++ [val])
            Right _   -> throwError "append of expression to list of values"
        Exps (firstExp, exps) -> case valOrExp of
            Right exp -> ins $ Exps (firstExp, exps ++ [exp])
            Left  _   -> throwError "append of value to list of expressions"
        Empty -> case valOrExp of
            Left  val -> ins $ Vals (val, [])
            Right exp -> ins $ Exps (exp, [])

heapPrepend :: Heap -> Name -> Either Val Exp -> GVCalc Heap
heapPrepend heap@(Heap heapMap) n valOrExp = do
    let ins = return . (insertHeapContents heap n)
    valsOrExps <- mlookup heapMap n
    case valsOrExps of
        Vals (firstVal, vals) -> case valOrExp of
            Left  val -> ins $ Vals (val, firstVal : vals)
            Right _   -> throwError "prepend of expression to list of values"
        Exps (firstExp, exps) -> case valOrExp of
            Right exp -> ins $ Exps (exp, firstExp : exps)
            Left  _   -> throwError "prepend of value to list of expressions"
        Empty -> case valOrExp of
            Left  val -> ins $ Vals (val, [])
            Right exp -> ins $ Exps (exp, [])

fromConfig :: Config -> GVCalc Machine
fromConfig cfg = do
    (_, machine) <- runStateT (fc cfg) emptyMachine
    return machine

    where

    fc :: Config -> StateT Machine GVCalc ()
    fc cfg = case cfg of
        Exe e            -> putQueue e
        ChanBuf c d i ms -> undefined
        Par p q          -> do { fc q ; fc p }
        New c d p        -> do
            c'  <- lift freshName
            d'  <- lift freshName
            p'  <- lift $ chanSub c' c p
            p'' <- lift $ chanSub d' d p'
            fc p''

    putQueue :: Monad m => Exp -> StateT Machine m ()
    putQueue e = modify $ \(Machine (h, Queue q)) -> Machine (h, Queue (e : q))

step :: Machine -> GVCalc (Maybe Machine)
step (Machine (_, Queue [])) = return Nothing
step machine@(Machine (heap@(Heap heapMap), q@(Queue (qlist@(qhead:qtail))))) =
    undefined
