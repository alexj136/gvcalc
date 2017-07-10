module Interpreter where

import Util
import Syntax

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

newtype Machine =
    Machine (Queue, Heap, ChanPairs)
    deriving (Show, Eq, Ord)

newtype Heap =
    Heap (M.Map Name HeapContents)
    deriving (Show, Eq, Ord)

newtype ChanPairs =
    ChanPairs (M.Map Name Name)
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
emptyMachine = Machine (Queue [], Heap M.empty, ChanPairs M.empty)

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

-- Create a Machine from a Config
fromConfig :: Config -> GVCalc Machine
fromConfig cfg = do
    (_, machine) <- runStateT (fc cfg) emptyMachine
    return machine

    where

    fc :: Config -> StateT Machine GVCalc ()
    fc cfg = case cfg of
        Exe e            -> queuePrepend e
        ChanBuf c d i ms -> do { heapInsert c ms ; chanPairsInsert c d }
        Par p q          -> do { fc q ; fc p }
        New c d p        -> do
            c'  <- lift freshName
            d'  <- lift freshName
            p'  <- lift $ chanSub c' c p
            p'' <- lift $ chanSub d' d p'
            fc p''

    queuePrepend :: Monad m => Exp -> StateT Machine m ()
    queuePrepend e = modify $ \(Machine (Queue q, h, c)) ->
        Machine (Queue (e : q), h, c)

    heapInsert :: Name -> [Val] -> StateT Machine GVCalc ()
    heapInsert n ms = do
        Machine (q, Heap h, c) <- get
        lift $ assert (M.notMember n h) "Duplicate channel buffers found"
        let hc = case ms of { [] -> Empty ; (m : ms) -> Vals (m, ms) }
        put $ Machine (q, Heap $ M.insert n hc h, c)

    chanPairsInsert :: Name -> Name -> StateT Machine GVCalc ()
    chanPairsInsert n m = do
        Machine (q, h, ChanPairs c) <- get
        lift $ assert (M.notMember n c) "Duplicate channel buffers found"
        put $ Machine (q, h, ChanPairs $ M.insert n m c)

step :: Machine -> GVCalc (Maybe Machine)
step (Machine (Queue []           , _     , _          )) = return Nothing
step (Machine (Queue (qhead:qtail), Heap h, ChanPairs c)) = undefined
