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
    Heap (M.Map Name [Val])
    deriving (Show, Eq, Ord)

newtype ChanPairs =
    ChanPairs (M.Map Name Name)
    deriving (Show, Eq, Ord)

newtype Queue =
    Queue [Exp]
    deriving (Show, Eq, Ord)

emptyMachine :: Machine
emptyMachine = Machine (Queue [], Heap M.empty, ChanPairs M.empty)

getQueue :: Monad m => StateT Machine m Queue
getQueue = do { Machine (q, _, _) <- get ; return q }

putQueue :: Monad m => Queue -> StateT Machine m ()
putQueue q = modify $ \(Machine (_, h, c)) -> Machine (q, h, c)

queuePrepend :: Monad m => Exp -> StateT Machine m ()
queuePrepend e = modify $ \(Machine (Queue q, h, c)) ->
    Machine (Queue (e : q), h, c)

queueAppend :: Monad m => Exp -> StateT Machine m ()
queueAppend e = modify $ \(Machine (Queue q, h, c)) ->
    Machine (Queue (q ++ [e]), h, c)

getHeap :: Monad m => StateT Machine m Heap
getHeap = do { Machine (_, h, _) <- get ; return h }

putHeap :: Monad m => Heap -> StateT Machine m ()
putHeap h = modify $ \(Machine (q, _, c)) -> Machine (q, h, c)

putHeapContents :: Monad m => Name -> [Val] -> StateT Machine m ()
putHeapContents n vs = do
    Heap heap <- getHeap
    putHeap $ Heap $ M.insert n vs heap

getChanPairs :: Monad m => StateT Machine m ChanPairs
getChanPairs = do { Machine (_, _, c) <- get ; return c }

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

    heapInsert :: Name -> [Val] -> StateT Machine GVCalc ()
    heapInsert n vs = do
        Machine (q, Heap h, c) <- get
        lift $ assert (M.notMember n h) "Duplicate channel buffers found"
        put $ Machine (q, Heap $ M.insert n vs h, c)

    chanPairsInsert :: Name -> Name -> StateT Machine GVCalc ()
    chanPairsInsert n m = do
        Machine (q, h, ChanPairs c) <- get
        lift $ assert (M.notMember n c) "Duplicate channel buffers found"
        put $ Machine (q, h, ChanPairs $ M.insert n m c)

step :: Machine -> GVCalc (Maybe Machine)
step (Machine (Queue []           , _     , _          )) = return Nothing
step (Machine (Queue (qhead:qtail), Heap h, ChanPairs c)) = undefined

reduceExpTop :: Exp -> StateT Machine GVCalc Exp
reduceExpTop exp = case exp of
    App (App (Lit Send) (Lit v)) (Lit (Var c)) -> do
        handleSend v c
        return (Lit (Var c))
    App (Lit Receive) (Lit (Var c)) -> undefined

    App e1 e2       -> undefined
    Pair e1 e2      -> undefined
    Let n1 n2 e1 e2 -> undefined
    Select e1 e2    -> undefined
    Case e1 e2 e3   -> undefined
    Lit v           -> do
        ppexp <- lift $ pp exp
        lift $ throwError $ "Irreducible floating value: " ++ ppexp

reduceExpInner :: Exp -> Exp -> StateT Machine GVCalc Exp
reduceExpInner fullExp partExp = case partExp of
    App e1 e2       -> undefined
    Pair e1 e2      -> undefined
    Let n1 n2 e1 e2 -> undefined
    Select e1 e2    -> undefined
    Case e1 e2 e3   -> undefined
    Lit v           -> undefined

-- Send a given val over a given channel, placing it in the message buffer for
-- that channel
handleSend :: Val -> Name -> StateT Machine GVCalc ()
handleSend v c = do
    ChanPairs pairs <- getChanPairs
    bufferId        <- lift $ mlookup pairs c
    Heap heap       <- getHeap
    bufferContents  <- lift $ mlookup heap bufferId
    putHeapContents bufferId (bufferContents ++ [v])
