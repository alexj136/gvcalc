module Interpreter where

import Util
import Syntax

import Prelude hiding ((/))
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

newtype Machine =
    Machine (Queue, Heap)
    deriving (Show, Eq, Ord)

newtype Heap =
    Heap (M.Map Name (Name, Integer, [Val]))
    deriving (Show, Eq, Ord)

newtype Queue =
    Queue [Exp]
    deriving (Show, Eq, Ord)

emptyMachine :: Machine
emptyMachine = Machine (Queue [], Heap M.empty)

getQueue :: Monad m => StateT Machine m Queue
getQueue = do { Machine (q, _) <- get ; return q }

putQueue :: Monad m => Queue -> StateT Machine m ()
putQueue q = modify $ \(Machine (_, h)) -> Machine (q, h)

getHeap :: Monad m => StateT Machine m Heap
getHeap = do { Machine (_, h) <- get ; return h }

putHeap :: Monad m => Heap -> StateT Machine m ()
putHeap h = modify $ \(Machine (q, _)) -> Machine (q, h)

getHeapContents :: Name -> StateT Machine GVCalc (Name, Integer, [Val])
getHeapContents n = do
    Heap heap <- getHeap
    lift $ mlookup heap n

putHeapContents :: Monad m => Name -> (Name, Integer, [Val]) ->
    StateT Machine m ()
putHeapContents n hc = do
    Heap heap <- getHeap
    putHeap $ Heap $ M.insert n hc heap

toConfig :: Machine -> Config
toConfig (Machine (Queue q, Heap h)) = foldr1 Par $
    map Exe q ++ map (\(c, (d, i, vs)) -> ChanBuf c d i vs) (M.toList h)

-- Create a Machine from a Config
fromConfig :: Config -> GVCalc Machine
fromConfig cfg = do
    (_, machine) <- runStateT (fc cfg) emptyMachine
    return machine

    where

    fc :: Config -> StateT Machine GVCalc ()
    fc cfg = case cfg of
        Exe e            -> do { Queue q <- getQueue ; putQueue (Queue (e:q)) }
        ChanBuf c d i ms -> do
            Heap h <- getHeap
            lift $ assert (M.notMember c h) "Duplicate channel buffers found"
            putHeapContents c (d, i, ms)
        Par p q          -> do { fc q ; fc p }
        New c d p        -> do
            c'  <- lift freshName
            d'  <- lift freshName
            p'  <- lift $ chanSub c' c p
            p'' <- lift $ chanSub d' d p'
            fc p''

run :: Machine -> GVCalc Machine
run m = do
    (progress, m') <- runStateT step m
    (if progress then run else return) m'

step :: StateT Machine GVCalc Bool
step = do
    Queue q <- getQueue
    q' <- mapM reduce q
    putQueue $ Queue q'
    return $ q /= q'

reduce :: Exp -> StateT Machine GVCalc Exp
reduce exp = case exp of

    App (Lit (Lam x e)) (Lit v) -> lift $ (v/x) e
    App (Lit Fix) (Lit (Lam x e)) -> lift $ ((Lam x e)/x) e
    App (App (Lit Send) (Lit v)) (Lit (Var c)) -> do
        deliver v c
        return (Lit (Var c))
    App (Lit Receive) (Lit (Var c)) -> do
        m <- collect c
        case m of
            Just v  -> return $ Pair (Lit v) (Lit (Var c))
            Nothing -> return exp
    App  (Lit v) e  -> reduce e  >>= \e'  -> return $ App  (Lit v) e'
    App  e1 e2      -> reduce e1 >>= \e1' -> return $ App  e1' e2

    Pair (Lit v1) (Lit v2) -> return $ Lit $ ValPair v1 v2
    Pair (Lit v) e  -> reduce e  >>= \e'  -> return $ Pair (Lit v) e'
    Pair e1      e2 -> reduce e1 >>= \e1' -> return $ Pair e1' e2

    Let n1 n2 (Lit (ValPair v1 v2)) e -> lift $ ((v1/n1) >=> (v2/n2)) e
    Let n1 n2 e1 e2 -> reduce e1 >>= \e1' -> return $ Let n1 n2 e1' e2

    Select (Lit v) (Lit (Var c)) -> do
        deliver v c
        return (Lit (Var c))
    Select (Lit v) e  -> reduce e  >>= \e'  -> return $ Select (Lit v) e'
    Select e1      e2 -> reduce e1 >>= \e1' -> return $ Select e1' e2

    Case (Lit (Var c)) e1 e2 -> do
        m <- collect c
        case m of
            Just (Boolean True ) -> return $ App e1 (Lit (Var c))
            Just (Boolean False) -> return $ App e2 (Lit (Var c))
            Just (_            ) -> lift $
                throwError "Non-boolean collected by case expression"
            Nothing -> return exp
    Case e1 e2 e3 -> reduce e1 >>= \e1' -> return $ Case e1' e2 e3

    Lit v -> return exp

    where

    -- Send a given val over a given channel, placing it in the message buffer
    -- for that channel
    deliver :: Val -> Name -> StateT Machine GVCalc ()
    deliver v c = do
        (d , _, _ ) <- getHeapContents c
        (c', i, vs) <- getHeapContents d
        lift $ assert (c == c') "Channel duality inconsistency"
        lift $ assert (toInteger (length vs) < i) "Buffer size exceeded"
        putHeapContents d (c', i, vs ++ [v])

    -- inverse of deliver - collect a message from a given buffer for
    -- substitution into an expression
    collect :: Name -> StateT Machine GVCalc (Maybe Val)
    collect c = do
        contents <- getHeapContents c
        case contents of
            (_, _, []    ) -> return Nothing
            (d, i, (m:ms)) -> do
                putHeapContents c (d, i, ms)
                return $ Just m
