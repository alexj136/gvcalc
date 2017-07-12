module Interpreter (fromConfig, toConfig, run) where

import Util
import Syntax

import Prelude hiding ((/))
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

data Context
    = Hole
    | CLet Name Name Context Exp
    | CCase Context Exp Exp
    | CSelectL Context Exp
    | CSelectR Val     Context
    | CAppL    Context Exp
    | CAppR    Val     Context
    | CPairL   Context Exp
    | CPairR   Val     Context
    deriving (Show, Eq, Ord)

apply :: Context -> Exp -> Exp
apply ctx exp = case ctx of
    Hole           -> exp
    CLet n1 n2 c e -> Let n1 n2 (apply c exp) e
    CCase c e2 e3  -> Case (apply c exp) e2 e3
    CSelectL c e   -> Select (apply c exp) e
    CSelectR v c   -> Select (Lit v) (apply c exp)
    CAppL    c e   -> App (apply c exp) e
    CAppR    v c   -> App (Lit v) (apply c exp)
    CPairL   c e   -> Pair (apply c exp) e
    CPairR   v c   -> Pair (Lit v) (apply c exp)

extend ::  Context -> Context -> Context
extend outer inner = case outer of
    Hole           -> inner
    CLet n1 n2 c e -> CLet n1 n2 (extend c inner) e
    CCase c e2 e3  -> CCase (extend c inner) e2 e3
    CSelectL c e   -> CSelectL (extend c inner) e
    CSelectR v c   -> CSelectR v (extend c inner)
    CAppL    c e   -> CAppL (extend c inner) e
    CAppR    v c   -> CAppR v (extend c inner)
    CPairL   c e   -> CPairL (extend c inner) e
    CPairR   v c   -> CPairR v (extend c inner)

newtype Machine =
    Machine (Queue, Heap)
    deriving (Show, Eq, Ord)

newtype Queue =
    Queue [Exp]
    deriving (Show, Eq, Ord)

newtype Heap =
    Heap (M.Map Name HeapContents)
    deriving (Show, Eq, Ord)

type HeapContents =
    ( Name                          -- The dual channel
    , Integer                       -- Msg list maximum size
    , EitherList Val     Context    -- Msg list or waiting receivers
    , EitherList (Context, Integer) -- Waiting requesters or accepters
        (Context, Integer)
    )

emptyMachine :: Machine
emptyMachine = Machine (Queue [], Heap M.empty)

runnable :: Machine -> Bool
runnable (Machine (Queue q, _)) = not $ null q

getQueue :: Monad m => StateT Machine m Queue
getQueue = do { Machine (q, _) <- get ; return q }

putQueue :: Monad m => Queue -> StateT Machine m ()
putQueue q = modify $ \(Machine (_, h)) -> Machine (q, h)

getHeap :: Monad m => StateT Machine m Heap
getHeap = do { Machine (_, h) <- get ; return h }

putHeap :: Monad m => Heap -> StateT Machine m ()
putHeap h = modify $ \(Machine (q, _)) -> Machine (q, h)

getHeapContents :: Name -> StateT Machine GVCalc HeapContents
getHeapContents n = do { Heap h <- getHeap ; lift $ mlookup h n }

putHeapContents :: Monad m => Name -> HeapContents -> StateT Machine m ()
putHeapContents n hc = do
    Heap h <- getHeap
    putHeap $ Heap $ M.insert n hc h

toConfig :: Machine -> Config
toConfig (Machine (Queue q, Heap h)) = let

    queueConfigs :: [Config]
    queueConfigs = map Exe q

    heapConfigs :: [Config]
    heapConfigs = concat $ map heapContentsToConfigs (M.toList h)

    heapContentsToConfigs :: (Name, HeapContents) -> [Config]
    heapContentsToConfigs (c, (d, i, mor, roa)) = let

        chanBuf :: Config
        chanBuf = ChanBuf c d i $ case mor of Lefts (m, ms) -> (m:ms)

        rcvQ :: [Config]
        rcvQ = case toEither mor of
            Left  _    -> []
            Right ctxs ->
                map (Exe . (flip apply) (App (Lit Receive) (Lit (Var c)))) ctxs

        roaConfigs :: [Config]
        roaConfigs = let

            buildExp :: (Integer -> Val) -> (Context, Integer) -> Exp
            buildExp f (ctx, j) = apply ctx (App (Lit (f j)) (Lit (Var c)))

            exps :: [Exp]
            exps = either
                (map (buildExp Request))
                (map (buildExp Accept ))
                (toEither roa)
            in
            map Exe exps
        in
        chanBuf : rcvQ ++ roaConfigs
    in
    foldr1 Par $ queueConfigs ++ heapConfigs

-- Create a Machine from a Config
fromConfig :: Config -> GVCalc Machine
fromConfig cfg = do
    (_, machine) <- runStateT (fc cfg) emptyMachine
    return machine

    where

    fc :: Config -> StateT Machine GVCalc ()
    fc cfg = case cfg of
        Exe e            -> do
            Queue q <- getQueue
            putQueue (Queue (e:q))
        ChanBuf c d i ms -> do
            Heap h <- getHeap
            lift $ assert (M.notMember c h) "Duplicate channel buffers found"
            putHeapContents c (d, i, lefts ms, Empty)
        Par p q          -> do { fc q ; fc p }
        New c d p        -> do
            c'  <- lift $ freshNameReplacing c
            d'  <- lift $ freshNameReplacing d
            p'  <- lift $ chanSub c' c p
            p'' <- lift $ chanSub d' d p'
            fc p''

run :: Machine -> GVCalc Machine
run m = if runnable m then runStateT step m >>= run . snd else return m

step :: StateT Machine GVCalc ()
step = undefined
