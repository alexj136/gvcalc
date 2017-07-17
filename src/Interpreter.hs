module Interpreter (fromConfig, toConfig, run) where

import Util
import Syntax

import Prelude hiding ((/))
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

newtype Machine = Machine (Queue, Heap, Garbage)

newtype Queue = Queue [Exp]

newtype Heap = Heap (M.Map Name HeapContents)

newtype Garbage = Garbage [Exp]

type Context = Exp -> Exp

type HeapContents =
    ( Name                          -- The dual channel
    , Integer                       -- Msg list maximum size
    , EitherList Val     Context    -- Msg list or waiting receivers
    , EitherList (Context, Integer) -- Waiting requesters or accepters
        (Context, Integer)
    )

emptyMachine :: Machine
emptyMachine = Machine (Queue [], Heap M.empty, Garbage [])

runnable :: Machine -> Bool
runnable (Machine (Queue q, _, _)) = not $ null q

getQueue :: Monad m => StateT Machine m Queue
getQueue = do { Machine (q, _, _) <- get ; return q }

putQueue :: Monad m => Queue -> StateT Machine m ()
putQueue q = modify $ \(Machine (_, h, g)) -> Machine (q, h, g)

enqueueBack :: Monad m => Exp -> StateT Machine m ()
enqueueBack e = modify $ \(Machine (Queue q, h, g)) ->
    Machine (Queue (q ++ [e]), h, g)

enqueueFront :: Monad m => Exp -> StateT Machine m ()
enqueueFront e = modify $ \(Machine (Queue q, h, g)) ->
    Machine (Queue (e:q), h, g)

dequeue :: StateT Machine GVCalc Exp
dequeue = do
    Machine (Queue q, h, g) <- get
    lift $ assert (not (null q)) "Dequeue of empty queue"
    put $ Machine (Queue (tail q), h, g)
    return (head q)

getHeap :: Monad m => StateT Machine m Heap
getHeap = do { Machine (_, h, _) <- get ; return h }

putHeap :: Monad m => Heap -> StateT Machine m ()
putHeap h = modify $ \(Machine (q, _, g)) -> Machine (q, h, g)

getHeapContents :: Name -> StateT Machine GVCalc HeapContents
getHeapContents n = do { Heap h <- getHeap ; lift $ mlookup h n }

putHeapContents :: Monad m => Name -> HeapContents -> StateT Machine m ()
putHeapContents n hc = do
    Heap h <- getHeap
    putHeap $ Heap $ M.insert n hc h

getMessage :: Name -> StateT Machine GVCalc (Maybe Val)
getMessage c = do
    (d, i, mor, roa) <- getHeapContents c
    let (maybeVal, mor') = elHeadLeft mor
    putHeapContents c (d, i, mor', roa)
    return maybeVal

getWaitingReceive :: Name -> StateT Machine GVCalc (Maybe Context)
getWaitingReceive c = do
    (d, i, mor, roa) <- getHeapContents c
    let (maybeCtx, mor') = elHeadRight mor
    putHeapContents c (d, i, mor', roa)
    return maybeCtx

getWaitingRequest :: Name -> StateT Machine GVCalc (Maybe (Context, Integer))
getWaitingRequest c = do
    (d, i, mor, roa) <- getHeapContents c
    let (maybeCI, roa') = elHeadLeft roa
    putHeapContents c (d, i, mor, roa')
    return maybeCI

getWaitingAccept :: Name -> StateT Machine GVCalc (Maybe (Context, Integer))
getWaitingAccept c = do
    (d, i, mor, roa) <- getHeapContents c
    let (maybeCI, roa') = elHeadRight roa
    putHeapContents c (d, i, mor, roa')
    return maybeCI

putMessage :: Name -> Val -> StateT Machine GVCalc ()
putMessage c v = do
    (d, i, mor, roa) <- getHeapContents c
    mor' <- lift $ elAppend (Left v) mor
    lift $ assert (elLength mor' <= i) "channel buffer length exceeded"
    putHeapContents c (d, i, mor', roa)

putWaitingReceive :: Name -> Context -> StateT Machine GVCalc ()
putWaitingReceive c ctx = do
    (d, i, mor, roa) <- getHeapContents c
    mor' <- lift $ elAppend (Right ctx) mor
    putHeapContents c (d, i, mor', roa)

putWaitingRequest :: Name -> (Context, Integer) -> StateT Machine GVCalc ()
putWaitingRequest c r = do
    (d, i, mor, roa) <- getHeapContents c
    roa' <- lift $ elAppend (Left r) roa
    putHeapContents c (d, i, mor, roa')

putWaitingAccept :: Name -> (Context, Integer) -> StateT Machine GVCalc ()
putWaitingAccept c a = do
    (d, i, mor, roa) <- getHeapContents c
    roa' <- lift $ elAppend (Right a) roa
    putHeapContents c (d, i, mor, roa')

twinChannel :: Name -> StateT Machine GVCalc Name
twinChannel c = do { (d, _, _, _) <- getHeapContents c ; return d }

putGarbage :: Monad m => Exp -> StateT Machine m ()
putGarbage e = modify $ \(Machine (h, q, Garbage g)) ->
    Machine (h, q, Garbage (e:g))

toConfig :: Machine -> Config
toConfig (Machine (Queue queue, Heap heap, Garbage garbage)) = let

    queueConfigs :: [Config]
    queueConfigs = map Exe queue

    heapConfigs :: [Config]
    heapConfigs = concat $ map heapContentsToConfigs (M.toList heap)

    garbageConfigs :: [Config]
    garbageConfigs = map Exe garbage

    heapContentsToConfigs :: (Name, HeapContents) -> [Config]
    heapContentsToConfigs (c, (d, i, mor, roa)) = let

        chanBuf :: Config
        chanBuf = ChanBuf c d i $ either id (\_ -> []) (toEither mor)

        rcvQ :: [Config]
        rcvQ = case toEither mor of
            Left  _    -> []
            Right ctxs ->
                map (\ctx -> Exe (ctx (App (Lit Receive) (Lit (Var c))))) ctxs

        roaConfigs :: [Config]
        roaConfigs = let

            buildExp :: (Integer -> Val) -> (Context, Integer) -> Exp
            buildExp f (ctx, j) = ctx (App (Lit (f j)) (Lit (Var c)))

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
    foldr1 Par $ queueConfigs ++ heapConfigs ++ garbageConfigs

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
step = dequeue >>= handleExp id

handleExp :: Context -> Exp -> StateT Machine GVCalc ()
handleExp ctx exp = case exp of

    App (App (Lit Send) (Lit v)) (Lit (Var c)) -> do
        d <- twinChannel c
        maybeRcv <- getWaitingReceive d
        case maybeRcv of
            Nothing  -> putMessage d v
            Just rcv -> enqueueBack $ rcv $ Lit v
        enqueueFront $ ctx $ Lit $ Var c

    App (Lit Receive) (Lit (Var c)) -> do
        maybeMsg <- getMessage c
        case maybeMsg of
            Just msg -> enqueueFront $ ctx $ Pair (Lit msg) (Lit (Var c))
            Nothing  -> putWaitingReceive c $ ctx . (\k -> Pair k (Lit (Var c)))

    App (Lit (Accept n)) (Lit (Var c)) -> do
        maybeReq <- getWaitingRequest c
        case maybeReq of
            Just (reqCtx, reqN) -> dispatchAccReq True ctx n reqCtx reqN
            Nothing             -> putWaitingAccept c (ctx, n)

    App (Lit (Request n)) (Lit (Var c)) -> do
        maybeAcc <- getWaitingAccept c
        case maybeAcc of
            Just (accCtx, accN) -> dispatchAccReq False accCtx accN ctx n
            Nothing             -> putWaitingRequest c (ctx, n)

    App (Lit (Lam x e)) (Lit v) -> lift ((v/x) e) >>= handleExp ctx

    App (Lit Fix) (Lit (Lam x e)) ->
        lift (expSubExp (App (Lit Fix) (Lit (Lam x e))) x e) >>= handleExp ctx

    App (Lit Fork) e -> do
        enqueueFront $ ctx $ Lit $ Unit
        enqueueBack e

    App (Lit v) e  -> handleExp (ctx . (\k -> App (Lit v) k )) e
    App e1      e2 -> handleExp (ctx . (\k -> App k       e2)) e1

    Pair (Lit v1) (Lit v2) -> enqueueFront $ ctx $ Lit $ ValPair v1 v2
    Pair (Lit v)  e        -> handleExp (ctx . (\k -> Pair (Lit v) k )) e
    Pair e1       e2       -> handleExp (ctx . (\k -> Pair k       e2)) e1

    Let n1 n2 (Lit (ValPair v1 v2)) e -> do
        e' <- lift $ ((v1/n1) >=> (v2/n2)) e
        enqueueFront $ ctx e'
    Let n1 n2 e1 e2 -> handleExp (ctx . (\k -> Let n1 n2 k e2)) e1

    Select (Lit v) (Lit (Var c)) -> do
        d <- twinChannel c
        maybeCase <- getWaitingReceive d
        case maybeCase of
            Nothing  -> putMessage d v
            Just rcv -> enqueueBack $ rcv $ Lit v
        enqueueFront $ ctx $ Lit $ Var c
    Select (Lit v) e  -> handleExp (ctx . (\k -> Select (Lit v) k )) e
    Select e1      e2 -> handleExp (ctx . (\k -> Pair   k       e2)) e1

    Case (Lit (Var c)) e1 e2 -> do
        maybeMsg <- getMessage c
        case maybeMsg of
            Nothing -> putWaitingReceive c $ (ctx . (\k -> case k of
                Lit (Boolean True ) -> e1
                Lit (Boolean False) -> e2
                _ -> error "Type error - case got non-bool."))
            Just (Boolean True ) -> enqueueFront $ ctx e1
            Just (Boolean False) -> enqueueFront $ ctx e2
            Just _ -> lift $ throwError "Type error - case got non-bool."
    Case e1 e2 e3 -> handleExp (ctx . (\k -> Case k e2 e3)) e1

    _ -> putGarbage $ ctx exp

    where

    dispatchAccReq :: Bool -> Context -> Integer -> Context -> Integer ->
        StateT Machine GVCalc ()
    dispatchAccReq accInitiates accCtx accI reqCtx reqI = do
        c <- lift freshName
        d <- lift freshName
        undefined
