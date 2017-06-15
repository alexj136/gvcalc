module Interpreter where

import qualified Data.Map as M
import Control.Monad.State

import Syntax

data Context
    = Hole
    | CtxExpApp Context Exp
    | ValCtxApp Val Context
    | CtxExpPair Context Exp
    | ValCtxPair Val Context
    | CtxLet Name Name Context Exp
    | CtxSelect Bool Context
    | CtxCase Context Exp Exp
    deriving (Show, Eq, Ord)

newtype BufContents
    = BufContents (Integer, [Either Val Label], Integer, [Either Val Label])
    deriving (Show, Eq, Ord)

type Buffers = M.Map Name BufContents

hasSpace :: Bool -> Name -> Buffers -> Bool
hasSpace isIn c bufs = case bufs M.! c of
    BufContents (nI, bsI, nO, bsO) -> (if isIn then nI else nO) <
        ((toInteger (length (if isIn then bsI else bsO))) - 1)

doSend :: Name -> Either Val Label -> Buffers -> Buffers
doSend c v bufs = case bufs M.! c of
    BufContents (nI, bsI, nO, bsO) ->
        if nO < (toInteger (length bsO)) - 1 then
            M.insert c (BufContents (nI, bsI, nO, bsO ++ [v])) bufs
        else error "buffer overrun"

step :: (Buffers, [Exp]) -> StateT Name Maybe (Buffers, [Exp])
step (_   , []      ) = lift Nothing
step (bufs, exp:exps) = case exp of
    App (App (Lit (Send   )) (Lit v)) (Lit (Chan c)) | hasSpace False c bufs ->
        case bufs M.! c of
            BufContents (nI, bsI, nO, bsO) ->
                return (doSend c (Left v) bufs, Lit (Chan c) : exps)
    App (App (Lit (Receive)) (Lit v)) (Lit (Chan c)) -> undefined
    Lit v              -> undefined
    App e1 e2          -> undefined
    ExpPair e1 e2      -> undefined
    ExpLet n1 n2 e1 e2 -> undefined
    ExpSelect b e      -> undefined
    ExpCase e t f      -> undefined
