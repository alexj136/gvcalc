module Syntax where

import Util 

import qualified Data.Set as S
import Control.Monad.State

type Label = String

data Val
    = Var Name
    | Chan Name
    | Fix
    | Fork
    | Request Integer
    | Accept Integer
    | Send
    | Receive
    | Lam Name Exp
    | ValPair Val Val
    | UnitE
    deriving (Show, Eq, Ord)

data Exp
    = Lit Val
    | App Exp Exp
    | Pair Exp Exp
    | Let Name Name Exp Exp
    | Select Bool Exp
    | Case Exp Exp Exp
    deriving (Show, Eq, Ord)

data Config
    = Exe Exp
    | ChanBuf Name Name Integer [Val]
    | Par Config Config
    | New Name Name Config
    deriving (Show, Eq, Ord)

class Free t where
    free :: t -> S.Set Name

instance Free Val where
    free v = case v of
        (Var n        ) -> S.singleton n
        (Lam n e      ) -> S.delete n $ free e
        (ValPair v1 v2) -> S.union (free v1) (free v2)
        _               -> S.empty

instance Free Exp where
    free e = case e of
        Lit v           -> free v
        App e1 e2       -> free e1 `S.union` free e2
        Pair e1 e2      -> free e1 `S.union` free e2
        Let n1 n2 e1 e2 ->
            (S.delete n1 (S.delete n2 (free e1))) `S.union` free e2
        Select _ e      -> free e
        Case e t f      -> free e `S.union` free t `S.union` free f

class Subst t where
    (//) :: Val -> Name -> t -> GVCalc t

instance Subst Val where
    (//) val name bodyVal = case bodyVal of
        Var n   | n == name -> return val
        Lam n e | n /= name -> do
            n'  <- freshName
            e'  <- (((Var n') // n) >=> (val // name)) e
            return $ Lam n' e'
        ValPair v1 v2       ->
            liftM2 ValPair ((val // name) v1) ((val // name) v2)
        _                   -> return bodyVal

instance Subst Exp where
    (//) val name bodyExp = case bodyExp of
        Lit v           -> liftM Lit ((val // name) v)
        App e1 e2       ->
            liftM2 App ((val // name) e1) ((val // name) e2)
        Pair e1 e2      ->
            liftM2 Pair ((val // name) e1) ((val // name) e2)
        Let n1 n2 e1 e2 -> do
            n1' <- if name == n1 then return name else freshName
            n2' <- if name == n2 then return name else freshName
            e1' <- (val // name) e1
            e2' <- (((Var n2') // n2) >=> ((Var n1') // n1)
                >=> (val // name)) e2
            return $ Let n1' n2' e1' e2'
        Select b e      -> liftM (Select b) ((val // name) e)
        Case e t f      ->
            liftM3 Case ((val // name) e) ((val // name) t) ((val // name) f)
