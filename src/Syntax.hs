module Syntax where

import Util 

import Prelude hiding ((/))
import Data.List (intersperse)
import qualified Data.Set as S
import Control.Monad.State

data Val
    = Var Name
    | Number Integer
    | Boolean Bool
    | Fix
    | Fork
    | Request Integer
    | Accept Integer
    | Send
    | Receive
    | Plus
    | Minus
    | Lam Name Exp
    | ValPair Val Val
    | Unit
    deriving (Show, Eq, Ord)

data Exp
    = Lit Val
    | App Exp Exp
    | Pair Exp Exp
    | Let Name Name Exp Exp
    | Select Exp Exp
    | Case Exp Exp Exp
    deriving (Show, Eq, Ord)

data Config
    = Exe Exp
    | ChanBuf Name Name Integer [Val]
    | Par Config Config
    | New Name Name Config
    deriving (Show, Eq, Ord)

instance PrettyPrint Val where
    pp val = case val of
        Var n         -> pp n
        Number i      -> return $ show i
        Boolean b     -> return $ if b then "true" else "false"
        Fix           -> return "fix"
        Fork          -> return "fork"
        Request i     -> return $ "request " ++ show i
        Accept i      -> return $ "accept " ++ show i
        Send          -> return "send"
        Receive       -> return "receive"
        Plus          -> return "(+)"
        Minus         -> return "(-)"
        Lam n e       -> do
            nstr <- pp n
            estr <- pp e
            return $ "lam " ++ nstr ++ ". " ++ estr
        ValPair v1 v2 -> do
            v1str <- pp v1
            v2str <- pp v2
            return $ "(" ++ v1str ++ ", " ++ v2str ++ ")"
        Unit          -> return "unit"

instance PrettyPrint Exp where
    pp exp = case exp of
        Lit v           -> pp v
        App e1 e2       -> do
            e1str <- pp e1
            e2str <- pp e2
            return $ "(" ++ e1str ++ " " ++ e2str ++ ")"
        Pair e1 e2      -> do
            e1str <- pp e1
            e2str <- pp e2
            return $ "(" ++ e1str ++ ", " ++ e2str ++ ")"
        Let n1 n2 e1 e2 -> do
            n1str <- pp n1
            n2str <- pp n2
            e1str <- pp e1
            e2str <- pp e2
            return $ "let " ++ n1str ++ ", " ++ n2str ++ " = " ++ e1str
                ++ " in " ++ e2str
        Select e1 e2    -> do
            e1str <- pp e1
            e2str <- pp e2
            return $ "select " ++ e1str ++ " " ++ e2str
        Case e1 e2 e3   -> do
            e1str <- pp e1
            e2str <- pp e2
            e3str <- pp e3
            return $ "case " ++ e1str ++ " " ++ e2str ++ " " ++ e3str

instance PrettyPrint Config where
    pp cfg = case cfg of
        Exe e              -> do
            estr <- pp e
            return $ "<" ++ estr ++ ">"
        ChanBuf n1 n2 i vs -> do
            n1str <- pp n1
            n2str <- pp n2
            vstrs <- mapM pp vs
            return $ n1str ++ " -> (" ++ n2str ++ ", " ++ show i ++ ", ["
                ++ (concat (intersperse " " vstrs)) ++ "])"
        Par c1 c2          -> do
            c1str <- pp c1
            c2str <- pp c2
            return $ c1str ++ " || " ++ c2str
        New n1 n2 c        -> do
            n1str <- pp n1
            n2str <- pp n2
            cstr  <- pp c
            return $ "(new " ++ n1str ++ " " ++ n2str ++ " in " ++ cstr ++ ")"

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
        Select e1 e2    -> free e1 `S.union` free e2
        Case e t f      -> free e `S.union` free t `S.union` free f

class Subst t where
    (/) :: Val -> Name -> t -> GVCalc t

instance Subst Val where
    (/) val name bodyVal = case bodyVal of
        Var n   | n == name -> return val
        Lam n e | n /= name -> do
            n'  <- freshName
            e'  <- (((Var n') / n) >=> (val / name)) e
            return $ Lam n' e'
        ValPair v1 v2       ->
            liftM2 ValPair ((val / name) v1) ((val / name) v2)
        _                   -> return bodyVal

instance Subst Exp where
    (/) val name bodyExp = case bodyExp of
        Lit v           -> liftM Lit ((val / name) v)
        App e1 e2       ->
            liftM2 App ((val / name) e1) ((val / name) e2)
        Pair e1 e2      ->
            liftM2 Pair ((val / name) e1) ((val / name) e2)
        Let n1 n2 e1 e2 -> do
            n1' <- if name == n1 then return name else freshName
            n2' <- if name == n2 then return name else freshName
            e1' <- (val / name) e1
            e2' <- (((Var n2') / n2) >=> ((Var n1') / n1) >=> (val / name)) e2
            return $ Let n1' n2' e1' e2'
        Select e1 e2    -> liftM2 Select ((val / name) e1) ((val / name) e2)
        Case e t f      ->
            liftM3 Case ((val / name) e) ((val / name) t) ((val / name) f)

chanSub :: Name -> Name -> Config -> GVCalc Config
chanSub to from bodyCfg = case bodyCfg of
        Exe e            -> liftM Exe ((Var to / from) e)
        ChanBuf c d i ms -> do
            let c' = if from == c then to else c
            let d' = if from == d then to else d
            ms' <- mapM (Var to / from) ms
            return $ ChanBuf c' d' i ms'
        Par p q          -> liftM2 Par (chanSub to from p) (chanSub to from q)
        New c d p        -> if from `elem` [c, d] then return bodyCfg else
            liftM (New c d) (chanSub to from p)
