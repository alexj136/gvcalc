module Types where

import Util
import Syntax

import Prelude hiding ((+))
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Writer

--------------------------------------------------------------------------------
-- Typing types
--------------------------------------------------------------------------------

data Type
    = TSession Session
    | TUnit
    | TBool
    | TInt
    | TLinearPair Type Type
    | TFunction Type Type
    | TLinearFunction Type Type
    | TRequest Session
    | TAccept Session
    | TAccessPoint Session Session
    | TVar Name
    deriving (Show, Eq, Ord)

data Session
    = SEnd
    | SIn Type Session
    | SOut Type Session
    | SCase Session Session
    | SSelect Session Session
    | SVar Name
    | SRec Name Session
    deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Some shorthand for writing down types
--------------------------------------------------------------------------------

class ToSession a where
    toSession :: a -> Session

instance ToSession Session where
    toSession = id

instance ToSession Name where
    toSession = SVar

class ToType a where
    toType :: a -> Type

instance ToType Type where
    toType = id

instance ToType Name where
    toType = TVar

instance ToType Session where
    toType = TSession

(^->), (^-*), (|*|) :: ToType a => ToType b => a -> b -> Type
(^->) t1 t2 = TFunction       (toType t1) (toType t2)
(^-*) t1 t2 = TLinearFunction (toType t1) (toType t2)
(|*|) t1 t2 = TLinearPair     (toType t1) (toType t2)

(^?), (^!) :: ToType a => ToSession b => a -> b -> Session
(^?) t s = SIn  (toType t) (toSession s)
(^!) t s = SOut (toType t) (toSession s)

--------------------------------------------------------------------------------
-- Free variable definitions
--------------------------------------------------------------------------------

instance Free Type where
    free ty = case ty of
        TSession s            -> free s
        TUnit                 -> S.empty
        TBool                 -> S.empty
        TInt                  -> S.empty
        TLinearPair t1 t2     -> free t1 `S.union` free t2
        TFunction t1 t2       -> free t1 `S.union` free t2
        TLinearFunction t1 t2 -> free t1 `S.union` free t2
        TRequest s            -> free s
        TAccept s             -> free s
        TAccessPoint s1 s2    -> free s1 `S.union` free s2
        TVar n                -> S.singleton n

instance Free Session where
    free st = case st of
        SEnd        -> S.empty
        SIn  t s    -> free t `S.union` free s
        SOut t s    -> free t `S.union` free s
        SCase   t f -> free t `S.union` free f
        SSelect t f -> free t `S.union` free f
        SVar n      -> S.singleton n
        SRec n s    -> S.delete n (free s)

α :: Alpha t => t -> t -> Bool
α t t' = case alpha t t' of { Just _ -> True ; _ -> False }

class Alpha t where
    alpha :: t -> t -> Maybe (M.Map Name Name)

instance Alpha Type where
    alpha ty1 ty2 = case (ty1, ty2) of
        (TLinearPair t t'    , TLinearPair u u'    ) ->
            alpha t u `alphaAgree` alpha t' u'
        (TFunction t t'      , TFunction u u'      ) ->
            alpha t u `alphaAgree` alpha t' u'
        (TLinearFunction t t', TLinearFunction u u') ->
            alpha t u `alphaAgree` alpha t' u'
        (TAccessPoint s s'   , TAccessPoint t t'   ) ->
            alpha s t `alphaAgree` alpha s' t'
        (TSession s          , TSession s'         ) -> alpha s s'
        (TRequest s          , TRequest s'         ) -> alpha s s'
        (TAccept s           , TAccept s'          ) -> alpha s s'
        (TUnit               , TUnit               ) -> Just M.empty
        (TBool               , TBool               ) -> Just M.empty
        (TInt                , TInt                ) -> Just M.empty
        (TVar n              , TVar n'             ) -> Just $ M.singleton n n'
        (_                   , _                   ) -> Nothing

instance Alpha Session where
    alpha st1 st2 = case (st1, st2) of
        (SEnd       , SEnd         ) -> Just M.empty
        (SIn  t s   , SIn  t' s'   ) -> alpha t t' `alphaAgree` alpha s s'
        (SOut t s   , SOut t' s'   ) -> alpha t t' `alphaAgree` alpha s s'
        (SCase   t f, SCase   t' f') -> alpha t t' `alphaAgree` alpha f f'
        (SSelect t f, SSelect t' f') -> alpha t t' `alphaAgree` alpha f f'
        (SVar n     , SVar n'      ) -> Just $ M.singleton n n'
        (SRec n s   , SRec n' s'   ) -> alphaBindAgree (alpha s s') n n'
        (_          , _            ) -> Nothing

alphaAgree :: Maybe (M.Map Name Name) -> Maybe (M.Map Name Name) ->
    Maybe (M.Map Name Name)
alphaAgree ma mb = do
    a <- ma
    b <- mb
    if and (M.elems (M.intersectionWith (==) a b)) then
        return $ a `M.union` b
    else
        Nothing

alphaBindAgree :: Maybe (M.Map Name Name) -> Name -> Name ->
    Maybe (M.Map Name Name)
alphaBindAgree maybeA n m = do
    a <- maybeA
    if        n `notElem` (M.keys a) && m `notElem` (M.elems a) then maybeA
    else if   M.lookup n a == Just m                            then maybeA
    else Nothing

class ContractiveWithIllegals t where
    cwi :: [Name] -> t -> Bool

instance ContractiveWithIllegals Type where
    cwi il ty = case ty of
        TSession s            -> cwi il s
        TUnit                 -> True
        TBool                 -> True
        TInt                  -> True
        TLinearPair t1 t2     -> cwi [] t1 && cwi [] t2
        TFunction t1 t2       -> cwi [] t1 && cwi [] t2
        TLinearFunction t1 t2 -> cwi [] t1 && cwi [] t2
        TRequest s            -> cwi [] s
        TAccept s             -> cwi [] s
        TAccessPoint s1 s2    -> cwi [] s1 && cwi [] s2
        TVar n                -> notElem n il

instance ContractiveWithIllegals Session where
    cwi il st = case st of
        SEnd        -> True
        SIn  t s    -> cwi [] t && cwi [] s
        SOut t s    -> cwi [] t && cwi [] s
        SCase   t f -> cwi [] t && cwi [] f
        SSelect t f -> cwi [] t && cwi [] f
        SVar n      -> notElem n il
        SRec n s    -> cwi (n : il) s

-- Contractive: every variable bound with SRec (μ in literature) occurs only
-- under a constructor such as functions or pairs, and never directly under
-- its binding. This property is sometimes also called 'guarded'.
contractive :: ContractiveWithIllegals t => t -> Bool
contractive = cwi []

-- Closed: contains no free variables
closed :: Free t => t -> Bool
closed = S.null . free

dual :: Session -> Session
dual st = case st of
    SEnd        -> SEnd
    SIn  t s    -> SOut t (dual s)
    SOut t s    -> SIn  t (dual s)
    SCase   t f -> SSelect (dual t) (dual f)
    SSelect t f -> SCase   (dual t) (dual f)
    SVar n      -> SVar n
    SRec n s    -> SRec n (dual s)

(<:) :: Subtype t => t -> t -> Bool
(<:) = subtypeWithAssumptions M.empty

class Subtype t where
    subtypeWithAssumptions :: M.Map Name Name -> t -> t -> Bool

instance Subtype Type where
    subtypeWithAssumptions m t u = case (t, u) of
        (TUnit              , TUnit              ) -> True
        (TBool              , TBool              ) -> True
        (TInt               , TInt               ) -> True
        (TSession a         , TSession x         ) -> a <: x
        (TFunction a b      , TFunction x y      ) -> x <: a && b <: y
        (TLinearFunction a b, TLinearFunction x y) -> x <: a && b <: y
        (TRequest a         , TRequest x         ) -> a <: x
        (TAccept a          , TAccept x          ) -> a <: x
        (TAccessPoint a b   , TAccessPoint x y   ) -> a <: x && b <: y
        (TAccessPoint a b   , TAccept x          ) | a `α` x ->
            all contractive [a, b] && all closed [a, b]
        (TAccessPoint a b   , TRequest y         ) | b `α` y ->
            all contractive [a, b] && all closed [a, b]
        (TFunction a b      , TLinearFunction x y) | a `α` x && b `α` y ->
            all contractive [a , b] && all closed [a , b]
        (TVar a             , TVar x             ) -> M.lookup a m == Just x
        (_                  , _                  ) -> False
        where
        (<:) :: Subtype t => t -> t -> Bool
        (<:) = subtypeWithAssumptions m

instance Subtype Session where
    subtypeWithAssumptions m a b = case (a, b) of
        (SEnd       , SEnd       ) -> True
        (SIn a b    , SIn x y    ) -> a <: x && b <: y
        (SOut a b   , SOut x y   ) -> x <: a && b <: y
        (SCase a b  , SCase x y  ) -> x <: a && b <: y
        (SSelect a b, SSelect x y) -> x <: a && b <: y
        (SVar a     , SVar x     ) -> M.lookup a m == Just x
        (SRec a b   , SRec x y   ) ->
            subtypeWithAssumptions (M.insert a x m) b y
        (_          , _          ) -> False
        where
        (<:) :: Subtype t => t -> t -> Bool
        (<:) = subtypeWithAssumptions m

class Multiplicity a where
    linear :: a -> Bool
    unlim :: a -> Bool

instance Multiplicity Type where
    linear ty = case ty of
        TSession a          -> linear a
        TLinearPair _ _     -> True
        TLinearFunction _ _ -> True
        _                   -> False
    unlim ty = case ty of
        TFunction _ _       -> True
        TRequest _          -> True
        TAccept _           -> True
        TAccessPoint _ _    -> True
        _                   -> False

instance Multiplicity Session where
    linear = (/=) SEnd
    unlim  = (==) SEnd

newtype Env = Env { getMap :: M.Map Name Type } deriving (Show, Eq, Ord)

envLookup :: Env -> Name -> WriterT [Constraint] GVCalc Type
envLookup (Env env) = lift . mlookup env

(+) :: Env -> (Name, Type) -> Env
(+) (Env env) (n, t) = Env $ M.insert n t env

instance Multiplicity Env where
    linear = undefined
    unlim  = undefined

data Constraint
    = CEqual Type Type
    | CDual  Session Session
    | CUnlim Type
    | CBound Integer
    | COneOf Constraint Constraint
    deriving (Show, Eq, Ord)

class ConGen t where
    gen :: Env -> t -> WriterT [Constraint] GVCalc Type

instance ConGen Val where
    gen env val = case val of
        Var n       -> envLookup env n
        Number _    -> return TInt
        Boolean _   -> return TBool
        Fix         -> do t <- lift freshName; return $ (t ^-> t) ^-> t
        Fork        -> do t <- lift freshName; return $ t ^-> TUnit
        Request n   -> do
            s <- liftM SVar $ lift freshName
            t <- liftM SVar $ lift freshName
            tell [CDual s t]
            return $ (TRequest s) ^-> t
        Accept n    -> do
            s <- liftM SVar $ lift freshName
            return $ (TRequest s) ^-> s
        Send        -> undefined
        Receive     -> do
            t <- lift freshName
            s <- lift freshName
            return $ (t ^? s) ^-> (t |*| s)
        Plus        -> return $ TInt ^-> (TInt ^-> TInt)
        Minus       -> return $ TInt ^-> (TInt ^-> TInt)
        Lam n e     -> do
            tN <- liftM TVar $ lift freshName
            tE <- gen (env + (n, tN)) e
            return $ tN ^-> tE
        ValPair v w -> do tV <- gen env v; tW <- gen env w; return (tV |*| tW)
        Unit        -> return TUnit

instance ConGen Exp where
    gen env exp = case exp of
        Lit v           -> gen env v
        App e1 e2       -> undefined
        Pair e1 e2      -> undefined
        Let n1 n2 e1 e2 -> undefined
        Select e1 e2    -> undefined
        Case e1 e2 e3   -> undefined
