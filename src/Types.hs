module Types where

import Util
import Syntax

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.State

data Type
    = TSession Session
    | TUnit
    | TLinearPair Type Type
    | TFunction Type Type
    | TLinearFunction Type Type
    | TRequest Session
    | TAccept Session
    | TAccessPoint Session Session
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

instance Free Type where
    free ty = case ty of
        TSession s            -> free s
        TUnit                 -> S.empty
        TLinearPair t1 t2     -> free t1 `S.union` free t2
        TFunction t1 t2       -> free t1 `S.union` free t2
        TLinearFunction t1 t2 -> free t1 `S.union` free t2
        TRequest s            -> free s
        TAccept s             -> free s
        TAccessPoint s1 s2    -> free s1 `S.union` free s2

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
        TLinearPair t1 t2     -> cwi [] t1 && cwi [] t2
        TFunction t1 t2       -> cwi [] t1 && cwi [] t2
        TLinearFunction t1 t2 -> cwi [] t1 && cwi [] t2
        TRequest s            -> cwi [] s
        TAccept s             -> cwi [] s
        TAccessPoint s1 s2    -> cwi [] s1 && cwi [] s2

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
-- its binding.
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

type Env = M.Map Name Type

unlim :: Env -> Bool
unlim = undefined

class Check t where
    (⊢) :: Env -> t -> GVCalc ()

instance Check Exp where
    (⊢) env exp = case exp of
        Lit v           -> undefined
        App e1 e2       -> undefined
        Pair e1 e2      -> undefined
        Let n1 n2 e1 e2 -> undefined
        Select e1 e2    -> undefined
        Case e1 e2 e3   -> undefined
