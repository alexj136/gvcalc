module Types where

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.State

import Syntax

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
        TLinearPair t1 t2     -> S.union (free t1) (free t2)
        TFunction t1 t2       -> S.union (free t1) (free t2)
        TLinearFunction t1 t2 -> S.union (free t1) (free t2)
        TRequest s            -> free s
        TAccept s             -> free s
        TAccessPoint s1 s2    -> S.union (free s1) (free s2)

instance Free Session where
    free st = case st of
        SEnd        -> S.empty
        SIn  t s    -> free t `S.union` free s
        SOut t s    -> free t `S.union` free s
        SCase   t f -> free t `S.union` free f
        SSelect t f -> free t `S.union` free f
        SVar n      -> S.singleton n
        SRec n s    -> S.delete n (free s)

class Alpha t where
    alpha :: t -> t -> Maybe (M.Map Name Name)

instance Alpha Type where
    alpha ty1 ty2 = case (ty1, ty2) of
        (TSession s          , TSession s'         ) ->
            alpha s s'
        (TUnit               , TUnit               ) ->
            Just M.empty
        (TLinearPair t t'    , TLinearPair u u'    ) ->
            alphaAgree (alpha t u) (alpha t' u')
        (TFunction t t'      , TFunction u u'      ) ->
            alphaAgree (alpha t u) (alpha t' u')
        (TLinearFunction t t', TLinearFunction u u') ->
            alphaAgree (alpha t u) (alpha t' u')
        (TRequest s          , TRequest s'         ) ->
            alpha s s'
        (TAccept s           , TAccept s'          ) ->
            alpha s s'
        (TAccessPoint s1 s1' , TAccessPoint s2 s2' ) ->
            alphaAgree (alpha s1 s2) (alpha s1' s2')
        (_                   , _                   ) ->
            Nothing

instance Alpha Session where
    alpha st1 st2 = case (st1, st2) of
        (SEnd       , SEnd         ) -> Just M.empty
        (SIn  t s   , SIn  t' s'   ) -> alphaAgree (alpha t t') (alpha s s')
        (SOut t s   , SOut t' s'   ) -> alphaAgree (alpha t t') (alpha s s')
        (SCase   t f, SCase   t' f') -> undefined
        (SSelect t f, SSelect t' f') -> undefined
        (SVar n     , SVar n'      ) -> Just $ M.singleton n n'
        (SRec n s   , SRec n' s'   ) -> undefined
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

class Subtype t where
    (<:) :: t -> t -> Bool

instance Subtype Type where
    (TSession s          ) <: (TSession s'         ) = s <: s'
    (TUnit               ) <: (TUnit               ) = True
    (TLinearPair t t'    ) <: (TLinearPair u u'    ) = undefined
    (TFunction t t'      ) <: (TFunction u u'      ) = u <: t && t' <: u'
    (TLinearFunction t t') <: (TLinearFunction u u') = u <: t && t' <: u'
    (TRequest s          ) <: (TRequest s'         ) = undefined
    (TAccept s           ) <: (TAccept s'          ) = undefined
    (TAccessPoint s1 s1' ) <: (TAccessPoint s2 s2' ) = undefined
    _ <: _ = undefined

instance Subtype Session where
    SEnd          <: SEnd            = True
    (SIn t s    ) <: (SIn t' s'    ) = t <: t' && s <: s'
    (SOut t s   ) <: (SOut t' s'   ) = t' <: t && s <: s'
    (SCase t f  ) <: (SCase t' f'  ) = undefined
    (SSelect t f) <: (SSelect t' f') = undefined
    (SVar n     ) <: (SVar n'      ) = undefined
    (SRec n s   ) <: (SRec n' s'   ) = undefined
    _             <: _               = undefined
