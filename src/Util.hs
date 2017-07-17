module Util where

import Data.List (nub)
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

--------------------------------------------------------------------------------
-- Class for pretty-printing
--------------------------------------------------------------------------------

class PrettyPrint t where
    pp :: t -> GVCalc String

--------------------------------------------------------------------------------
-- Names
--------------------------------------------------------------------------------

newtype Name = Name Integer deriving (Eq, Ord)

instance Show Name where
    show (Name s) = "n" ++ show s

after :: Name -> Name
after (Name n) = Name (n + 1)

instance PrettyPrint Name where
    pp n = do
        (_, nmap) <- get
        return $ M.findWithDefault (show n) n nmap

--------------------------------------------------------------------------------
-- Source code positions
--------------------------------------------------------------------------------

data Pos = Pos Int Int Int Int deriving (Eq, Ord)

leftLine, leftCol, rightLine, rightCol :: Pos -> Int
leftLine  (Pos i _ _ _) = i
leftCol   (Pos _ i _ _) = i
rightLine (Pos _ _ i _) = i
rightCol  (Pos _ _ _ i) = i

instance Show Pos where
    show (Pos lineL colL lineR colR) =
        "Line " ++ show lineL ++ " - " ++ show lineR
        ++ ", Column " ++ show colL ++ " - " ++ show colR

--------------------------------------------------------------------------------
-- Application monad - a StateT with the fresh name and a map for printing
-- names, and an Except String for error messages
--------------------------------------------------------------------------------

type GVCalc = StateT (Name, M.Map Name String) (Except String)

runGVCalc :: GVCalc a -> (Name, M.Map Name String) ->
    Either String (a, (Name, M.Map Name String))
runGVCalc a = runExcept . runStateT a

-- Query an M.Map in the GVCalc monad
mlookup :: Ord k => M.Map k v -> k -> GVCalc v
mlookup map k = case M.lookup k map of
    Nothing -> throwError "key not found"
    Just v  -> return v

-- Generate a fresh name with default printing behaviour
freshName :: GVCalc Name
freshName = do
    (fn, nmap) <- get
    put (after fn, nmap)
    return fn

-- Generate a fresh name that prints like a given name
freshNameReplacing :: Name -> GVCalc Name
freshNameReplacing rn = do
    (fn, nmap) <- get
    rnString   <- mlookup nmap rn
    put (after fn, M.insert fn (rnString ++ "'") nmap)
    return fn

-- Perform a consistency check, and error out with the given message if it fails
assert :: Bool -> String -> GVCalc ()
assert b s = if b then return () else throwError s

--------------------------------------------------------------------------------
-- Misc convenience datatypes and functions
--------------------------------------------------------------------------------

-- An EitherList a b is a bit like an Either [a] [b], except that we want to
-- be able to treat the Left [] and Right [] cases as equal.

data EitherList a b
    = Empty
    | Lefts  (a, [a])
    | Rights (b, [b])
    deriving (Show, Eq, Ord)

elLength :: EitherList a b -> Integer
elLength = toInteger . (either length length) . toEither

lefts :: [a] -> EitherList a b
lefts []     = Empty
lefts (a:as) = Lefts (a, as)

rights :: [b] -> EitherList a b
rights []     = Empty
rights (b:bs) = Rights (b, bs)

toEither :: EitherList a b -> Either [a] [b]
toEither Empty            = Left  []
toEither (Lefts  (a, as)) = Left  (a:as)
toEither (Rights (b, bs)) = Right (b:bs)

elAppend :: Either a b -> EitherList a b -> GVCalc (EitherList a b)
elAppend eab bdlab = case (eab, bdlab) of
    (Left  a, Empty          ) -> return $ Lefts  (a, []        )
    (Right b, Empty          ) -> return $ Rights (b, []        )
    (Left  a, Lefts  (a1, as)) -> return $ Lefts  (a1, as ++ [a])
    (Right b, Rights (b1, bs)) -> return $ Rights (b1, bs ++ [b])
    (_      , _              ) -> throwError "EitherList direction mismatch"

elHeadLeft :: EitherList a b -> (Maybe a, EitherList a b)
elHeadLeft (Lefts (a, []     )) = (Just a, Empty         )
elHeadLeft (Lefts (a, (a1:as))) = (Just a, Lefts (a1, as))
elHeadLeft el = (Nothing, el)

elHeadRight :: EitherList a b -> (Maybe b, EitherList a b)
elHeadRight (Rights (b, []     )) = (Just b, Empty          )
elHeadRight (Rights (b, (b1:bs))) = (Just b, Rights (b1, bs))
elHeadRight el = (Nothing, el)

-- Flip a map. Both the keys and values must be in Ord, since the output will
-- use the values of the input as keys. Since we cannot have duplicate keys, if
-- the input contains duplicates in the values, there will be a conflict as to
-- which corresponding key becomes the mapped value in the output. We do not
-- resolve such conflicts, but check that they do not occur, erroring out when
-- they do.
rev :: Ord k => Ord v => M.Map k v -> M.Map v k
rev m = let elems = M.elems m in
    if length (nub elems) == length elems then
        M.fromList $ map (\(k, v) -> (v, k)) $ M.toList m
    else
        error "rev: duplicate values"
