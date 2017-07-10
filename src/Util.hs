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
        mlookup nmap n

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

-- Perform a consistency check, and error out with the given string if it fails.
assert :: Bool -> String -> GVCalc ()
assert b s = if b then return () else throwError s

--------------------------------------------------------------------------------
-- Misc convenience functions
--------------------------------------------------------------------------------

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
