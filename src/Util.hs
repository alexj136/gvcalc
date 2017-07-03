module Util where

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

--------------------------------------------------------------------------------
-- Names
--------------------------------------------------------------------------------

newtype Name = Name Integer deriving (Eq, Ord)

instance Show Name where
    show (Name s) = "n" ++ show s

after :: Name -> Name
after (Name n) = Name (n + 1)

--------------------------------------------------------------------------------
-- Source code positions
--------------------------------------------------------------------------------

data Pos = Pos Int Int Int Int deriving (Eq, Ord)

instance Show Pos where
    show (Pos lineL colL lineR colR) =
        "Line " ++ show lineL ++ " - " ++ show lineR
        ++ ", Column " ++ show colL ++ " - " ++ show colR

{-------------------------------------------------------------------------------
Application monad - a StateT with the fresh name and a map for printing names,
and an Except String for error messages.
-------------------------------------------------------------------------------}

type GVCalc = StateT (Name, M.Map Name String) (Except String)

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

-- Generate a fresh name that should print as the given string
freshNamePrintingAs :: String -> GVCalc Name
freshNamePrintingAs fnString = do
    (fn, nmap) <- get
    put (after fn, M.insert fn fnString nmap)
    return fn
