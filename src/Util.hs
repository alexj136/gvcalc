module Util where

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

freshName :: Monad m => StateT Name m Name
freshName = state $ \n -> (n, after n)

--------------------------------------------------------------------------------
-- Source code positions
--------------------------------------------------------------------------------

data Pos = Pos Int Int Int Int deriving (Eq, Ord)

instance Show Pos where
    show (Pos lineL colL lineR colR) =
        "Line " ++ show lineL ++ " - " ++ show lineR
        ++ ", Column " ++ show colL ++ " - " ++ show colR

--------------------------------------------------------------------------------
-- Result monad
--------------------------------------------------------------------------------

type Result = Except String
