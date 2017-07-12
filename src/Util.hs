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

elPrepend :: Either a b -> EitherList a b -> GVCalc (EitherList a b)
elPrepend eab bdlab = case (eab, bdlab) of
    (Left  a, Empty          ) -> return $ Lefts  (a, []   )
    (Right b, Empty          ) -> return $ Rights (b, []   )
    (Left  a, Lefts  (a1, as)) -> return $ Lefts  (a, a1:as)
    (Right b, Rights (b1, bs)) -> return $ Rights (b, b1:bs)
    (_      , _              ) -> throwError "EitherList direction mismatch"

elAppend :: Either a b -> EitherList a b -> GVCalc (EitherList a b)
elAppend eab bdlab = case (eab, bdlab) of
    (Left  a, Empty          ) -> return $ Lefts  (a, []        )
    (Right b, Empty          ) -> return $ Rights (b, []        )
    (Left  a, Lefts  (a1, as)) -> return $ Lefts  (a1, as ++ [a])
    (Right b, Rights (b1, bs)) -> return $ Rights (b1, bs ++ [b])
    (_      , _              ) -> throwError "EitherList direction mismatch"

elHeadSplitLeft :: EitherList a b -> GVCalc (a, EitherList a b)
elHeadSplitLeft (Lefts (a, []     )) = return (a, Empty         )
elHeadSplitLeft (Lefts (a, (a1:as))) = return (a, Lefts (a1, as))
elHeadSplitLeft _ = throwError "elHeadSplitLeft of Rights or Empty EitherList"

elLastSplitLeft :: EitherList a b -> GVCalc (a, EitherList a b)
elLastSplitLeft (Lefts (a, []      )) = return (a      , Empty             )
elLastSplitLeft (Lefts (a, as@(_:_))) = return (last as, Lefts (a, init as))
elLastSplitLeft _ = throwError "elLastSplitLeft of Rights or Empty EitherList"

elHeadSplitRight :: EitherList a b -> GVCalc (b, EitherList a b)
elHeadSplitRight (Rights (b, []     )) = return (b, Empty          )
elHeadSplitRight (Rights (b, (b1:bs))) = return (b, Rights (b1, bs))
elHeadSplitRight _ = throwError "elHeadSplitRight of Lefts or Empty EitherList"

elLastSplitRight :: EitherList a b -> GVCalc (b, EitherList a b)
elLastSplitRight (Rights (b, []      )) = return (b      , Empty              )
elLastSplitRight (Rights (b, bs@(_:_))) = return (last bs, Rights (b, init bs))
elLastSplitRight _ = throwError "elLastSplitRight of Lefts or Empty EitherList"

elMap :: (a -> c) -> (b -> d) -> EitherList a b -> EitherList c d
elMap acf bdf el = case el of
    Empty          -> Empty
    Lefts  (a, as) -> Lefts  (acf a, map acf as)
    Rights (b, bs) -> Rights (bdf b, map bdf bs)

eitherList :: (a -> c) -> (b -> c) -> EitherList a b -> [c]
eitherList acf bcf el = case el of
    Empty          -> []
    Lefts  (a, as) -> map acf (a:as)
    Rights (b, bs) -> map bcf (b:bs)

data EitherListDirection = ELEmpty | ELLefts | ELRights deriving (Show, Eq, Ord)

elDir :: EitherList a b -> EitherListDirection
elDir  Empty     = ELEmpty
elDir (Lefts  _) = ELLefts
elDir (Rights _) = ELRights

elIsEmpty, elIsLefts, elIsRights :: EitherList a b -> Bool
elIsEmpty  = (== ELEmpty ) . elDir
elIsLefts  = (== ELLefts ) . elDir
elIsRights = (== ELRights) . elDir

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
