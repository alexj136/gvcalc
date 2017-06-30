{
module Lexer (Token (..), TokenType (..), tkTy, scan) where

import Util

import Control.Monad.Except
import qualified Data.Map as M
}

%wrapper "monadUserState"

$digit  = 0-9
$nozero = 1-9
$lower  = a-z
$upper  = A-Z
$alpha  = [$lower $upper]
$alnum  = [$alpha $digit]

tokens :-
    $white+                ;
    \/\/.*\n               ; -- C-style single line comments
    "fork"              { mkTK TK_Fork   }
    "fix"               { mkTK TK_Fix    }
    "accept"            { mkTK TK_Accept }
    "request"           { mkTK TK_Request}
    "send"              { mkTK TK_Send   }
    "receive"           { mkTK TK_Receive}
    "select"            { mkTK TK_Select }
    "case"              { mkTK TK_Case   }
    "lam"               { mkTK TK_Lam    }
    "let"               { mkTK TK_Let    }
    "in"                { mkTK TK_In     }
    "new"               { mkTK TK_New    }
    "unit"              { mkTK TK_Unit   }
    "-"                 { mkTK TK_Minus  }
    "+"                 { mkTK TK_Plus   }
    ","                 { mkTK TK_Comma  }
    ":"                 { mkTK TK_Colon  }
    "."                 { mkTK TK_Dot    }
    "="                 { mkTK TK_Equal  }
    "("                 { mkTK TK_LParen }
    ")"                 { mkTK TK_RParen }
    "{"                 { mkTK TK_LCurl  }
    "}"                 { mkTK TK_RCurl  }
    "<"                 { mkTK TK_LSqu   }
    ">"                 { mkTK TK_RSqu   }
    "||"                { mkTK TK_Bars   }
    $alpha [$alnum \_]* { mkName         }
    $nozero ($digit*)   { mkNum          }
    .                   { mkTK TK_Error  }

{
newtype Token = Token (Maybe Pos, TokenType) deriving (Show, Eq)

tkTy :: Token -> TokenType
tkTy (Token (_, ty)) = ty

data TokenType
    = TK_Fork
    | TK_Fix
    | TK_Accept
    | TK_Request
    | TK_Send
    | TK_Receive
    | TK_Select
    | TK_Case
    | TK_Lam
    | TK_Let
    | TK_In
    | TK_New
    | TK_Unit
    | TK_Minus
    | TK_Plus
    | TK_Comma
    | TK_Colon
    | TK_Dot
    | TK_Equal
    | TK_LParen
    | TK_RParen
    | TK_LCurl
    | TK_RCurl
    | TK_LSqu
    | TK_RSqu
    | TK_Bars
    | TK_Name Name
    | TK_Num Integer
    | TK_Error
    | TK_EOF
    deriving (Show, Eq)

scan :: String -> Result (M.Map String Name, Name, [Token])
scan = (either throwError return) . lexer

lexer :: String -> Either String (M.Map String Name, Name, [Token])
lexer s = runAlex s loop where
    loop = do
        tk <- alexMonadScan
        if tkTy tk == TK_EOF then do
            AlexUserState map nn <- getUserState
            return (map, nn, [])
        else do
            (map, nn, tks) <- loop
            AlexUserState map nn <- getUserState
            return (map, nn, (tk:tks))

mkTK :: TokenType -> AlexInput -> Int -> Alex Token
mkTK tk (AlexPn _ r c, _, _, _) l =
    return $ Token (Just (Pos r c r (c + l)), tk)

mkNum :: AlexInput -> Int -> Alex Token
mkNum (AlexPn _ r c, _, _, s) l =
    return $ Token (Just (Pos r c r (c + l)),
        TK_Num ((read (take l s)) :: Integer))

mkName :: AlexInput -> Int -> Alex Token
mkName (AlexPn _ r c, _, _, s) l = do
    let text = take l s
    map <- getNameMap
    nn  <- getNextName
    if text `M.member` map then
        return $ Token (Just (Pos r c r (c + l)), TK_Name (map M.! text))
    else do
        setNextName $ after nn
        setNameMap $ M.insert text nn map
        return $ Token (Just (Pos r c r (c + l)), TK_Name nn)

getNameMap :: Alex (M.Map String Name)
getNameMap = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, nameMap ust)

setNameMap :: (M.Map String Name) -> Alex ()
setNameMap map = Alex $ \s -> Right (s{alex_ust=(alex_ust s){nameMap=map}}, ())

getNextName :: Alex Name
getNextName = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, nextName ust)

setNextName :: Name -> Alex ()
setNextName nn = Alex $ \s -> Right (s{alex_ust=(alex_ust s){nextName=nn}}, ())

getUserState ::  Alex AlexUserState
getUserState = Alex $ \s -> Right (s, alex_ust s)

data AlexUserState = AlexUserState
    { nameMap  :: M.Map String Name
    , nextName :: Name
    } deriving Show

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState M.empty (Name 0)

alexEOF :: Alex Token
alexEOF = return $ Token (Nothing, TK_EOF)
}
