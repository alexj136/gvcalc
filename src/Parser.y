{
module Parser where

import Util
import Lexer
import Syntax
import Types

import Prelude hiding (span)
import qualified Data.Map as M
import Control.Monad (liftM, ap)
import Control.Monad.Except (throwError)
}

%monad { Result }
%name parseExps EXPS
%name parseConfig CONFIG

%left Name
%right Plus Minus
%nonassoc Unit

%tokentype { Token      }
%error     { parseError }

%token
    Fork    { Token ( _ , TK_Fork    ) }
    Fix     { Token ( _ , TK_Fix     ) }
    Accept  { Token ( _ , TK_Accept  ) }
    Request { Token ( _ , TK_Request ) }
    Send    { Token ( _ , TK_Send    ) }
    Receive { Token ( _ , TK_Receive ) }
    Select  { Token ( _ , TK_Select  ) }
    Case    { Token ( _ , TK_Case    ) }
    Lam     { Token ( _ , TK_Lam     ) }
    Let     { Token ( _ , TK_Let     ) }
    In      { Token ( _ , TK_In      ) }
    New     { Token ( _ , TK_New     ) }
    Unit    { Token ( _ , TK_Unit    ) }
    Minus   { Token ( _ , TK_Minus   ) }
    Plus    { Token ( _ , TK_Plus    ) }
    Comma   { Token ( _ , TK_Comma   ) }
    Colon   { Token ( _ , TK_Colon   ) }
    Dot     { Token ( _ , TK_Dot     ) }
    Equal   { Token ( _ , TK_Equal   ) }
    LParen  { Token ( _ , TK_LParen  ) }
    RParen  { Token ( _ , TK_RParen  ) }
    LCurl   { Token ( _ , TK_LCurl   ) }
    RCurl   { Token ( _ , TK_RCurl   ) }
    LSqu    { Token ( _ , TK_LSqu    ) }
    RSqu    { Token ( _ , TK_RSqu    ) }
    Bars    { Token ( _ , TK_Bars    ) }
    Name    { Token ( _ , TK_Name _  ) }
    Num     { Token ( _ , TK_Num  _  ) }
    Error   { Token ( _ , TK_Error   ) }
%%

EXP :: { Exp }
EXP
    : Fix  { Lit Fix  }
    | Send { Lit Send }

EXPS :: { [Exp] }
EXPS : EXP EXPS { $1 : $2 } | EXP { [$1] }

CONFIG :: { Config }
CONFIG
    : { undefined }
    | { undefined }

{
parse :: [Token] -> Result Exp
parse = (fmap many) . parseExps

parseError :: [Token] -> Result a
parseError tokens = throwError $ case tokens of
    []                     -> "Reached end of file while parsing"
    (Token (pos, tk):rest) -> "Parse error: " ++ showMPos pos

many :: [Exp] -> Exp
many [ ] = error "many of []"
many [e] = e
many es  = App (many (init es)) (last es)
}
