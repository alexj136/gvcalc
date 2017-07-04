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

%monad { GVCalc }
%name parseExps EXPS
%name parseConfig CONFIG

%right Plus Minus Bars
%nonassoc Fork Fix Accept Request Send Receive Select Case Lam Let Int New Unit
    True False Comma Colon Dot Equal LParen RParen LCurl RCurl LSqu RSqu LAngle
    RAngle Name

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
    True    { Token ( _ , TK_True    ) }
    False   { Token ( _ , TK_False   ) }
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
    LAngle  { Token ( _ , TK_LAngle  ) }
    RAngle  { Token ( _ , TK_RAngle  ) }
    Bars    { Token ( _ , TK_Bars    ) }
    Name    { Token ( _ , TK_Name _  ) }
    Num     { Token ( _ , TK_Num  _  ) }
    Error   { Token ( _ , TK_Error   ) }
%%

NAME :: { Name }
NAME : Name { case $1 of Token (_, TK_Name n) -> n }

NUM :: { Integer }
NUM : Num { case $1 of Token (_, TK_Num n) -> n }

BOOL :: { Bool }
BOOL : True { True } | False { False }

VAL :: { Val }
VAL
    : Fix                         { Fix           }
    | Fork                        { Fork          }
    | Unit                        { Unit          }
    | Send                        { Send          }
    | Receive                     { Receive       }
    | Request NUM                 { Request $2    }
    | Accept  NUM                 { Accept  $2    }
    | LParen Plus RParen          { Plus          }
    | LParen Minus RParen         { Minus         }
    | Lam NAME Dot EXP            { Lam $2 $4     }
    | LParen VAL Comma VAL RParen { ValPair $2 $4 }
    | NAME                        { Var $1        }
    | NUM                         { Number $1     }
    | LParen VAL RParen           { $2            }

EXP :: { Exp }
EXP
    : VAL                                  { Lit $1                      }
    | LParen EXP Comma EXP RParen          { Pair $2 $4                  }
    | Let NAME Comma NAME Equal EXP In EXP { Let $2 $4 $6 $8             }
    | Select BOOL EXP                      { Select $2 $3                }
    | Case EXP EXP EXP                     { Case $2 $3 $4               }
    | EXPS                                 { many $1                     }
    | EXP Plus EXP                         { App (App (Lit Plus) $1) $3  }
    | EXP Minus EXP                        { App (App (Lit Minus) $1) $3 }
    | LParen EXP RParen                    { $2                          }

EXPS :: { [Exp] }
EXPS : EXP EXPS { $1 : $2 } | EXP { [$1] }

CONFIG :: { Config }
CONFIG
    : LAngle EXP RAngle       { Exe $2       }
    | CONFIG Bars CONFIG      { $1 `Par` $3  }
    | New NAME NAME In CONFIG { New $2 $3 $5 }
    | LParen CONFIG RParen    { $2           }

{
parse :: [Token] -> GVCalc Config
parse = parseConfig

parseError :: [Token] -> GVCalc a
parseError tokens = throwError $ case tokens of
    []                     -> "Reached end of file while parsing"
    (Token (pos, tk):rest) -> "Parse error: " ++ show pos

many :: [Exp] -> Exp
many [ ] = error "many of []"
many [e] = e
many es  = App (many (init es)) (last es)
}
