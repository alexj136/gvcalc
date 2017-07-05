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
%name parseExp EXP
%name parseConfig CONFIG

%right Plus Minus Bars
%nonassoc Fork Fix Accept Request Send Receive Select Case Of Else Lam Let In
    End New Unit True False Comma Semi Colon Dot Equal LParen RParen LCurl RCurl
    LSqu RSqu LAngle RAngle LArrow RArrow Name

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
    Of      { Token ( _ , TK_Of      ) }
    Else    { Token ( _ , TK_Else    ) }
    Lam     { Token ( _ , TK_Lam     ) }
    Let     { Token ( _ , TK_Let     ) }
    In      { Token ( _ , TK_In      ) }
    End     { Token ( _ , TK_End     ) }
    New     { Token ( _ , TK_New     ) }
    Unit    { Token ( _ , TK_Unit    ) }
    True    { Token ( _ , TK_True    ) }
    False   { Token ( _ , TK_False   ) }
    Minus   { Token ( _ , TK_Minus   ) }
    Plus    { Token ( _ , TK_Plus    ) }
    Comma   { Token ( _ , TK_Comma   ) }
    Semi    { Token ( _ , TK_Semi    ) }
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
    LArrow  { Token ( _ , TK_LArrow  ) }
    RArrow  { Token ( _ , TK_RArrow  ) }
    Bars    { Token ( _ , TK_Bars    ) }
    Name    { Token ( _ , TK_Name _  ) }
    Num     { Token ( _ , TK_Num  _  ) }
    Error   { Token ( _ , TK_Error   ) }
%%

NAME :: { Name }
NAME : Name { case $1 of Token (_, TK_Name n) -> n }

NUM :: { Integer }
NUM : Num { case $1 of Token (_, TK_Num n) -> n }

VAL :: { Val }
VAL
    : Fix                         { Fix           }
    | Fork                        { Fork          }
    | Unit                        { Unit          }
    | Send                        { Send          }
    | Receive                     { Receive       }
    | Request NUM                 { Request $2    }
    | Accept  NUM                 { Accept  $2    }
    | Plus                        { Plus          }
    | Minus                       { Minus         }
    | Lam NAME Dot EXP            { Lam $2 $4     }
    | NAME                        { Var $1        }
    | NUM                         { Number $1     }
    | True                        { Boolean True  }
    | False                       { Boolean False }

EXP :: { Exp }
EXP : EXPOPTS { many $1 }

EXPOPT :: { Exp }
EXPOPT
    : VAL                                  { Lit $1          }
    | LParen EXP Comma EXP RParen          { Pair $2 $4      }
    | Let NAME Comma NAME Equal EXP In EXP { Let $2 $4 $6 $8 }
    | Select EXP Of EXP                    { Select $2 $4    }
    | Case EXP Of EXP Else EXP             { Case $2 $4 $6   }
    | LParen EXP RParen                    { $2              }

EXPOPTS :: { [Exp] }
EXPOPTS : EXPOPT EXPOPTS { $1 : $2 } | EXPOPT { [$1] }

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
    (Token (pos, tk):rest) -> "Parse error: " ++ show tk ++ " at " ++ show pos

many :: [Exp] -> Exp
many [ ] = error "many of []"
many [e] = e
many es  = App (many (init es)) (last es)
}
