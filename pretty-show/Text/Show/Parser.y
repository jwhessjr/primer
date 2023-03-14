{
-- We use these options because Happy generates code with a lot of warnings.
{-# LANGUAGE Trustworthy #-}
module Text.Show.Parser (parseValue) where

import Text.Show.Value
import Language.Haskell.Lexer
import Data.List(intercalate)
}

%token

        '='             { (Reservedop, (_,"=")) }
        '('             { (Special, (_,"(")) }
        ')'             { (Special, (_,")")) }
        '{'             { (Special, (_,"{")) }
        '}'             { (Special, (_,"}")) }
        '['             { (Special, (_,"[")) }
        ']'             { (Special, (_,"]")) }
        '<'             { (Varsym, (_,"<")) }
        '>'             { (Varsym, (_,">")) }
        ','             { (Special, (_,",")) }
        '-'             { (Varsym,  (_,"-")) }
        '%'             { (Varsym,  (_,"%")) }
        '`'             { (Special, (_,"`")) }
        ':'             { (Reservedop,  (_,":")) }

        INT             { (IntLit,   (_,$$)) }
        FLOAT           { (FloatLit, (_,$$)) }
        STRING          { (StringLit, (_,$$)) }
        CHAR            { (CharLit,  (_,$$)) }

        VARID           { (Varid,    (_,$$)) }
        QVARID          { (Qvarid,   (_,$$)) }
        VARSYM          { (Varsym,   (_,$$)) }
        QVARSYM         { (Qvarsym,  (_,$$)) }
        CONID           { (Conid,    (_,$$)) }
        QCONID          { (Qconid,   (_,$$)) }
        CONSYM          { (Consym,   (_,$$)) }
        QCONSYM         { (Qconsym,  (_,$$)) }
        QQUOTE          { (QQuote,   (_,$$)) }
        RESOP           { (Reservedop, (_,$$)) }
        RESID           { (Reservedid, (_,$$)) }



%monad { Maybe } { (>>=) } { return }
%name parseValue value
%tokentype { PosToken }


%%

value                        :: { Value }
  : value '%' app_value         { Ratio $1 $3 }
  | app_value                   { $1 }
  | app_value list1(infixelem)  { InfixCons $1 $2 }

infixelem                    :: { (String,Value) }
  : infixcon app_value          { ($1,$2) }

app_value                    :: { Value }
  : list1(avalue)               { mkValue $1 }
  | '-' avalue                  { Neg $2 }


avalue                       :: { Value }
  : '(' value ')'               { $2 }  -- hmm, we loose some parens here
  | '[' sep(value,',') ']'      { List $2 }
  | '(' tuple ')'               { Tuple $2 }
  | con '{' sep(field,',') '}'  { Rec $1 $3 }
  | con                         { Con $1 [] }
  | INT                         { Integer $1 }
  | FLOAT                       { Float $1 }
  | STRING                      { String $1 }
  | CHAR                        { Char $1 }
  | INT '-' INT '-' INT         { mkDate $1 $3 $5 }
  | INT ':' INT ':' INT         { mkTime $1 $3 $5 }
  | INT ':' INT ':' FLOAT       { mkTime $1 $3 $5 }
  | QQUOTE                      { Quote $1 }


con                          :: { String }
  : CONID                       { $1 }
  | QCONID                      { $1 }
  | prefix(CONSYM)              { $1 }
  | prefix(QCONSYM)             { $1 }
  -- to support things like "fromList x"
  | VARID                       { $1 }
  | QVARID                      { $1 }
  | prefix(VARSYM)              { $1 }
  | prefix(QVARSYM)             { $1 }
  | '<' VARID '>'               { "<" ++ $2 ++ ">" } -- note: looses space
  | '<' CONID '>'               { "<" ++ $2 ++ ">" } -- ditto
  | RESID                       { $1 }

infixcon                     :: { String }
  : CONSYM                      { $1 }
  | QCONSYM                     { $1 }
  | VARSYM                      { $1 }
  | QVARSYM                     { $1 }
  | '`' CONID '`'               { backtick $2 }
  | '`' QCONID '`'              { backtick $2 }
  | RESOP                       { $1 }

field                        :: { (Name,Value) }
  : VARID '=' value             { ($1,$3) }

tuple                        :: { [Value] }
  :                             { [] }
  | value ',' sep1(value,',')   { $1 : $3 }

-- Common Rule Patterns --------------------------------------------------------
prefix(p)       : '(' p ')'           { "(" ++ $2 ++ ")" }

sep1(p,q)       : p list(snd(q,p))    { $1 : $2 }
sep(p,q)        : sep1(p,q)           { $1 }
                |                     { [] }

snd(p,q)        : p q                 { $2 }

list1(p)        : rev_list1(p)        { reverse $1 }
list(p)         : list1(p)            { $1 }
                |                     { [] }

rev_list1(p)    : p                   { [$1] }
                | rev_list1(p) p      { $2 : $1 }



{
backtick :: String -> String
backtick s = "`" ++ s ++ "`"

happyError :: [PosToken] -> Maybe a
happyError ((_,(p,_)) : _) = Nothing -- error ("Parser error at: " ++ show p)
happyError []              = Nothing -- error ("Parser error at EOF")

mkDate :: String -> String -> String -> Value
mkDate a b c = Date (intercalate "-" [a,b,c])

mkTime :: String -> String -> String -> Value
mkTime a b c = Time (intercalate ":" [a,b,c])



mkValue :: [Value] -> Value
mkValue [v]                 = v
mkValue (Con "" [] : vs)    = mkValue vs
mkValue (Con x as : vs)     = Con x (as ++ vs)
mkValue vs                  = mkFakeCon vs

mkFakeCon :: [Value] -> Value
mkFakeCon vs = Con "" (concatMap expand vs)
  where expand (Con "" vs) = vs
        expand v           = [v]


}
