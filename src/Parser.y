{
module Parser where

import Model
import Prelude hiding ( Left, Right )
}

%name parser
%tokentype { Token }

%token
  '->'     { TArrow }
  '.'      { TDot }
  ','      { TComma }

  go       { TGo }
  take     { TTake }
  mark     { TMark }
  nothing  { TNothing }
  turn     { TTurn }

  case     { TCase }
  of       { TOf }
  end      { TEnd }

  left     { TLeft }
  right    { TRight }
  front    { TFront }
  ';'      { TDotComma }

  Empty    { TEmpty }
  Lambda   { TLambda }
  Debris   { TDebris }
  Asteroid { TAsteroid }
  Boundary { TBoundary }
  '_'      { TUnderscore }

  ident    { TIdent $$ }

%%

Program :: { [Rule] }
        : {- empty -}  { [] }
        | Rule         { [$1] }
        | Program Rule { $2 : $1 } 

Rule :: { Rule }
     : ident '->' Cmds '.' { Rule $1 $3 }

Cmds :: { [Command] }
     : {- empty -}  { [] }
     | Cmd          { [$1] }
     | Cmds ',' Cmd { $3 : $1 }

Cmd :: { Command }
    : go   { CGo }
    | take { CTake }
    | mark { CMark }
    | nothing { CNothing }
    | turn Dir { CTurn $2 }
    | case Dir Alts { CCase $2 $3 }
    | ident { CIdent $1 }

Dir :: { Dir }
    : left { Left }
    | right { Right }
    | front { Front }

Alts :: { [Alt] }
     : {- empty -}  { [] }
     | Alt          { [$1] }
     | Alts ';' Alt { $3 : $1 }

Alt :: { Alt }
    : Pat '->' Cmds { Alt $1 $3 }

Pat :: { Pat }
    : Empty    { PEmpty }
    | Lambda   { PLambda }
    | Debris   { PDebris }
    | Asteroid { PAstroid }
    | Boundary { PBoundary }
    | '_'      { PUnderscore }

{

happyError _ = error "parse error"

}
