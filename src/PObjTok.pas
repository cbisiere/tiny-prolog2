{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObjTok.pas                                                }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                          T O K E N   O B J E C T                           }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Unit PObjTok;

Interface

Uses
  ShortStr,
  Errs,
  Trace,
  IChar,
  Memory,
  PObj,
  PObjStr;

{ Tokens; spaces and comments are always allowed between tokens }
Type
  TTokenType = (
    TOKEN_UNKNOWN,
    TOKEN_STRING,
    TOKEN_INTEGER,
    TOKEN_REAL,
    TOKEN_IDENT,       { atom }
    TOKEN_VARIABLE,
    TOKEN_ARROW,       { :- or -> }
    TOKEN_CUT,         { cut ! or / }
    TOKEN_LEFT_PAR,    { parentheses ( ) }
    TOKEN_RIGHT_PAR,
    TOKEN_LEFT_CUR,    { curly brackets }
    TOKEN_RIGHT_CUR,
    TOKEN_LEFT_CHE,    { chevrons < >}
    TOKEN_RIGHT_CHE,
    TOKEN_LEFT_BRA,    { square brackets [ ]}
    TOKEN_RIGHT_BRA,
    TOKEN_DOT,
    TOKEN_COMMA,
    TOKEN_SEMICOLON,
    TOKEN_EQUAL,       { equal sign in pIIc constraints }
    TOKEN_PIPE,
    TOKEN_END_OF_INPUT
  );
  TTokenSet = Set Of TTokenType;

{ token names in error messages }
Type
  TypeTokenStr = Array[TTokenType] Of String[21];
Const TokenStr : TypeTokenStr = (
    'unknown',
    'string',
    'integer number',
    'real number',
    'identifier',
    'variable',
    'arrow',
    'cut',
    'left parenthesis',
    'right parenthesis',
    'left curly brace',
    'right curly brace',
    'left chevron',
    'right chevron',
    'left square bracket',
    'right square bracket',
    'dot',
    'comma',
    'semicolon',
    'equal sign',
    'pipe',
    'end of input'
  );

{ token object; 
 - the linked list structure may be used to implement a first (tokenization) 
  pass; we opt for a single compilation pass, though; 
 - we register the location (line and column) of the start of the 
  token *including any preceding blank spaces*, in order to be able to "unread" 
  the token; this is required to correctly handle goals like "in(t) in_char(c)"
  as c is supposed to be set to the character just after the term in input; 
  if the user enter "aaa ;", then c must be matched with " ", so this token 
  must be unread along with its preceding blank space; as a consequence, the
  parser will fail to parse "in(t) in_char(c)" goals when the user type a huge
  number of spaces after a term }
Type
  TokenPtr = ^TObjToken;
  TObjToken = Record
    PO_META : TObjMeta;
    { not deep copied: }
    TK_NEXT : TokenPtr;
    TK_STRI : StrPtr; { string object representing the token or Nil }
    { extra data: }
    TK_TYPE : TTokenType;
    TK_LINE : TLineNum; { line number in the input stream }
    TK_CHAR : TCharPos { index in the input line }
  End;


Function Token_New( typ : TTokenType ) : TokenPtr;

Function Token_GetNext( K : TokenPtr ) : TokenPtr;
Procedure Token_SetNext( K,N : TokenPtr );
Function Token_GetStr( K : TokenPtr ) : StrPtr;
Procedure Token_GetLocation( K : TokenPtr; Var line : TLineNum; Var col : TCharPos); 
Procedure Token_SetLocation( K : TokenPtr; line : TLineNum; col : TCharPos );
Function Token_GetType( K : TokenPtr ) : TTokenType;
Function Token_GetTypeAsString( K : TokenPtr ) : TString;


Implementation
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------}
{ constructors                                                          }
{-----------------------------------------------------------------------}

{ create a new token of type typ }
Function Token_New( typ : TTokenType ) : TokenPtr;
Var 
  K : TokenPtr;
  ptr : TObjectPtr Absolute K;
Begin
  ptr := NewRegisteredPObject(TK,SizeOf(TObjToken),2,False,0);
  With K^ Do
  Begin
    TK_NEXT := Nil;
    TK_STRI := Nil;
    TK_TYPE := typ;
    TK_LINE := 0;
    TK_CHAR := 0
  End;
  Token_New := K
End;

{-----------------------------------------------------------------------}
{ get / set                                                             }
{-----------------------------------------------------------------------}

{ get next token }
Function Token_GetNext( K : TokenPtr ) : TokenPtr;
Begin
  CheckCondition(K <> Nil,'Token_GetNext: Nil');
  Token_GetNext := K^.TK_NEXT
End;

{ set next token }
Procedure Token_SetNext( K,N : TokenPtr );
Begin
  CheckCondition(K <> Nil,'Token_SetNext: Nil');
  K^.TK_NEXT := N
End;

{ get a token's str }
Function Token_GetStr( K : TokenPtr ) : StrPtr;
Begin
  Token_GetStr := K^.TK_STRI
End;

{ get the line and column number of the start of token K, *including
 preceding blank spaces* }
Procedure Token_GetLocation( K : TokenPtr; Var line : TLineNum; 
    Var col : TCharPos );
Begin
  With K^ Do
  Begin
    line := TK_LINE;
    col := TK_CHAR
  End
End;

{ set the line and column number of the start of token K, *including
 preceding blank spaces* }
Procedure Token_SetLocation( K : TokenPtr; line : TLineNum; col : TCharPos );
Begin
  With K^ Do
  Begin
    TK_LINE := line;
    TK_CHAR := col
  End
End;

{ get token type }
Function Token_GetType( K : TokenPtr ) : TTokenType;
Begin
  CheckCondition(K <> Nil,'Token_GetType: Nil');
  Token_GetType := K^.TK_TYPE
End;

{-----------------------------------------------------------------------}
{ methods                                                               }
{-----------------------------------------------------------------------}

{ get token type as a string }
Function Token_GetTypeAsString( K : TokenPtr ) : TString;
Begin
  Token_GetTypeAsString := TokenStr[Token_GetType(K)]
End;

End.