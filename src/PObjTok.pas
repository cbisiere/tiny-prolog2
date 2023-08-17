{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Tokenize.pas                                               }
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


{ Tokens; spaces and comments are always allowed between tokens }
Type
  TTokenType = (
    TOKEN_UNKNOWN,
    TOKEN_STRING,
    TOKEN_NUMBER,
    TOKEN_IDENT,
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
    TOKEN_EQUAL,
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
    'number',
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
    'left square brackets',
    'right square brackets',
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

{-----------------------------------------------------------------------}
{ constructors                                                          }
{-----------------------------------------------------------------------}

{ create a new token of type typ }
Function NewToken( typ : TTokenType ) : TokenPtr;
Var 
  K : TokenPtr;
  ptr : TPObjPtr Absolute K;
Begin
  ptr := NewRegisteredObject(TK,2,False,0);
  With K^ Do
  Begin
    TK_NEXT := Nil;
    TK_STRI := Nil;
    TK_TYPE := typ;
    TK_LINE := 0;
    TK_CHAR := 0
  End;
  NewToken := K
End;

{-----------------------------------------------------------------------}
{ methods                                                               }
{-----------------------------------------------------------------------}

{ get the line and column number of the start of token K, *including
 preceding blank spaces* }
Procedure GetTokenLocation( K : TokenPtr; Var line : TLineNum; 
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
Procedure SetTokenLocation( K : TokenPtr; line : TLineNum; col : TCharPos );
Begin
  With K^ Do
  Begin
    TK_LINE := line;
    TK_CHAR := col
  End
End;

{ get next token }
Function NextToken( K : TokenPtr ) : TokenPtr;
Begin
  CheckCondition(K <> Nil,'Cannot compute the next term of Nil');
  NextToken := K^.TK_NEXT
End;

{ set next token }
Procedure SetNextToken( K,N : TokenPtr );
Begin
  CheckCondition(K <> Nil,'Cannot set the next term of Nil');
  K^.TK_NEXT := N
End;

{ get token type }
Function TokenType( K : TokenPtr ) : TTokenType;
Begin
  CheckCondition(K <> Nil,'Cannot compute the type of token Nil');
  TokenType := K^.TK_TYPE
End;

{ get token type as a string }
Function TokenTypeAsString( K : TokenPtr ) : TString;
Begin
  TokenTypeAsString := TokenStr[TokenType(K)]
End;


{----------------------------------------------------------------------------}
{ DEBUG                                                                      }
{----------------------------------------------------------------------------}

Procedure DumpTokens( K : TokenPtr );
Begin
  While K <> Nil Do
  Begin
    WriteExtraData(TPObjPtr(K));
    CWriteLn;
    K := NextToken(K)
  End;
  CWriteLn
End;