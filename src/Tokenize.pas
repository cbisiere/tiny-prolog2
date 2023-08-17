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
{                              T O K E N I Z E R                             }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ Notes about the tokenizer:

  This tokenizer can be used as a first pass, or during a single compilation
  phase. We opt for a single pass, as giving feedback on errors is much more
  difficult with two passes. Indeed, he input line where the error occurred is
  no more at hand during the second phase.
  
  There is also one specific issue with the two passes approach: the old
  Prolog II syntax allowed to end an input file with an extra semicolon, 
  everything after being considered as a comment.(*) It is a bit tricky to give 
  the tokenizer enough context to understand that this semicolon is 
  equivalent to an end of input.

  (*) I came to this conclusion while looking at two of my old Prolog 
  assignments, which end with the string ";End world: Normal". Maybe this was 
  appended by the internal text editor of the Prolog system I was using at that 
  time? The only reference to this string on the Internet is the following:
  https://papers.cumincad.org/data/works/att/ef95.content.pdf (page 8.1.4)
  This syntax quirk is not mentioned in the Giannesini at al.'s Prolog book,
  so I guess this final ";" was preprocessed by the editor itself.  
}

{----------------------------------------------------------------------------}
{ codepoint                                                                  }
{----------------------------------------------------------------------------}

{ append to a string codepoints while their first letter belongs to a 
 certain set; return the number of codepoints appended }
Function GetCharWhile( Ch : StrPtr; E : CharSet ) : LongInt;
Var 
  c : TChar;
  n : LongInt;
  Found : Boolean;
Begin
  GetCharWhile := 0;
  n := 0;
  Repeat
    c := GetChar(c);
    If Error Then Exit;
    Found := c[1] In E;
    If Not Found Then
      UnGetChar
    Else
    Begin
      StrAppend(Ch,c);
      n := n + 1
    End
  Until Not Found Or Error;
  GetCharWhile := n
End;

{ append to a string codepoints until a codepoint has its first letter 
 in a certain set; return that codepoint }
Function GetCharUntil( Ch : StrPtr; E : CharSet ) : TChar;
Var 
  c : TChar;
  Found : Boolean;
Begin
  GetCharUntil := '';
  Repeat
    c := GetChar(c);
    If Error Then Exit;
    Found := c[1] In E;
    If Found Then
      UnGetChar
    Else
      StrAppend(Ch,c)
  Until Found Or Error;
  GetCharUntil := c
End;

{ return the next non-blank codepoint without consuming it }
Function NextCharNb( Var c : TChar ) : TChar;
Begin
  NextCharNb := GetCharNb(c);
  If Error Then Exit;
  UnGetChar
End;

{ verify that all the 1-byte chars in string s is next in input }
Procedure Verify( s : TString );
Var
  q : TString;
  c  : TChar;
Begin
  q := s;
  Repeat
    c := GetChar(c);
    If Error Then Exit;
    If c <> q[1] Then 
      SyntaxError('"' + s + '" expected');
    If Error Then Exit;
    Delete(q,1,1);
  Until (Length(q) = 0) Or Error
End;


{----------------------------------------------------------------------------}
{ encoding-dependant entities                                                }
{----------------------------------------------------------------------------}

{ chars and charsets }
Const
  Letters : CharSet = ['a'..'z','A'..'Z']; { 7-bit ASCII letters }
  Alpha : CharSet = ['a'..'z','A'..'Z','_','0'..'9']; { 7-bit ASCII alpha }
  Digits  : CharSet = ['0'..'9']; { digits }
  ISO8859_Letters : CharSet = [#$C0..#$FF] - [#$D7,#$F7];
  CP850_Letters : CharSet = [#$80..#$9B,#$9D,#$A0..#$A5,#$B5..#$B7,
    #$C6,#$C7,#$D0..#$D4,#$D6..#$D8,#$DE,#$E0..#$E5,#$E7..#$ED];
  CP437_Letters : CharSet = [#$80..#$99,#$A0..#$A5,#$E1];


{ try to read one letter, as defined page R1-2 of the Prolog II+ manual:
  "A"|...|"Z"|"a"|...|"z"|"À" ... "ß" - "x" | "à" ... "ÿ" - "÷"
  where x is the multiplication sign, and ordering is ISO 8859.
  it is assumed that, if the input is ISO-8859-1, there is no "Ã" (C3)
  followed by a char that would make the 2-byte sequence an UTF8
  letter; hopefully 80-9F are undefined in ISO-8859-1; A0-BF is possible
  but none of those combinations (e.g. Ã¢) would be a valid part of an
  identifier, so either the input is UTF8 and there is a syntax error,
  or the input is UTF8, which we therefore assume;
  "big letters" are A-Z only, see PrologII+ manual, R 1-23  }
Function GrabOneLetter( Var Ch : StrPtr; Var IsUpper : Boolean ) : Boolean;
Var
  c : TChar;
  Found : Boolean;
  Enc : TEncoding;

  Procedure Accept( Enc : TEncoding );
  Begin
    c := GetChar(c);
    If Error Then Exit;
    StrAppend(Ch,c);
    SetInputEncoding(Enc);
    GrabOneLetter := True
  End;

Begin
  GrabOneLetter := False;
  c := NextChar(c);
  If Error Then Exit;
  IsUpper := False;
  Enc := InputEncoding;

  { simplest case: ASCII letter }
  Found := (Length(c) = 1) And (c[1] In Letters);
  If Found Then
  Begin
    IsUpper := c[1] In ['A'..'Z'];
    Accept(Enc);
    Exit
  End;

  { UTF8: https://www.utf8-chartable.de/ }
  If InputEncoding = UTF8 Then
  Begin
    If Length(c) = 2 Then
      If (c[1] = #$C3) And (c[2] In ([#$80..#$BF] - [#$97,#$B7])) Then
      Begin
        Accept(Enc);
        Exit
      End;
    Exit
  End;

  { ISO-8859-1: https://fr.wikipedia.org/wiki/ISO/CEI_8859-1 }
  If InputEncoding In [UNDECIDED,ISO8859] Then
  Begin
    If c[1] In ISO8859_Letters Then
      Accept(ISO8859);
    Exit
  End;

  { CP850 and 858: https://en.wikipedia.org/wiki/Code_page_850#Character_set }
  If InputEncoding = CP850 Then
  Begin
    If c[1] In CP850_Letters Then
      Accept(CP850);
    Exit
  End;

  { CP437: https://en.wikipedia.org/wiki/Code_page_437#Character_set }
  If InputEncoding = CP437 Then
  Begin
    If c[1] In CP437_Letters Then
      Accept(CP437);
    Exit
  End
End;

{ grab letters; return the number of letters appended to Ch }
Function GrabLetters( Var Ch : StrPtr ) : LongInt;
Var
  n : LongInt;
  IsUpper : Boolean;
Begin
  GrabLetters := 0;
  n := 0;
  While GrabOneLetter(Ch,IsUpper) And (Not Error) Do
    n := n + 1;
  If Error Then Exit;
  GrabLetters := n
End;

{ append to a string any alphanumeric characters (plus underscore); 
 return the number of characters added to the string }
Function GrabAlpha( Var Ch : StrPtr ) : LongInt;
Var
  n : LongInt;
  More : Boolean;
  IsUpper : Boolean;
Begin
  GrabAlpha := 0;
  n := 0;
  More := True;
  While More And Not Error Do
  Begin
    n := n + GetCharWhile(Ch,Alpha);
    If Error Then Exit;
    More := GrabOneLetter(Ch,IsUpper);
    If Error Then Exit;
    If More Then
      n := n + 1
  End;
  If Error Then Exit;
  GrabAlpha := n
End;


{----------------------------------------------------------------------------}
{ tokens                                                                     }
{----------------------------------------------------------------------------}

{ read a double-quoted constant string }
{ TODO: escaped chars }
Function ReadString : TokenPtr;
Var
  K : TokenPtr;
  c : TChar;
  Done : Boolean;
Begin
  ReadString := Nil;
  c := GetChar(c);
  If Error Then Exit;
  CheckCondition(c = '"',  TokenStr[TOKEN_STRING] + ' expected');
  K := NewToken(TOKEN_STRING);
  With K^ Do
  Begin
    TK_STRI := NewString;
    Repeat
      Done := False;
      { look for the next char with special meaning inside a string }
      c := GetCharUntil(TK_STRI, ['"','\',NewLine,EndOfInput]);
      If Error Then Exit;
      If c = '"' Then { doubled double quote or end of string }
      Begin
        c := GetChar(c); { discard it }
        If Error Then Exit;
        c := NextChar(c);
        If Error Then Exit;
        If c = '"' Then { doubled: keep only one }
        Begin
          c := GetChar(c);
          If Error Then Exit;
          StrAppend(TK_STRI,c)
        End
        Else
          Done := True
      End
      Else If c = '\' Then { backslash or line continuation }
      Begin
        c := NextNextChar(c);
        If Error Then Exit;
        if c = NewLine Then
        Begin
          c := GetChar(c); { discard '\' }
          If Error Then Exit;
          c := GetChar(c)  { discard NewLine } 
        End
        Else
        Begin
          c := GetChar(c);
          If Error Then Exit;
          StrAppend(TK_STRI,c) { append '\' }
        End
      End
      Else If c = NewLine Then
        SyntaxError('end of line while reading ' + TokenStr[TOKEN_STRING])
      Else If c = EndOfInput Then
        SyntaxError('end of input while reading ' + TokenStr[TOKEN_STRING])
    Until Error or Done;
    If Error Then Exit
  End;
  ReadString := K
End;

{ skip a *-style comment, with symbol '/' or '|'; *-style comments can be 
  nested, e.g. "/* ... |* .... *| .... */" (even with the same symbol) }
Procedure ReadComment( symbol : TChar );
Var
  c,c2 : TChar;
  Stop : Boolean;
Begin
  Verify(symbol + '*');
  If Error Then Exit;
  Repeat
    Stop := False;
    Repeat
      c := GetChar(c)
    Until Error Or (c = '*') Or (c = '/') Or (c = '|') Or (c = EndOfInput);
    If Error Then Exit;
    If c = '*' Then
    Begin
      c := NextChar(c);
      If Error Then Exit;
      If c = symbol Then
      Begin
        c := GetChar(c);
        If Error Then Exit;
        Stop := true
      End
    End
    Else If (c = '/') Or (c = '|') Then
    Begin
      c2 := NextChar(c2);
      If Error Then Exit;
      If c2 = '*' Then
      Begin
        UnGetChar;
        ReadComment(c);
        If Error Then Exit
      End
    End
    Else
      SyntaxError('end of input within a comment')
  Until Stop Or Error
End;

{ skip any sequence of blank spaces and comments }
Procedure ReadSpaces( y : TSyntax );
Var
  c,c2 : TChar;
  Stop : Boolean;
Begin
  Repeat
    Stop := True;
    c := NextCharNb(c);
    If Error Then Exit;
    c2 := NextNextChar(c2);
    If Error Then Exit;
    If y In [PrologIIp,Edinburgh] Then
    Begin
      If c = '%' Then
      Begin
        Repeat
          c := GetChar(c)
        Until Error Or (c = NewLine) Or (c = EndOfInput); 
        If Error Then Exit;
        If c = EndOfInput Then
          UnGetChar;
        Stop := False
      End
      Else If ((c = '/') Or (c = '|')) And (c2 = '*') Then
      Begin
        ReadComment(c);
        If Error Then Exit;
        Stop := False
      End
    End
  Until Stop Or Error
End;

{ read a number }
{ TODO: floating point numbers }
Function ReadNumber : TokenPtr;
Var
  K : TokenPtr;
  c : TChar;
  n : LongInt;
Begin
  ReadNumber := Nil;
  K := NewToken(TOKEN_NUMBER);
  With K^ Do
  Begin
    TK_STRI := NewString;
    c := NextChar(c);
    If Error Then Exit;
    If c = '-' Then
    Begin
      c := GetChar(c);
      If Error Then Exit;
      StrAppend(TK_STRI,c)
    End;
    n := GetCharWhile(TK_STRI,Digits);
    If Error Then Exit;
    CheckCondition(n > 0,'Number expected')
  End;
  ReadNumber := K
End;

{ create a token of type typ while checking a given string of 1-byte
 chars is in input }
Function GrabToken( typ : TTokenType; s : TString ) : TokenPtr;
Var
  K : TokenPtr;
Begin
  GrabToken := Nil;
  K := Nil;
  Verify(s);
  If Error Then Exit;
  K := NewToken(typ);
  GrabToken := K
End;

{ read a token, following syntax y }
{ TODO: Edinburgh: identifiers made of graphic_char }
Function ReadToken( y : TSyntax ) : TokenPtr;
Var
  K : TokenPtr;
  e : TIChar;
  c,c2 : TChar;
  IsUpper : Boolean;
  n,m : LongInt;
Begin
  ReadToken := Nil;
  K := Nil;
  { save the next char for undo purpose; this is the char of the token but
   including blank spaces; this is needed to "unread" tokens, which is required
   to handle in_char(c) goals }
  e := NextIChar(e);
  ReadSpaces(y);
  If Error Then Exit;
  c := NextChar(c);
  If Error Then Exit;
  c2 := NextNextChar(c2);
  If Error Then Exit;
  Case c[1] Of
  EndOfInput:
    K := NewToken(TOKEN_END_OF_INPUT);
  '0'..'9':
    K := ReadNumber;
  '-': 
    If c2[1] In Digits Then
      K := ReadNumber
    Else If (c2 = '>') And (y <> Edinburgh) Then { '->' }
      K := GrabToken(TOKEN_ARROW,'->');
  ':':
    If (c2 = '-') And (y = Edinburgh) Then { ':-' }
      K := GrabToken(TOKEN_ARROW,':-');
  '_':
    If y In [PrologIIp,Edinburgh] Then  { a variable: PrologII+ basic syntax }
    Begin
      K := NewToken(TOKEN_VARIABLE);
      With K^ Do
      Begin
        TK_STRI := NewString;
        n := GrabAlpha(TK_STRI) { letters (inc. accented), digits, underscore }
      End
    End;
  '"':
    K := ReadString;
  '!':
    If y In [PrologIIp,Edinburgh] Then
      K := GrabToken(TOKEN_CUT,c);
  '/':
    If y In [PrologII,PrologIIc] Then
      K := GrabToken(TOKEN_CUT,c);
  ';':
    K := GrabToken(TOKEN_SEMICOLON,c);
  '(':
    K := GrabToken(TOKEN_LEFT_PAR,c);
  ')':
    K := GrabToken(TOKEN_RIGHT_PAR,c);
  '{':
    K := GrabToken(TOKEN_LEFT_CUR,c);
  '}':
    K := GrabToken(TOKEN_RIGHT_CUR,c);
  '<':
    K := GrabToken(TOKEN_LEFT_CHE,c);
  '>':
    K := GrabToken(TOKEN_RIGHT_CHE,c);
  '[':
    K := GrabToken(TOKEN_LEFT_BRA,c);
  ']':
    K := GrabToken(TOKEN_RIGHT_BRA,c);
  '.':
    K := GrabToken(TOKEN_DOT,c);
  ',':
    K := GrabToken(TOKEN_COMMA,c);
  '=':
    K := GrabToken(TOKEN_EQUAL,c);
  '|':
    K := GrabToken(TOKEN_PIPE,c);
  Else
    Begin { should be a variable or an identifiers }
      K := NewToken(TOKEN_UNKNOWN);
      With K^ Do
      Begin
        TK_STRI := NewString;
        If GrabOneLetter(TK_STRI,IsUpper) Then
        Begin
          { Edinburgh variables start with a "big letter"; Marseille variables 
            start with a single letter }
          If y = Edinburgh Then
            If IsUpper Then
              TK_TYPE := TOKEN_VARIABLE
            Else
              TK_TYPE := TOKEN_IDENT
          Else
            If Not GrabOneLetter(TK_STRI,IsUpper) Then
              TK_TYPE := TOKEN_VARIABLE
            Else
              TK_TYPE := TOKEN_IDENT;

          If Error Then Exit;
          If y In [PrologII,PrologIIc] Then { old Prolog II syntax, w/ accented letters }
          Begin
            Repeat
              c := NextChar(c);
              If Error Then Exit;
              If c = '-' Then { "-"<word> continuation }
              Begin
                c := GetChar(c);
                If Error Then Exit;
                StrAppend(TK_STRI,c)
              End;
              n := GrabLetters(TK_STRI);
              If Error Then Exit;
              m := GetCharWhile(TK_STRI,Digits);
              If Error Then Exit;
              m := GetCharWhile(TK_STRI,['''']);
              If Error Then Exit;
              If (c = '-') And (n = 0) Then
                SyntaxError('dash must be followed by a letter');
              If Error Then Exit;
              c := NextChar(c);
              If Error Then Exit
            Until (c <> '-') Or Error;
          End
          Else { PrologII+ and Edinburgh extended syntax }
          Begin
            n := GrabAlpha(TK_STRI);
            If Error Then Exit;
            If y <> Edinburgh Then
              n := GetCharWhile(TK_STRI,[''''])
          End
        End
        Else
          K := Nil { not a letter: error }
      End
    End
  End;
  If Error Then Exit;
  If K = Nil Then
  Begin
    c := GetChar(c); { read the char so error message points to it }
    If Error Then Exit;
    SyntaxError('that character is not allowed here')
  End;
  If Error Then Exit;
  SetTokenLocation(K,e.Lnb,e.Pos);
  ReadToken := K
End;
