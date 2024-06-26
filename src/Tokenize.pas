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
  everything after being considered as a comment.[*] It is a bit tricky to give 
  the tokenizer enough context to understand that this semicolon is 
  equivalent to an end of input.

  [*] I came to this conclusion while looking at two of my old Prolog 
  assignments, which end with the string ";End world: Normal". Maybe this was 
  appended by the internal text editor of the Prolog system I was using at that 
  time? The only reference to this string on the Internet is the following:
  https://papers.cumincad.org/data/works/att/ef95.content.pdf (page 8.1.4)
  This syntax quirk is not mentioned in the Giannesini at al.'s Prolog book,
  so I guess this final ";" was preprocessed by the editor itself.  
}

Unit Tokenize;

Interface

Uses
  Errs,
  ShortStr,
  Chars,
  IChar,
  PObjIO,
  PObjStr,
  PObjTok,
  PObjDef;
  
Procedure ReadBlanks( f : StreamPtr );
Function ReadString( f : StreamPtr ) : TokenPtr;
Function ReadInteger( f : StreamPtr ) : TokenPtr;
Function ReadNumber( f : StreamPtr; y : TSyntax ) : TokenPtr;
Function ReadVariableOrIdentifier( f : StreamPtr; y : TSyntax ) : TokenPtr;
Function ReadToken( f : StreamPtr; y : TSyntax ) : TokenPtr;

Implementation
{-----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ TChars                                                                     }
{----------------------------------------------------------------------------}

{ append TChars to a string while their first letter belongs to a 
 certain set; return the number of TChars appended }
Function GetCharWhile( f : StreamPtr; Ch : StrPtr; E : CharSet ) : LongInt;
Var 
  c : TChar;
  n : LongInt;
  Found : Boolean;
Begin
  GetCharWhile := 0;
  n := 0;
  Repeat
    c := Stream_GetChar(f,c);
    If Error Then Exit;
    Found := c[1] In E;
    If Not Found Then
      Stream_UngetChar(f)
    Else
    Begin
      Str_AppendChar(Ch,c);
      n := n + 1
    End
  Until Not Found Or Error;
  GetCharWhile := n
End;

{ append TChars to a string until a TChar has its first letter 
 in a certain set; return that TChar }
Function GetCharUntil( f : StreamPtr; Ch : StrPtr; E : CharSet ) : TChar;
Var 
  c : TChar;
  Found : Boolean;
Begin
  GetCharUntil := '';
  Repeat
    c := Stream_GetChar(f,c);
    If Error Then Exit;
    Found := c[1] In E;
    If Found Then
      Stream_UngetChar(f)
    Else
      Str_AppendChar(Ch,c)
  Until Found Or Error;
  GetCharUntil := c
End;

{ return the next non-blank TChar without consuming it }
Function NextCharNb( f : StreamPtr; Var c : TChar ) : TChar;
Begin
  NextCharNb := Stream_GetCharNb(f,c);
  If Error Then Exit;
  Stream_UngetChar(f)
End;

{ verify that all the 1-byte chars in string s is next in input }
Procedure Verify( f : StreamPtr; s : TString );
Var
  q : TString;
  c  : TChar;
Begin
  q := s;
  Repeat
    c := Stream_GetChar(f,c);
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
  ISO8859_Letters : CharSet = [#$C0..#$D6,#$D8..#$F6,#$F8..#$FF];
  CP850_Letters : CharSet = [#$80..#$9B,#$9D,#$A0..#$A5,#$B5..#$B7,
    #$C6,#$C7,#$D0..#$D4,#$D6..#$D8,#$DE,#$E0..#$E5,#$E7..#$ED];
  CP437_Letters : CharSet = [#$80..#$99,#$A0..#$A5,#$E1];
  { identifiers can be unquoted graphic chars; PII+ p48-49 }
  PROLOG_Graphic : CharSet = ['#','$','&','*','+','-','/',':','=','?','\',
    '@','^','~',#$A0..#$BF,#$D7,#$F7];
  EDINBURGH_Graphic: CharSet = [';','<','>',
    '#','$','&','*','+','-','/',':','=','?', '\','@','^','~',
    #$A0..#$BF,#$D7,#$F7];


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
Function GrabOneLetter( f : StreamPtr; Var Ch : StrPtr; 
    Var IsUpper : Boolean ) : Boolean;
Var
  c : TChar;
  Found : Boolean;
  Enc : TEncoding;

  Procedure Accept( Enc : TEncoding );
  Begin
    c := Stream_GetChar(f,c);
    If Error Then Exit;
    Str_AppendChar(Ch,c);
    Stream_SetEncoding(f,Enc);
    GrabOneLetter := True
  End;

Begin
  GrabOneLetter := False;
  c := Stream_NextChar(f,c);
  If Error Then Exit;
  IsUpper := False;
  Enc := Stream_GetEncoding(f);

  { simplest case: 7-bit letter; valid in all encodings }
  Found := (Length(c) = 1) And (c[1] In Letters);
  If Found Then
  Begin
    IsUpper := c[1] In ['A'..'Z'];
    Accept(Enc);
    Exit
  End;

  { UTF8: https://www.utf8-chartable.de/ }
  If Stream_GetEncoding(f) = UTF8 Then
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
  If Stream_GetEncoding(f) In [UNDECIDED,ISO8859] Then
  Begin
    If c[1] In ISO8859_Letters Then
      Accept(ISO8859);
    Exit
  End;

  { CP850 and 858: https://en.wikipedia.org/wiki/Code_page_850#Character_set }
  If Stream_GetEncoding(f) = CP850 Then
  Begin
    If c[1] In CP850_Letters Then
      Accept(CP850);
    Exit
  End;

  { CP437: https://en.wikipedia.org/wiki/Code_page_437#Character_set }
  If Stream_GetEncoding(f) = CP437 Then
  Begin
    If c[1] In CP437_Letters Then
      Accept(CP437);
    Exit
  End
End;

{ grab letters; return the number of letters appended to Ch }
Function GrabLetters( f : StreamPtr; Var Ch : StrPtr ) : LongInt;
Var
  n : LongInt;
  IsUpper : Boolean;
Begin
  GrabLetters := 0;
  n := 0;
  While GrabOneLetter(f,Ch,IsUpper) And (Not Error) Do
    n := n + 1;
  If Error Then Exit;
  GrabLetters := n
End;

{ append to a string any alphanumeric characters (plus underscore); 
 return the number of characters added to the string }
Function GrabAlpha( f : StreamPtr; Var Ch : StrPtr ) : LongInt;
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
    n := n + GetCharWhile(f,Ch,Alpha);
    If Error Then Exit;
    More := GrabOneLetter(f,Ch,IsUpper);
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

{ read a run of chars delimited by a given 1-byte quote char; when the quote  
 char appears within the run, it must be doubled; long lines can be broken up
 using \<NEWLINE>; the result is a token of type tt; this definition is broad 
 enough to encompass double-quoted constant string and single-quoted 
 identifiers; if quiet just return Nil when there is no quoted run of chars  }
{ TODO: escaped chars }
Function ReadQuotedRunOfChars( f : StreamPtr; quote : Char; tt : TTokenType; 
    keep : Boolean; quiet : Boolean ) : TokenPtr;
Var
  K : TokenPtr;
  c : TChar;
  Done : Boolean;
Begin
  ReadQuotedRunOfChars := Nil;
  c := Stream_NextChar(f,c);
  If Error Then Exit;
  If c <> quote Then 
  Begin
    If Not quiet Then
      SyntaxError(quote + ' expected');
    Exit
  End;
  c := Stream_GetChar(f,c);
  If Error Then Exit;
  K := Token_New(tt);
  With K^ Do
  Begin
    TK_STRI := Str_New;
    If keep Then
      Str_Append(TK_STRI,quote);
    Repeat
      Done := False;
      { look for the next char with special meaning inside a string }
      c := GetCharUntil(f,TK_STRI, [quote,'\',NewLine,EndOfInput]);
      If Error Then Exit;
      If c = quote Then { doubled quote or end of run }
      Begin
        c := Stream_GetChar(f,c); { discard it }
        If Error Then Exit;
        c := Stream_NextChar(f,c);
        If Error Then Exit;
        If c = quote Then { doubled quotes }
        Begin
          c := Stream_GetChar(f,c);
          If Error Then Exit;
          Str_AppendChar(TK_STRI,c);
          If keep Then
            Str_Append(TK_STRI,quote) { keep both only if enclosing quotes }
        End
        Else
        Begin
          Done := True;
          If keep Then
            Str_Append(TK_STRI,quote)
        End
      End
      Else If c = '\' Then { backslash or line continuation }
      Begin
        c := Stream_NextNextChar(f,c);
        If Error Then Exit;
        if c = NewLine Then
        Begin
          c := Stream_GetChar(f,c); { discard '\' }
          If Error Then Exit;
          c := Stream_GetChar(f,c)  { discard NewLine } 
        End
        Else
        Begin
          c := Stream_GetChar(f,c);
          If Error Then Exit;
          Str_AppendChar(TK_STRI,c) { append '\' }
        End
      End
      Else If quiet Then 
        Exit 
      Else If c = NewLine Then
        SyntaxError('end of line while reading ' + TokenStr[tt])
      Else If c = EndOfInput Then
        SyntaxError('end of input while reading ' + TokenStr[tt])
    Until Error or Done;
    If Error Then Exit
  End;
  ReadQuotedRunOfChars := K
End;

{ read a double quoted string }
Function ReadString( f : StreamPtr ) : TokenPtr;
Begin
  ReadString := ReadQuotedRunOfChars(f,'"',TOKEN_STRING,False,True)
End;

{ skip a *-style comment, with symbol '/' or '|'; *-style comments can be 
  nested, e.g. "/* ... |* .... *| .... */" (even with the same symbol) }
Procedure ReadComment( f : StreamPtr; symbol : TChar );
Var
  c,c2 : TChar;
  Stop : Boolean;
Begin
  Verify(f,symbol + '*');
  If Error Then Exit;
  Repeat
    Stop := False;
    Repeat
      c := Stream_GetChar(f,c)
    Until Error Or (c = '*') Or (c = '/') Or (c = '|') Or (c = EndOfInput);
    If Error Then Exit;
    If c = '*' Then
    Begin
      c := Stream_NextChar(f,c);
      If Error Then Exit;
      If c = symbol Then
      Begin
        c := Stream_GetChar(f,c);
        If Error Then Exit;
        Stop := true
      End
    End
    Else If (c = '/') Or (c = '|') Then
    Begin
      c2 := Stream_NextChar(f,c2);
      If Error Then Exit;
      If c2 = '*' Then
      Begin
        Stream_UngetChar(f);
        ReadComment(f,c);
        If Error Then Exit
      End
    End
    Else
      SyntaxError('end of input within a comment')
  Until Stop Or Error
End;

{ skip a (possibly empty) sequence of blank characters }
Procedure ReadBlanks( f : StreamPtr );
Var
  n : LongInt;
Begin
  n := GetCharWhile(f,Str_New,[' '])
End;

{ skip any sequence of blank spaces and comments }
Procedure ReadSpaces( f : StreamPtr; y : TSyntax );
Var
  c,c2 : TChar;
  Stop : Boolean;
Begin
  Repeat
    Stop := True;
    c := NextCharNb(f,c);
    If Error Then Exit;
    c2 := Stream_NextNextChar(f,c2);
    If Error Then Exit;
    If y In [PrologIIp,Edinburgh] Then
    Begin
      If c = '%' Then
      Begin
        Repeat
          c := Stream_GetChar(f,c)
        Until Error Or (c = NewLine) Or (c = EndOfInput); 
        If Error Then Exit;
        If c = EndOfInput Then
          Stream_UngetChar(f);
        Stop := False
      End
      Else If ((c = '/') Or (c = '|')) And (c2 = '*') Then
      Begin
        ReadComment(f,c);
        If Error Then Exit;
        Stop := False
      End
    End
  Until Stop Or Error
End;

{ read an integer or return Nil }
Function ReadInteger( f : StreamPtr ) : TokenPtr;
Var
  K : TokenPtr;
  n : LongInt;
Begin
  ReadInteger := Nil;
  K := Token_New(TOKEN_INTEGER);
  With K^ Do
  Begin
    TK_STRI := Str_New;
    n := GetCharWhile(f,TK_STRI,Digits);
    If n = 0 Then
      Exit
  End;
  ReadInteger := K
End;

{ read a number and return its canonical string representation;
 if syntax is not Edinburgh, then real numbers must have an explicit exponent (e.g.
 1.2e+3); See footnote 3 p.47; 
 otherwise, "1.2" would be ambiguous, as it 
 could also a list as well as a real number; note that expressions like 
 "10e+3" remain ambiguous, because the syntax states that the possibly signed
 integer number after "E" is optional; in that case we assume the "+3" is part
 of the real number, and not an addition; note that input like "1.2e+3.4e" 
 remain difficult to parse, as "+3" should be attributed to the second real
 number, realizing very late that the second dot cannot be a dot list 
 operator;  }
Function ReadNumber( f : StreamPtr; y : TSyntax ) : TokenPtr;
Var
  K : TokenPtr;
  e1,e2 : TIChar; { undo points }
  c : TChar;
  n : LongInt;
  s,s2 : StrPtr;
  Stop : Boolean; { stop parsing }
  ListDot : Boolean; { not a real: we must unread all chars from the dot }
Begin
  ReadNumber := Nil;
  { integer part }
  K := ReadInteger(f);
  CheckCondition(K <> Nil,'Number expected');
  If Error Then Exit;
  { optional real part }
  With K^ Do
  Begin
    s := Str_New;
    c := Stream_NextChar(f,c);
    If c = '.' Then
    Begin
      Stream_GetIChar(f,e1); { undo point: the dot char }
      Str_Append(s,'.');
      n := GetCharWhile(f,s,Digits);
      ListDot := n = 0; { no digits after the dot? the dot was part of a list }
      Stop := ListDot;
      If Not Stop Then
      Begin
        { optional exponent sign }
        c := Stream_NextChar(f,c);
        Stop := (c <> 'E') And (c <> 'e') And (c <> 'D') And (c <> 'd');
        If Stop Then
          ListDot := y <> Edinburgh { exponent are optional in Edinburgh }
        Else
        Begin
          c := Stream_GetChar(f,c); { grab the exponent mark }
          Str_Append(s,'E')
        End;
        If Not Stop Then { we had the exponent mark, now look for its value }
        Begin
          { optional exponent value: "+3" in "1.2e+3" or "3" in "1.2e3";
           "1.2e" is valid syntax, so "1.2e+X" is read as an addition, and in
           that case, the "+" will have to be unread }
          c := Stream_NextChar(f,c);
          If (c = '-') Or (c = '+') Then
          Begin
            Stream_GetIChar(f,e2); { another undo point: the exp sign }
            s2 := Str_New;
            Str_AppendChar(s2,c);
            n := GetCharWhile(f,s2,Digits);
            If n > 0 Then
              Str_Concat(s,s2)
            Else
              Stream_UngetChars(f,e2.Lnb,e2.Pos) { sign may be binary op }
          End
          Else
          Begin
            n := GetCharWhile(f,s,Digits);
            If n = 0 Then
              Str_Append(s,'0') { as Pascal's Val/2 cannot convert "1.2e" }
          End
        End
      End;
      If Not ListDot Then 
      Begin
        TK_TYPE := TOKEN_REAL;
        Str_Concat(TK_STRI,s)
      End
      Else
        Stream_UngetChars(f,e1.Lnb,e1.Pos) { no real part: undo }
    End
  End;
  ReadNumber := K
End;

{ create a token of type typ while checking a given string of 1-byte
 chars is in input }
Function GrabToken( f : StreamPtr; typ : TTokenType; s : TString ) : TokenPtr;
Var
  K : TokenPtr;
Begin
  GrabToken := Nil;
  K := Nil;
  Verify(f,s);
  If Error Then Exit;
  K := Token_New(typ);
  GrabToken := K
End;

{ read a variable or an identifier (including single-quoted identifier) }
Function ReadVariableOrIdentifier( f : StreamPtr; y : TSyntax ) : TokenPtr;
Var
  K : TokenPtr;
  c : TChar;
  IsUpper : Boolean;
  n,m : LongInt;
Begin 
  ReadVariableOrIdentifier := Nil;
  { single-quoted identifier? }
  K := ReadQuotedRunOfChars(f,'''',TOKEN_IDENT,True,True);
  If K = Nil Then { nope }
  Begin
    K := Token_New(TOKEN_UNKNOWN);
    With K^ Do
    Begin
      TK_STRI := Str_New;
      If GrabOneLetter(f,TK_STRI,IsUpper) Then
      Begin
        { Edinburgh variables start with a "big letter"; Marseille variables 
          start with a single letter }
        If y = Edinburgh Then
          If IsUpper Then
            TK_TYPE := TOKEN_VARIABLE
          Else
            TK_TYPE := TOKEN_IDENT
        Else
          If Not GrabOneLetter(f,TK_STRI,IsUpper) Then
            TK_TYPE := TOKEN_VARIABLE
          Else
            TK_TYPE := TOKEN_IDENT;

        If Error Then Exit;
        If y In [PrologIIc,PrologII] Then { old Prolog II syntax, w/ accented letters }
        Begin
          Repeat
            c := Stream_NextChar(f,c);
            If Error Then Exit;
            If c = '-' Then { "-"<word> continuation }
            Begin
              c := Stream_GetChar(f,c);
              If Error Then Exit;
              Str_AppendChar(TK_STRI,c)
            End;
            n := GrabLetters(f,TK_STRI);
            If Error Then Exit;
            m := GetCharWhile(f,TK_STRI,Digits);
            If Error Then Exit;
            m := GetCharWhile(f,TK_STRI,['''']);
            If Error Then Exit;
            If (c = '-') And (n = 0) Then
              SyntaxError('dash must be followed by a letter');
            If Error Then Exit;
            c := Stream_NextChar(f,c);
            If Error Then Exit
          Until (c <> '-') Or Error;
        End
        Else { PrologII+ and Edinburgh extended syntax }
        Begin
          n := GrabAlpha(f,TK_STRI);
          If Error Then Exit;
          If y <> Edinburgh Then
            n := GetCharWhile(f,TK_STRI,[''''])
        End
      End
      Else
        K := Nil { not a letter: error }
    End
  End;
  ReadVariableOrIdentifier := K
End;

{ read a token, following syntax y }
{ NOTE *: about Edinburgh identifiers made of graphic_char: the dot '.' is not
 a graphic char. However, =.. is a predefined operator that can be used 
 *unquoted*. Rule 18 p48 allows to have dots after the second graphic char, but
 the doc says it is a PrologII+-only rule. Weird. Is it a typo? s}
Function ReadToken( f : StreamPtr; y : TSyntax ) : TokenPtr;
Var
  K : TokenPtr;
  e : TIChar;
  c,c2 : TChar;
  n : LongInt;
  GraphicChars : CharSet;
Begin
  ReadToken := Nil;
  K := Nil;
  { save the next char for undo purpose; this is the char of the token but
   *including leading blank spaces*; this is needed to "unread" tokens, which 
   is required to correctly handle in_char(c) goals }
  Stream_NextIChar(f,e);
  { from PII+doc p28: "spaces can be inserted anywhere except inside constants 
   and variables"}
  ReadSpaces(f,y);
  If Error Then Exit;
  c := Stream_NextChar(f,c);
  If Error Then Exit;
  c2 := Stream_NextNextChar(f,c2);
  If Error Then Exit;
  { arrow must be checked before identifiers made of graphic chars }
  If (c = '-') And (c2 = '>') And (y <> Edinburgh) Then { '->' }
    K := GrabToken(f,TOKEN_ARROW,'->')
  Else If (c = ':') And (c2 = '-') And (y = Edinburgh) Then { ':-' }
    K := GrabToken(f,TOKEN_ARROW,':-')
  { identifiers made of graphic chars }
  Else If (c[1] In PROLOG_Graphic) And (y = PrologIIp) Or 
      (c[1] In EDINBURGH_Graphic) And (y = Edinburgh) Then
  Begin
    If y = PrologIIp Then
      GraphicChars := PROLOG_Graphic
    Else
      GraphicChars := EDINBURGH_Graphic + ['.']; { allows =.. unquoted }
    K := Token_New(TOKEN_IDENT);
    With K^ Do
    Begin
      TK_STRI := Str_New;
      n := GetCharWhile(f,TK_STRI,GraphicChars)
    End
  End
  Else Case c[1] Of
  EndOfInput:
    K := Token_New(TOKEN_END_OF_INPUT);
  '0'..'9':
    K := ReadNumber(f,y); 
  '_':
    If y In [PrologIIp,Edinburgh] Then  { a variable: PrologII+ basic syntax }
    Begin
      K := Token_New(TOKEN_VARIABLE);
      With K^ Do
      Begin
        TK_STRI := Str_New;
        n := GrabAlpha(f,TK_STRI) { letters (inc. accented), digits, underscore }
      End
    End;
  '"':
    K := ReadQuotedRunOfChars(f,'"',TOKEN_STRING,False,False);
  '!':
    If y In [PrologIIp,Edinburgh] Then
      K := GrabToken(f,TOKEN_CUT,c);
  '/':
    If y In [PrologIIc,PrologII] Then
      K := GrabToken(f,TOKEN_CUT,c);
  ';':
    K := GrabToken(f,TOKEN_SEMICOLON,c);
  '(':
    K := GrabToken(f,TOKEN_LEFT_PAR,c);
  ')':
    K := GrabToken(f,TOKEN_RIGHT_PAR,c);
  '{':
    K := GrabToken(f,TOKEN_LEFT_CUR,c);
  '}':
    K := GrabToken(f,TOKEN_RIGHT_CUR,c);
  '<':
    K := GrabToken(f,TOKEN_LEFT_CHE,c);
  '>':
    K := GrabToken(f,TOKEN_RIGHT_CHE,c);
  '[':
    K := GrabToken(f,TOKEN_LEFT_BRA,c);
  ']':
    K := GrabToken(f,TOKEN_RIGHT_BRA,c);
  '.':
    K := GrabToken(f,TOKEN_DOT,c);
  ',':
    K := GrabToken(f,TOKEN_COMMA,c);
  '=':
    If y In [PrologIIc,PrologII] Then
      K := GrabToken(f,TOKEN_EQUAL,c);
  '#':
    If y In [PrologIIc,PrologII] Then
      K := GrabToken(f,TOKEN_DIFF,c);
  '|':
    K := GrabToken(f,TOKEN_PIPE,c);
  Else
    K := ReadVariableOrIdentifier(f,y)
  End;

  If Error Then Exit;
  If K = Nil Then
  Begin
    c := Stream_GetChar(f,c); { read the char so error message points to it }
    If Error Then Exit;
    SyntaxError('that character is not allowed here')
  End;
  If Error Then Exit;
  Token_SetLocation(K,e.Lnb,e.Pos);
  ReadToken := K
End;

End.