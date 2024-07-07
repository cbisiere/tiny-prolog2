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
  Common,
  Errs,
  ShortStr,
  Chars,
  IChar,
  PObjIO,
  PObjStr,
  PObjTok,
  PObjDef;

Function IsUnquotedIdentifier( s : StrPtr; y : TSyntax ) : Boolean;

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
  cc : TChar;
  n : LongInt;
  Found : Boolean;
Begin
  GetCharWhile := 0;
  n := 0;
  Repeat
    Stream_GetChar(f,cc);
    If Error Then Exit;
    Found := cc.Bytes[1] In E;
    If Not Found Then
      Stream_UngetChar(f)
    Else
    Begin
      Str_AppendChar(Ch,cc);
      n := n + 1
    End
  Until Not Found Or Error;
  GetCharWhile := n
End;

{ append TChars to a string until a TChar is in a certain set; 
 return that TChar or set an Error if the char is not found }
Procedure GetCharUntil( f : StreamPtr; Ch : StrPtr; E : CharSet; Var cc : TChar );
Var 
  Found : Boolean;
Begin
  Repeat
    Stream_GetChar(f,cc);
    If Error Then Exit;
    Found := IsIn(cc,E);
    If Found Then
      Stream_UngetChar(f)
    Else
      Str_AppendChar(Ch,cc)
  Until Found Or Error
End;

{ return the next non-blank TChar without consuming it }
Procedure NextCharNb( f : StreamPtr; Var c : TChar );
Begin
  Stream_GetCharNb(f,c);
  If Error Then Exit;
  Stream_UngetChar(f)
End;

{ verify that TChar cc is next in input }
Procedure Verify( f : StreamPtr; cc : TChar );
Var
  c  : TChar;
Begin
  Stream_GetChar(f,c);
  If Error Then Exit;
  If c.Bytes <> cc.Bytes Then 
    SyntaxError('"' + cc.Bytes + '" expected');
End;


{----------------------------------------------------------------------------}
{ encoding-dependant entities                                                }
{----------------------------------------------------------------------------}

{ chars and charsets;

 "letter" is as defined on page R1-2 of the Prolog II+ manual:
 "A"|...|"Z"|"a"|...|"z"|"À" ... "ß" - "x" | "à" ... "ÿ" - "÷"
 where x is the multiplication sign, and ordering is ISO 8859.

assumptions and detection policy regarding encoding:
 (1) it is assumed that in all encodings, codepoints for a-z letters, digits and 
  underscore are as in 7-bit ASCII; 
  we rely on this when testing for a letter when the encoding of the input 
  stream is still unknown
 (2) it is assumed that, if the input is ISO-8859-1, there is no "Ã" ($C3) 
  followed by a char that would make the 2-byte sequence an UTF8 letter; 
  hopefully 80-9F are undefined in ISO-8859-1; A0-BF is possible but none of 
  those combinations (e.g. Ã¢) would be a valid part of an identifier, so 
  either the input is UTF8 and there is a syntax error, or the input is UTF8, 
  which we therefore assume;
 (3) when testing for a letter, if the encoding is undecided and the character 
  is an ISO8859 letter, then the whole input is assumed to be ISO8859; this 
  policy recognizes ISO8859 as more "likely" than other 1-byte encodings 

 References:
 UTF8: 
  https://www.utf8-chartable.de/
 ISO-8859-1: 
  https://fr.wikipedia.org/wiki/ISO/CEI_8859-1
 CP850 and 858: 
  https://en.wikipedia.org/wiki/Code_page_850#Character_set 
 CP437: 
  https://en.wikipedia.org/wiki/Code_page_437#Character_set }

 Const
  Letters : CharSet = ['a'..'z','A'..'Z']; { 7-bit ASCII letters }
  Alpha : CharSet = ['a'..'z','A'..'Z','_','0'..'9']; { 7-bit ASCII alpha }
  Digits  : CharSet = ['0'..'9']; { digits }
  ISO8859_Letters : CharSet = ['a'..'z','A'..'Z',
    #$C0..#$D6,#$D8..#$F6,#$F8..#$FF];
  CP850_Letters : CharSet = ['a'..'z','A'..'Z',
    #$80..#$9B,#$9D,#$A0..#$A5,#$B5..#$B7,
    #$C6,#$C7,#$D0..#$D4,#$D6..#$D8,#$DE,#$E0..#$E5,#$E7..#$ED];
  CP437_Letters : CharSet = ['a'..'z','A'..'Z',
    #$80..#$99,#$A0..#$A5,#$E1];
  { identifiers can be unquoted graphic chars; PII+ p48-49 }
  PROLOG_Graphic : CharSet = ['#','$','&','*','+','-','/',':','=','?','\',
    '@','^','~',#$A0..#$BF,#$D7,#$F7];
  EDINBURGH_Graphic: CharSet = [';','<','>',
    '#','$','&','*','+','-','/',':','=','?', '\','@','^','~',
    #$A0..#$BF,#$D7,#$F7];


{ is TChar cc is a letter in UTF8? }
Function IsUTF8Letter( cc : TChar ) : Boolean;
Begin
  IsUTF8Letter := IsIn(cc,Letters) Or (Length(cc.Bytes) = 2) And 
    (cc.Bytes[1] = #$C3) And (cc.Bytes[2] In ([#$80..#$BF] - [#$97,#$B7]))
End;

{ is TChar cc is a letter in ISO8859? }
Function IsISO8859Letter( cc : TChar ) : Boolean;
Begin
  IsISO8859Letter := IsIn(cc,ISO8859_Letters)
End;

{ is TChar cc is a letter in CP850? }
Function IsCP850Letter( cc : TChar ) : Boolean;
Begin
  IsCP850Letter := IsIn(cc,CP850_Letters)
End;

{ is TChar cc is a letter in CP437? }
Function IsCP437Letter( cc : TChar ) : Boolean;
Begin
  IsCP437Letter := IsIn(cc,CP437_Letters)
End;

{ is TChar cc known to be a letter? }
Function IsLetter( cc : TChar ) : Boolean;
Begin
  Case cc.Encoding Of 
  UTF8:
    IsLetter := IsUTF8Letter(cc);
  ISO8859:
    IsLetter := IsISO8859Letter(cc);
  CP850:
    IsLetter := IsCP850Letter(cc);
  CP437:
    IsLetter := IsCP437Letter(cc);
  Else
    IsLetter := IsIn(cc,Letters)
  End
End;

{ is TChar cc known to be a big letter?
  "big letters" are A-Z only, see PrologII+ manual, R 1-23 }
Function IsBigLetter( cc : TChar ) : Boolean;
Begin
  IsBigLetter := IsIn(cc,['A'..'Z'])
End;

{ is TChar cc known to be a digit in encoding Enc? }
Function IsDigit( cc : TChar ) : Boolean;
Begin
  IsDigit := IsIn(cc,Digits)
End;

{ is TChar cc known to be an alphanumeric character in encoding Enc? }
Function IsAlpha( cc : TChar ) : Boolean;
Begin
  IsAlpha := IsLetter(cc) Or IsDigit(cc) Or (cc.Bytes = '_')
End;

{----------------------------------------------------------------------------}
{ grabbing characters of different classes                                   }
{----------------------------------------------------------------------------}

{ grab one letter from stream f, and, if any, detect uppercase; also, try to 
 set the stream encoding if it is not set yet }
Function GrabOneLetter( f : StreamPtr; Var Ch : StrPtr; 
    Var IsUpper : Boolean ) : Boolean;
Var
  cc : TChar;
  Enc : TEncoding;

  { cc is a letter and its encoding is Enc }
  Procedure Accept( Enc : TEncoding );
  Begin
    Stream_GetChar(f,cc);
    If Error Then Exit;
    Str_AppendChar(Ch,cc);
    Stream_SetEncoding(f,Enc);
    GrabOneLetter := True
  End;

Begin
  GrabOneLetter := False;
  Stream_NextChar(f,cc);
  If Error Then Exit;

  Enc := Stream_GetEncoding(f);
  IsUpper := IsBigLetter(cc);

  If IsLetter(cc) Then
    Accept(Enc)
  Else If (Enc = UNDECIDED) And IsISO8859Letter(cc) Then { policy }
    Accept(ISO8859)
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
  Stream_NextChar(f,c);
  If Error Then Exit;
  If c.Bytes <> quote Then 
  Begin
    If Not quiet Then
      SyntaxError(quote + ' expected');
    Exit
  End;
  Stream_GetChar(f,c);
  If Error Then Exit;
  K := Token_New(tt);
  With K^ Do
  Begin
    TK_STRI := Stream_NewStr(f);
    TK_QUOT := True;
    If keep Then
      Str_Append(TK_STRI,quote);
    Done := False;
    Repeat
      { look for the next char with special meaning inside a string }
      GetCharUntil(f,TK_STRI,[quote,'\',NewLine,EndOfInput],c);
      If Error Then Exit;
      If c.Bytes = quote Then { doubled quote or end of run }
      Begin
        Stream_GetChar(f,c); { discard it }
        If Error Then Exit;
        Stream_NextChar(f,c);
        If Error Then Exit;
        If c.Bytes = quote Then { doubled quotes }
        Begin
          Stream_GetChar(f,c);
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
      Else If c.Bytes = '\' Then { backslash or line continuation }
      Begin
        Stream_NextNextChar(f,c);
        If Error Then Exit;
        if c.Bytes = NewLine Then
        Begin
          Stream_GetChar(f,c); { discard '\' }
          If Error Then Exit;
          Stream_GetChar(f,c)  { discard NewLine } 
        End
        Else
        Begin
          Stream_GetChar(f,c);
          If Error Then Exit;
          Str_AppendChar(TK_STRI,c) { append '\' }
        End
      End
      Else If quiet Then 
        Exit 
      Else If c.Bytes = NewLine Then
        SyntaxError('end of line while reading ' + TokenStr[tt])
      Else If c.Bytes = EndOfInput Then
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
  c0,c,c2 : TChar;
  Stop : Boolean;
Begin
  Verify(f,symbol);
  If Error Then Exit;
  ASCIIChar(c0,'*');
  Verify(f,c0);
  If Error Then Exit;
  Repeat
    Stop := False;
    Repeat
      Stream_GetChar(f,c)
    Until Error Or IsIn(c,['*','/','|',EndOfInput]);
    If Error Then Exit;
    If c.Bytes = '*' Then
    Begin
      Stream_NextChar(f,c);
      If Error Then Exit;
      If c.Bytes = symbol.Bytes Then
      Begin
        Stream_GetChar(f,c);
        If Error Then Exit;
        Stop := true
      End
    End
    Else If IsIn(c,['/','|']) Then
    Begin
      Stream_NextChar(f,c2);
      If Error Then Exit;
      If c2.Bytes = '*' Then
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
  n := GetCharWhile(f,Stream_NewStr(f),[' '])
End;

{ skip any sequence of blank spaces and comments }
Procedure ReadSpaces( f : StreamPtr; y : TSyntax );
Var
  c,c2 : TChar;
  Stop : Boolean;
Begin
  Repeat
    Stop := True;
    NextCharNb(f,c);
    If Error Then Exit;
    Stream_NextNextChar(f,c2);
    If Error Then Exit;
    If y In [PrologIIp,Edinburgh] Then
    Begin
      If c.Bytes = '%' Then
      Begin
        Repeat
          Stream_GetChar(f,c)
        Until Error Or IsIn(c,[NewLine,EndOfInput]); 
        If Error Then Exit;
        If c.Bytes = EndOfInput Then
          Stream_UngetChar(f);
        Stop := False
      End
      Else If IsIn(c,['/','|']) And (c2.Bytes = '*') Then
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
    TK_STRI := Stream_NewStr(f);
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
    s := Stream_NewStr(f);
    Stream_NextChar(f,c);
    If Error Then Exit;
    If c.Bytes = '.' Then
    Begin
      Stream_GetIChar(f,e1); { undo point: the dot char }
      Str_Append(s,'.');
      n := GetCharWhile(f,s,Digits);
      ListDot := n = 0; { no digits after the dot? the dot was part of a list }
      Stop := ListDot;
      If Not Stop Then
      Begin
        { optional exponent sign }
        Stream_NextChar(f,c);
        If Error Then Exit;
        Stop := Not IsIn(c,['E','e','D','d']);
        If Stop Then
          ListDot := y <> Edinburgh { exponent are optional in Edinburgh }
        Else
        Begin
          Stream_GetChar(f,c); { grab the exponent mark }
          Str_Append(s,'E')
        End;
        If Not Stop Then { we had the exponent mark, now look for its value }
        Begin
          { optional exponent value: "+3" in "1.2e+3" or "3" in "1.2e3";
           "1.2e" is valid syntax, so "1.2e+X" is read as an addition, and in
           that case, the "+" will have to be unread }
          Stream_NextChar(f,c);
          If Error Then Exit;
          If IsIn(c,['-','+']) Then
          Begin
            Stream_GetIChar(f,e2); { another undo point: the exp sign }
            s2 := Stream_NewStr(f);
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

{ create a token of type typ while checking a given TChar }
Function GrabToken( f : StreamPtr; typ : TTokenType; cc : TChar ) : TokenPtr;
Var
  K : TokenPtr;
Begin
  GrabToken := Nil;
  K := Nil;
  Verify(f,cc);
  If Error Then Exit;
  K := Token_New(typ);
  GrabToken := K
End;

{ return True if string s is a valid (unquoted) identifier in syntax y;
 TODO: what about identifiers made of graphic chars }
Function IsUnquotedIdentifier( s : StrPtr; y : TSyntax ) : Boolean;
Var 
  State : Byte;
  Iter : StrIter;
  cc : TChar;
Begin
  IsUnquotedIdentifier := False;
  If Str_Length(s) = 0 Then
    Exit;
  State := 0;
  StrIter_ToStart(Iter,s);
  While StrIter_NextChar(Iter,cc) Do
  Begin
    Case y Of
    Edinburgh:
      Case State Of
      0:
        If IsLetter(cc) And Not IsBigLetter(cc) Then
          State := 1
        Else
          Exit;
      1: { one small letter }
        If IsAlpha(cc) Then
          Pass
        Else
          Exit
      End;
    PrologIIp:
      Case State Of
      0:
        If IsLetter(cc) Then
          State := 1
        Else
          Exit;
      1: { one letter }
        If IsLetter(cc) Then
          State := 2
        Else
          Exit;
      2: { two letters }
        If IsAlpha(cc) Then
          Pass
        Else If cc.Bytes = '''' Then
          State := 3
        Else
          Exit;
      3: { quotes }
        If cc.Bytes = '''' Then
          Pass
        Else
          Exit
      End;
    PrologII,PrologIIc:
      Case State Of
      0:
        If IsLetter(cc) Then
          State := 1
        Else
          Exit;
      1: { one letter in a long word }
        If IsLetter(cc) Then
          State := 2
        Else
          Exit;
      2: { a letter in a word }
        If IsLetter(cc) Then
          Pass
        Else If cc.Bytes = '-' Then
          State := 3
        Else If IsDigit(cc) Then
          State := 4
        Else If cc.Bytes = '''' Then
          State := 5
        Else
          Exit;
      3: { dash }
        If IsLetter(cc) Then
          State := 2
        Else
          Exit;
      4: { digits }
        If IsDigit(cc) Then
          Pass
        Else If cc.Bytes = '-' Then
          State := 3
        Else If cc.Bytes = '''' Then
          State := 5
        Else
          Exit;
      5: { quotes }
        If cc.Bytes = '''' Then
          Pass
        Else If cc.Bytes = '-' Then
          State := 3
        Else
          Exit
      End
    End
  End;
  IsUnquotedIdentifier := True
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
  { single-quoted identifier? note that surrounding quotes are NOT kept }
  K := ReadQuotedRunOfChars(f,'''',TOKEN_IDENT,False,True);
  If K = Nil Then { nope }
  Begin
    K := Token_New(TOKEN_UNKNOWN);
    With K^ Do
    Begin
      TK_STRI := Stream_NewStr(f);
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
            Stream_NextChar(f,c);
            If Error Then Exit;
            If c.Bytes = '-' Then { "-"<word> continuation }
            Begin
              Stream_GetChar(f,c);
              If Error Then Exit;
              Str_AppendChar(TK_STRI,c)
            End;
            n := GrabLetters(f,TK_STRI);
            If Error Then Exit;
            m := GetCharWhile(f,TK_STRI,Digits);
            If Error Then Exit;
            m := GetCharWhile(f,TK_STRI,['''']);
            If Error Then Exit;
            If (c.Bytes = '-') And (n = 0) Then
              SyntaxError('dash must be followed by a letter');
            If Error Then Exit;
            Stream_NextChar(f,c);
            If Error Then Exit
          Until (c.Bytes <> '-') Or Error;
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
  Stream_NextChar(f,c);
  If Error Then Exit;
  Stream_NextNextChar(f,c2);
  If Error Then Exit;
  { arrow must be checked before identifiers made of graphic chars }
  If (c.Bytes = '-') And (c2.Bytes = '>') And (y <> Edinburgh) Or
      (c.Bytes = ':') And (c2.Bytes = '-') And (y = Edinburgh) Then
  Begin
    Stream_GetChar(f,c);
    If Error Then Exit;
    Stream_GetChar(f,c2);
    If Error Then Exit;
    K := Token_New(TOKEN_ARROW)
  End
  { identifiers made of graphic chars; FIXME: move this to ReadVariableOrIdentifier? }
  Else If IsIn(c,PROLOG_Graphic) And (y = PrologIIp) Or 
      IsIn(c,EDINBURGH_Graphic) And (y = Edinburgh) Then
  Begin
    If y = PrologIIp Then
      GraphicChars := PROLOG_Graphic
    Else
      GraphicChars := EDINBURGH_Graphic + ['.']; { allows =.. unquoted }
    K := Token_New(TOKEN_IDENT);
    With K^ Do
    Begin
      TK_STRI := Stream_NewStr(f);
      n := GetCharWhile(f,TK_STRI,GraphicChars)
    End
  End
  Else Case c.Bytes[1] Of
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
        TK_STRI := Stream_NewStr(f);
        n := GrabAlpha(f,TK_STRI); { letters (inc. accented), digits, underscore }
        Token_SetAnonymous(K,n = 1) { anonymous: '_'}
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
    Stream_GetChar(f,c); { read the char so error message points to it }
    If Error Then Exit;
    SyntaxError('that character is not allowed here')
  End;
  If Error Then Exit;
  Token_SetLocation(K,e.Lnb,e.Pos);
  ReadToken := K
End;

End.