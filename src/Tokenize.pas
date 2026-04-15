{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Tokenize.pas                                               }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022-2026                                                  }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                              T O K E N I Z E R                             }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }

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

Function GrabLetters( f : StreamPtr; Var Ch : StrPtr ) : TStrLength;
Function GrabDigits( f : StreamPtr; Var Ch : StrPtr ) : TStrLength;

Function IsValidUnquotedIdentifier( s : StrPtr; y : TSyntax ) : Boolean;
Function SupportsQuotedIdentifiers( y : TSyntax ) : Boolean;

Procedure ReadBlanks( f : StreamPtr );
Function ReadString( f : StreamPtr ) : TokenPtr;
Function ReadInteger( f : StreamPtr; Sign : Boolean ) : TokenPtr;
Function ReadNumber( f : StreamPtr; y : TSyntax; Sign : Boolean ) : TokenPtr;
Function ReadVariableOrIdentifier( f : StreamPtr; y : TSyntax ) : TokenPtr;
Function ReadToken( f : StreamPtr; y : TSyntax ) : TokenPtr;

Implementation
{-----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ higher-level primitives for characters                                     }
{----------------------------------------------------------------------------}

{ append a character to a string }
Procedure Str_AppendIChar( Ch : StrPtr; e : TIChar );
Begin
  Str_AppendChar(Ch,e.Val)
End;

{ append characters to a string while they belong to a certain set; return 
 the number of characters appended }
Function GetCharWhile( f : StreamPtr; Ch : StrPtr; EE : CharSet ) : TStrLength;
Var 
  e : TIChar;
  n : TStrLength;
  InSet : Boolean;
Begin
  GetCharWhile := 0;
  n := 0;
  Repeat
    Stream_GetChar(f,e);
    If Error Then Exit;
    InSet := TICharIsIn(e,EE);
    If Not InSet Then
      Stream_UngetChars(f,e)
    Else
    Begin
      Str_AppendIChar(Ch,e);
      n := n + 1
    End
  Until Not InSet Or Error;
  GetCharWhile := n
End;

{ append characters to a string until a char is in a certain set; always stop on
 end-of-line or end-of-input; return the char on which we stopped; that char is
 put back in the stream's buffer }
Procedure GetCharUntil( f : StreamPtr; Ch : StrPtr; EE : CharSet; 
    Var e : TIChar );
Var 
  Stop : Boolean;
Begin
  Repeat
    Stream_GetChar(f,e);
    If Error Then Exit;
    Stop := TICharIsEndOfInput(e) Or TICharIsEol(e) Or TICharIsIn(e,EE);
    If Stop Then
      Stream_UngetChars(f,e)
    Else
      Str_AppendIChar(Ch,e)
  Until Stop Or Error
End;

{ return the next non-blank character without consuming it }
Procedure NextCharNb( f : StreamPtr; Var e : TIChar );
Begin
  Stream_GetCharNb(f,e);
  If Error Then Exit;
  Stream_UngetChars(f,e)
End;

{ verify that TChar cc is next in input }
Procedure Verify( f : StreamPtr; cc : TChar );
Var
  e  : TIChar;
Begin
  Stream_GetChar(f,e);
  If Error Then Exit;
  If Not TICharIs(e,TCharGetBytes(cc)) Then 
    SyntaxError('"' + TCharGetBytes(cc) + '" expected');
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
 ENC_UTF8: 
  https://www.utf8-chartable.de/
 ISO-8859-1: 
  https://fr.wikipedia.org/wiki/ISO/CEI_8859-1
 CP850 and 858: 
  https://en.wikipedia.org/wiki/Code_page_850#Character_set 
 ENC_CP437: 
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
Function IsUTF8Letter( e : TIChar ) : Boolean;
Begin
  IsUTF8Letter := TICharIsIn(e,Letters) Or 
      (Not TICharIsSoftMark(e)) And (TICharGetLength(e) = 2) And 
      (TICharGetByte(e,1) = #$C3) And 
      (TICharGetByte(e,2) In ([#$80..#$BF] - [#$97,#$B7]))
End;

{ is TChar cc is a letter in ISO8859? }
Function IsISO8859Letter( e : TIChar ) : Boolean;
Begin
  IsISO8859Letter := TICharIsIn(e,ISO8859_Letters)
End;

{ is TChar cc is a letter in CP850? }
Function IsCP850Letter( e : TIChar ) : Boolean;
Begin
  IsCP850Letter := TICharIsIn(e,CP850_Letters)
End;

{ is TChar cc is a letter in CP437? }
Function IsCP437Letter( e : TIChar ) : Boolean;
Begin
  IsCP437Letter := TICharIsIn(e,CP437_Letters)
End;

{ is TChar cc known to be a letter? }
Function IsLetter( e : TIChar ) : Boolean;
Begin
  If TICharIsSoftMark(e) Then
    IsLetter := False
  Else
  Begin
    Case TICharGetEncoding(e) Of 
    ENC_UTF8:
      IsLetter := IsUTF8Letter(e);
    ENC_ISO8859:
      IsLetter := IsISO8859Letter(e);
    ENC_CP850:
      IsLetter := IsCP850Letter(e);
    ENC_CP437:
      IsLetter := IsCP437Letter(e);
    Else
      IsLetter := TICharIsIn(e,Letters)
    End
  End
End;

{ is character e known to be a big letter?
  "big letters" are A-Z only, see PrologII+ manual, R 1-23 }
Function IsBigLetter( e : TIChar ) : Boolean;
Begin
  IsBigLetter := TICharIsIn(e,['A'..'Z'])
End;

{ is TChar cc known to be a digit in encoding Enc? }
Function IsDigit( e : TIChar ) : Boolean;
Begin
  IsDigit := TICharIsIn(e,Digits)
End;

{ is TChar cc known to be an alphanumeric character in encoding Enc? }
Function IsAlpha( e : TIChar ) : Boolean;
Begin
  IsAlpha := IsLetter(e) Or IsDigit(e) Or TICharIs(e,'_')
End;

{ is TChar cc an exponential symbol in syntax y? }
Function IsExpSymbol( e : TIChar; y : TSyntax ) : Boolean;
Begin
  IsExpSymbol := TICharIsIn(e,['E','e']) Or 
    ((y In [PrologIIp,Edinburgh]) And TICharIsIn(e,['D','d']))
End;


{----------------------------------------------------------------------------}
{ grabbing characters of different classes                                   }
{----------------------------------------------------------------------------}

{ grab one letter from stream f, and, if any, detect uppercase; also, try to 
 set the stream encoding if it is not set yet }
Function GrabOneLetter( f : StreamPtr; Var Ch : StrPtr; 
    Var IsUpper : Boolean ) : Boolean;
Var
  e : TIChar;
  Enc : TEncoding;

  { cc is a letter and its encoding is Enc }
  Procedure Accept( Enc : TEncoding );
  Begin
    Stream_GetChars(f,e); { accept e }
    If Error Then Exit;
    Str_AppendIChar(Ch,e);
    Stream_SetEncoding(f,Enc);
    GrabOneLetter := True
  End;

Begin
  GrabOneLetter := False;
  Stream_NextChar(f,e);
  If Error Then Exit;

  Enc := Stream_GetEncoding(f);
  IsUpper := IsBigLetter(e);

  If IsLetter(e) Then
    Accept(Enc)
  Else If (Enc = ENC_UNDECIDED) And IsISO8859Letter(e) Then { policy }
    Accept(ENC_ISO8859)
End;

{ grab letters; return the number of letters appended to Ch }
Function GrabLetters( f : StreamPtr; Var Ch : StrPtr ) : TStrLength;
Var
  n : TStrLength;
  IsUpper : Boolean;
Begin
  GrabLetters := 0;
  n := 0;
  While GrabOneLetter(f,Ch,IsUpper) And (Not Error) Do
    n := n + 1;
  If Error Then Exit;
  GrabLetters := n
End;

{ grab digits; return the number of digits appended to Ch }
Function GrabDigits( f : StreamPtr; Var Ch : StrPtr ) : TStrLength;
Begin
  GrabDigits := GetCharWhile(f,Ch,Digits)
End;

{ append to a string any alphanumeric characters (plus underscore); 
 return the number of characters added to the string }
Function GrabAlpha( f : StreamPtr; Var Ch : StrPtr ) : TStrLength;
Var
  n : TStrLength;
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
 char appears within the run, it must be doubled; the result is a token of type 
 tt; this definition is broad enough to encompass double-quoted constant string 
 and single-quoted identifiers; if quiet just return Nil when there is no 
 quoted run of chars  }
{ TODO: escaped chars }
Function ReadQuotedRunOfChars( f : StreamPtr; quote : Char; tt : TTokenType; 
    keep : Boolean; quiet : Boolean ) : TokenPtr;
Var
  K : TokenPtr;
  e1,e2,e3,e4 : TIChar;
  Done : Boolean;
Begin
  ReadQuotedRunOfChars := Nil;
  Stream_NextChar(f,e1);
  If Error Then Exit;
  If Not TICharIs(e1,quote) Then 
  Begin
    If Not quiet Then
      SyntaxError(quote + ' expected');
    Exit
  End;
  Stream_GetChars(f,e1); { accept the quote }
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
      { look for the next quote on the current line }
      GetCharUntil(f,TK_STRI,[quote],e2);
      If Error Then Exit;
      If TICharIs(e2,quote) Then { doubled quote or end of run }
      Begin
        Stream_GetChars(f,e2); { consume it }
        If Error Then Exit;
        Stream_NextChar(f,e3);
        If Error Then Exit;
        If TICharIs(e3,quote) Then { doubled quotes }
        Begin
          Stream_GetChars(f,e3); { accept both single quotes }
          If Error Then Exit;
          Str_AppendIChar(TK_STRI,e3);
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
      Else If quiet Then 
        Exit 
      Else If TICharIsEol(e2) Then
        SyntaxError('end of line while reading ' + TokenStr[tt])
      Else If TICharIsEndOfInput(e2) Then
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
  c0 : TChar;
  e,e1,e2 : TIChar;
  Stop : Boolean;
Begin
  Verify(f,symbol);
  If Error Then Exit;
  TCharSetFromAscii(c0,'*');
  Verify(f,c0);
  If Error Then Exit;
  Repeat
    Stop := False;
    Repeat
      Stream_GetChar(f,e)
    Until Error Or TICharIsEndOfInput(e) Or TICharIsIn(e,['*','/','|']);
    If Error Then Exit;
    If TICharIs(e,'*') Then
    Begin
      Stream_NextChar(f,e1);
      If Error Then Exit;
      If TICharIs(e1,TCharGetBytes(symbol)) Then
      Begin
        Stream_GetChars(f,e1); { accept closing (*/ or *|) }
        If Error Then Exit;
        Stop := true
      End
    End
    Else If TICharIsIn(e,['/','|']) Then
    Begin
      Stream_NextChar(f,e2);
      If Error Then Exit;
      If TICharIs(e2,'*') Then
      Begin
        Stream_UngetChars(f,e); { put back the whole comment opening }
        ReadComment(f,e.Val);
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
  n : TStrLength;
Begin
  n := GetCharWhile(f,Stream_NewStr(f),[' '])
End;

{ skip any sequence of blank spaces (including line breaks) and comments }
Procedure ReadSpaces( f : StreamPtr; y : TSyntax );
Var
  e,e2 : TIChar;
  Stop : Boolean;
Begin
  Repeat
    { skip a run of spaces }
    Stream_GetCharNb(f,e);
    If Error Then Exit;
    { we have a non-space char e; we stop on EOF or if comments are not
     allowed within rules in the current Prolog syntax }
    Stop := TICharIsEndOfInput(e) Or (y In [PrologIIv1,PrologIIv2]);
    { skip a comment, if any }
    If Not Stop Then
    Begin
      If TICharIs(e,'%') Then { line comment }
      Begin
        { consume all characters up to the end of the line; note that EOL is a 
         space and thus will be consumed by Stream_GetCharNb at the beginning 
         of the next loop}
        Repeat
          Stream_GetChar(f,e)
        Until Error Or TICharIsEol(e) Or TICharIsEndOfInput(e); 
        If Error Then Exit;
        Stop := TICharIsEndOfInput(e)
      End
      Else If TICharIsIn(e,['/','|']) Then { possibly a *-style comment }
      Begin
        Stream_GetChar(f,e2);
        If Error Then Exit;
        Stop := Not TICharIs(e2,'*');
        If Not Stop Then
        Begin
          Stream_UngetChars(f,e); { put back the comment start }
          ReadComment(f,e.Val) { consume the whole comment }
        End
      End
      Else
        Stop := True { not a comment: job was completed by Stream_GetCharNb }
    End
  Until Stop Or Error;
  Stream_UngetChars(f,e) { put back the non-space character(s) }
End;

{ read an integer or return Nil }
Function ReadInteger( f : StreamPtr; Sign : Boolean ) : TokenPtr;
Var
  K : TokenPtr;
  n : TStrLength;
  e : TIChar;
  HasSign : Boolean;
Begin
  ReadInteger := Nil;
  HasSign := False;
  K := Token_New(TOKEN_INTEGER);
  With K^ Do
  Begin
    TK_STRI := Stream_NewStr(f);
    { optional sign }
    If Sign Then 
    Begin
      Stream_NextChar(f,e);
      If TICharIsIn(e,['+','-']) Then
      Begin
        Stream_GetChars(f,e);
        Str_AppendIChar(TK_STRI,e);
        HasSign := True
      End
    End;
    { digits }
    n := GrabDigits(f,TK_STRI);
    { no digits? clean up and fail }
    If n = 0 Then
    Begin
      If HasSign Then
        Stream_UngetChars(f,e); { put back the sign }
      Exit
    End
  End;
  ReadInteger := K
End;

{ read a number (integer or real), while being greedy, allowing for a sign if 
 Sign is True; return Nil if no number can be read
 - if syntax is not Edinburgh, then real numbers must have an explicit exponent 
  (e.g. 1.2e+3); See footnote 3 p.47; otherwise, "1.2" would be ambiguous, as 
  it could also a list as well as a real number; 
 - expressions like "10e+3" remain ambiguous, because the syntax states that the 
  possibly signed integer number after "E" is optional; in that case we assume 
  the "+3" is part of the real number, and not an addition;
 - input like "1.2e+3.4e" remain difficult to parse, as "+3" may be attributed 
  to the second real number, realizing very late that the second dot cannot be 
  a dot list operator; in fact, experiment shows that PII+ treats "1.2e+3.4e" as 
  dot("1.2e+3","4e"), so PII+ is greedy when parsing real numbers 
- so, there are two cases in which the dot is a list operator rather than the 
  decimal point of a real number: 1) there is not digits after the dot 
  (e.g. "1.nil"), and 2) there is one or more digits after the dot but 
  the syntax is not Edinburgh and there is no "e" after the run of digits
 }
Function ReadNumber( f : StreamPtr; y : TSyntax; Sign : Boolean ) : TokenPtr;
Var
  K : TokenPtr;
  e1,e2,e3 : TIChar;
  n : TStrLength;
  s,s2 : StrPtr;
  Done : Boolean; { stop parsing, we are done }
  IsList : Boolean; { true: the dot is actually a list operator }
  IsReal : Boolean; { true: the number is actually a floating point value }
  HasExpSign : Boolean; { sign after "e"? }
Begin
  ReadNumber := Nil;
  Done := False;
  IsList := False;
  IsReal := False;

  { 1) integer }
  K := ReadInteger(f,Sign);
  If Error Then Exit;

  { not an integer }
  If K = Nil Then
    Exit;

  { 2) real part, if any }
  s := Stream_NewStr(f);

  { 2a) potential decimal part }
  Stream_NextChar(f,e1); { first char after the leading integer }
  If Error Then Exit;
  If TICharIs(e1,'.') Then
  Begin
    Stream_GetChars(f,e1); { undo point: the dot char }
    Str_Append(s,'.');
    n := GrabDigits(f,s);
    { not a real, e.g. "1.nil", or "1.2" when not in Edinburgh mode }
    IsList := n = 0;
    If Not IsList Then
    Begin
      Stream_NextChar(f,e2);
      If Error Then Exit;
      IsList := Not IsExpSymbol(e2,y) And (y <> Edinburgh)
    End;
    If IsList Then
      Stream_UngetChars(f,e1); { undo up to, and including, '.' }
    IsReal := Not IsList;
    Done := IsList
  End;

  { 2b) exponent part }
  If Not Done Then
  Begin
    Stream_NextChar(f,e2);
    If Error Then Exit;
    If IsExpSymbol(e2,y) Then
    Begin
      IsReal := True;
      Stream_GetChars(f,e2); { undo point: the exponent mark }
      Str_Append(s,'E');
      { we put the remaining part in a different string, as we may decide not
       to append it to s, as "+z" in "1.2e+z" }
      s2 := Stream_NewStr(f);
      { exponent: "+3" in "1.2e+3" or "3" in "1.2e3";
        "1.2e" is valid syntax in PII+, so "1.2e+X" is read as an addition (and
        "+" has to be unread }
      Stream_NextChar(f,e3);
      If Error Then Exit;
      { optional sign }
      HasExpSign := TICharIsIn(e3,['-','+']);
      If HasExpSign Then
      Begin
        Stream_GetChars(f,e3); { undo point: the sign }
        Str_AppendIChar(s2,e3)
      End;
      { exp value }
      n := GrabDigits(f,s2);
      If n = 0 Then { "1e" or "1e+" }
      Begin
        If y in [PrologIIv1,PrologIIv2] Then { digits are mandatory }
        Begin
          Stream_UngetChars(f,e1); { in the end, what we have is an integer }
          IsReal := False
        End
        Else { valid real: "1e+" or "1e", forget about s2 }
        Begin
          { sign after "e" may be a binary op: put it back }
          If HasExpSign Then { "1e+" }
            Stream_UngetChars(f,e3);
          Str_Append(s,'0') { as Pascal's Val/2 cannot convert "1e" }
        End
      End
      Else
        Str_Concat(s,s2) { most complete form: "1e2", "1.1e2", "1e+2" }
    End
  End;

  { we do have a real number: update the token }
  If IsReal Then 
    With K^ Do
    Begin
      TK_TYPE := TOKEN_REAL;
      Str_Concat(TK_STRI,s)
    End;
  ReadNumber := K
End;

{ create a token of type typ while checking a given character }
Function GrabToken( f : StreamPtr; typ : TTokenType; e : TIChar ) : TokenPtr;
Var
  K : TokenPtr;
Begin
  GrabToken := Nil;
  K := Nil;
  Verify(f,e.Val);
  If Error Then Exit;
  K := Token_New(typ);
  GrabToken := K
End;

{ return True if string s is a valid (unquoted) identifier in syntax y;
 TODO: what about identifiers made of graphic chars }
Function IsValidUnquotedIdentifier( s : StrPtr; y : TSyntax ) : Boolean;
Var 
  State : Byte;
  Valid : Boolean; { is the current state a valid exit state? }
  Iter : StrIter;
  e : TIChar;
  c : TChar;
Begin
  IsValidUnquotedIdentifier := False;
  Valid := False;
  If Str_Length(s) = 0 Then
    Exit;
  State := 0;
  StrIter_ToStart(Iter,s);
  While StrIter_NextChar(Iter,c) Do
  Begin
    TICharSet(e,c,0,0); { make it a TIChar }
    Case y Of
    Edinburgh:
      Case State Of
      0:
        If IsLetter(e) And Not IsBigLetter(e) Then
        Begin
          State := 1;
          Valid := True
        End
        Else
          Exit;
      1: { one small letter }
        If IsAlpha(e) Then
          Pass
        Else
          Exit
      End;
    PrologIIp:
      Case State Of
      0:
        If IsLetter(e) Then
          State := 1
        Else
          Exit;
      1: { one letter }
        If IsLetter(e) Then
        Begin
          State := 2;
          Valid := True
        End
        Else
          Exit;
      2: { two letters }
        If IsAlpha(e) Then
          Pass
        Else If TICharIs(e,'''') Then
          State := 3
        Else
          Exit;
      3: { quotes }
        If TICharIs(e,'''') Then
          Pass
        Else
          Exit
      End;
    PrologIIv1,PrologIIv2:
      Case State Of
      0:
        If IsLetter(e) Then
          State := 1
        Else
          Exit;
      1: { one letter in a long word }
        If IsLetter(e) Then
        Begin
          State := 2;
          Valid := True
        End
        Else
          Exit;
      2: { a letter in a word }
        If IsLetter(e) Then
          Pass
        Else If TICharIs(e,'-') Then
        Begin
          State := 3;
          Valid := False { e.g. 'a-' is not a valid identifier }
        End
        Else If IsDigit(e) Then
          State := 4
        Else If TICharIs(e,'''') Then
          State := 5
        Else
          Exit;
      3: { dash }
        If IsLetter(e) Then
        Begin
          State := 2;
          Valid := True { 'a-b' is a valid identifier }
        End
        Else
          Exit;
      4: { digits }
        If IsDigit(e) Then
          Pass
        Else If TICharIs(e,'-') Then
        Begin
          State := 3;
          Valid := False
        End
        Else If TICharIs(e,'''') Then
          State := 5
        Else
          Exit;
      5: { quotes }
        If TICharIs(e,'''') Then
          Pass
        Else If TICharIs(e,'-') Then
        Begin
          State := 3;
          Valid := False
        End
        Else
          Exit
      End
    End
  End;
  IsValidUnquotedIdentifier := Valid
End;

{ return true if Prolog flavour y supports quoted identifiers }
Function SupportsQuotedIdentifiers( y : TSyntax ) : Boolean;
Begin
  SupportsQuotedIdentifiers := y In [PrologIIp,Edinburgh]
End;

{ read a variable or an identifier (including single-quoted identifier when
 the syntax allows it); throw an error if the string does not contain a valid
 identifier }
Function ReadVariableOrIdentifier( f : StreamPtr; y : TSyntax ) : TokenPtr;
Var
  K : TokenPtr;
  e,e1 : TIChar;
  IsUpper : Boolean;
  n,m : TStrLength;
Begin 
  ReadVariableOrIdentifier := Nil;
  { single-quoted identifier? note that surrounding quotes are NOT kept }
  K := ReadQuotedRunOfChars(f,'''',TOKEN_IDENT,False,True);
  { got one, but is it allowed? }
  If (K <> Nil) And (Not SupportsQuotedIdentifiers(y)) Then
    SyntaxError('this Prolog flavour does not support quoted identifiers');
  If Error Then Exit;
  { no quote: variable or unquoted identifier expected }
  If K = Nil Then
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
        If y In [PrologIIv1,PrologIIv2] Then { old Prolog II syntax, w/ accented letters }
        Begin
          Repeat
            Stream_NextChar(f,e);
            If Error Then Exit;
            If TICharIs(e,'-') Then { "-"<word> continuation }
            Begin
              Stream_GetChars(f,e); { accept the '-'}
              If Error Then Exit;
              Str_AppendIChar(TK_STRI,e)
            End;
            n := GrabLetters(f,TK_STRI);
            If Error Then Exit;
            m := GetCharWhile(f,TK_STRI,Digits);
            If Error Then Exit;
            m := GetCharWhile(f,TK_STRI,['''']);
            If Error Then Exit;
            If TICharIs(e,'-') And (n = 0) Then
              SyntaxError('dash must be followed by a letter');
            If Error Then Exit;
            Stream_NextChar(f,e1);
            If Error Then Exit
          Until (Not TICharIs(e1,'-')) Or Error;
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
 the doc says it is a PrologII+-only rule. Weird. Is it a typo? }
Function ReadToken( f : StreamPtr; y : TSyntax ) : TokenPtr;
Var
  K : TokenPtr;
  e,e1,e2 : TIChar;
  n : TStrLength;
  GraphicChars : CharSet;
Begin
  ReadToken := Nil;
  K := Nil;
  { save the next char for undo purpose; this is the char of the token but
   *including leading blank spaces*; this is needed to "unread" tokens, which 
   is required to correctly handle in_char(c) goals }
  Stream_NextChar(f,e);
  { from PII doc p4 and PII+doc p28: "spaces can be inserted anywhere except 
   inside constants and variables"}
  ReadSpaces(f,y);
  If Error Then Exit;
  Stream_NextChar(f,e1);
  If Error Then Exit;
  If TICharIsEndOfInput(e1) Then
  Begin
    Stream_GetChars(f,e1); { accept the EOF char }
    K := Token_New(TOKEN_END_OF_INPUT)
  End
  Else
  Begin
    Stream_NextNextChar(f,e2);
    If Error Then Exit;
    { arrow must be checked before identifiers made of graphic chars; 
    FIXME: spaces between the two characters of the arrow must be allowed, which 
    is tricky because it would require a giant lookahead }
    If TICharIs(e1,'-') And TICharIs(e2,'>') And (y <> Edinburgh) Or
        TICharIs(e1,':') And TICharIs(e2,'-') And (y = Edinburgh) Then
    Begin
      Stream_GetChars(f,e2); { accept the arrow }
      If Error Then Exit;
      K := Token_New(TOKEN_ARROW);
      { In Edinburgh syntax, ':-' is also an operator, so we need to save the 
      string in the token, for the expression parser to function properly }
      If y = Edinburgh Then
        K^.TK_STRI := Str_NewFromShortString(':-')
    End
    { identifiers made of graphic chars; FIXME: move this to ReadVariableOrIdentifier? }
    Else If TICharIsIn(e1,PROLOG_Graphic) And (y = PrologIIp) Or 
        TICharIsIn(e1,EDINBURGH_Graphic) And (y = Edinburgh) Then
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
    Else Case TICharGetByte(e1,1) Of
    '0'..'9':
      Begin
        K := ReadNumber(f,y,False);
        CheckCondition(K <> Nil,'unsigned numerical constant expected')
      End; 
    '_':
      If y In [PrologIIp,Edinburgh] Then  { a variable: PrologII+ basic syntax }
      Begin
        K := Token_New(TOKEN_VARIABLE);
        With K^ Do
        Begin
          TK_STRI := Stream_NewStr(f);
          n := GrabAlpha(f,TK_STRI); { letters (inc. accented), digits, _ }
          Token_SetAnonymous(K,n = 1) { anonymous: '_'}
        End
      End;
    '"':
      K := ReadQuotedRunOfChars(f,'"',TOKEN_STRING,False,False);
    '!':
      If y In [PrologIIp,Edinburgh] Then
        K := GrabToken(f,TOKEN_CUT,e1);
    '/':
      If y In [PrologIIv1,PrologIIv2] Then
        K := GrabToken(f,TOKEN_CUT,e1);
    ';':
      K := GrabToken(f,TOKEN_SEMICOLON,e1);
    '(':
      K := GrabToken(f,TOKEN_LEFT_PAR,e1);
    ')':
      K := GrabToken(f,TOKEN_RIGHT_PAR,e1);
    '{':
      K := GrabToken(f,TOKEN_LEFT_CUR,e1);
    '}':
      K := GrabToken(f,TOKEN_RIGHT_CUR,e1);
    '<':
      K := GrabToken(f,TOKEN_LEFT_CHE,e1);
    '>':
      K := GrabToken(f,TOKEN_RIGHT_CHE,e1);
    '[':
      K := GrabToken(f,TOKEN_LEFT_BRA,e1);
    ']':
      K := GrabToken(f,TOKEN_RIGHT_BRA,e1);
    '.':
      K := GrabToken(f,TOKEN_DOT,e1);
    ',':
      Begin
        K := GrabToken(f,TOKEN_COMMA,e1);
        { In Edinburgh syntax, ',' is also an operator, so we need to save the 
        string in the token, for the expression parser to function properly }
        If y = Edinburgh Then
          K^.TK_STRI := Str_NewFromShortString(',')
      End;
    '=':
      If y In [PrologIIv1,PrologIIv2] Then
        K := GrabToken(f,TOKEN_EQUAL,e1);
    '#':
      If y In [PrologIIv1,PrologIIv2] Then
        K := GrabToken(f,TOKEN_DIFF,e1);
    '|':
      K := GrabToken(f,TOKEN_PIPE,e1);
    Else
      K := ReadVariableOrIdentifier(f,y)
    End
  End;

  If Error Then Exit;
  If K = Nil Then
  Begin
    Stream_GetChars(f,e1); { accept the char so error message points to it }
    If Error Then Exit;
    SyntaxError('that character is not allowed here')
  End;
  If Error Then Exit;
  Token_SetLocation(K,e.Lnb,e.Pos);
  ReadToken := K
End;

End.