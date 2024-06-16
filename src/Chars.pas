{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Chars.pas                                                  }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                       M U L T I - B Y T E   C H A R                        }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ multi-byte characters }

{
  ASSUMPTIONS AND LIMITS:
  -----------------------
  1) MaxBytesPerChar: 4
    input streams (files or console) are made of one to four-byte 
    chars
  2) keyboard input stream is single-byte encoding or UTF8
}

{
  TODO:
  [ ] track end-of-line style (along w/ encoding)
  [ ] use BOM
  [ ] improve encoding detection
  [ ] always convert to terminal encoding 
}

Unit Chars;

Interface

Uses
{$IFNDEF MSDOS}
  Unixcp,
{$ENDIF}
  ShortStr,
  Num,
  Errs;

Const
  { code for 'new line':
    in the input system, 1) CR, LF and CR-LF in files, and 2) CR in terminal 
    input, are replaced with that single char;
    for convenience, and since there is no other great choice, we use the 
    ASCII code for LF 
    see: https://en.wikipedia.org/wiki/Newline }
  NewLine = #$0A;
  { UTF-8 byte order mark (BOM) }
  BOM : Array[1..3] Of Char = (#$EF,#$BB,#$BF);
  { maximum number of bytes per char }
  MaxBytesPerChar = 4;

Type 
  { supported encodings }
  TEncoding = (UNDECIDED,SINGLE_BYTE,CP437,CP850,ISO8859,UTF8);
  { encoding of a single, possible multi-byte character }
  TChar = String[MaxBytesPerChar];
  TCodePoint = PosInt;

Function GetSystemCEncoding: TEncoding;
Function CodePointToShortString( v : TCodePoint ) : TString;
Function TCharToCodePoint( cc : TChar; Enc : TEncoding; 
    Var cp : TCodePoint ) : Boolean;
Function CodePointToTChar( cp : TCodePoint; Var cc : TChar; 
    Enc : TEncoding ) : Boolean;
Function GetOneTCharNL( Var s : TString; Var cc : TChar;
    Var Encoding : TEncoding ) : Boolean;

Implementation
{-----------------------------------------------------------------------------}

{$IFDEF MSDOS}
Type TSystemCodePage = Word;
Function GetSystemCodepage: TSystemCodePage;
Begin
  GetSystemCodepage := 28591
End;
{$ENDIF}

Function GetSystemCEncoding: TEncoding;
Var
  Enc : TEncoding;
Begin
  Case GetSystemCodepage Of
  437: 
    Enc := CP437;
  850: 
    Enc := CP850;
  28591: 
    Enc := ISO8859;
  0,65001: 
    Enc := UTF8;
  Else
    Enc := SINGLE_BYTE
  End;
  GetSystemCEncoding := Enc
End;


{ return a string representation of a codepoint }
Function CodePointToShortString( v : TCodePoint ) : TString;
Begin
  CodePointToShortString := PosIntToShortString(v)
End;

{ return True if cc is a valid TChar in encoding End, and if so set cp to this 
 codepoint of a character; do not throw errors  
 see https://en.wikipedia.org/wiki/UTF-8 }
Function TCharToCodePoint( cc : TChar; Enc : TEncoding; 
    Var cp : TCodePoint ) : Boolean;
Begin
  TCharToCodePoint := False;
  If (Length(cc) = 0) Or (Enc <> UTF8) And (Length(cc) <> 1) Or 
      (Length(cc) > MaxBytesPerChar) Then
    Exit;
  Case Length(cc) Of
  1: { works for all encoding }
    cp := Byte(cc[1]) And $EF;
  2:
    cp := (((Byte(cc[1]) And $1F) Shl 8) Shr 2) Or 
      (Byte(cc[2]) And $3F);
  3:
    cp := (((Byte(cc[1]) And $0F) Shl 8) Shl 4) Or 
      (((Byte(cc[2]) And $3F) Shl 8) Shr 2) Or 
      (Byte(cc[3]) And $3F);
  4:
    cp := (((Byte(cc[1]) And $07) Shl 16) Shl 2) Or 
      (((Byte(cc[2]) And $3F) Shl 8) Shl 4) Or 
      (((Byte(cc[3]) And $3F) Shl 8) Shr 2) Or 
      (Byte(cc[4]) And $3F);
  End;
  TCharToCodePoint := True
End;

{ return True if codepoint cp is a valid codepoint in encoding Enc, and if so 
 set TChar cc to be this encoding; do not throw errors 
 see https://en.wikipedia.org/wiki/UTF-8 }
Function CodePointToTChar( cp : TCodePoint; Var cc : TChar; 
    Enc : TEncoding ) : Boolean;
Begin
  CodePointToTChar := False;
  If (Enc <> UTF8) And (cp > $FF) Or (cp > $10FFFF) Then
    Exit;
  If (Enc <> UTF8) Or (cp < $80) Then { 1-byte encoding }
    cc := Chr(cp)
  Else If cp < $0800 Then { 2-byte UTF-8 encoding; cp has 2 significant bytes }
    cc := Char(($C000 Or ((cp And $0700) Shl 2) Or ((cp And $00C0) Shl 2)) Shr 8) +
      Char($0080 Or (cp And $0030) Or (cp And $000F))
  Else If cp < $010000 Then { 3-byte UTF-8 encoding; cp has 2 significant bytes  }
    cc := Chr(($E00000 Or ((cp And $F000) Shl 4)) Shr 16) +
      Char(($8000 Or ((cp And $0F00) Shl 2) Or ((cp And $00C0) Shl 2)) Shr 8) +
      Char($0080 Or (cp And $0030) Or (cp And $000F))
  Else { 4-byte UTF-8 encoding; cp has 3 significant bytes  }
    cc := Char(($F0000000 Or ((cp And $100000) Shl 6) Or ((cp And $0C0000) Shl 6)) Shr 24) +
      Char(($00800000 Or ((cp And $030000) Shl 4) Or ((cp And $00F000) Shl 4)) Shr 16) +
      Char(($00008000 Or ((cp And $000F00) Shl 2) Or ((cp And $0000C0) Shl 2)) Shr 8) +
      Char($00000080 Or (cp And $0030) Or (cp And $000F));
  CodePointToTChar := True
End;

{ extract one TChar cc at the beginning of string s, assuming a
 certain encoding enc 
 - if cc is set, delete cc from the beginning of s and return True
   (since SyntaxError halts the program, this should always be the case)
 - recognize CRLF as a single code point, as in DOS and Windows,
   and most non-Unix, non-IBM systems:
   https://en.wikipedia.org/wiki/Newline
 - when the encoding is undecided, the function might discover
   and set the encoding
 - encoding detection "on the fly" is based on:
   https://en.wikipedia.org/wiki/UTF-8 
   (section: "Codepage layout")
   https://fr.wikipedia.org/wiki/ISO/CEI_8859-1 
   (section "ISO/CEI 8859-1 par rapport à ISO-8859-1") 
   https://en.wikipedia.org/wiki/Code_page_437#Characters
   https://en.wikipedia.org/wiki/Code_page_850#Character_set
   }
Function GetOneTChar( 
  Var s : TString; 
  Var cc : TChar;
  Var Encoding : TEncoding ) : Boolean;
Var
  c : Char;
  n,m : 0..MaxBytesPerChar; { expected / actual length of UTF-8 sequence }

  { cc is indeed a valid TChar }
  Procedure Success;
  Begin
    Delete(s,1,Length(cc));
    GetOneTChar := True
  End;

Begin
  CheckCondition(Length(s) > 0,'GetOneTChar: empty input');
  
  { recognize CRLF as a single entity }
  If StartsWith(s,CRLF) Then
  Begin
    cc := CRLF;
    Success;
    Exit
  End;

  { first byte }
  c := s[1];
  cc := c;

  { input is a stream of 1-byte chars }
  If Not (Encoding In [UNDECIDED,UTF8]) Then
  Begin
    Success;
    Exit
  End;

  { encoding is either undecided or UTF-8 } 
  Case c Of 
  #$80..#$BF, { continuation bytes: cannot start a sequence }
  #$C0,#$C1,#$F5..#$FF: { invalid in any sequence }
    Begin
      { cannot be UTF-8 }
      If Encoding = UTF8 Then
      Begin
        SyntaxError('broken UTF-8 encoding');
        GetOneTChar := False;
        Exit
      End;
      Encoding := SINGLE_BYTE
    End;
  #$C2..#$F4:
    Begin
      { might be a UTF-8 leading byte }
      { expected length of the UTF-8 sequence }
      Case c Of
      #$C2..#$DF:
        n := 2;
      #$E0..#$EF:
        n := 3;
      #$F0..#$F4:
        n := 4;
      End;
      { number of continuation bytes }
      m := StartsCount(Copy(s,2,StringMaxSize),[#$80..#$BF]);
      Case Encoding Of 
      UTF8:
        Begin
          If m <> n-1 Then
          Begin
            SyntaxError('broken UTF-8 encoding');
            GetOneTChar := False;
            Exit
          End;
          cc := cc + Copy(s,2,m)
        End;
      UNDECIDED: { detect encoding }
        Begin 
          If m = n-1 Then
          Begin
            cc := cc + Copy(s,2,m);
            Encoding := UTF8
          End
          Else
            Encoding := SINGLE_BYTE
        End
      End
    End
  End;
  Success
End;

{ same as above but transform CR, LF and CRLF into NewLine; return true
 if cc is set by the function }
Function GetOneTCharNL( Var s : TString; Var cc : TChar; 
    Var Encoding : TEncoding ) : Boolean;
Var
  res : Boolean;
Begin
  res := GetOneTChar(s,cc,Encoding);
  If res Then
    If (cc = #13) Or (cc = #10) Or (cc = CRLF) Then
      cc := NewLine;
  GetOneTCharNL := res
End;

End.