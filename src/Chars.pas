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
  { supported encodings; when analyzing a stream of characters, we can be more
   precise as we collect more evidence, moving e.g. from UNDECIDED to ASCII and
   then to UTF8; some transitions are impossible and trigger an error, as e.g.
   switching from UTF8 to any single-byte encoding }
  TEncoding = (
    UNDECIDED, { waiting to be set based on any evidence we might collect }
    ASCII, { 7-bit chars so far; might be any encoding below }
    UTF8,
    SINGLE_BYTE, { not UTF8; might be any encoding below }
    CP437,
    CP850,
    ISO8859
  );
  TCharBytes = String[MaxBytesPerChar];
  { encoding of a single, possible multi-byte character }
  TChar = Record
    Bytes : TCharBytes;
    Encoding : TEncoding { most likely encoding, context-dependent }
  End;
  TCodePoint = PosInt;

Function AreCompatibleEncodings( Enc1,Enc2: TEncoding ) : Boolean;
Procedure UpdateContainerEncoding( Var Cont : TEncoding; Enc : TEncoding );

Procedure ASCIIChar( Var cc : TChar; c : Char );
Function IsMultibyte( cc : TChar ) : Boolean;
Function IsIn( cc : TChar; E : CharSet ) : Boolean;


Function UpdatableEncoding( Cont,Enc : TEncoding ) : Boolean;
Function GetSystemCEncoding: TEncoding;
Function CodePointToShortString( v : TCodePoint ) : TString;
Function TCharToCodePoint( cc : TChar; Var cp : TCodePoint ) : Boolean;
Function CodePointToTChar( cp : TCodePoint; Var cc : TChar; 
    Enc : TEncoding ) : Boolean;
Function GetOneTCharNL( Var s : TString; Var cc : TChar; 
    Var Cont : TEncoding ) : Boolean;

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
  CodePage : Word;
  Enc : TEncoding;
Begin
  CodePage := GetSystemCodepage;
  Case CodePage Of
  437: 
    Enc := CP437;
  850: 
    Enc := CP850;
  20127:
    Enc := ASCII;
  28591: 
    Enc := ISO8859;
  0: 
    Enc := UTF8;
  Else
    If CodePage = 65001 Then { TP: case constant must within integer range }
      Enc := UTF8
    Else
      Enc := SINGLE_BYTE
  End;
  GetSystemCEncoding := Enc
End;

{ return True if it makes sense to update a container's encoding Cont with an 
 element's encoding Enc, that is, Enc is compatible and more precise than Enc; 
 for instance, updating an ASCII container with a UTF8 element makes sense, as 
 the container's encoding must be updated to UTF8; always return False when 
 the container's encoding is an actual code pages (UTF8, CP437...) since we 
 cannot be more precise }
Function UpdatableEncoding( Cont,Enc : TEncoding ) : Boolean;
Begin
  UpdatableEncoding := True;
  Case Cont Of 
  UNDECIDED: 
    If Enc > UNDECIDED Then
    Exit;
  ASCII:
    If Enc > ASCII Then
      Exit;
  SINGLE_BYTE:
    If Enc In [CP437,CP850,ISO8859] Then
      Exit;
  End;
  UpdatableEncoding := False
End;


{ return True if two encodings are compatible }
Function AreCompatibleEncodings( Enc1,Enc2: TEncoding ) : Boolean;
Begin
  AreCompatibleEncodings := True;
  If Enc1 = Enc2 Then
    Exit;
  If (Enc1 In [UNDECIDED,ASCII]) Or (Enc2 In [UNDECIDED,ASCII]) Then
    Exit;
  If (Enc1 = SINGLE_BYTE) And (Enc2 In [CP437,CP850,ISO8859]) Then
    Exit;
  If (Enc2 = SINGLE_BYTE) And (Enc1 In [CP437,CP850,ISO8859]) Then
    Exit;
  AreCompatibleEncodings := False
End;

{ update the encoding of a container Cont (e.g. a string) given a new element 
 with known encoding Enc came in; raise an error if an encoding inconsistency 
 is detected }
Procedure UpdateContainerEncoding( Var Cont : TEncoding; Enc : TEncoding );
Begin
  CheckCondition(Enc <> UNDECIDED,'undecided encoding');
  If Not AreCompatibleEncodings(Cont,Enc) Then 
    EncodingError('cannot mix incompatible character encodings') { FIXME: could be a "user input" error  }
  Else If UpdatableEncoding(Cont,Enc) Then
    Cont := Enc
End;

{ set a TChar from a single 7-bit ASCII char }
Procedure ASCIIChar( Var cc : TChar; c : Char );
Begin
  CheckCondition(Ord(c) < $80,'ASCIIChar: not 7-bit ASCII');
  cc.Bytes := c;
  cc.Encoding := ASCII
End;

{ is a character multibyte? }
Function IsMultibyte( cc : TChar ) : Boolean;
Begin
  IsMultibyte := Length(cc.Bytes) > 1
End;


{ is TChar cc in a set of 1-byte characters? }
Function IsIn( cc : TChar; E : CharSet ) : Boolean;
Begin
  IsIn := (Length(cc.Bytes) = 1) And (cc.Bytes[1] In E)
End;

{ return a string representation of a codepoint }
Function CodePointToShortString( v : TCodePoint ) : TString;
Begin
  CodePointToShortString := PosIntToShortString(v)
End;

{ return True if cc is a valid TChar, and if so set cp to its codepoint; 
 do not throw errors  
 see https://en.wikipedia.org/wiki/UTF-8 }
Function TCharToCodePoint( cc : TChar; Var cp : TCodePoint ) : Boolean;
Begin
  TCharToCodePoint := False;
  If (Length(cc.Bytes) = 0) Or 
      (cc.Encoding <> UTF8) And (Length(cc.Bytes) <> 1) Or 
      (Length(cc.Bytes) > MaxBytesPerChar) Then
    Exit;
  Case Length(cc.Bytes) Of
  1: { works for all encoding }
    cp := Byte(cc.Bytes[1]); {// And $EF;}
  2:
    cp := (((Byte(cc.Bytes[1]) And $1F) Shl 8) Shr 2) Or 
      (Byte(cc.Bytes[2]) And $3F);
  3:
    cp := (((Byte(cc.Bytes[1]) And $0F) Shl 8) Shl 4) Or 
      (((Byte(cc.Bytes[2]) And $3F) Shl 8) Shr 2) Or 
      (Byte(cc.Bytes[3]) And $3F);
  4:
    cp := (((Byte(cc.Bytes[1]) And $07) Shl 16) Shl 2) Or 
      (((Byte(cc.Bytes[2]) And $3F) Shl 8) Shl 4) Or 
      (((Byte(cc.Bytes[3]) And $3F) Shl 8) Shr 2) Or 
      (Byte(cc.Bytes[4]) And $3F);
  End;
  TCharToCodePoint := True
End;

{ return True if codepoint cp is a valid codepoint in encoding Enc, and if so 
 set TChar cc to be this encoding (or simpler); do not throw errors 
 see https://en.wikipedia.org/wiki/UTF-8 }
Function CodePointToTChar( cp : TCodePoint; Var cc : TChar; 
    Enc : TEncoding ) : Boolean;
Begin
  CodePointToTChar := False;
  If (Enc <> UTF8) And (cp > $FF) Or (cp > $10FFFF) Then
    Exit;
  cc.Encoding := Enc;
  If Enc <> UTF8 Then { 1-byte encoding }
    cc.Bytes := Chr(cp)
  Else If cp < $80 Then { 1-byte UTF-8, downgraded as ASCII }
  Begin
    cc.Bytes := Chr(cp);
    cc.Encoding := ASCII
  End
  Else If cp < $0800 Then { 2-byte UTF-8 encoding; cp has 2 significant bytes }
    cc.Bytes := Char(($C000 Or ((cp And $0700) Shl 2) Or ((cp And $00C0) Shl 2)) Shr 8) +
      Char($0080 Or (cp And $0030) Or (cp And $000F))
  Else If cp < $010000 Then { 3-byte UTF-8 encoding; cp has 2 significant bytes  }
    cc.Bytes := Chr(($E00000 Or ((cp And $F000) Shl 4)) Shr 16) +
      Char(($8000 Or ((cp And $0F00) Shl 2) Or ((cp And $00C0) Shl 2)) Shr 8) +
      Char($0080 Or (cp And $0030) Or (cp And $000F))
  Else { 4-byte UTF-8 encoding; cp has 3 significant bytes  }
    cc.Bytes := Char(($F0000000 Or ((cp And $100000) Shl 6) Or ((cp And $0C0000) Shl 6)) Shr 24) +
      Char(($00800000 Or ((cp And $030000) Shl 4) Or ((cp And $00F000) Shl 4)) Shr 16) +
      Char(($00008000 Or ((cp And $000F00) Shl 2) Or ((cp And $0000C0) Shl 2)) Shr 8) +
      Char($00000080 Or (cp And $0030) Or (cp And $000F));
  CodePointToTChar := True
End;

{ extract one TChar cc at the beginning of Pascal string s
 - Cont is the context encoding, that is, what is known about the encoding of 
   the input stream the character comes from
 - based on such context, try to guess the  encoding of the next, possibly 
   multi-bytes, character in s
 - in case of success, delete cc from the beginning of s and return True
   (since EncodingError halts the program, this should always be the case)
 - update the encoding context Cont whenever possible
 - recognize CRLF as a single code point, as in DOS and Windows,
   and most non-Unix, non-IBM systems:
   https://en.wikipedia.org/wiki/Newline
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
  Var Cont : TEncoding ) : Boolean;
Var
  c : Char;
  n,m : 0..MaxBytesPerChar; { expected / actual length of UTF-8 sequence }

  { cc is indeed a valid TChar }
  Procedure Success;
  Begin
    Delete(s,1,Length(cc.Bytes));
    GetOneTChar := True
  End;

Begin
  CheckCondition(Length(s) > 0,'GetOneTChar: empty input');
  
  { recognize CRLF as a single entity }
  If StartsWith(s,CRLF) Then
  Begin
    cc.Bytes := CRLF;
    cc.Encoding := ASCII;
    Success;
    Exit
  End;

  { first byte }
  c := s[1];
  cc.Bytes := c;
  cc.Encoding := Cont;

  { input is known to be a stream of 1-byte chars }
  If Cont In [SINGLE_BYTE,CP437,CP850,ISO8859] Then
  Begin
    Success;
    Exit
  End;

  { encoding is either undecided, ASCII or UTF-8 } 
  Case c Of 
  #$00..#$7F: { 7-bit ASCII char }
    Begin
      cc.Encoding := ASCII;
      If Cont = UNDECIDED Then
        Cont := ASCII
    End;
  #$80..#$BF, { continuation bytes: cannot start a sequence }
  #$C0,#$C1,#$F5..#$FF: { invalid in any sequence }
    Begin
      { cannot be UTF-8 }
      If Cont = UTF8 Then
      Begin
        EncodingError('broken UTF-8 encoding');
        GetOneTChar := False;
        Exit
      End;
      cc.Encoding := SINGLE_BYTE;
      If Cont = UNDECIDED Then
        Cont := SINGLE_BYTE
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
      Case Cont Of 
      UTF8:
        Begin
          If m <> n-1 Then
          Begin
            EncodingError('broken UTF-8 encoding');
            GetOneTChar := False;
            Exit
          End;
          { an additional multi-byte char in stream already identified as 
           a UTF-8 stream }
          cc.Bytes := cc.Bytes + Copy(s,2,m);
          cc.Encoding := UTF8
        End;
      UNDECIDED,ASCII: { refine encoding }
        Begin 
          If m = n-1 Then
          Begin
            { a multi-byte char in a UTF-8 stream }
            cc.Bytes := cc.Bytes + Copy(s,2,m);
            cc.Encoding := UTF8;
            Cont := UTF8
          End
          Else
          Begin
            cc.Encoding := SINGLE_BYTE;
            If Cont = UNDECIDED Then
              Cont := SINGLE_BYTE
          End
        End
      End
    End
  End;
  Success
End;

{ same as above but transform CR, LF and CRLF into NewLine; return true
 if cc is set by the function }
Function GetOneTCharNL( Var s : TString; Var cc : TChar; 
    Var Cont : TEncoding ) : Boolean;
Var
  res : Boolean;
Begin
  res := GetOneTChar(s,cc,Cont);
  If res Then
    If (cc.Bytes = #13) Or (cc.Bytes = #10) Or (cc.Bytes = CRLF) Then
      cc.Bytes := NewLine;
  GetOneTCharNL := res
End;

End.