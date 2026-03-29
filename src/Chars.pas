{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Chars.pas                                                  }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022-2026                                                  }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                       M U L T I - B Y T E   C H A R                        }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }

{ handle multi-byte characters and special characters (soft marks) }

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
{$IFDEF UNIX}
  Unixcp,
{$ENDIF}
  ShortStr,
  Num,
  Errs,
  Trace;

{ soft marks:
 - end-of-line: in the input system, 1) CR, LF and CR-LF in files, and 2) CR in 
   terminal input, are replaced with that single soft mark;
   see: https://en.wikipedia.org/wiki/Newline }
Type
  TSoftMarkCode = (
    SOFT_MARK_END_OF_LINE,
    SOFT_MARK_END_OF_INPUT,
    SOFT_MARK_ATOM_BEGIN,
    SOFT_MARK_TOP_TERM_BEGIN,
    SOFT_MARK_TOP_TERM_END
  );

Const
  { UTF-8 byte order mark (BOM) }
  BOM : Array[1..3] Of Char = (#$EF,#$BB,#$BF);
  { maximum number of bytes per char }
  MaxBytesPerChar = 4;

Type 
  { code page, e.g. 437, 850, 858, 65000, 65001 }
  TCodePage = PosInt;
  { representations of EOL; detection of EOL style is based on the first EOL 
   character in a stream; note that EOL style detection is only used to detect
   inconsistent EOL characters in a stream, as Pascal's writeln procedure always 
   uses the system EOL style }
  TEolStyle = (
    EOL_UNDECIDED,
    EOL_CR,
    EOL_LF,
    EOL_CRLF
  );
  { supported encodings; when analyzing a stream of characters, we can be more
   precise as we collect more evidence, moving e.g. from ENC_UNDECIDED to ASCII and
   then to UTF8; some transitions are impossible and trigger an error, as e.g.
   switching from UTF8 to any single-byte encoding }
  TEncoding = (
    ENC_UNDECIDED, { waiting to be set based on any evidence we might collect }
    ENC_ASCII, { 7-bit chars so far; might be any encoding below }
    ENC_UTF8,
    ENC_SINGLE_BYTE, { not UTF8; might be any encoding below }
    ENC_CP437,
    ENC_CP850,
    ENC_ISO8859
  );
  TCharBytes = String[MaxBytesPerChar];
  TCharByteIndex = 1..MaxBytesPerChar;
  { encoding of a single, possible multi-byte character }
  TCharacter = Record
    Bytes : TCharBytes;
    Encoding : TEncoding { most likely encoding, context-dependent }
  End;
  { encoding of a soft mark w/ a long integer payload }
  TSoftMarkPayload = LongInt;
  TSoftMark = Record
    Code : TSoftMarkCode;
    Payload : TSoftMarkPayload { semantics is up to the user }
  End;
  TChar = Record { opaque }
    Case IsSoftMark : Boolean Of
      True : (SoftMark : TSoftMark);
      False : (Character : TCharacter);
  End;
  TCodePoint = PosInt;

{ pre-defined characters or soft marks }
Var 
  CC_BLANK_SPACE,
  CC_END_OF_LINE,
  CC_END_OF_INPUT : TChar;


Function AreCompatibleEncodings( Enc1,Enc2: TEncoding ) : Boolean;
Procedure UpdateContainerEncoding( Var Cont : TEncoding; Enc : TEncoding );

{ TChar }
Function TCharIsSoftMark( cc : TChar ) : Boolean;
Procedure TCharSetAsSoftMark( Var cc : TChar; IsSoft : Boolean);

{ TChar: helpers for soft marks }
Procedure TCharSetSoftMark( Var cc : TChar; code : TSoftMarkCode; 
    pl : TSoftMarkPayload);
Function TCharGetSoftMarkCode( cc : TChar ) : TSoftMarkCode;
Function TCharGetSoftMarkPayload( cc : TChar ) : TSoftMarkPayload;

{ TChar: regular characters: init }
Procedure TCharSetBytes( Var cc : TChar; b : TCharBytes );
Procedure TCharSetEncoding( Var cc : TChar; Enc : TEncoding );
Procedure TCharSetFromAscii( Var cc : TChar; c : Char );

{ TChar: regular characters }
Function TCharGetEncoding( cc : TChar ) : TEncoding;
Function TCharGetBytes( cc : TChar ) : TCharBytes;
Function TCharGetLength( cc : TChar ) : TCharByteIndex;
Function TCharGetByte( cc : TChar; i : TCharByteIndex ) : Char;
Function TCharSameBytes( cc1,cc2 : TChar ) : Boolean;

{ TChar: tests for useful soft marks }
Function TCharIsEndOfInput( cc : TChar ) : Boolean;
Procedure TCharSetToEndOfInput( Var cc : TChar );
Function TCharIsEol( cc : TChar ) : Boolean;
Procedure TCharSetToEol( Var cc : TChar );

{ TChar: tests for regular characters }
Function TCharIsMultibyte( cc : TChar ) : Boolean;
Function TCharIsIn( cc : TChar; E : CharSet ) : Boolean;
Function TCharIs( cc : TChar; b : TCharBytes ) : Boolean;
Function TCharIsSpace( cc : TChar ) : Boolean;
Function TCharIsPrintable( cc : TChar ) : Boolean;

Function GetSystemEolStyle : TEolStyle;

Procedure SetCodePage( c : TCodePage );
Function UpdatableEncoding( Cont,Enc : TEncoding ) : Boolean;
Function GetSystemEncoding : TEncoding;
Function CodePointToShortString( v : TCodePoint ) : TString;
Function TCharToCodePoint( cc : TChar; Var cp : TCodePoint ) : Boolean;
Function TCharSetFromCodePoint( Var cc : TChar; cp : TCodePoint; 
    Enc : TEncoding ) : Boolean;
Function TCharGetOne( Var s : TString; Var cc : TChar; 
    Var Enc : TEncoding; Var Style : TEolStyle ) : Boolean;

{ TChar: debug }
Function TCharToDebugShortString( cc : TChar ) : TString;
Procedure TCharDump( cc : TChar );

Implementation
{-----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ Helpers for EOL style                                                      }
{----------------------------------------------------------------------------}

{ get the system Eol style: see
 - https://en.wikipedia.org/wiki/Newline
 - https://lazarus-ccr.sourceforge.io/docs/rtl/system/lineending.html 
 - https://www.freepascal.org/docs-html/prog/progap7.html }
Function GetSystemEolStyle : TEolStyle;
Begin
  GetSystemEolStyle := EOL_LF; { arbitrary default, not supposed to happen }
{$IFDEF FPC}
  If LineEnding = #10 Then
    GetSystemEolStyle := EOL_LF
  Else If LineEnding = #13 Then
    GetSystemEolStyle := EOL_CR
  Else If LineEnding = #13#10 Then
    GetSystemEolStyle := EOL_CRLF
{$ELSE}
{$IFDEF TPC}
  GetSystemEolStyle := EOL_CRLF
{$ENDIF}
{$ENDIF}
End;

{----------------------------------------------------------------------------}
{ Helpers for encoding                                                       }
{----------------------------------------------------------------------------}

Var
  CodePage : TCodePage; { current code page }

{ GetSystemCodepage: Unixcp has it right away; FPC on non-unixes has 
 DefaultSystemCodepage; TP has none }
{$IFNDEF UNIX}
{$IFDEF FPC}
Function GetSystemCodepage: TSystemCodePage;
Begin
  GetSystemCodepage := DefaultSystemCodepage
End;
{$ELSE}
Type TSystemCodePage = Word;
Function GetSystemCodepage: TSystemCodePage;
Begin
  GetSystemCodepage := 28591 { ISO-8859-1 aka Latin1 }
End;
{$ENDIF}
{$ENDIF}

{ set what is the code page of the current terminal; does *not* change the 
 actual code page }
Procedure SetCodePage( c : TCodePage );
Begin
  CodePage := c
End;

{ get system encoding; 
 - codes are what Free Pascal's GetSystemCodepage returns; see
   https://www.freepascal.org/docs-html/rtl/unixcp/unixcpmap.html
 - compared to codepage 850, codepage 858 changes D5 from a dotless i to the 
   euro sign, which does not change what Prolog consider as a letter }
Function GetSystemEncoding : TEncoding;
Var
  Enc : TEncoding;
Begin 
  Case CodePage Of
  437: 
    Enc := ENC_CP437;
  850,858:
    Enc := ENC_CP850;
  20127:
    Enc := ENC_ASCII;
  28591: 
    Enc := ENC_ISO8859;
  0:
    Enc := ENC_UTF8;
  Else
    If CodePage = 65001 Then { TP: case constant must within integer range }
      Enc := ENC_UTF8
    Else
      Enc := ENC_SINGLE_BYTE
  End;
  GetSystemEncoding := Enc
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
  ENC_UNDECIDED: 
    If Enc > ENC_UNDECIDED Then
      Exit;
  ENC_ASCII:
    If Enc > ENC_ASCII Then
      Exit;
  ENC_SINGLE_BYTE:
    If Enc In [ENC_CP437,ENC_CP850,ENC_ISO8859] Then
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
  If (Enc1 In [ENC_UNDECIDED,ENC_ASCII]) Or (Enc2 In [ENC_UNDECIDED,ENC_ASCII]) Then
    Exit;
  If (Enc1 = ENC_SINGLE_BYTE) And (Enc2 In [ENC_CP437,ENC_CP850,ENC_ISO8859]) Then
    Exit;
  If (Enc2 = ENC_SINGLE_BYTE) And (Enc1 In [ENC_CP437,ENC_CP850,ENC_ISO8859]) Then
    Exit;
  AreCompatibleEncodings := False
End;

{ update the encoding of a container Cont (e.g. a string) given a new element 
 with known encoding Enc came in; raise an error if an encoding inconsistency 
 is detected }
Procedure UpdateContainerEncoding( Var Cont : TEncoding; Enc : TEncoding );
Begin
  CheckCondition(Enc <> ENC_UNDECIDED,'undecided encoding');
  If Not AreCompatibleEncodings(Cont,Enc) Then 
    EncodingError('cannot mix incompatible character encodings') { FIXME: could be a "user input" error  }
  Else If UpdatableEncoding(Cont,Enc) Then
    Cont := Enc
End;

{----------------------------------------------------------------------------}
{ TChar                                                                      }
{----------------------------------------------------------------------------}

{ true if the character is a soft mark }
Function TCharIsSoftMark( cc : TChar ) : Boolean;
Begin
  TCharIsSoftMark := cc.IsSoftMark
End;

{ set a character type: regular character or soft mark }
Procedure TCharSetAsSoftMark( Var cc : TChar; IsSoft : Boolean);
Begin
  cc.IsSoftMark := IsSoft
End;

{-----------------------------------------------------------------------}
{ soft marks: helpers                                                   }
{-----------------------------------------------------------------------}

{ make cc a soft mark }
Procedure TCharSetSoftMark( Var cc : TChar; code : TSoftMarkCode; 
    pl : TSoftMarkPayload);
Begin
  TCharSetAsSoftMark(cc,True);
  cc.SoftMark.Code := code;
  cc.SoftMark.Payload := pl
End;

{ return the code of soft mark cc }
Function TCharGetSoftMarkCode( cc : TChar ) : TSoftMarkCode;
Begin
  CheckCondition(TCharIsSoftMark(cc),'TCharGetSoftMarkCode: not a soft mark');
  TCharGetSoftMarkCode := cc.SoftMark.Code
End;

{ return the payload of soft mark cc }
Function TCharGetSoftMarkPayload( cc : TChar ) : TSoftMarkPayload;
Begin
  CheckCondition(TCharIsSoftMark(cc),'TCharGetSoftMarkPayload: not a soft mark');
  TCharGetSoftMarkPayload := cc.SoftMark.Payload
End;

{-----------------------------------------------------------------------}
{ regular character: init                                               }
{-----------------------------------------------------------------------}

{ these functions create new regular characters }

{ set cc as a regular character and set its bytes }
Procedure TCharSetBytes( Var cc : TChar; b : TCharBytes );
Begin
  TCharSetAsSoftMark(cc,False);
  cc.Character.Bytes := b
End;

{ set cc as a regular character and set its encoding }
Procedure TCharSetEncoding( Var cc : TChar; Enc : TEncoding );
Begin
  TCharSetAsSoftMark(cc,False);
  cc.Character.Encoding := Enc
End;

{ set a TChar from a single 7-bit ASCII char }
Procedure TCharSetFromAscii( Var cc : TChar; c : Char );
Begin
  TCharSetAsSoftMark(cc,False);
  CheckCondition(Ord(c) < $80,'TCharSetFromAscii: not 7-bit ASCII');
  TCharSetBytes(cc,c);
  TCharSetEncoding(cc,ENC_ASCII)
End;

{-----------------------------------------------------------------------}
{ regular character                                                     }
{-----------------------------------------------------------------------}

{ these functions bug on soft marks }

{ return the encoding of cc }
Function TCharGetEncoding( cc : TChar ) : TEncoding;
Begin
  CheckCondition(Not TCharIsSoftMark(cc),'TCharGetEncoding: soft mark');
  TCharGetEncoding := cc.Character.Encoding
End;

{ return the characters in cc, as a string }
Function TCharGetBytes( cc : TChar ) : TCharBytes;
Begin
  CheckCondition(Not TCharIsSoftMark(cc),'TCharGetBytes: soft mark');
  TCharGetBytes := cc.Character.Bytes
End;

{ return the number of bytes of cc }
Function TCharGetLength( cc : TChar ) : TCharByteIndex;
Begin
  CheckCondition(Not TCharIsSoftMark(cc),'TCharGetLength: soft mark');
  TCharGetLength := Length(TCharGetBytes(cc))
End;

{ return byte number i of cc as a char }
Function TCharGetByte( cc : TChar; i : TCharByteIndex ) : Char;
Begin
  CheckCondition(Not TCharIsSoftMark(cc),'TCharGetByte: soft mark');
  TCharGetByte := cc.Character.Bytes[i]
End;

{ append bytes to cc }
Procedure TCharAppendBytes( Var cc : TChar; b : TCharBytes );
Begin
  CheckCondition(Not TCharIsSoftMark(cc),'TCharAppendBytes: soft mark');
  CheckCondition(TCharGetLength(cc) + Length(b) <= MaxBytesPerChar,
      'TCharAppendBytes: too many chars');
  cc.Character.Bytes := cc.Character.Bytes + b
End;

{ have two TChars the same byte representation? }
Function TCharSameBytes( cc1,cc2 : TChar ) : Boolean;
Begin
  TCharSameBytes := TCharGetBytes(cc1) = TCharGetBytes(cc2)
End;

{-----------------------------------------------------------------------}
{ soft marks: tests                                                     }
{-----------------------------------------------------------------------}

{ these functions work on soft marks and regular characters }

{ is TChar cc an end-of-line mark }
Function TCharIsEol( cc : TChar ) : Boolean;
Begin
  TCharIsEol := TCharIsSoftMark(cc) And 
      (TCharGetSoftMarkCode(cc) = SOFT_MARK_END_OF_LINE)
End;

{ set a TChar to end-of-line }
Procedure TCharSetToEol( Var cc : TChar );
Begin
  TCharSetSoftMark(cc,SOFT_MARK_END_OF_LINE,0)
End;

{ is TChar cc an end-of-input mark }
Function TCharIsEndOfInput( cc : TChar ) : Boolean;
Begin
  TCharIsEndOfInput := TCharIsSoftMark(cc) And 
      (TCharGetSoftMarkCode(cc) = SOFT_MARK_END_OF_INPUT)
End;

{ set a TChar to end-of-input }
Procedure TCharSetToEndOfInput( Var cc : TChar );
Begin
  TCharSetSoftMark(cc,SOFT_MARK_END_OF_INPUT,0)
End;

{-----------------------------------------------------------------------}
{ regular characters: tests                                             }
{-----------------------------------------------------------------------}

{ these functions work on soft marks and regular characters }

{ is TChar cc a multibyte character? }
Function TCharIsMultibyte( cc : TChar ) : Boolean;
Begin
  TCharIsMultibyte := Not TCharIsSoftMark(cc) And (TCharGetLength(cc) > 1)
End;

{ is TChar cc in a set of 1-byte characters? }
Function TCharIsIn( cc : TChar; E : CharSet ) : Boolean;
Begin
  TCharIsIn := Not TCharIsSoftMark(cc) And 
      (TCharGetLength(cc) = 1) And (TCharGetByte(cc,1) In E)
End;

{ is TChar cc a certain string of bytes }
Function TCharIs( cc : TChar; b : TCharBytes ) : Boolean;
Begin
  TCharIs := Not TCharIsSoftMark(cc) And (TCharGetBytes(cc) = b)
End;

{ is TChar cc a space character? TODO: other UTF8 space-like characters? }
Function TCharIsSpace( cc : TChar ) : Boolean;
Begin
  TCharIsSpace := TCharIsIn(cc,[' ',#9])
End;


{ is TChar cc a printable character; this exclude control characters (including
 backspace, line break, etc.) }
Function TCharIsPrintable( cc : TChar ) : Boolean;
Var
  c1, c2 : Char;
Begin
  TCharIsPrintable := False;
  { soft marks are considered as not printable }
  If TCharIsSoftMark(cc) Then
    Exit;
  c1 := TCharGetByte(cc,1);
  { any single byte in the ASCII control char range is considered as control 
   char, whatever the actual encoding (even code page 437, see comment about 
   Character set in https://en.wikipedia.org/wiki/Code_page_437#Characters) }
  If (TCharGetLength(cc) = 1) Then 
  Begin
    If (Ord(c1) <= 31) Or (Ord(c1) = 127) Then
      Exit
  End
  { UTF8 multi-byte control chars; FIXME: to be completed, see
   https://en.wikipedia.org/wiki/Unicode_control_characters }
  Else
  Begin
    c2 := TCharGetByte(cc,2);
    If (TCharGetEncoding(cc) = ENC_UTF8) And (TCharGetLength(cc) = 2) Then
    Begin
      If (Ord(c1) = $C2) And (Ord(c2) <= $9F) Then
        Exit
    End
  End;
  TCharIsPrintable := True
End;

{----------------------------------------------------------------------------}
{ Code points                                                                }
{----------------------------------------------------------------------------}

{ return a string representation of a codepoint }
Function CodePointToShortString( v : TCodePoint ) : TString;
Begin
  CodePointToShortString := PosIntToShortString(v)
End;

{ return True if cc is a valid TChar, and if so set cp to its codepoint; 
 do not throw errors (TODO: Why??)
 see https://en.wikipedia.org/wiki/UTF-8 }
Function TCharToCodePoint( cc : TChar; Var cp : TCodePoint ) : Boolean;
Begin
  TCharToCodePoint := False;

  { special case: soft marks}
  If TCharIsSoftMark(cc) Then
  Begin
    If TCharIsEol(cc) Then { line break treated as '\n' in all encodings }
    Begin
      TCharToCodePoint := True;
      cp := 10
    End;
    Exit
  End;

  { obviously broken encoding }
  If (TCharGetEncoding(cc) <> ENC_UTF8) And (TCharGetLength(cc) <> 1) Then
    Exit;
  
  { regular character, possibly multibyte }
  Case TCharGetLength(cc) Of
  1: { works for all encoding }
    cp := Byte(TCharGetByte(cc,1)); {// And $EF;}
  2:
    cp := (((Byte(TCharGetByte(cc,1)) And $1F) Shl 8) Shr 2) Or 
      (Byte(TCharGetByte(cc,2)) And $3F);
  3:
    cp := (((Byte(TCharGetByte(cc,1)) And $0F) Shl 8) Shl 4) Or 
      (((Byte(TCharGetByte(cc,2)) And $3F) Shl 8) Shr 2) Or 
      (Byte(TCharGetByte(cc,3)) And $3F);
  4:
    cp := (((Byte(TCharGetByte(cc,1)) And $07) Shl 16) Shl 2) Or 
      (((Byte(TCharGetByte(cc,2)) And $3F) Shl 8) Shl 4) Or 
      (((Byte(TCharGetByte(cc,3)) And $3F) Shl 8) Shr 2) Or 
      (Byte(TCharGetByte(cc,4)) And $3F);
  End;
  TCharToCodePoint := True
End;

{ return True if codepoint cp is a valid codepoint in encoding Enc, and if so 
 set TChar cc to be this encoding (or simpler); do not throw errors 
 see https://en.wikipedia.org/wiki/UTF-8 }
Function TCharSetFromCodePoint( Var cc : TChar; cp : TCodePoint; 
    Enc : TEncoding ) : Boolean;
Begin
  TCharSetFromCodePoint := False;
  If (Enc <> ENC_UTF8) And (cp > $FF) Or (cp > $10FFFF) Then
    Exit;
  TCharSetEncoding(cc,Enc);
  If Enc <> ENC_UTF8 Then { 1-byte encoding }
    TCharSetBytes(cc,Chr(cp))
  Else If cp < $80 Then { 1-byte UTF-8, downgraded as ASCII }
  Begin
    TCharSetBytes(cc,Chr(cp));
    TCharSetEncoding(cc,ENC_ASCII)
  End
  Else If cp < $0800 Then { 2-byte UTF-8 encoding; cp has 2 significant bytes }
    TCharSetBytes(cc,Char(($C000 Or ((cp And $0700) Shl 2) Or ((cp And $00C0) Shl 2)) Shr 8) +
      Char($0080 Or (cp And $0030) Or (cp And $000F)))
  Else If cp < $010000 Then { 3-byte UTF-8 encoding; cp has 2 significant bytes  }
    TCharSetBytes(cc,Chr(($E00000 Or ((cp And $F000) Shl 4)) Shr 16) +
      Char(($8000 Or ((cp And $0F00) Shl 2) Or ((cp And $00C0) Shl 2)) Shr 8) +
      Char($0080 Or (cp And $0030) Or (cp And $000F)))
  Else { 4-byte UTF-8 encoding; cp has 3 significant bytes  }
    TCharSetBytes(cc,Char(($F0000000 Or ((cp And $100000) Shl 6) Or ((cp And $0C0000) Shl 6)) Shr 24) +
      Char(($00800000 Or ((cp And $030000) Shl 4) Or ((cp And $00F000) Shl 4)) Shr 16) +
      Char(($00008000 Or ((cp And $000F00) Shl 2) Or ((cp And $0000C0) Shl 2)) Shr 8) +
      Char($00000080 Or (cp And $0030) Or (cp And $000F)));
  TCharSetFromCodePoint := True
End;

{----------------------------------------------------------------------------}
{ TChar parser                                                               }
{----------------------------------------------------------------------------}

{ extract one TChar cc at the beginning of Pascal string s
 - Enc is the context encoding, that is, what is known about the encoding of 
   the input stream the character comes from
 - based on such context, try to guess the  encoding of the next, possibly 
   multi-bytes, character in s
 - in case of success, delete cc from the beginning of s and return True
   (since EncodingError halts the program, this should always be the case)
 - update the encoding context Enc whenever possible
 - ditto for end-of-line style
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
Function TCharGetOne( 
  Var s : TString; 
  Var cc : TChar;
  Var Enc : TEncoding;
  Var Style : TEolStyle ) : Boolean;
Var
  c : Char;
  n,m : 0..MaxBytesPerChar; { expected / actual length of UTF-8 sequence }

  { cc is indeed a valid TChar of n bytes }
  Procedure Success( n : TCharByteIndex );
  Begin
    Delete(s,1,n);
    TCharGetOne := True
  End;

  { Update the eol Style parameter; raise an error and return False if the new 
   style st is incoherent with the current one, Style }
  Function UpdateStyle( st : TEolStyle ) : Boolean;
  Var
    Ok : Boolean;
  Begin
    If Style = EOL_UNDECIDED Then { we can update }
      Style := st;
    Ok := (Style = st); { must end up with the same Eol style }
    If Not Ok Then
      EolStyleError('mangled end-of-line styles');
    UpdateStyle := Ok
  End;

Begin
  TCharGetOne := False;  
  CheckCondition(Length(s) > 0,'TCharGetOne: empty input');
  { 'end of file' character (ASCII 26 SUB / Ctrl-Z); in some old OS, this 
   marks the end of a file;
   see: https://en.wikipedia.org/wiki/Substitute_character 
   see: TP4 Owners Manual p80 and p301 (CheckEOF)
   return a end-of-input soft mark }
  If StartsWith(s,#26) Then
  Begin
    If Length(s) > 1 Then
    Begin
      EncodingError('SUB is not the last character of the input stream');
      Exit
    End;
    cc := CC_END_OF_INPUT; { soft mark }
    Success(1);
    Exit
  End;

  { end-of-line: CRLF (2 chars); must be the first EOL test, obviously }
  If StartsWith(s,#13#10) Then
  Begin
    If Not UpdateStyle(EOL_CRLF) Then
      Exit;
    cc := CC_END_OF_LINE; { soft mark }
    Success(2);
    Exit
  End;

  { end-of-line: CR (1 char) }
  If StartsWith(s,#13) Then
  Begin
    If Not UpdateStyle(EOL_CR) Then
      Exit;
    cc := CC_END_OF_LINE; { soft mark }
    Success(1);
    Exit
  End;

  { end-of-line: LF (1 char) }
  If StartsWith(s,#10) Then
  Begin
    If Not UpdateStyle(EOL_LF) Then
      Exit;
    cc := CC_END_OF_LINE; { soft mark }
    Success(1);
    Exit
  End;

  { start by assuming we have a single-byte char }
  c := s[1];
  TCharSetBytes(cc,c);
  TCharSetEncoding(cc,Enc);

  { input is known to be a stream of 1-byte chars }
  If Enc In [ENC_SINGLE_BYTE,ENC_CP437,ENC_CP850,ENC_ISO8859] Then
  Begin
    Success(1);
    Exit
  End;

  { encoding is either undecided, ASCII or UTF-8 } 
  Case c Of 
  #$00..#$7F: { 7-bit ASCII char }
    Begin
      TCharSetEncoding(cc,ENC_ASCII);
      If Enc = ENC_UNDECIDED Then
        Enc := ENC_ASCII
    End;
  #$80..#$BF, { continuation bytes: cannot start a sequence }
  #$C0,#$C1,#$F5..#$FF: { invalid in any sequence }
    Begin
      { cannot be UTF-8 }
      If Enc = ENC_UTF8 Then
      Begin
        EncodingError('broken UTF-8 encoding');
        Exit
      End;
      TCharSetEncoding(cc,ENC_SINGLE_BYTE);
      If Enc = ENC_UNDECIDED Then
        Enc := ENC_SINGLE_BYTE
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
      Case Enc Of 
      ENC_UTF8:
        Begin
          If m <> n-1 Then
          Begin
            EncodingError('broken UTF-8 encoding');
            Exit
          End;
          { an additional multi-byte char in stream already identified as 
           a UTF-8 stream }
          TCharAppendBytes(cc,Copy(s,2,m));
          TCharSetEncoding(cc,ENC_UTF8)
        End;
      ENC_UNDECIDED,ENC_ASCII: { refine encoding }
        Begin 
          If m = n-1 Then
          Begin
            { a multi-byte char in a UTF-8 stream }
            TCharAppendBytes(cc,Copy(s,2,m));
            TCharSetEncoding(cc,ENC_UTF8);
            Enc := ENC_UTF8
          End
          Else
          Begin
            TCharSetEncoding(cc,ENC_SINGLE_BYTE);
            If Enc = ENC_UNDECIDED Then
              Enc := ENC_SINGLE_BYTE
          End
        End
      End
    End
  End;
  Success(1)
End;

{----------------------------------------------------------------------------}
{ Debug                                                                      }
{----------------------------------------------------------------------------}

{ dump the content of char cc }
Function TCharToDebugShortString( cc : TChar ) : TString;
Var
  i : 0..MaxBytesPerChar;
  s : TString;
Begin
  s := '[';
  If Not TCharIsSoftMark(cc) Then
  Begin
    For i := 1 to TCharGetLength(cc) Do
    Begin
      s := s + IntToShortString(Ord(TCharGetByte(cc,i)));
      If i < TCharGetLength(cc) Then
        s := s + ','
    End;
    If TCharIsPrintable(cc) Then
      s := s + ':''' + TCharGetBytes(cc) + ''''
  End
  Else
  Begin
    s := s + '*' + IntToShortString(Ord(TCharGetSoftMarkCode(cc))) +
        ':' + IntToShortString(Ord(TCharGetSoftMarkPayload(cc)))
  End;
  s := s + ']';
  TCharToDebugShortString := s
End;

{ dump the content of char cc }
Procedure TCharDump( cc : TChar );
Begin
  WriteToTraceFile(TCharToDebugShortString(cc))
End;


{----------------------------------------------------------------------------}
{ Init                                                                       }
{----------------------------------------------------------------------------}

Begin
  SetCodePage(GetSystemCodepage);
  { characters }
  TCharSetFromAscii(CC_BLANK_SPACE,' ');
  { soft marks }
  TCharSetToEol(CC_END_OF_LINE);
  TCharSetToEndOfInput(CC_END_OF_INPUT)
End.