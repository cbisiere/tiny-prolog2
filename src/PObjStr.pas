{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObjStr.pas                                                }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                      L O N G   S T R I N G S                               }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }

Unit PObjStr;

Interface

Uses
  ShortStr,
  Num,
  Errs,
  Chars,
  CWrites,
  Common,
  Memory,
  PObj;

{-----------------------------------------------------------------------}
{ type                                                                  }
{-----------------------------------------------------------------------}

{ maximum number of characters in a chunk; since most strings in a Prolog 
 program are very short, we set a small value to save space }
Const
  StrCharsMaxLen = 15;
Type 
  TStrCharsLen = 0..StrCharsMaxLen;
  TStrChars = Record
    Len : TStrCharsLen;
    Chars : Array[TStrCharsLen] Of TChar
  End;

{ GC-managed string that can grow; 
  invariants: 1) all chunks but the last are full; 2) no chunks are empty, 
  except when the string is empty }

Type 
  StrPtr = ^TObjStr;
  StrDataPtr = ^TObjStrData;

  TObjStr = Record
    PO_META: TObjMeta;
    { deep copied: }
    ST_FDAT: StrDataPtr; { first chunk }
    ST_LDAT: StrDataPtr; { last chunk }
    { extra data: }
    ST_NDAT: Longint; { number of chunks }
    ST_CONT: TEncoding; { encoding of the stream this string is part of }
    ST_ENCO: TEncoding; { actual encoding of the string }
    ST_TLEN: Longint { total length in number of TChars }
  End;

  TObjStrData = Record
    PO_META: TObjMeta;
    { deep copied: }
    SD_PREV: StrDataPtr; { previous chunk or Nil }
    SD_NEXT: StrDataPtr; { next chunk or Nil }
    { extra data: }
    SD_DATA: TStrChars { char storage }
  End;

  { char iterator within a string }
  StrIter = Record
    Data : StrDataPtr; { current data chunk }
    LastRead : TStrCharsLen { index of the last char read }
  End;


Function Str_New( Context : TEncoding ) : StrPtr;
Function Str_NewFromShortString(ps: TString): StrPtr;
Function Str_NewFromBytes( bytes : TString; Enc : TEncoding ) : StrPtr;

Function Str_Length(s: StrPtr): longint;
Function Str_GetEncodingContext( s : StrPtr ) : TEncoding;
Function Str_GetEncoding( s : StrPtr ) : TEncoding;

Function Str_GetShortStringTruncate(s: StrPtr): TString;
Function Str_AsShortString( s: StrPtr ): TString;
Procedure Str_Append( s : StrPtr; ps : TString );
Procedure Str_AppendChar( s : StrPtr; cc : TChar );
Procedure Str_Concat(s1, s2: StrPtr);
Function Str_Clone(s: StrPtr): StrPtr;
Procedure Str_SingleQuoteAndEscape( s : StrPtr );
Procedure Str_DoubleQuoteAndEscape( s : StrPtr );
Function Str_Comp(s1, S2: StrPtr): TComp;
Function Str_Equal(s1, s2: StrPtr): boolean;
Function Str_EqualToShortString(s: StrPtr; c: TString): boolean;
Function Str_FirstChar( s : StrPtr; Var cc : TChar ) : Boolean;
Function Str_LastChar( s : StrPtr; Var cc : TChar ) : Boolean;
Function Str_StartsWith(s: StrPtr; E: CharSet): boolean;
Function Str_EndsWith(s: StrPtr; E: CharSet): boolean;
Procedure Str_DeleteLastCharUntil( s : StrPtr; StopChars : CharSet );

Procedure Str_CWrite(s: StrPtr);

Procedure StrIter_ToStart( Var Iter : StrIter; s : StrPtr );
Procedure StrIter_ToEnd( Var Iter : StrIter; s : StrPtr );
Function StrIter_NextChar( Var Iter : StrIter; Var cc : TChar ) : Boolean;
Function StrIter_PrevChar( Var Iter : StrIter; Var cc : TChar ) : Boolean;

Implementation
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------}
{ constructors                                                          }
{-----------------------------------------------------------------------}

{ new empty string data chunk }
Function StrData_New : StrDataPtr;
Var 
  sda: StrDataPtr;
  ptr: TObjectPtr Absolute sda;
Begin
  ptr := NewRegisteredPObject(SD, SizeOf(TObjStrData), 2, True, 2);
  With sda^ Do
  Begin
    SD_PREV := Nil;
    SD_NEXT := Nil;
    SD_DATA.Len := 0
  End;
  StrData_New := sda
End;

{ set or reset a string to be an empty string; old string data, if any, will 
 be GC'ed; retain the encoding context }
Procedure Str_Zap( s : StrPtr );
Begin
  With s^ Do
  Begin
    ST_FDAT := StrData_New;
    ST_ENCO := UNDECIDED;
    ST_LDAT := ST_FDAT;
    ST_NDAT := 1;
    ST_TLEN := 0
  End
End;

{ new string as part of a input or output steam whose encoding is Enc }
Function Str_New( Context : TEncoding ) : StrPtr;
Var 
  s: StrPtr;
  ptr: TObjectPtr Absolute s;
Begin
  ptr := NewRegisteredPObject(ST, SizeOf(TObjStr), 2, True, 2);
  Str_Zap(s);
  With s^ Do
  Begin
    ST_CONT := Context
  End;
  Str_New := s
End;


{-----------------------------------------------------------------------}
{ Str: get                                                              }
{-----------------------------------------------------------------------}

{ return the length of a string, in number of chars }
Function Str_Length( s : StrPtr ) : LongInt;
Begin
  Str_Length := s^.ST_TLEN
End;

{ return the number of chunks in a string }
Function Str_NumberOfStrData( s : StrPtr ) : LongInt;
Begin
  Str_NumberOfStrData := s^.ST_NDAT
End;

{ return the first data chunk of a string }
Function Str_GetFirstStrData( s : StrPtr ) : StrDataPtr;
Begin
  Str_GetFirstStrData := s^.ST_FDAT
End;

{ return the last data chunk of a string }
Function Str_GetLastStrData( s : StrPtr ) : StrDataPtr;
Begin
  Str_GetLastStrData := s^.ST_LDAT
End;

{ return the encoding context of a string }
Function Str_GetEncodingContext( s : StrPtr ) : TEncoding;
Begin
  Str_GetEncodingContext := s^.ST_CONT
End;

{ return the encoding of a string; the encoding must always remain compatible
 with the encoding context but might be less demanding; for instance the 
 context might be UTF8 while the actual encoding is ASCII, if the string
 contains only 7-bit characters }
Function Str_GetEncoding( s : StrPtr ) : TEncoding;
Begin
  Str_GetEncoding := s^.ST_ENCO
End;

{-----------------------------------------------------------------------}
{ StrData: get                                                          }
{-----------------------------------------------------------------------}

{ previous chunk, or Nil if the chunk is the first one }
Function StrData_Prev( sd : StrDataPtr ) : StrDataPtr;
Begin
  StrData_Prev := sd^.SD_PREV
End;

{ next chunk, or Nil if the chunk is the last one }
Function StrData_Next( sd : StrDataPtr ) : StrDataPtr;
Begin
  StrData_Next := sd^.SD_NEXT
End;

{ how many TChar in a chunk? }
Function StrData_Len( sd : StrDataPtr ) : TStrCharsLen;
Begin
  StrData_Len := sd^.SD_DATA.Len
End;

{ is a chunk full? }
Function StrData_IsFull( sd : StrDataPtr ) : Boolean;
Begin
  StrData_IsFull := StrData_Len(sd) = StrCharsMaxLen
End;

{ retrieve i's TChar in a chunk }
Procedure StrData_Char( sd : StrDataPtr; i : TStrCharsLen; Var cc : TChar );
Begin
  CheckCondition(i <= StrData_Len(sd),'StrData_Char: out of bound index');
  cc := sd^.SD_DATA.Chars[i]
End;

{-----------------------------------------------------------------------}
{ StrIter: iterate                                                      }
{-----------------------------------------------------------------------}

{ initialize a forward iterator to iterate on a string's characters }
Procedure StrIter_ToStart( Var Iter : StrIter; s : StrPtr );
Begin
  Iter.Data := Str_GetFirstStrData(s);
  Iter.LastRead := 0
End;

{ initialize a backward iterator to iterate on a string's characters }
Procedure StrIter_ToEnd( Var Iter : StrIter; s : StrPtr );
Begin
  Iter.Data := Str_GetLastStrData(s);
  Iter.LastRead := 0 { means: "nothing read yet" }
End;

{ return True if there is at least char to consider next }
Function StrIter_HasNext( Iter : StrIter ) : Boolean;
Begin
  With Iter Do
    StrIter_HasNext := (LastRead < StrData_Len(Data)) Or 
        (StrData_Next(Data) <> Nil)
End;

{ get the next char (if any) in a Str; return False if the iterator has 
 reached the end of the string and thus no char can be returned }
Function StrIter_NextChar( Var Iter : StrIter; Var cc : TChar ) : Boolean;
Begin
  StrIter_NextChar := False;
  With Iter Do
  Begin
    If LastRead >= StrData_Len(Data) Then
    Begin
      Data := StrData_Next(Data);
      LastRead := 0;
      If Data = Nil Then
        Exit
    End;
    LastRead := LastRead + 1;
    StrData_Char(Data,LastRead,cc)
  End;
  StrIter_NextChar := True
End;

{ get the previous char (if any) in a string; return False if the iterator has 
 reached the beginning of the string and thus no char can be returned }
Function StrIter_PrevChar( Var Iter : StrIter; Var cc : TChar ) : Boolean;
Begin
  StrIter_PrevChar := False;
  With Iter Do
  Begin
    Case LastRead Of
    0: { first iteration; the string might be empty }
      Begin
        LastRead := StrData_Len(Data);
        If LastRead = 0 Then
          Exit
      End;
    1: { current data chunk was fully iterated }
      Begin
        Data := StrData_Prev(Data);
        If Data = Nil Then
          Exit;
        LastRead := StrData_Len(Data)
      End;
    Else { in the 'middle' of the current data chunk: previous character }
      LastRead := LastRead - 1
    End;
    StrData_Char(Data,LastRead,cc)
  End;
  StrIter_PrevChar := True
End;

{-----------------------------------------------------------------------}
{ Str: get (cont.)                                                      }
{-----------------------------------------------------------------------}

{ first character, or False }
Function Str_FirstChar( s : StrPtr; Var cc : TChar ) : Boolean;
Var 
  Iter : StrIter;
Begin
  StrIter_ToStart(Iter,s);
  Str_FirstChar := StrIter_NextChar(Iter,cc)
End;

{ last character, or False }
Function Str_LastChar( s : StrPtr; Var cc : TChar ) : Boolean;
Var 
  Iter : StrIter;
Begin
  StrIter_ToEnd(Iter,s);
  Str_LastChar := StrIter_PrevChar(Iter,cc)
End;

{ string starts with a char in a set of 1-byte chars }
Function Str_StartsWith( s : StrPtr; E : CharSet ) : Boolean;
Var 
  cc : TChar;
Begin
  Str_StartsWith := False;
  If Not Str_FirstChar(s,cc) Then
    Exit;
  Str_StartsWith := IsIn(cc,E)
End;

{ string ends with a char in a set of 1-byte chars }
Function Str_EndsWith( s : StrPtr; E : CharSet ) : Boolean;
Var 
  cc : TChar;
Begin
  Str_EndsWith := False;
  If Not Str_LastChar(s,cc) Then
    Exit;
  Str_EndsWith := IsIn(cc,E)
End;

{ return the (possibly truncated) value of a Str as a Pascal string }
Function Str_GetShortStringTruncate( s : StrPtr ) : TString;
Var
  ps : TString;
  Iter : StrIter;
  cc : TChar;
Begin
  ps := '';
  StrIter_ToStart(Iter,s);
  While StrIter_NextChar(Iter,cc) And 
      (Length(ps) + Length(cc.Bytes) <= StringMaxSize) Do
    ps := ps + cc.Bytes;
  Str_GetShortStringTruncate := ps
End;

{ return the value of a Str as a Pascal string, raising an error if it does not
 fit }
Function Str_AsShortString( s : StrPtr ) : TString;
Begin
  CheckCondition(Str_Length(s) <= StringMaxSize,
    'Str_AsShortString: too long to fit in a Pascal string');
  Str_AsShortString := Str_GetShortStringTruncate(s)
End;

{-----------------------------------------------------------------------}
{ Str: edit                                                             }
{-----------------------------------------------------------------------}

{ force the encoding of a string }
Procedure Str_SetEncoding( s : StrPtr; Enc : TEncoding );
Begin
  s^.ST_ENCO := Enc
End;

{ append a chunk to a string, also updating the number of characters in the 
 string }
Procedure Str_AppendData( s : StrPtr; sd : StrDataPtr );
Begin
  With s^ Do
  Begin
    ST_LDAT^.SD_NEXT := sd;
    sd^.SD_PREV := ST_LDAT;
    ST_LDAT := sd;
    ST_NDAT := ST_NDAT + 1;
    ST_TLEN := ST_TLEN + StrData_Len(sd)
  End
End;

{ remove the last chunk of a string, also updating the number of characters in 
 the string; assume the string has more than one chunk }
Procedure Str_DeleteLastData( s : StrPtr );
Begin
  With s^ Do
  Begin
    CheckCondition(ST_NDAT > 1,'Str_DeleteLastData: only one chunk');
    ST_TLEN := ST_TLEN - StrData_Len(ST_LDAT);
    ST_LDAT^.SD_PREV^.SD_NEXT := Nil;
    ST_LDAT := ST_LDAT^.SD_PREV
  End
End;

{ append a TChar to a chunk, assuming there is enough room for it }
Procedure StrData_AppendChar( sd : StrDataPtr; cc : TChar );
Begin
  With sd^.SD_DATA Do
  Begin
    CheckCondition(Len < StrCharsMaxLen,'StrData_AppendChar: chunk is full');
    Len := Len + 1;
    Chars[Len] := cc
  End
End;

{ delete the last char of a chunk, assuming it contains at least one char }
Procedure StrData_DeleteLastChar( sd : StrDataPtr );
Begin
  With sd^.SD_DATA Do
  Begin
    CheckCondition(Len > 0,'StrData_DeleteLastChar: chunk is empty');
    Len := Len - 1
  End
End;

{ append a TChar to a string, creating a new chuck when needed; update the
 string encoding when appropriate }
Procedure Str_AppendChar( s : StrPtr; cc : TChar );
Var
  sd : StrDataPtr;
Begin
  sd := Str_GetLastStrData(s);
  If Not StrData_IsFull(sd) Then
  Begin
    StrData_AppendChar(sd,cc);
    s^.ST_TLEN := s^.ST_TLEN + 1
  End
  Else { create a new chunk and append it }
  Begin
    sd := StrData_New;
    StrData_AppendChar(sd,cc);
    Str_AppendData(s,sd)
  End;
  { infer encoding }
  UpdateContainerEncoding(s^.ST_ENCO,cc.Encoding);
  If ErrorState = ENCODING_ERROR Then Exit;
  If Not AreCompatibleEncodings(s^.ST_CONT,s^.ST_ENCO) Then
    SyntaxError('incompatible character encodings') { FIXME: could be a "user input" error  }
End;

{ append s2 to s1 }
Procedure Str_Concat( s1,s2 : StrPtr );
Var 
  Iter : StrIter;
  cc : TChar;
Begin
  StrIter_ToStart(Iter,s2);
  While (ErrorState <> ENCODING_ERROR) And StrIter_NextChar(Iter,cc) Do 
    Str_AppendChar(s1,cc)
End;

{ append a Pascal string of 7-bit chars to a Str }
Procedure Str_Append( s : StrPtr; ps : TString );
Var 
  i : TStringSize;
  cc : TChar;
Begin
  CheckCondition(s <> Nil, 'Cannot append to a Nil string');
  For i := 1 To Length(ps) Do
  Begin
    ASCIIChar(cc,ps[i]);
    Str_AppendChar(s,cc)
  End
End;

{ new string with a Pascal string made of 1-byte characters as an initial 
 value }
Function Str_NewFromShortString( ps : TString ) : StrPtr;
Var 
  s : StrPtr;
Begin
  s := Str_New(ASCII);
  Str_Append(s,ps);
  Str_NewFromShortString := s
End;

{ create a new string from a series of bytes stored into a Pascal string; the
 function decomposes the bytes into TChars; the encoding context is Enc; usages
 must be limited to cases of string coming from the environment (command line or
 OS function returning file paths), encoded in a possibly non-ASCII encoding }
Function Str_NewFromBytes( bytes : TString; Enc : TEncoding ) : StrPtr;
Var 
  s : StrPtr;
  cc : TChar;
Begin
  s := Str_New(Enc);
  While (ErrorState <> ENCODING_ERROR) And 
      (Length(bytes) > 0) And GetOneTCharNL(bytes,cc,Enc) Do
    Str_AppendChar(s,cc);
  Str_NewFromBytes := s
End;

{ clone a string }
Function Str_Clone( s : StrPtr ) : StrPtr;
Begin
  Str_Clone := StrPtr(DeepCopy(TObjectPtr(s)))
End;

{ in-place quote and escape a string; double the quote char }
Procedure Str_QuoteAndEscape( s : StrPtr; quote : Char );
Var 
  s2 : StrPtr;
  Iter : StrIter;
  cc : TChar;
Begin
  s2 := Str_Clone(s);
  Str_Zap(s);
  Str_Append(s,quote);
  StrIter_ToStart(Iter,s2);
  While (ErrorState <> ENCODING_ERROR) And StrIter_NextChar(Iter,cc) Do 
  Begin
    If cc.Bytes = quote Then
    Begin
      Str_Append(s,quote);
      Str_Append(s,quote)
    End
    Else If cc.Bytes = NewLine Then { FIXME: use the output CRLF mode? }
      Str_Append(s,'\n')
    Else If cc.Bytes = #$08 Then
      Str_Append(s,'\b')
    Else If cc.Bytes = #$09 Then
      Str_Append(s,'\t')
    Else
      Str_AppendChar(s,cc)
  End;
  Str_Append(s,quote)
End;

{ double quote and escape a string }
Procedure Str_DoubleQuoteAndEscape( s : StrPtr );
Begin
  Str_QuoteAndEscape(s,'"')
End;

{ single quote and escape a string }
Procedure Str_SingleQuoteAndEscape( s : StrPtr );
Begin
  Str_QuoteAndEscape(s,'''')
End;

{ compare s1 with s2 }
Function Str_Comp( s1,S2 : StrPtr) : TComp;
Var
  Iter1,Iter2 : StrIter;
  cc1,cc2 : TChar;
  cp1,cp2: TCodePoint;
  cmp : TComp;
Begin
  CheckCondition((s1 <> Nil) And (s2 <> Nil),'Str_Comp: Nil');
  cmp := CompUndefined;
  If s1 = s2 Then { same object }
    cmp := CompEqual
  Else
  Begin
    StrIter_ToStart(Iter1,s1);
    StrIter_ToStart(Iter2,s2);
    While (cmp = CompUndefined) And  
        StrIter_NextChar(Iter1,cc1) And StrIter_NextChar(Iter2,cc2) Do
    Begin
      If Not TCharToCodePoint(cc1,cp1) Then
        Exit;
      If Not TCharToCodePoint(cc2,cp2) Then
        Exit;
      If cp1 < cp2 Then
        cmp := CompLower
      Else If cp1 > cp2 Then
        cmp := CompGreater
    End;
    If cmp = CompUndefined Then
    Begin
      If Str_Length(s1) = Str_Length(s2) Then
        cmp := CompEqual
      Else If Str_Length(s1) > Str_Length(s2) Then
        cmp := CompGreater
      Else
        cmp := CompLower
    End
  End;
  Str_Comp := cmp
End;

{ are two strings equal? }
Function Str_Equal( s1,s2 : StrPtr ) : Boolean;
Begin
  Str_Equal := Str_Comp(s1,s2) = CompEqual
End;

{ is a string equal to a Pascal string made of 1-byte chars? }
Function Str_EqualToShortString( s : StrPtr; c : TString ) : Boolean;
Begin
  Str_EqualToShortString := (Str_Length(s) = Length(c)) 
      And (Str_GetShortStringTruncate(s) = c)
End;

{ delete the last char of a string; for efficiency reasons encoding is not 
 updated, even if it could (e.g. when the operation deletes the only multi-byte 
 character of a UTF-8 string); FIXME: update encoding by tracking the number of
 multi-byte characters }
Procedure Str_DeleteLastChar( s : StrPtr );
Var
  sd : StrDataPtr;
Begin
  CheckCondition(Str_Length(s) > 0,'Str_DeleteLastChar: empty');
  sd := Str_GetLastStrData(s);
  if (Str_NumberOfStrData(s) > 1) And (StrData_Len(sd) = 1) Then
    Str_DeleteLastData(s)
  Else
  Begin
    StrData_DeleteLastChar(sd);
    s^.ST_TLEN := s^.ST_TLEN - 1
  End
End;

{ delete last char of a string until empty or any of the 1-byte characters in 
 a set is reached }
Procedure Str_DeleteLastCharUntil( s : StrPtr; StopChars : CharSet );
Var 
  cc : TChar;
Begin
  While Str_LastChar(s,cc) And Not IsIn(cc,StopChars) Do 
    Str_DeleteLastChar(s)
End;

{-----------------------------------------------------------------------}
{ methods: display on screen (and to echo file)                         }
{-----------------------------------------------------------------------}

{ write a string to Crt }
Procedure Str_CWrite( s : StrPtr );
Var
  Iter : StrIter;
  cc : TChar;
Begin
  StrIter_ToStart(Iter,s);
  While StrIter_NextChar(Iter,cc) Do 
    CWriteChar(cc)
End;

End.
