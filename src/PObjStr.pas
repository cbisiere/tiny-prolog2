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

{$R+}{ Range checking on. }
{$V-}{ No strict type checking for strings. }

Unit PObjStr;

Interface

Uses
  ShortStr,
  Num,
  Errs,
  Chars,
  Trace,
  Common,
  Memory,
  PObj,
  PObjIO;

{-----------------------------------------------------------------------}
{ type                                                                  }
{-----------------------------------------------------------------------}

{ maximum number of characters in a chunk; must be at most 255; since most
 strings are very short, we set a small value to save space }
Const
  StrDataMaxSize = 15;
Type 
  TStrDataLength = 0..StrDataMaxSize;
  TStrData = String[StrDataMaxSize];

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
    ST_TLEN: Longint { total length }
  End;

  TObjStrData = Record
    PO_META: TObjMeta;
    { deep copied: }
    SD_PREV: StrDataPtr; { previous chunk or Nil }
    SD_NEXT: StrDataPtr; { next chunk or Nil }
    { extra data: }
    SD_DATA: TStrData { storage }
  End;

  { char iterator within a string }
  StrIter = Record
    Data : StrDataPtr; { current data chunk }
    LastRead : TStrDataLength { index of the last char read }
  End;


Function Str_New: StrPtr;
Function Str_Length(s: StrPtr): longint;
Function Str_GetShortString(s: StrPtr): TString;
Function Str_AsShortString(s: StrPtr): TString;
Procedure Str_Append( s : StrPtr; ps : TString );
Procedure Str_AppendChar( s : StrPtr; cc : TChar );
Function Str_NewFromShortString(ps: TString): StrPtr;
Function Str_NewFromBytes( bytes : TString ) : StrPtr;
Procedure Str_Concat(s1, s2: StrPtr);
Function Str_Clone(s: StrPtr): StrPtr;
Procedure Str_Enclose( s : StrPtr; QStart,QEnd : TString );
Procedure Str_Quote( s : StrPtr );
Procedure Str_DoubleQuoteAndEscape( s : StrPtr );
Procedure Str_DeleteLastChar(s: StrPtr);
Procedure Str_DeleteFirstChar(s: StrPtr);
Function Str_Comp(s1, S2: StrPtr): TComp;
Function Str_Equal(s1, s2: StrPtr): boolean;
Function Str_EqualToShortString(s: StrPtr; c: TString): boolean;
Function Str_StartsWith(s: StrPtr; E: CharSet): boolean;
Function Str_EndsWith(s: StrPtr; E: CharSet): boolean;
Procedure Str_CWrite(s: StrPtr);
Procedure Str_Write( f : StreamPtr; s : StrPtr) ;
Procedure Str_Writeln( f : StreamPtr; s : StrPtr) ;

Procedure StrIter_ToStart( Var Iter : StrIter; s : StrPtr );
Procedure StrIter_ToEnd( Var Iter : StrIter; s : StrPtr );
Function StrIter_NextChar( Var Iter : StrIter; Var cc : TChar ) : Boolean;
Function StrIter_PrevChar( Var Iter : StrIter; Var cc : TChar ) : Boolean;

Implementation
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------}
{ constructor                                                           }
{-----------------------------------------------------------------------}

{ new string data }
Function StrData_New( ps : TStrData ) : StrDataPtr;
Var 
  sda: StrDataPtr;
  ptr: TObjectPtr Absolute sda;
Begin
  ptr := NewRegisteredPObject(SD, SizeOf(TObjStrData), 2, True, 2);
  With sda^ Do
  Begin
    SD_PREV := Nil;
    SD_NEXT := Nil;
    SD_DATA := ps
  End;
  StrData_New := sda
End;

{ set or reset a string to be an empty string; old string data will be GC'ed }
Procedure Str_Zap( s : StrPtr );
Begin
  With s^ Do
  Begin
    ST_FDAT := StrData_New('');
    ST_LDAT := ST_FDAT;
    ST_NDAT := 1;
    ST_TLEN := 0
  End
End;

{ new string }
Function Str_New : StrPtr;
Var 
  s: StrPtr;
  ptr: TObjectPtr Absolute s;
Begin
  ptr := NewRegisteredPObject(ST, SizeOf(TObjStr), 2, True, 2);
  Str_Zap(s);
  Str_New := s
End;


{-----------------------------------------------------------------------}
{ methods                                                               }
{-----------------------------------------------------------------------}

{ return the length of a string }
Function Str_Length( s : StrPtr ) : LongInt;
Begin
  Str_Length := s^.ST_TLEN
End;

{ return the value of the first chunk in a string }
Function Str_GetFirstData( s : StrPtr ) : TStrData;
Begin
  Str_GetFirstData := s^.ST_FDAT^.SD_DATA
End;

{ return the (possibly truncated) value of a str as a Pascal string }
Function Str_GetShortString( s : StrPtr ) : TString;
Var
  ps : TString;
  sd : StrDataPtr;
  n : TStrDataLength;
Begin
  ps := '';
  sd := s^.ST_FDAT;
  While (Length(ps) < StringMaxSize) And (sd <> Nil) Do
  Begin
    n := Min(Length(sd^.SD_DATA),StringMaxSize-Length(ps));
    ps := ps + Copy(sd^.SD_DATA,1,n);
    sd := sd^.SD_NEXT
  End;
  Str_GetShortString := ps
End;

{ return the value of a string as a Pascal string }
Function Str_AsShortString( s : StrPtr ) : TString;
Begin
  CheckCondition(Str_Length(s) <= StringMaxSize,
    'string is too long to fit in a Pascal string');
  Str_AsShortString := Str_GetShortString(s)
End;

{ append a chunk to a string }
Procedure Str_AppendData( s : StrPtr; sd : StrDataPtr );
Begin
  With s^ Do
  Begin
    ST_LDAT^.SD_NEXT := sd;
    sd^.SD_PREV := ST_LDAT;
    ST_LDAT := sd;
    ST_NDAT := ST_NDAT + 1;
    ST_TLEN := ST_TLEN + Length(sd^.SD_DATA)
  End
End;

{ append a Pascal string to a string; in the Pascal string, each 1-byte char is 
 assumed to be a valid TChar }
Procedure Str_Append( s : StrPtr; ps : TString );
Var 
  n : TStrDataLength; { number of free chars in the current chunk }
  sd : StrDataPtr; { additional chunk when necessary }
  c : TStrData;
Begin
  CheckCondition(s <> Nil, 'Cannot append to a Nil string');
  If Length(ps) > 0 Then
  Begin
    With s^.ST_LDAT^ Do
    Begin
      { fill the last chunk as much as possible }
      n := Min(Length(ps),StrDataMaxSize-Length(SD_DATA));
      If n > 0 Then
      Begin
        c := Copy(ps,1,n);
        SD_DATA := SD_DATA + c;
        s^.ST_TLEN := s^.ST_TLEN + Length(c);
        Delete(ps,1,n)
      End;
      { append new chunks with the remaining part of the Pascal string if any }
      While Length(ps) > 0 Do
      Begin
        n := Min(Length(ps),StrDataMaxSize);
        c := Copy(ps,1,n);
        Delete(ps,1,n);
        sd := StrData_New(c);
        Str_AppendData(s,sd)
      End
    End
  End
End;

{ append a TChar to a string }
Procedure Str_AppendChar( s : StrPtr; cc : TChar );
Begin
  Str_Append(s,cc)
End;

{ new string with a Pascal string (made of 1-byte TChars) as an initial 
 value }
Function Str_NewFromShortString( ps : TString ) : StrPtr;
Var 
  s : StrPtr;
  i : TStringSize;
Begin
  s := Str_New;
  For i := 1 To Length(ps) Do
    Str_AppendChar(s,ps[i]);
  Str_NewFromShortString := s
End;

{ create a new string from a series of bytes stored into a Pascal string; the
 function decomposes the bytes into TChars }
Function Str_NewFromBytes( bytes : TString ) : StrPtr;
Var 
  s : StrPtr;
  cc : TChar;
  Enc : TEncoding;
Begin
  s := Str_New;
  Enc := UNDECIDED;
  While (Length(bytes) > 0) And GetOneTCharNL(bytes,cc,Enc) And Not Error Do
    Str_AppendChar(s,cc);
  Str_NewFromBytes := s
End;

{ concatenate two strings into the first one }
Procedure Str_Concat( s1,s2 : StrPtr );
Var 
  sd : StrDataPtr;
Begin
  sd := s2^.ST_FDAT;
  While sd <> Nil Do
  Begin
    Str_Append(s1, sd^.SD_DATA);
    sd := sd^.SD_NEXT
  End
End;

{ clone a string }
Function Str_Clone( s : StrPtr ) : StrPtr;
Var 
  so : TObjectPtr Absolute s;
  r : StrPtr;
  ro : TObjectPtr Absolute r;
Begin
  ro := DeepCopy(so);
  Str_Clone := r
End;

{ in-place enclose a string within an opening and ending Pascal string }
Procedure Str_Enclose( s : StrPtr; QStart,QEnd : TString );
Var 
  s2 : StrPtr;
Begin
  s2 := Str_Clone(s);
  Str_Zap(s);
  Str_Append(s,QStart);
  Str_Concat(s,s2);
  Str_Append(s,QEnd)
End;

{ in-place escape a string }
Procedure Str_DoubleQuoteAndEscape( s : StrPtr );
Var 
  s2 : StrPtr;
  sd : StrDataPtr;
  i : TStringSize;
  cc : TChar;
Begin
  s2 := Str_Clone(s);
  Str_Zap(s);
  Str_Append(s,'"');
  sd := s2^.ST_FDAT;
  While sd <> Nil Do
  Begin
    For i := 1 To Length(sd^.SD_DATA) Do
    Begin
      Case sd^.SD_DATA[i] Of { TBC }
      NewLine:
        cc := '\n';
      Else
        cc := sd^.SD_DATA[i]
      End;
      Str_Append(s,cc);
    End;
    sd := sd^.SD_NEXT
  End;
  Str_Append(s,'"');
End;

{ single quote a string }
{ FIXME: escape? double single quotes inside the path }
Procedure Str_Quote( s : StrPtr );
Begin
  Str_Enclose(s,'''','''')
End;

{ delete the last char from a string; removed chunk, if any, will be GC'ed }
Procedure Str_DeleteLastChar( s : StrPtr );
Var 
  len : TStrDataLength;
Begin
  CheckCondition(Str_Length(s) > 0,
    'Cannot delete the last char of an empty string');
  With s^.ST_LDAT^ Do
  Begin
    Delete(SD_DATA, Length(SD_DATA), 1);
    s^.ST_TLEN := s^.ST_TLEN - 1;
    len := Length(SD_DATA)
  End;
  { remove the last chunk if empty }
  With s^ Do
  Begin
    If (len = 0) And (ST_NDAT > 1) Then
    Begin
      ST_LDAT := ST_LDAT^.SD_PREV;
      ST_LDAT^.SD_NEXT := Nil;
      ST_NDAT := ST_NDAT - 1
    End
  End
End;

{ delete the first char from a string }
Procedure Str_DeleteFirstChar( s : StrPtr );
Var 
  s2 : StrPtr;
  sd : StrDataPtr;
  chunk : TStrData;
Begin
  CheckCondition(Str_Length(s) > 0,
    'Cannot delete the first char of an empty string');
  { clone and zap, then work on the string passed as parameter }
  { TODO: OPT "steal" the chunks instead of cloning }
  s2 := Str_Clone(s);
  Str_Zap(s);
  { copy the first chunk without its first char }
  chunk := Str_GetFirstData(s2);
  Delete(chunk,1,1);
  Str_Append(s,chunk);
  { append the other chunks }
  sd := s2^.ST_FDAT^.SD_NEXT;
  While sd <> Nil Do
  Begin
    Str_Append(s,sd^.SD_DATA); { append, compacting }
    sd := sd^.SD_NEXT
  End;
End;

{ compare two string data; TODO: UTF8 }
Function Str_DataComp( sd1,sd2 : StrDataPtr ) : TComp;
Var 
  Cmp : TComp;
Begin
  If (sd1 = Nil) And (sd2 = Nil) Then
    Cmp := CompEqual
  Else If (sd1 = Nil) And (sd2 <> Nil) Then { see invariant 2 }
         Cmp := CompLower
  Else If (sd1 <> Nil) And (sd2 = Nil) Then
         Cmp := CompGreater
  Else If sd1^.SD_DATA < sd2^.SD_DATA Then
         Cmp := CompLower
  Else If sd1^.SD_DATA > sd2^.SD_DATA Then
         Cmp := CompGreater
  Else
    Cmp := Str_DataComp(sd1^.SD_NEXT, sd2^.SD_NEXT);
  Str_DataComp := Cmp
End;

{ compare s1 with s2 }
Function Str_Comp( s1,S2 : StrPtr) : TComp;
Begin
  Str_Comp := Str_DataComp(s1^.ST_FDAT, s2^.ST_FDAT)
End;

{ are two strings equal? }
Function Str_Equal( s1,s2 : StrPtr ) : Boolean;
Begin
  Str_Equal := Str_Comp(s1, s2) = CompEqual
End;

{ is a string equal to a Pascal string? }
Function Str_EqualToShortString( s : StrPtr; c : TString ) : Boolean;
Begin
  Str_EqualToShortString := (Str_Length(s) = Length(c)) 
      And (Str_GetShortString(s) = c)
End;

{ string starts with a char in a set; not UTF-8 aware }

Function Str_StartsWith( s : StrPtr; E : CharSet ) : Boolean;
Var 
  b : Boolean;
  c : TStrData;
Begin
  c := Str_GetFirstData(s);
  b := Length(c) > 0;
  If b Then
    b := c[1] In E;
  Str_StartsWith := b
End;

{ string ends with a char in a set; not UTF-8 aware }
Function Str_EndsWith( s : StrPtr; E : CharSet ) : Boolean;
Var 
  b : Boolean;
Begin
  b := Str_Length(s) > 0;
  If b Then
    With s^.ST_LDAT^ Do
      b := SD_DATA[Length(SD_DATA)] In E;
  Str_EndsWith := b
End;

{-----------------------------------------------------------------------}
{ methods: display on screen (and to echo file)                         }
{-----------------------------------------------------------------------}

{ write all string data to Crt }
Procedure Str_CWriteData( sd : StrDataPtr );
Begin
  If sd <> Nil Then
  Begin
    With sd^ Do
    Begin
      CWrite(SD_DATA);
      Str_CWriteData(SD_NEXT)
    End
  End
End;

{ write a string to Crt }
Procedure Str_CWrite( s : StrPtr );
Begin
  Str_CWriteData(s^.ST_FDAT)
End;

{-----------------------------------------------------------------------}
{ methods: write to file                                                }
{-----------------------------------------------------------------------}

{ write all string data to output file f }
Procedure Str_WriteData( f : StreamPtr; sd : StrDataPtr );
Begin
  If sd <> Nil Then
  Begin
    With sd^ Do
    Begin
      Stream_WriteShortString(f,SD_DATA);
      Str_WriteData(f,SD_NEXT)
    End
  End
End;

{ write a string to a stream }
Procedure Str_Write( f : StreamPtr; s : StrPtr ) ;
Begin
  Str_WriteData(f,s^.ST_FDAT)
End;

{ writeln a string to a stream }
Procedure Str_Writeln( f : StreamPtr; s : StrPtr ) ;
Begin
  Str_Write(f,s);
  Stream_WritelnShortString(f,'')
End;

{-----------------------------------------------------------------------}
{ iterators                                                             }
{-----------------------------------------------------------------------}

{ initialize a forward iterator to iterate on a string's characters }
Procedure StrIter_ToStart( Var Iter : StrIter; s : StrPtr );
Begin
  Iter.Data := s^.ST_FDAT;
  Iter.LastRead := 0
End;

{ initialize a backward iterator to iterate on a string's characters }
Procedure StrIter_ToEnd( Var Iter : StrIter; s : StrPtr );
Begin
  Iter.Data := s^.ST_LDAT;
  Iter.LastRead := 0 { means: "nothing read yet" }
End;

{ get the next char (if any) in a string; return False if the iterator has 
 reached the end of the string }
Function StrIter_NextChar( Var Iter : StrIter; Var cc : TChar ) : Boolean;
Begin
  StrIter_NextChar := False;
  With Iter Do
  Begin
    If LastRead >= Length(Data^.SD_DATA) Then
    Begin
      Data := Data^.SD_NEXT;
      LastRead := 0;
      If Data = Nil Then
        Exit
    End;
    LastRead := LastRead + 1;
    cc := Data^.SD_DATA[LastRead] { FIXME: handle UTF8 }
  End;
  StrIter_NextChar := True
End;


{ get the previous char (if any) in a string; return False if the iterator has 
 reached the beginning of the string }
Function StrIter_PrevChar( Var Iter : StrIter; Var cc : TChar ) : Boolean;
Begin
  StrIter_PrevChar := False;
  With Iter Do
  Begin
    Case LastRead Of
    0: { first iteration; the string might be empty }
      Begin
        LastRead := Length(Data^.SD_DATA);
        If LastRead = 0 Then
          Exit
      End;
    1: { current data chunk was fully iterated }
      Begin
        Data := Data^.SD_PREV;
        If Data = Nil Then
          Exit;
        LastRead := Length(Data^.SD_DATA)
      End;
    Else { in the 'middle' of the current data chunk: previous character }
      LastRead := LastRead - 1
    End;
    cc := Data^.SD_DATA[LastRead] { FIXME: handle UTF8 }
  End;
  StrIter_PrevChar := True
End;

End.
