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
  Errs,
  OStack,
  Trace,
  Common,
  Memory,
  PObj;

{-----------------------------------------------------------------------}
{ type                                                                  }
{-----------------------------------------------------------------------}

{ TODO: allows for shorter storage string }

Type 
  TStrLength = 0..StringMaxSize; { maximum number of characters in a chunk }

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
    ST_NDAT: longint; { number of chunks }
    ST_TLEN: longint { total length }
  End;

  TObjStrData = Record
    PO_META: TObjMeta;
    { deep copied: }
    SD_PREV: StrDataPtr; { previous chunk or Nil }
    SD_NEXT: StrDataPtr; { next chunk or Nil }
    { extra data: }
    SD_DATA: TString { storage: Pascal string }
  End;


Function NewString: StrPtr;
Function StrLength(s: StrPtr): longint;
Function StrGetFirstData(s: StrPtr): TString;
Function StrGetString(s: StrPtr): TString;
Procedure StrAppend(s: StrPtr; c: TString);
Function NewStringFrom(ps: TString): StrPtr;
Procedure StrAppendCR(s: StrPtr);
Procedure StrConcat(s1, s2: StrPtr);
Function StrClone(s: StrPtr): StrPtr;
Procedure StrEnclose( s : StrPtr; QStart,QEnd : TString );
Procedure StrQuote( s : StrPtr );
Procedure StrDeleteLastChar(s: StrPtr);
Procedure StrDeleteFirstChar(s: StrPtr);
Function StrComp(s1, S2: StrPtr): TComp;
Function StrEqual(s1, s2: StrPtr): boolean;
Function StrEqualTo(s: StrPtr; c: TString): boolean;
Function StrStartsWith(s: StrPtr; E: CharSet): boolean;
Function StrEndsWith(s: StrPtr; E: CharSet): boolean;
Procedure StrWrite(s: StrPtr);
Procedure StrWriteToCurrentFile(s: StrPtr);

Implementation
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------}
{ constructor                                                           }
{-----------------------------------------------------------------------}

{ new string data }
Function NewStringData( ps: TString ) : StrDataPtr;
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
  NewStringData := sda
End;

{ set or reset a string to be an empty string }
Procedure ZapString( s: StrPtr );
Begin
  With s^ Do
  Begin
    ST_FDAT := NewStringData('');
    ST_LDAT := ST_FDAT;
    ST_NDAT := 1;
    ST_TLEN := 0
  End
End;

{ new string }
Function NewString : StrPtr;
Var 
  s: StrPtr;
  ptr: TObjectPtr Absolute s;
Begin
  ptr := NewRegisteredPObject(ST, SizeOf(TObjStr), 2, True, 2);
  ZapString(s);
  NewString := s
End;


{-----------------------------------------------------------------------}
{ methods                                                               }
{-----------------------------------------------------------------------}

{ return the length of a string }
Function StrLength( s : StrPtr ) : LongInt;
Begin
  StrLength := s^.ST_TLEN
End;

{ return the value of the first Pascal string in a string }
Function StrGetFirstData( s : StrPtr ) : TString;
Begin
  StrGetFirstData := s^.ST_FDAT^.SD_DATA
End;

{ return the value of a string as a Pascal string }

Function StrGetString( s : StrPtr ) : TString;
Begin
  CheckCondition(StrLength(s) <= StringMaxSize,
    'string is too long to fit in a Pascal string');
  StrGetString := StrGetFirstData(s)
End;

{ append a chunk to a string }
Procedure StrAppendData( s : StrPtr; sd : StrDataPtr );
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

{ append a Pascal string to a string }
Procedure StrAppend( s : StrPtr; c : TString );
Var 
  n: TStrLength; { number of free chars in the current chunk }
  sd: StrDataPtr; { additional chunk when necessary }
  c1: TString; { part of c that can be stored in the current chunk }
Begin
  CheckCondition(s <> Nil, 'Cannot append to a Nil string');
  If Length(c) > 0 Then
  Begin
    With s^.ST_LDAT^ Do
    Begin
      { fill the last chunk as much as possible }
      n := StringMaxSize - Length(SD_DATA);
      c1 := Copy(c, 1, n);
      SD_DATA := SD_DATA + c1;
      s^.ST_TLEN := s^.ST_TLEN + Length(c1);
      { append a new chunk with the remaining part if any }
      If Length(c) > n Then
      Begin
        sd := NewStringData(Copy(c, n + 1, StringMaxSize));
        StrAppendData(s, sd)
      End
    End
  End
End;

{ new string with a Pascal string as an initial value }
Function NewStringFrom( ps : TString ) : StrPtr;
Var 
  s: StrPtr;
Begin
  s := NewString;
  StrAppend(s, ps);
  NewStringFrom := s
End;

{ append a char to a string }
Procedure StrAppendChar( s : StrPtr; c : char );
Begin
  StrAppend(s, c)
End;

{ append a carriage return to a string }
Procedure StrAppendCR( s : StrPtr );
Begin
  StrAppend(s, CRLF)
End;

{ concatenate two strings into the first one }
Procedure StrConcat( s1,s2 : StrPtr );
Var 
  sd: StrDataPtr;
Begin
  sd := s2^.ST_FDAT;
  While sd <> Nil Do
  Begin
    StrAppend(s1, sd^.SD_DATA);
    sd := sd^.SD_NEXT
  End
End;

{ clone a string }
Function StrClone( s : StrPtr ) : StrPtr;
Var 
  so: TObjectPtr Absolute s;
  r: StrPtr;
  ro: TObjectPtr Absolute r;
Begin
  ro := DeepCopy(so);
  StrClone := r
End;

{ enclose a string within an opening and ending Pascal string }
Procedure StrEnclose( s : StrPtr; QStart,QEnd : TString );
Var 
  s2: StrPtr;
Begin
  s2 := StrClone(s);
  ZapString(s);
  StrAppend(s,QStart);
  StrConcat(s,s2);
  StrAppend(s,QEnd)
End;

{ single quote a string }
{ FIXME: escape? double single quotes inside the path }
Procedure StrQuote( s : StrPtr );
Begin
  StrEnclose(s,'''','''')
End;

{ delete the last char from a string; removed chunk, if any, will be GC'ed }
Procedure StrDeleteLastChar( s : StrPtr );
Var 
  len: TStrLength;
Begin
  CheckCondition(StrLength(s) > 0,
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
Procedure StrDeleteFirstChar( s : StrPtr );
Var 
  s2: StrPtr;
  sd: StrDataPtr;
  chunk: TString;
Begin
  CheckCondition(StrLength(s) > 0,
    'Cannot delete the first char of an empty string');
  { clone and zap, then work on the string passed as parameter }
  { TODO: OPT "steal" the chunks instead of cloning }
  s2 := StrClone(s);
  ZapString(s);
  { copy the first chunk without its first char }
  chunk := StrGetFirstData(s2);
  Delete(chunk, 1, 1);
  StrAppend(s, chunk);
  { append the other chunks }
  sd := s2^.ST_FDAT^.SD_NEXT;
  While sd <> Nil Do
  Begin
    StrAppend(s, sd^.SD_DATA); { append, compacting }
    sd := sd^.SD_NEXT
  End;
End;

{ compare two string data; TODO: UTF8 }
Function StrDataComp( sd1,sd2 : StrDataPtr ) : TComp;
Var 
  Cmp: TComp;
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
    Cmp := StrDataComp(sd1^.SD_NEXT, sd2^.SD_NEXT);
  StrDataComp := Cmp
End;

{ compare s1 with s2 }
Function StrComp( s1,S2 : StrPtr) : TComp;
Begin
  StrComp := StrDataComp(s1^.ST_FDAT, s2^.ST_FDAT)
End;

{ are two strings equal? }
Function StrEqual( s1,s2 : StrPtr ) : Boolean;
Begin
  StrEqual := StrComp(s1, s2) = CompEqual
End;

{ is a string equal to a Pascal string? }
Function StrEqualTo( s : StrPtr; c : TString ) : Boolean;
Begin
  StrEqualTo := (StrGetFirstData(s) = c) And (s^.ST_NDAT = 1)
End;

{ string starts with a char in a set; not UTF-8 aware }

Function StrStartsWith( s : StrPtr; E : CharSet ) : Boolean;
Var 
  b: Boolean;
  c: TString;
Begin
  c := StrGetFirstData(s);
  b := Length(c) > 0;
  If b Then
    b := c[1] In E;
  StrStartsWith := b
End;

{ string ends with a char in a set; not UTF-8 aware }
Function StrEndsWith( s : StrPtr; E : CharSet ) : Boolean;
Var 
  b: Boolean;
Begin
  b := StrLength(s) > 0;
  If b Then
    With s^.ST_LDAT^ Do
      b := SD_DATA[Length(SD_DATA)] In E;
  StrEndsWith := b
End;

{-----------------------------------------------------------------------}
{ methods: print                                                        }
{-----------------------------------------------------------------------}

{ write all string data }
Procedure StrDataWrite( sd : StrDataPtr );
Begin
  If sd <> Nil Then
  Begin
    With sd^ Do
    Begin
      CWrite(SD_DATA);
      StrDataWrite(SD_NEXT)
    End
  End
End;

{ write all string data to the current output file }
Procedure StrDataWriteToCurrentFile( sd : StrDataPtr );
Begin
  If sd <> Nil Then
  Begin
    With sd^ Do
    Begin
      WriteToCurrentOutput(SD_DATA);
      StrDataWriteToCurrentFile(SD_NEXT)
    End
  End
End;

{ write a string }
Procedure StrWrite( s : StrPtr );
Begin
  StrDataWrite(s^.ST_FDAT)
End;

{ write a string to the current output file }
Procedure StrWriteToCurrentFile( s : StrPtr) ;
Begin
  StrDataWriteToCurrentFile(s^.ST_FDAT)
End;

End.
