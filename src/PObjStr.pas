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

Type
  CharSet   = Set Of Char;

{-----------------------------------------------------------------------}
{ type                                                                  }
{-----------------------------------------------------------------------}

Type
  TStrLength = 0..AnyStrMaxSize; { maximum number of characters in a chunk }

{ GC-managed string that can grow; 
  invariants: 1) all chunks but the last are full; 2) no chunks are empty, 
  except when the string is empty }
Type 
  StrPtr = ^TObjStr;
  StrDataPtr = ^TObjStrData;

  TObjStr = Record
    PO_META : TObjMeta;
    { deep copied: }
    ST_FDAT : StrDataPtr; { first chunk }
    ST_LDAT : StrDataPtr; { last chunk }
    { extra data: }
    ST_NDAT : LongInt; { number of chunks }
    ST_TLEN : LongInt { total length }
  End;

  TObjStrData = Record
    PO_META : TObjMeta;
    { deep copied: }
    SD_PREV : StrDataPtr; { previous chunk or Nil }
    SD_NEXT : StrDataPtr; { next chunk or Nil }
    { extra data: }
    SD_DATA : AnyStr { storage: Pascal string }
  End;

{-----------------------------------------------------------------------}
{ constructor                                                           }
{-----------------------------------------------------------------------}

{ new string data }
Function NewStringData( ps : AnyStr ) : StrDataPtr;
Var 
  sda : StrDataPtr;
  ptr : TPObjPtr Absolute sda;
Begin
  ptr := NewRegisteredObject(SD,2,True,2);
  With sda^ Do
  Begin
    SD_PREV := Nil;
    SD_NEXT := Nil;
    SD_DATA := ps
  End;
  NewStringData := sda
End;

{ new string }
Function NewString : StrPtr;
Var 
  s : StrPtr;
  ptr : TPObjPtr Absolute s;
Begin
  ptr := NewRegisteredObject(ST,2,True,2);
  With s^ Do
  Begin
    ST_FDAT := NewStringData('');
    ST_LDAT := ST_FDAT;
    ST_NDAT := 1;
    ST_TLEN := 0
  End;
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
Function StrGetFirstData( s : StrPtr ) : AnyStr;
Begin
  StrGetFirstData := s^.ST_FDAT^.SD_DATA
End;

{ return the value of a string as a Pascal string }
Function StrGetString( s : StrPtr ) : AnyStr;
Begin
  CheckCondition(StrLength(s)<=AnyStrMaxSize,
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
Procedure StrAppend( s : StrPtr; c : AnyStr );
Var 
  n : TStrLength; { number of free chars in the current chunk }
  sd : StrDataPtr; { additional chunk when necessary }
  c1 : AnyStr; { part of c that can be stored in the current chunk }
Begin
  CheckCondition(s<>Nil,'Cannot append to a Nil string');
  If Length(c)>0 Then
  Begin
    With s^.ST_LDAT^ Do
    Begin
      { fill the last chunk as much as possible }
      n := AnyStrMaxSize-Length(SD_DATA);
      c1 := Copy(c,1,n);
      SD_DATA := SD_DATA + c1;
      s^.ST_TLEN := s^.ST_TLEN + Length(c1);
      { append a new chunk with the remaining part if any }
      If Length(c) > n Then
      Begin
        sd := NewStringData(Copy(c,n+1,AnyStrMaxSize));
        StrAppendData(s,sd)
      End
    End
  End
End;

{ new string with a Pascal string as an initial value }
Function NewStringFrom( ps : AnyStr ) : StrPtr;
Var s : StrPtr;
Begin
  s := NewString;
  StrAppend(s,ps);
  NewStringFrom := s
End;

{ append a char to a string }
Procedure StrAppendChar( s : StrPtr; c : Char );
Begin
  StrAppend(s,c)
End;

{ append a carriage return to a string }
Procedure StrAppendCR( s : StrPtr );
Begin
  StrAppend(s,CRLF)
End;

{ delete the last char from a string; remove chunk, if any, will be GC'ed }
Procedure StrDeleteLastChar( s : StrPtr );
Var len : TStrLength;
Begin
  CheckCondition(StrLength(s)>0,'Cannot delete the last char of an empty string');
  With s^.ST_LDAT^ Do
  Begin
    Delete(SD_DATA,Length(SD_DATA),1);
    s^.ST_TLEN := s^.ST_TLEN - 1;
    len := Length(SD_DATA)
  End;
  { remove the last chunk if empty }
  With s^ Do
    If (len=0) And (ST_NDAT>1) Then
    Begin
      ST_LDAT := ST_LDAT^.SD_PREV;
      ST_LDAT^.SD_NEXT := Nil;
      ST_NDAT := ST_NDAT - 1
    End
End;

{ concatenate two strings into the first one }
Procedure StrConcat( s1,s2 : StrPtr );
Var sd : StrDataPtr;
Begin
  sd := s2^.ST_FDAT;
  While sd<>Nil Do
  Begin 
    StrAppend(s1,sd^.SD_DATA);
    sd := sd^.SD_NEXT
  End
End;

{ clone a string }
Function StrClone( s : StrPtr ) : StrPtr;
Var 
  so : TPObjPtr Absolute s;
  r : StrPtr;
  ro : TPObjPtr Absolute r;
Begin
  ro := DeepCopy(so);
  StrClone := r
End;

{ are two string data equal? }
Function StrDataEqual( sd1,sd2 : StrDataPtr ) : Boolean;
Var eq : Boolean;
Begin
  eq := Not ((sd1=Nil) Xor (sd2=Nil));
  If eq And (sd1<>Nil) And (sd2<>Nil) Then
  Begin
    eq := sd1^.SD_DATA = sd2^.SD_DATA;
    If eq Then
      eq := StrDataEqual(sd1^.SD_NEXT,sd2^.SD_NEXT)
  End;
  StrDataEqual := eq
End;

{ are two strings equal? }
Function StrEqual( s1,s2 : StrPtr ) : Boolean;
Begin
  StrEqual := StrDataEqual(s1^.ST_FDAT,s2^.ST_FDAT)
End;

{ is a string equal to a Pascal string? }
Function StrEqualTo( s : StrPtr; c : AnyStr ) : Boolean;
Begin
  StrEqualTo := (StrGetFirstData(s)=c) And (s^.ST_NDAT=1)
End;

{ string starts with a char in a set; not UTF-8 aware }
Function StrStartsWith( s : StrPtr; E : CharSet ) : Boolean;
Var 
  b : Boolean;
  c : AnyStr;
Begin
  c := StrGetFirstData(s);
  b := Length(c)>0;
  If b Then
    b := c[1] In E;
  StrStartsWith := b
End;

{ string ends with a char in a set; not UTF-8 aware }
Function StrEndsWith( s : StrPtr; E : CharSet ) : Boolean;
Var b : Boolean;
Begin
  b := StrLength(s)>0;
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
  If sd<>Nil Then
    With sd^ Do
    Begin
      CWrite(SD_DATA);
      StrDataWrite(SD_NEXT)
    End
End;

{ write all string data to the current output file }
Procedure StrDataWriteToCurrentFile( sd : StrDataPtr );
Begin
  If sd<>Nil Then
    With sd^ Do
    Begin
      WriteToCurrentOutput(SD_DATA);
      StrDataWriteToCurrentFile(SD_NEXT)
    End
End;

{ write a string }
Procedure StrWrite( s : StrPtr );
Begin
  StrDataWrite(s^.ST_FDAT)
End;

{ write a string to the current output file }
Procedure StrWriteToCurrentFile( s : StrPtr );
Begin
  StrDataWriteToCurrentFile(s^.ST_FDAT)
End;

