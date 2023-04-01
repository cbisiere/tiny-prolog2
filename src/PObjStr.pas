{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObjStr.pas                                                }
{   Author      : Christophe Bisière                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                      L O N G   S T R I N G S                               }
{                                                                            }
{----------------------------------------------------------------------------}

{-----------------------------------------------------------------------}
{ type                                                                  }
{-----------------------------------------------------------------------}

Const
  StrChunkSize = 255; { must be at least the maximum length of AnyStr }

Type
  TStrLength = 0..StrChunkSize;

{ GC-managed string that can grow }
Type 
  StrPtr = ^TObjStr;
  TObjStr = Record
    PO_META : TObjMeta;
    { deep copied: }
    ST_NEXT : StrPtr; { next chunk or Nil }
    ST_LAST : StrPtr; { last chunk (field updated for the first chunk only) }
    { extra data: }
    ST_DATA : String[StrChunkSize] { chunk }
  End;


{-----------------------------------------------------------------------}
{ constructor                                                           }
{-----------------------------------------------------------------------}

{ new string }
Function NewString : StrPtr;
Var 
  s : StrPtr;
  ptr : TPObjPtr Absolute s;
Begin
  ptr := NewPrologObject(ST, SizeOf(TObjStr), 1, 1);
  With s^ Do
  Begin
    ST_NEXT := Nil;
    ST_LAST := s;
    ST_DATA := ''
  End;
  NewString := s
End;

{-----------------------------------------------------------------------}
{ methods                                                               }
{-----------------------------------------------------------------------}

Procedure StrAppend( s : StrPtr; c : AnyStr );
Var 
  n : TStrLength; { number of free chars in the current chunk }
  s2 : StrPtr; { additional chunk when necessary }
Begin
  CheckCondition(s<>Nil,'Cannot append to a Nil string');
  If Length(c)>0 Then
    With s^.ST_LAST^ Do
    Begin
      n := StrChunkSize-Length(ST_DATA);
      ST_DATA := ST_DATA + Copy(c,1,n);
      If Length(c) > n Then
      Begin
        { append a chunk; since maxlen(data) >= maxlen(AnyStr), a single chunk 
          is always enough }
        s2 := NewString;
        ST_NEXT := s2;
        s^.ST_LAST := s2;
        s2^.ST_DATA := Copy(c,n+1,StrChunkSize)
      End
    End
End;

Procedure StrWrite( s : StrPtr );
Begin
  If s<>Nil Then
  Begin
    Write(s^.ST_DATA);
    StrWrite(s^.ST_NEXT)
  End
End;

Procedure StrWriteln( s : StrPtr );
Begin
  StrWrite(s);
  Writeln
End;
