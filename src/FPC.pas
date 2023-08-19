{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : FPC.pas                                                    }
{   Author      : Christophe Bisiere                                         }
{   Date        : 2022-09-17                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{         F R E E   P A S C A L   C O M P I L E R   C O M P A T              }
{                                                                            }
{----------------------------------------------------------------------------}

{ compatibility with Turbo Pascal 3.02 }
Uses 
  Crt, Sysutils; { TrimLeft }

{ longest non dynamic string }
Const
  StringMaxSize = 255;
Type
  TString = String[StringMaxSize];
  TStringSize = 0..StringMaxSize;

Type
  LongLongInt = Real; { simulate a very long integer }
  LongReal = Extended; { highest precision real }

{ round a long real to a long integer value }
Function LongRealToLongInt( v : LongReal ) : LongInt;
Begin
  LongRealToLongInt := Round(v)
End;

{ format a LongInt for display }
Function LongIntToStr( v : LongInt ) : TString;
Var s : TString;
Begin
  Str(v,s);
  LongIntToStr := s
End;

{ format a LongLongInt for display }
Function LongLongIntToStr( v : LongLongInt ) : TString;
Var s : TString;
Begin
  Str(v:StringMaxSize:0,s);
  LongLongIntToStr := TrimLeft(s)
End;

{ format a LongReal for display }
Function LongRealToStr( v : LongReal ) : TString;
Var s : TString;
Begin
  Str(v,s);
  LongRealToStr := TrimLeft(s)
End;

{ convert a Pascal string to a LongInt; code is 0 if the operation succeeds,
  or the index of the character preventing the conversion }
Function StrToLongInt( s : TString; Var code : Integer ) : LongInt;
Var v : LongInt;
Begin
  Val(s,v,code);
  StrToLongInt := v
End;

{ convert a Pascal string to a high precision Real; code is 0 if the operation 
 succeeds, or the index of the character preventing the conversion }
Function StrToLongReal( s : TString; Var code : Integer ) : LongReal;
Var v : LongReal;
Begin
  Val(s,v,code);
  StrToLongReal := v
End;

{ integer division of two LongInt values }
Function LongIntDiv( x,y : LongInt ) : LongInt;
Begin
  LongIntDiv := x Div y
End;

{ set out-of-memory detection }
Procedure InitMalloc;
Begin
  ReturnNilIfGrowHeapFails := True
End;

{ return the number of bytes allocated on the heap for the memory block 
  pointed by pointer p; size is equal to the value given by SizeOf() }
Function MemSizeOf( p : Pointer; size : Integer ) : Integer;
Begin
  MemSizeOf := MemSize(p)
End;

{ get the directory separator }
Function GetDirectorySeparator : Char;
Begin
  GetDirectorySeparator := DirectorySeparator
End;

{ screen width in number of 1-byte characters }
Function GetScreenWidth : TCrtCoord;
Begin
  GetScreenWidth := ScreenWidth
End;

{ screen height in number of rows }
Function GetScreenHeight : TCrtCoord;
Begin
  GetScreenHeight := ScreenHeight
End;

Const
  { input buffer size }
  BufSize = 1024;
  { maximum number of bytes per char }
  MaxBytesPerChar = 4;
