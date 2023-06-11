{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : TP3.pas                                                    }
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
  Crt,
  Sysutils; { TrimLeft }

{ non dynamic short string }
Const
  AnyStrMaxSize = 255;
Type
  AnyStr = String[AnyStrMaxSize];
  TAnyStrSize = 0..AnyStrMaxSize;

Type
  LongLongInt = Real; { simulate a very long integer }

{ format a LongInt for display }
Function LongIntToStr( v : LongInt ) : AnyStr;
Var s : AnyStr;
Begin
  Str(v,s);
  LongIntToStr := s
End;

{ format a LongLongInt for display }
Function LongLongIntToStr( v : LongLongInt ) : AnyStr;
Var s : AnyStr;
Begin
  Str(v:AnyStrMaxSize:0,s);
  LongLongIntToStr := TrimLeft(s)
End;

{ convert a Pascal string to a LongInt; code is 0 if the operation succeeded,
  or the index of the character preventing the conversion }
Function StrToLongInt( s : AnyStr; Var code : Integer ) : LongInt;
Var v : LongInt;
Begin
  Val(s,v,code);
  StrToLongInt := v
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
