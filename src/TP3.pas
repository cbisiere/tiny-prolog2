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
{                   T U R B O   P A S C A L  3   C O M P A T                 }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ non dynamic short string }
Const
  AnyStrMaxSize = 255;
Type
  AnyStr = String[AnyStrMaxSize];
  TAnyStrSize = 0..AnyStrMaxSize;

Type 
  LongInt = Real; { simulate a LongInt }
  LongLongInt = Real; { simulate a very LongInt }
  Pointer = ^Integer; { generic pointer }

{ trim whitespace from the beginning of a string }
Function TrimLeft( s : AnyStr ) : AnyStr;
Var 
  c,i : Byte;
Label
  Break;
Begin
  c := 0;
  For i := 1 to Length(s) Do
    If s[i]=' ' Then
      c := c + 1
    Else
      Goto Break;
  Break: If c>0 Then
    Delete(s,1,c);
  TrimLeft := s
End;

{ format a LongInt for display }
Function LongIntToStr( v : LongInt ) : AnyStr;
Var s : AnyStr;
Begin
  Str(v:AnyStrMaxSize:0,s);
  LongIntToStr := TrimLeft(s);
End;

{ format a LongLongInt for display }
Function LongLongIntToStr( v : LongLongInt ) : AnyStr;
Begin
  LongLongIntToStr := LongIntToStr(v)
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
  LongIntDiv := Int(x/y)
End;

{ ReadKey, as in Free Pascal Compiler's Crt module }
Function ReadKey : Char;
Var c : Char;
Begin
  Read(Kbd, c);
  If (c=#27) And KeyPressed Then
    c := #00;
  ReadKey := c
End;

{ set out-of-memory detection }
Procedure InitMalloc;
Begin
End;

{ return the number of bytes allocated on the heap for the memory block 
  pointed by pointer p; size is equal to the value given by SizeOf() }
Function MemSizeOf( p : Pointer; size : Integer ) : Integer;
Begin
  MemSizeOf := size
End;