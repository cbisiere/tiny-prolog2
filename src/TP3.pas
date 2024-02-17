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

{ longest non dynamic string }
Const
  StringMaxSize = 255;
  MaxLongInt = 1e+38;
Type
  TString = String[StringMaxSize];
  TStringSize = 0..StringMaxSize;

Type 
  LongInt = Real; { simulate a LongInt }
  LongLongInt = Real; { simulate a very LongInt }
  LongReal = Real; { high precision real }
  Pointer = ^Integer; { generic pointer }

{ trim whitespace from the beginning of a string }
Function TrimLeft( s : TString ) : TString;
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

{ round a long real to a long integer value }
Function LongRealToLongInt( v : LongReal ) : LongInt;
Begin
  LongRealToLongInt := v
End;

{ format a LongInt for display }
Function LongIntToStr( v : LongInt ) : TString;
Var s : TString;
Begin
  Str(v:StringMaxSize:0,s);
  LongIntToStr := TrimLeft(s);
End;

{ format a LongLongInt for display }
Function LongLongIntToStr( v : LongLongInt ) : TString;
Begin
  LongLongIntToStr := LongIntToStr(v)
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
  LongIntDiv := Int(x/y)
End;

{ ReadKey, as in Free Pascal Compiler's Crt module;
 see:
 - Turbo Pascal 3 documentation, page 374 
 - https://www.freepascal.org/docs-html/rtl/crt/readkey.html }
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

{ get the directory separator }
Function GetDirectorySeparator : Char;
Begin
  GetDirectorySeparator := '\'
End;

{ get the user directory (FPC emulation); when non empty, it includes a 
 trailing path separator
 MS-DOS: return the current directory (FIXME: return HOME env var if any?)  }
Function GetUserDir: TString;
Var
  s : String;
Begin
  GetDir(0,s);
  GetUserDir := s { FIXME: check length? }
End;

{ TODO: return the file path part of the filename fn including the final 
 directory separator (FPC emulation) }
Function ExtractFilePath( fn : TString ) : TString;
Begin
  ExtractFilePath := fn { TBD }
End;

Type
  TCrtCoord = 1..255;

{ screen width in number of 1-byte characters }
Function GetScreenWidth : TCrtCoord;
Begin
  GetScreenWidth := 80
End;

{ screen height in number of rows }
Function GetScreenHeight : TCrtCoord;
Begin
  GetScreenHeight := 25
End;

Const
  { input buffer size }
  BufSize = 255;
  { maximum number of bytes per char }
  MaxBytesPerChar = 1;
