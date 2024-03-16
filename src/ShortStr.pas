{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : ShortStr.pas                                               }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                         S H O R T   S T R I N G S                          }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Unit ShortStr;

Interface

{$IFDEF FPC}
Uses
  Sysutils;
{$ENDIF}

{ longest non dynamic string }
Const
  StringMaxSize = 255;
Type
  CharSet   = Set Of Char;
  TString = String[StringMaxSize];
  TStringSize = 0..StringMaxSize;

Const
  { 'end of line' sequence used for output text files }
  CRLF : Array[1..2] Of Char = (#13,#10);

Function TrimLeftSpaces( s : TString ) : TString;
Function RAlign( s : TString; width : TStringSize ) : TString;
Function IntToStr( v : Integer ) : TString;
Function BoolToStr( b : Boolean ) : TString;
Function StartsWith( s,b : TString ) : Boolean;
Function EndsWith( s,b : TString ) : Boolean;
Function StartsCount( s : TString; E : CharSet ) : TStringSize;

Implementation
{-----------------------------------------------------------------------------}
{ TP4/FPC compatibility code }
{$IFDEF MSDOS}

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

{$ENDIF}
{-----------------------------------------------------------------------------}

{ trim whitespace from the beginning of a string }
Function TrimLeftSpaces( s : TString ) : TString;
Begin
  TrimLeftSpaces := TrimLeft(s)
End;

{ right-align a string inside a blank string of size width }
Function RAlign( s : TString; width : TStringSize ) : TString;
Var
  i : TStringSize;
  rs : TString;
Begin
  rs := '';
  If width > Length(s) Then
    For i := 1 to width - Length(s) Do
      rs := rs + ' ';
  RAlign := rs + s
End;

{ convert a byte or an integer to a string }
Function IntToStr( v : Integer ) : TString;
Var
  s : TString;
Begin
  Str(v,s);
  IntToStr := s
End;

{ convert a boolean to a string }
Function BoolToStr( b : Boolean ) : TString;
Var
  s : TString;
Begin
  If b Then
    s := 'TRUE'
  Else
    s := 'FALSE';
  BoolToStr := s
End;

{ push a char at the end of a string }
Procedure PushChar( Var s : TString; c : Char );
Begin
  s := s + c
End;

{ pop a char from the end of a string }
Procedure PopChar( Var s : TString; Var c : Char );
Begin
  c := s[Length(s)];
  Delete(s,Length(s),1)
End;

{ return True if s starts with b }
Function StartsWith( s,b : TString ) : Boolean;
Begin
  StartsWith := Copy(s,1,Length(b)) = b
End;

{ return True if s ends with b }
Function EndsWith( s,b : TString ) : Boolean;
Begin
  If Length(s) < Length(b) Then
    EndsWith := False
  Else
    EndsWith := Copy(s,Length(s)-Length(b)+1,Length(b)) = b
End;

{ return the number of chars in E s starts with  }
Function StartsCount( s : TString; E : CharSet ) : TStringSize;
Var
  i : TStringSize;
Begin
  For i := 1 To Length(s) Do
    If Not (s[i] in E) Then
    Begin
      StartsCount := i - 1;
      Exit
    End;
  StartsCount := Length(s)
End;

End.