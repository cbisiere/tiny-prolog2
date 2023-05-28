{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Strings.pas                                                }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                                S T R I N G S                               }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Const
  { Code for 'end of line':
    in the input system, 1) CR,LF and CR-LF in files, 2) CR in terminal input,
    are replaced with that single char }
  EndOfLine = #10;

  { 'end of line' sequence used for output text files }
  CRLF : Array[1..2] Of Char = (#13,#10);


{ right-align a string inside a blank string of size width }
Function RAlign( s : AnyStr; width : TAnyStrSize ) : AnyStr;
Var
  i : TAnyStrSize;
  rs : AnyStr;
Begin
  rs := ' ';
  For i := 1 to width - Length(s) Do
    rs := rs + ' ';
  RAlign := rs + s
End;

{ convert a byte or an integer to a string }
Function IntToStr( v : Integer ) : AnyStr;
Var
  s : AnyStr;
Begin
  Str(v,s);
  IntToStr := s
End;

{ convert a boolean to a string }
Function BoolToStr( b : Boolean ) : AnyStr;
Var
  s : AnyStr;
Begin
  If b Then
    s := 'TRUE'
  Else
    s := 'FALSE';
  BoolToStr := s
End;
