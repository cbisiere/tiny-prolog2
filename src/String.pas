{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Restore.pas                                                }
{   Author      : Christophe Bisière                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                                S T R I N G S                               }
{                                                                            }
{----------------------------------------------------------------------------}

Type AnyStr = String[254];

Function LTrim( s : AnyStr ) : AnyStr;
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
  LTrim := s
End;

Function RealToStr( r : Real; m : Byte ) : AnyStr;
Var s : AnyStr;
Begin
  Str(r:254:m,s);
  RealToStr := LTrim(s);
End;