{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Common.pas                                                 }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                           M I S C   H E L P E R S                          }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Type
  CharSet   = Set Of Char;


{ do nothing }
Procedure Pass;
Begin
End;

{ maximum of two integers }
Function Max( a,b : Integer ) : Integer;
Begin
  If a >= b Then
    Max := a
  Else
    Max := b
End;

{ minimum of two integers }
Function Min( a,b : Integer ) : Integer;
Begin
  If a <= b Then
    Min := a
  Else
    Min := b
End;
