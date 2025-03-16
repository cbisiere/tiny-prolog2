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
{$I define.inc }

Unit Common;

Interface

Type
  TComp = (CompLower,CompEqual,CompGreater,CompUndefined);

Procedure Pass;

implementation

{ do nothing }
Procedure Pass;
Begin
End;

End.
