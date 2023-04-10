{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : TP3.pas                                                    }
{   Author      : Christophe Bisière                                         }
{   Date        : 2022-09-17                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{         F R E E   P A S C A L   C O M P I L E R   C O M P A T              }
{                                                                            }
{----------------------------------------------------------------------------}

{ compatibility with Turbo Pascal 3.02 }

Uses Crt;

{ non dynamic short string }
Const
  AnyStrMaxSize = 255;
Type
  AnyStr = String[AnyStrMaxSize];

{ format a LongInt for display }
Function LongIntToStr( v : LongInt ) : AnyStr;
Var s : AnyStr;
Begin
  Str(v,s);
  LongIntToStr := s
End;

{ set out-of-memory detection }
Procedure InitMalloc;
Begin
  ReturnNilIfGrowHeapFails := True
End;

{ allocate memory; return Nil if there is not enough memory }
Function Malloc( size : Integer ) : Pointer;
Var p : Pointer;
Begin
  GetMem(p,size);
  Malloc := p
End;

