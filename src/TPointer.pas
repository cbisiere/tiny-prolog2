{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : TPointer.pas                                               }
{   Author      : Christophe Bisiere                                         }
{   Date        : 2022-09-17                                                 }
{   Updated     : 2022-2026                                                  }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                        G N E R I C   P O I N T E R S                       }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }

Unit TPointer;

Interface

Type 
{$IFDEF TPC}
  Pointer = ^Integer; { generic pointer }
{$ENDIF}
  PointerPtr = ^Pointer;

Implementation

End.
