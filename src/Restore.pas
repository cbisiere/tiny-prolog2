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
{                  R E S T O R A T I O N   S T A C K                         }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ list of (addr, value) elements, where addr and value are pointers }
Type
  TRVal = Pointer; { a value to restore }
  TRAddr = ^TRVal; { an address where to store or restore a value }
  RestorePtr = ^TObjRestore;
  TObjRestore = Record
    RE_ADDR : TRAddr; { address of the pointer value }
    RE_PVAL : TRVal; { backup of the pointer value }
    RE_NEXT : RestorePtr
  End;

Procedure FreeRestore( Var U : RestorePtr );
Begin
  FreeMemory(RE, U,SizeOf(TObjRestore))
End;

Procedure NewRestore( Var U : RestorePtr );
Begin
  GetMemory(RE, U,SizeOf(TObjRestore))
End;


Procedure PushRestore( Var U : RestorePtr; p : TRAddr; V : TRVal );
Var NewU : RestorePtr;
Begin
  NewRestore(NewU);
  With NewU^ Do
  Begin
    RE_ADDR := p;
    RE_PVAL := V;
    RE_NEXT := U
  End;
  U := NewU
End;


Procedure SetMem( Var U : RestorePtr; p : TRAddr; V : TRVal; Backtrackable : Boolean);
Begin
  If Backtrackable Then 
    PushRestore(U,p,p^);
  p^ := V
End;

Procedure Restore( Var U : RestorePtr );
Begin
  If U <> Nil Then
    With U^ Do
    Begin
      Restore(RE_NEXT);
      RE_ADDR^ := RE_PVAL;
      FreeRestore(U)
    End
End;
