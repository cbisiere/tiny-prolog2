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

{ list of (addr, value) elements, where addr is the address of a Prolog object pointer,
  and value is a Prolog object pointers }
Type
  TRVal = TPObjPtr; { a value to restore }
  TRAddr = ^TRVal; { an address where to store or restore a value }
  RestorePtr = ^TObjRestore;
  TObjRestore = Record
    PO_META : TObjMeta;
    { not deep copied: }
    RE_NEXT : RestorePtr;
    RE_PVAL : TRVal; { backup of the pointer value }
    { extra data: }
    RE_ADDR : TRAddr { address of the pointer value }
  End;

{-----------------------------------------------------------------------}
{ constructor                                                           }
{-----------------------------------------------------------------------}

Function NewRestore : RestorePtr;
Var 
  U : RestorePtr;
  ptr : TPObjPtr Absolute U;
Begin
  ptr := NewPrologObject(RE,SizeOf(TObjRestore),2,0);
  With U^ Do
  Begin
    RE_PVAL := Nil;
    RE_ADDR := Nil;
    RE_NEXT := Nil
  End;
  NewRestore := U
End;

{-----------------------------------------------------------------------}
{ methods                                                               }
{-----------------------------------------------------------------------}

Procedure PushRestore( Var U : RestorePtr; p : TRAddr; V : TRVal );
Var NewU : RestorePtr;
Begin
  NewU := NewRestore;
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
      RE_ADDR^ := RE_PVAL
    End
End;
