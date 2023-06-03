{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObjRest.pas                                               }
{   Author      : Christophe Bisiere                                         }
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
  RestorePtr = ^TObjRestore;
  TObjRestore = Record
    PO_META : TObjMeta;
    { not deep copied: }
    RE_NEXT : RestorePtr;
    RE_POBJ : TPObjPtr; { object whose property is modified }
    RE_PVAL : TPObjPtr; { backup of the pointer value }
    { extra data: }
    RE_ADDR : ^TPObjPtr; { address of the pointer value }
    RE_DONE : Boolean { initial value has been restored (debug) }
  End;

{-----------------------------------------------------------------------}
{ constructor                                                           }
{-----------------------------------------------------------------------}

Function NewRestore : RestorePtr;
Var 
  U : RestorePtr;
  ptr : TPObjPtr Absolute U;
Begin
  ptr := NewRegisteredObject(RE,3,False,0);
  With U^ Do
  Begin
    RE_POBJ := Nil;
    RE_PVAL := Nil;
    RE_ADDR := Nil;
    RE_DONE := False;
    RE_NEXT := Nil
  End;
  NewRestore := U
End;

{-----------------------------------------------------------------------}
{ methods                                                               }
{-----------------------------------------------------------------------}

Procedure PushRestore( Var U : RestorePtr; obj : TPObjPtr; Var p : TPObjPtr );
Var NewU : RestorePtr;
Begin
  NewU := NewRestore;
  With NewU^ Do
  Begin
    RE_POBJ := obj;
    RE_ADDR := Addr(p);
    RE_PVAL := p;
    RE_NEXT := U
  End;
  U := NewU
End;


Procedure SetMem( Var U : RestorePtr; obj : TPObjPtr; 
    Var p : TPObjPtr; V : TPObjPtr; Backtrackable : Boolean);
Begin
  If p <> V Then
  Begin
    If Backtrackable Then 
      PushRestore(U,obj,p);
    p := V
  End
End;

Procedure Restore( Var U : RestorePtr );
Begin
  If U <> Nil Then
    With U^ Do
    Begin
      Restore(RE_NEXT);
      CheckCondition(Not RE_DONE,'double restore');
      CheckIsPObj(RE_POBJ,'Restore/obj');
      If RE_ADDR^ <> Nil Then 
        CheckIsPObj(RE_ADDR^,'Restore/addr');
      If RE_PVAL <> Nil Then 
        CheckIsPObj(RE_PVAL,'Restore/pval');
      RE_ADDR^ := RE_PVAL;
      RE_DONE := True
    End
End;
