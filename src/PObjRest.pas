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
{$I define.inc }

Unit PObjRest;

Interface

Uses
  Errs,
  Memory,
  PObj;

{ list of (addr, value) elements, where addr is the address of a pointer,
  and value is a pointer }
Type
  RestPtr = ^TObjRest;
  TObjRest = Record
    PO_META : TObjMeta;
    { not deep copied: }
    RE_NEXT : RestPtr;
    RE_POBJ : TObjectPtr; { object whose property is modified }
    RE_PVAL : TObjectPtr; { backup of the pointer value }
    { extra data: }
    RE_ADDR : ^TObjectPtr; { address of the pointer value }
    RE_DONE : Boolean { initial value has been restored (debug) }
  End;


Procedure Rest_SetMem( Var U : RestPtr; obj : TObjectPtr; 
    Var p : TObjectPtr; V : TObjectPtr; Undo : Boolean);
Procedure Rest_Restore( Var U : RestPtr );

Implementation
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------}
{ constructor                                                           }
{-----------------------------------------------------------------------}

Function Rest_New : RestPtr;
Var 
  U : RestPtr;
  ptr : TObjectPtr Absolute U;
Begin
  ptr := NewRegisteredPObject(RE,SizeOf(TObjRest),3,False,0);
  With U^ Do
  Begin
    RE_POBJ := Nil;
    RE_PVAL := Nil;
    RE_ADDR := Nil;
    RE_DONE := False;
    RE_NEXT := Nil
  End;
  Rest_New := U
End;

{-----------------------------------------------------------------------}
{ methods                                                               }
{-----------------------------------------------------------------------}

Procedure Rest_Push( Var U : RestPtr; obj : TObjectPtr; 
    Var p : TObjectPtr );
Var NewU : RestPtr;
Begin
  NewU := Rest_New;
  With NewU^ Do
  Begin
    RE_POBJ := obj;
    RE_ADDR := Addr(p);
    RE_PVAL := p;
    RE_NEXT := U
  End;
  U := NewU
End;

{ execute the assignment p := v when p is not equal to v, where p is a property 
 of an object obj; allows for later restoration if Undo is true;  }
Procedure Rest_SetMem( Var U : RestPtr; obj : TObjectPtr; 
    Var p : TObjectPtr; V : TObjectPtr; Undo : Boolean);
Begin
  If p <> V Then
  Begin
    If Undo Then 
      Rest_Push(U,obj,p);
    p := V
  End
End;

Procedure Rest_Restore( Var U : RestPtr );
Begin
  If U <> Nil Then
    With U^ Do
    Begin
      Rest_Restore(RE_NEXT);
      CheckCondition(Not RE_DONE,'double restore');
      CheckIsObject(RE_POBJ,'Rest_Restore/obj');
      If RE_ADDR^ <> Nil Then 
        CheckIsObject(RE_ADDR^,'Rest_Restore/addr');
      If RE_PVAL <> Nil Then 
        CheckIsObject(RE_PVAL,'Rest_Restore/pval');
      RE_ADDR^ := RE_PVAL;
      RE_DONE := True
    End
End;

End.