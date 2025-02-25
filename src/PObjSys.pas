{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObjSys.pas                                                }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                      P R O L O G   O B J E C T S :                         }
{                                                                            }
{   S Y S T E M   O F   E Q U A T I O N S   A N D  I N E Q U A T I O N S     }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Unit PObjSys;

Interface

Uses
  Errs,
  CWrites,
  Memory,
  PObj,
  PObjTerm,
  PObjEq;

{-----------------------------------------------------------------------}
{ types                                                                 }
{-----------------------------------------------------------------------}

{ non-reduced part of a system of equations and inequations }
Type 
  SysPtr = ^TObjSys;
  TObjSys = Record
    PO_META : TObjMeta;
    { deep copied: }
    SY_DATA : Array[EqType] Of EqPtr
  End;

Function Sys_New : SysPtr;
Function Sys_NewWithEq( T1,T2 : TermPtr ) : SysPtr;

Function Sys_Get( S : SysPtr; EType : EqType ) : EqPtr;
Procedure Sys_Set( S : SysPtr; EType : EqType; E : EqPtr );
Function Sys_Has( S : SysPtr; EType: EqType ) : Boolean;

Procedure Sys_InsertOneEq( S : SysPtr; E : EqPtr );
Procedure Sys_CopyOneEq( S : SysPtr; E : EqPtr );
Procedure Sys_CopyEqs( S : SysPtr; E : EqPtr );
Procedure Sys_Remove( S : SysPtr; E : EqPtr );
Function Sys_RemoveOne( S : SysPtr; EType: EqType ) : EqPtr;
Procedure Sys_RemoveTrivial( S : SysPtr; EType: EqType );
Function Sys_AsList( S : SysPtr ) : EqPtr;

Procedure Sys_Dump( S : SysPtr );

Implementation
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------}
{ constructors                                                          }
{-----------------------------------------------------------------------}

{ new system }
Function Sys_New : SysPtr;
Var 
  S : SysPtr;
  ptr : TObjectPtr Absolute S;
  t : EqType;
Begin
  ptr := NewRegisteredPObject(SY,SizeOf(TObjSys),3,True,3);
  With S^ Do
  Begin
    For t := REL_EQUA To REL_FROZ Do
      SY_DATA[t] := Nil
  End;
  Sys_New := S
End;

{ create a new system containing a single equation T1=T2 }
Function Sys_NewWithEq( T1,T2 : TermPtr ) : SysPtr;
Var 
  S : SysPtr;
  E : EqPtr;
Begin
  S := Sys_New;
  E := Eq_New(REL_EQUA,T1,T2);
  Sys_InsertOneEq(S,E);
  Sys_NewWithEq := S
End;

{-----------------------------------------------------------------------}
{ get / set                                                             }
{-----------------------------------------------------------------------}

{ return the list of equations or inequations of S }
Function Sys_Get( S : SysPtr; EType : EqType ) : EqPtr;
Begin
  Sys_Get := S^.SY_DATA[EType]
End;

{ set the list of equations or inequations of S }
Procedure Sys_Set( S : SysPtr; EType : EqType; E : EqPtr );
Begin
  S^.SY_DATA[EType] := E
End;

{ return true if there is at least one equation of a given type in S }
Function Sys_Has( S : SysPtr; EType : EqType ) : Boolean;
Begin
  Sys_Has := Sys_Get(S,EType) <> Nil
End;

{-----------------------------------------------------------------------}
{ methods                                                               }
{-----------------------------------------------------------------------}

{ insert equation E in system S }
Procedure Sys_InsertOneEq( S : SysPtr; E : EqPtr );
Var
  t : EqType;
Begin
  Eqs_SetPrev(E,Nil);
  t := Eq_GetType(E);
  Eqs_Chain(E,Sys_Get(S,t));
  Sys_Set(S,t,E)
End;

{ copy an equation into a system }
Procedure Sys_CopyOneEq( S : SysPtr; E : EqPtr );
Var 
  Ec : EqPtr;
Begin
  Ec := Eq_ShallowCopy(E);
  Sys_InsertOneEq(S,Ec)
End;

{ make a copy of a list of equations in a system; terms themselves are not 
 copied }
Procedure Sys_CopyEqs( S : SysPtr; E : EqPtr );
Begin
  If E <> Nil Then
  Begin
    Sys_CopyEqs(S,Eqs_GetNext(E));
    Sys_CopyOneEq(S,E)
  End
End;

{ remove an equation or inequation from a system }
Procedure Sys_Remove( S : SysPtr; E : EqPtr );
Var
  t : EqType;
Begin
  t := Eq_GetType(E);
  { if E was first in the list, update the system's list head }
  If E = Sys_Get(S,t) Then
    Sys_Set(S,t,Eqs_GetNext(E));
  { unchain E, also resetting its links }
  Eqs_Unchain(E)
End;

{ remove an return the first equation or inequation from a system; return Nil 
 if no equations of the required type is present }
Function Sys_RemoveOne( S : SysPtr; EType: EqType ) : EqPtr;
Var 
  E : EqPtr;
Begin
  E := Sys_Get(S,EType);
  If E <> Nil Then
    Sys_Remove(S,E);
  Sys_RemoveOne := E
End;

{ remove trivial equations or inequations from a system}
Procedure Sys_RemoveTrivial( S : SysPtr; EType: EqType );
Var 
  E,En : EqPtr;
Begin
  E := Sys_Get(S,EType);
  While E <> Nil Do
  Begin
    En := Eqs_GetNext(E);
    If Eq_IsTrivial(E) Then
      Sys_Remove(S,E);
    E := En
  End
End;

{ return the concatenation of all items in a system; use copies to avoid
 modifying the original system }
Function Sys_AsList( S : SysPtr ) : EqPtr;
Var
  E : EqPtr;
  t : EqType;
Begin
  E := Nil;
  For t := REL_EQUA To REL_FROZ Do
    E := Eqs_Concat(E,Eqs_ShallowCopy(Sys_Get(S,t)));
  Sys_AsList := E
End;

{-----------------------------------------------------------------------}
{ dump                                                                  }
{-----------------------------------------------------------------------}

Procedure Sys_Dump( S : SysPtr );
Var
  t : EqType;
Begin
  For t := REL_EQUA To REL_FROZ Do
    Eqs_Dump(Sys_Get(S,t))
End;

End.