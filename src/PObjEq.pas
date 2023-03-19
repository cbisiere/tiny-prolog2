{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Objects.pas                                                }
{   Author      : Christophe Bisière                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{            P R O L O G   O B J E C T S :   E Q U A T I O N S               }
{                                                                            }
{----------------------------------------------------------------------------}


{-----------------------------------------------------------------------}
{ types                                                                 }
{-----------------------------------------------------------------------}

{ equation or inequation, in the reduced or non-reduced system }
Type 
  EqPtr = ^TObjEq;
  EqType = (REL_EQUA, REL_INEQ);
  TObjEq = Record
    PO_META : TObjMeta;
    EQ_NEXT : EqPtr; { next equation or Nil }
    EQ_LTER : TermPtr; { left member }
    EQ_RTER : TermPtr; { right member }
    EQ_TYPE : EqType { type: equation or inequation }
  End;

{ non-reduced part of a system of equations and inequations }
Type 
  SysPtr = ^TObjSys;
  TObjSys = Record
    PO_META : TObjMeta;
    SY_EQUA : EqPtr; { list of equations }
    SY_INEQ : EqPtr { list of inequations }
  End;


{-----------------------------------------------------------------------}
{ create / destroy                                                      }
{-----------------------------------------------------------------------}

{ create a new equation as a GC-managed object }
Function PushEquation( Code : EqType; T1,T2 : TermPtr ) : EqPtr;
Var E : EqPtr;
Begin
  CheckCondition((Code=REL_EQUA) Or (Code=REL_INEQ), 'Unknown relation');
  E := EqPtr(NewPrologObject(EQ, SizeOf(TObjEq), 3));
  With E^ Do
  Begin
    EQ_TYPE := Code;
    EQ_LTER := T1;
    EQ_RTER := T2;
    EQ_NEXT := Nil
  End;
  PushEquation := E
End;


{ create a new equation }
Function NewEq( EType: EqType; T1,T2 : TermPtr ) : EqPtr;
Var E : EqPtr;
Begin
  GetMemory(EQ,E,SizeOf(TObjEq));
  With E^ Do
  Begin
    EQ_TYPE := EType;
    EQ_NEXT := Nil;
    EQ_LTER := T1;
    EQ_RTER := T2
  End;
  NewEq := E
End;

{ clone an equation }
Function CloneEq( E : EqPtr ) : EqPtr;
Begin
  With E^ Do
    CloneEq := NewEq(EQ_TYPE,EQ_LTER,EQ_RTER)
End;

{ free an equation }
Procedure FreeEq(E : EqPtr);
Begin
  FreeMemory(EQ,E,SizeOf(TObjEq))
End;

{ free all equations in a list }
Procedure FreeAllEq(E : EqPtr);
Begin
  If E<>Nil Then
  Begin
    FreeAllEq(E^.EQ_NEXT);
    FreeEq(E)
  End
End;

{ allocate a new system }
Function NewSys : SysPtr;
Var S : SysPtr;
Begin
  GetMemory(SY,S,SizeOf(TObjSys));
  With S^ Do
  Begin
    SY_EQUA := Nil;
    SY_INEQ := Nil
  End;
  NewSys := S
End;

{ free a system S and all its equations; terms themselves are not freed }
Procedure FreeSys(S : SysPtr);
Begin
  FreeAllEq(S^.SY_EQUA);
  FreeAllEq(S^.SY_INEQ);
  FreeMemory(SY,S,SizeOf(TObjSys))
End;


{-----------------------------------------------------------------------}
{ methods                                                               }
{-----------------------------------------------------------------------}

{ insert equation E in system S }
Procedure InsertOneEqInSys( S : SysPtr; E : EqPtr );
Begin
  Case E^.EQ_TYPE Of 
  REL_EQUA:
    Begin
      E^.EQ_NEXT := S^.SY_EQUA;
      S^.SY_EQUA := E
    End;
  REL_INEQ:
    Begin
      E^.EQ_NEXT := S^.SY_INEQ;
      S^.SY_INEQ := E
    End
  End
End;

{ copy a equation in a system }
Procedure CopyOneEqInSys( S : SysPtr; E : EqPtr );
Var Ec : EqPtr;
Begin
  Ec := CloneEq(E);
  InsertOneEqInSys(S,Ec)
End;

{ make a copy of a list of equations in a system; terms themselves are not copied }
Procedure CopyAllEqInSys( S : SysPtr; E : EqPtr );
Begin
  If E<>Nil Then
  Begin
    CopyAllEqInSys(S,E^.EQ_NEXT);
    CopyOneEqInSys(S,E)
  End
End;

{ return true if there is at least one equation of a given type in S }
Function HasEqInSys( S : SysPtr; EType: EqType ) : Boolean;
Begin
  Case EType Of
  REL_EQUA:
    HasEqInSys := S^.SY_EQUA<>Nil;
  REL_INEQ:
    HasEqInSys := S^.SY_INEQ<>Nil
  End
End;

{ remove an equation from a system; return Nil if no equation of the required type is present }
Function RemoveOneEqFromSys( S : SysPtr; EType: EqType ) : EqPtr;
Var E : EqPtr;
Begin
  E := Nil;
  Case EType Of
  REL_EQUA:
    If S^.SY_EQUA<>Nil Then
    Begin
      E := S^.SY_EQUA;
      S^.SY_EQUA := E^.EQ_NEXT;
      E^.EQ_NEXT := Nil
    End;
  REL_INEQ:
    If S^.SY_INEQ<>Nil Then
    Begin
      E := S^.SY_INEQ;
      S^.SY_INEQ := E^.EQ_NEXT;
      E^.EQ_NEXT := Nil
    End
  End;
  RemoveOneEqFromSys := E
End;
