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
    { deep copied: }
    EQ_NEXT : EqPtr; { next equation or Nil }
    EQ_LTER : TermPtr; { left member }
    EQ_RTER : TermPtr; { right member }
    { extra data: }
    EQ_TYPE : EqType { type: equation or inequation }
  End;

{ non-reduced part of a system of equations and inequations }

Type 
  SysPtr = ^TObjSys;
  TObjSys = Record
    PO_META : TObjMeta;
    { deep copied: }
    SY_EQUA : EqPtr; { list of equations }
    SY_INEQ : EqPtr { list of inequations }
  End;


{-----------------------------------------------------------------------}
{ constructors / copy                                                   }
{-----------------------------------------------------------------------}

{ new equation }
Function NewEquation( EType : EqType; T1,T2 : TermPtr ) : EqPtr;
Var 
  E : EqPtr;
  ptr : TPObjPtr Absolute E;
Begin
  CheckCondition((EType=REL_EQUA) Or (EType=REL_INEQ), 'Unknown relation');
  ptr := NewPrologObject(EQ, SizeOf(TObjEq), 3, 3);
  With E^ Do
  Begin
    EQ_TYPE := EType;
    EQ_LTER := T1;
    EQ_RTER := T2;
    EQ_NEXT := Nil
  End;
  NewEquation := E
End;

{ shallow copy of an equation (without copying the left and right terms) }
Function CopyEquation( E : EqPtr ) : EqPtr;
Var Ec : EqPtr;
Begin
  With E^ Do
     Ec := NewEquation(EQ_TYPE,EQ_LTER,EQ_RTER);
  CopyEquation := Ec
End;

{ new system }
Function NewSystem : SysPtr;
Var 
  S : SysPtr;
  ptr : TPObjPtr Absolute S;
Begin
  ptr := NewPrologObject(SY, SizeOf(TObjSys), 2, 2);
  With S^ Do
  Begin
    SY_EQUA := Nil;
    SY_INEQ := Nil
  End;
  NewSystem := S
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

{ copy an equation into a system }
Procedure CopyOneEqInSys( S : SysPtr; E : EqPtr );
Var Ec : EqPtr;
Begin
  Ec := CopyEquation(E);
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

{ assign an equation, possibly allowing for backtracking }
Procedure SetMemEq( Var U : RestorePtr; Var E : EqPtr; V : EqPtr; Backtrackable : Boolean);
Var 
  p : TPObjPtr Absolute E;
  pV : TPObjPtr Absolute V;
Begin
  SetMem(U,p,pV,Backtrackable)
End;

{ create a new system containing a single equation T1=T2 }
Function NewSystemWithEq( T1,T2 : TermPtr ) : SysPtr;
Var 
  S : SysPtr;
  E : EqPtr;
Begin
  S := NewSystem;
  E := NewEquation(REL_EQUA,T1,T2);
  InsertOneEqInSys(S,E);
  NewSystemWithEq := S
End;