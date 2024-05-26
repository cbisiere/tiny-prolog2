{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObjEq.pas                                                 }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{            P R O L O G   O B J E C T S :   E Q U A T I O N S               }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Unit PObjEq;

Interface

Uses
  Errs,
  Trace,
  Memory,
  PObj,
  PObjTerm,
  PObjRest;

{-----------------------------------------------------------------------}
{ types                                                                 }
{-----------------------------------------------------------------------}

{ equation or inequation, in the reduced or non-reduced system; REL_FROZ
 is used by the display system }
Type 
  EqPtr = ^TObjEq;
  EqType = (REL_EQUA, REL_INEQ, REL_FROZ);

  TObjEq = Record
    PO_META : TObjMeta;
    { deep copied: }
    EQ_PREV : EqPtr; { previous equation or Nil }
    EQ_NEXT : EqPtr; { next equation or Nil }
    EQ_LTER : TermPtr; { left member }
    EQ_RTER : TermPtr; { right member }
    { extra data: }
    EQ_TYPE : EqType { type: equation or inequation }
  End;

Function Eq_New( EType : EqType; T1,T2 : TermPtr ) : EqPtr;
Function Eq_ShallowCopy( E : EqPtr ) : EqPtr;

Function Eq_GetType( E : EqPtr ) : EqType;
Function Eq_GetLhs( E : EqPtr ) : TermPtr;
Function Eq_GetLhsAddr( E : EqPtr ) : TermPtrAddr;
Procedure Eq_SetLhs( E : EqPtr; T : TermPtr );
Function Eq_GetRhs( E : EqPtr ) : TermPtr;
Function Eq_GetRhsAddr( E : EqPtr ) : TermPtrAddr;
Procedure Eq_SetRhs( E : EqPtr; T : TermPtr );
Function Eq_SameAs( E1,E2 : EqPtr ) : Boolean;
Procedure Eq_SetMem( Var U : RestPtr; obj : TObjectPtr;
    Var E : EqPtr; V : EqPtr; Undo : Boolean);
Function Eq_IsTrivial( E : EqPtr ) : Boolean;

Function Eqs_GetNext( E : EqPtr ) : EqPtr;
Procedure Eqs_SetNext( E : EqPtr; E1 : EqPtr );
Function Eqs_Last( E : EqPtr ) : EqPtr;
Function Eqs_GetPrev( E : EqPtr ) : EqPtr;
Procedure Eqs_SetPrev( E : EqPtr; E1 : EqPtr );
Function Eqs_Next( E : EqPtr; Backward : Boolean ) : EqPtr;
Function Eqs_ShallowCopy( E : EqPtr ) : EqPtr;
Procedure Eqs_Chain( E1,E2 : EqPtr );
Procedure Eqs_Unchain( E : EqPtr );
Function Eqs_Concat( E1,E2 : EqPtr ) : EqPtr;

Procedure Eq_Dump( E : EqPtr );
Procedure Eqs_Dump( E : EqPtr );

Implementation
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------}
{ constructors / copy / compare                                         }
{-----------------------------------------------------------------------}

{ new equation }
Function Eq_New( EType : EqType; T1,T2 : TermPtr ) : EqPtr;
Var 
  E : EqPtr;
  ptr : TObjectPtr Absolute E;
Begin
  ptr := NewRegisteredPObject(EQ,SizeOf(TObjEq),4,True,4);
  With E^ Do
  Begin
    EQ_TYPE := EType;
    EQ_LTER := T1;
    EQ_RTER := T2;
    EQ_PREV := Nil;
    EQ_NEXT := Nil
  End;
  Eq_New := E
End;

{ shallow copy of an equation (without copying the left and right terms) }
Function Eq_ShallowCopy( E : EqPtr ) : EqPtr;
Var 
  Ec : EqPtr;
Begin
  Ec := Eq_New(Eq_GetType(E),Eq_GetLhs(E),Eq_GetRhs(E));
  Eq_ShallowCopy := Ec
End;

{ are two equations equivalent? }
Function Eq_SameAs( E1,E2 : EqPtr ) : Boolean;
Begin
  Eq_SameAs := (Eq_GetType(E1) = Eq_GetType(E2)) And
      (Term_SameAs(Eq_GetLhs(E1),Eq_GetLhs(E2)) And 
        Term_SameAs(Eq_GetRhs(E1),Eq_GetRhs(E2))) Or
      (Term_SameAs(Eq_GetLhs(E1),Eq_GetRhs(E2)) And
        Term_SameAs(Eq_GetRhs(E1),Eq_GetLhs(E2)))
End;

{-----------------------------------------------------------------------}
{ equation                                                              }
{-----------------------------------------------------------------------}

Function Eq_GetType( E : EqPtr ) : EqType;
Begin
  CheckCondition(E <> Nil,'Eq_GetType: Nil');
  Eq_GetType := E^.EQ_TYPE
End;

Function Eq_GetLhs( E : EqPtr ) : TermPtr;
Begin
  CheckCondition(E <> Nil,'Eq_GetLhs: Nil');
  Eq_GetLhs := E^.EQ_LTER
End;

Function Eq_GetLhsAddr( E : EqPtr ) : TermPtrAddr;
Begin
  CheckCondition(E <> Nil,'Eq_GetLhsAddr: Nil');
  Eq_GetLhsAddr := Addr(E^.EQ_LTER)
End;

Procedure Eq_SetLhs( E : EqPtr; T : TermPtr );
Begin
  CheckCondition(E <> Nil,'Eq_SetLhs: Nil');
  CheckCondition(T <> Nil,'Eq_SetLhs: Nil lhs');
  E^.EQ_LTER := T
End;

Function Eq_GetRhs( E : EqPtr ) : TermPtr;
Begin
  CheckCondition(E <> Nil,'Eq_GetRhs: Nil');
  Eq_GetRhs := E^.EQ_RTER
End;

Function Eq_GetRhsAddr( E : EqPtr ) : TermPtrAddr;
Begin
  CheckCondition(E <> Nil,'Eq_GetRhsAddr: Nil');
  Eq_GetRhsAddr := Addr(E^.EQ_RTER)
End;

Procedure Eq_SetRhs( E : EqPtr; T : TermPtr );
Begin
  CheckCondition(E <> Nil,'Eq_SetRhs: Nil');
  CheckCondition(T <> Nil,'Eq_SetRhs: Nil rhs');
  E^.EQ_RTER := T
End;

{ assign an equation, possibly allowing for backtracking }
Procedure Eq_SetMem( Var U : RestPtr; obj : TObjectPtr; Var E : EqPtr; 
    V : EqPtr; Undo : Boolean);
Var 
  p : TObjectPtr Absolute E;
  pV : TObjectPtr Absolute V;
Begin
  Rest_SetMem(U,obj,p,pV,Undo)
End;

{-----------------------------------------------------------------------}
{ equation:  methods                                                    }
{-----------------------------------------------------------------------}

{ is an equation trivially satisfied, that is, of the form t = t }
Function Eq_IsTrivial( E : EqPtr ) : Boolean;
Begin
  Eq_IsTrivial := Term_SameAs(Eq_GetLhs(E),Eq_GetRhs(E))
End;

{-----------------------------------------------------------------------}
{ list of equations: get / set                                          }
{-----------------------------------------------------------------------}

Function Eqs_GetNext( E : EqPtr ) : EqPtr;
Begin
  CheckCondition(E <> Nil,'Eqs_GetNext: Nil');
  Eqs_GetNext := E^.EQ_NEXT
End;

Procedure Eqs_SetNext( E : EqPtr; E1 : EqPtr );
Begin
  CheckCondition(E <> Nil,'Eqs_SetNext: Nil');
  E^.EQ_NEXT := E1
End;

Function Eqs_GetPrev( E : EqPtr ) : EqPtr;
Begin
  CheckCondition(E <> Nil,'Eqs_GetPrev: Nil');
  Eqs_GetPrev := E^.EQ_PREV
End;

Procedure Eqs_SetPrev( E : EqPtr; E1 : EqPtr );
Begin
  CheckCondition(E <> Nil,'Eqs_SetPrev: Nil');
  E^.EQ_PREV := E1
End;

{-----------------------------------------------------------------------}
{ list of equations: methods                                            }
{-----------------------------------------------------------------------}

Function Eqs_Last( E : EqPtr ) : EqPtr;
Begin
  CheckCondition(E <> Nil,'Eqs_Last: Nil');
  While Eqs_GetNext(E) <> Nil Do
    E := Eqs_GetNext(E);
  Eqs_Last := E
End;

Function Eqs_Next( E : EqPtr; Backward : Boolean ) : EqPtr;
Begin
  CheckCondition(E <> Nil,'Eqs_Next: Nil');
  If Backward Then
    Eqs_Next := Eqs_GetPrev(E)
  Else
    Eqs_Next := Eqs_GetNext(E)
End;

{ shallow copy of a list of equations (without copying the left and right 
 terms) }
Function Eqs_ShallowCopy( E : EqPtr ) : EqPtr;
Var 
  Ec,Pc,Fc : EqPtr; { copy, previous copy, first copy }
Begin
  Pc := Nil;
  Fc := Nil;
  While E <> Nil Do
  Begin
    Ec := Eq_ShallowCopy(E);
    If Pc = Nil Then
      Fc := Ec;
    Eqs_Chain(Pc,Ec);
    Pc := Ec;
    E := Eqs_GetNext(E)
  End;
  Eqs_ShallowCopy := Fc
End;

{ chain two equations; both can be Nil }
Procedure Eqs_Chain( E1,E2 : EqPtr );
Begin
  If E1 <> Nil Then
    Eqs_SetNext(E1,E2);
  If E2 <> Nil Then
    Eqs_SetPrev(E2,E1)
End;

{ remove an equation from a list }
Procedure Eqs_Unchain( E : EqPtr );
Begin
  Eqs_Chain(Eqs_GetPrev(E),Eqs_GetNext(E));
  Eqs_SetPrev(E,Nil);
  Eqs_SetNext(E,Nil)
End;

{ return a list that is the concatenation of two lists; this operations 
 *alters* the lists; both can be Nil }
Function Eqs_Concat( E1,E2 : EqPtr ) : EqPtr;
Var
  Es : EqPtr;
Begin
  If E1 <> Nil Then
  Begin
    Es := E1;
    Eqs_Chain(Eqs_Last(E1),E2)
  End
  Else
    Es := E2;
  Eqs_Concat := Es
End;

{-----------------------------------------------------------------------}
{ dump                                                                  }
{-----------------------------------------------------------------------}

Procedure Eq_Dump( E : EqPtr );
Begin
  CWrite(PtrToName(TObjectPtr(E)));
  CWrite(': ');
  CWrite(PtrToName(TObjectPtr(Eq_GetLhs(E))));
  Case Eq_GetType(E) Of 
  REL_EQUA:
    CWrite(' = ');
  REL_INEQ:
    CWrite(' <> ');
  REL_FROZ:
    CWrite(' ?? ')
  End;
  CWrite(PtrToName(TObjectPtr(Eq_GetRhs(E))))
End;

Procedure Eqs_Dump( E : EqPtr );
Begin
  While E <> Nil Do
  Begin
    Eq_Dump(E);
    CWriteln;
    E := Eqs_GetNext(E)
  End
End;

End.