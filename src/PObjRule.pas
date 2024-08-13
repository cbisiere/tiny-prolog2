{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObjRule.pas                                               }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                    P R O L O G   O B J E C T :   R U L E                   }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ Prolog rule }

Unit PObjRule;

Interface

Uses
  Num,
  Memory,
  PObj,
  PObjTerm,
  PObjFCVI,
  PObjEq,
  PObjDef,
  PObjBter;

Function Rule_New( y : TSyntax ) : RulePtr;

Function Rule_GetSyntax( R : RulePtr ) : TSyntax;
Function Rule_GetAccess( R : RulePtr ) : IdPtr;
Function Rule_GetArity( R : RulePtr ) : TArity;

Procedure Rule_SetStatement( R : RulePtr; St : StmtPtr );
Function Rule_GetHead( R : RulePtr ) : BTermPtr;
Function Rule_GetQueue( R : RulePtr ) : BTermPtr;
Function Rule_GetEqs( R : RulePtr ) : EqPtr;
Procedure Rule_SetEqs( R : RulePtr; Eqs : EqPtr );

Procedure Rule_GetSignature( R : RulePtr; 
    Var Access : IdPtr; Var Arity : TArity );
Function Rule_HeadIsValid( R : RulePtr ) : Boolean;
Procedure Rule_SetHeadAndQueue( R : RulePtr; B : BTermPtr );

Implementation

{-----------------------------------------------------------------------}
{ constructor                                                           }
{-----------------------------------------------------------------------}

{ new rule }
Function Rule_New( y : TSyntax ) : RulePtr;
Var 
  R : RulePtr;
  ptr : TObjectPtr Absolute R;
Begin
  ptr := NewRegisteredPObject(RU,SizeOf(TObjRule),4,True,2);
  With R^ Do
  Begin
    RU_FBTR := Nil;
    RU_SYST := Nil;
    RU_STMT := Nil;
    RU_ACCE := Nil;
    RU_ARIT := 0;
    RU_TYPE := GOAL_STD;
    RU_SYNT := y
  End;
  Rule_New := R
End;

{-----------------------------------------------------------------------}
{ get / set                                                             }
{-----------------------------------------------------------------------}

{ get a rule's syntax }
Function Rule_GetSyntax( R : RulePtr ) : TSyntax;
Begin
  Rule_GetSyntax := R^.RU_SYNT
End;

{ set a rule's statement }
Procedure Rule_SetStatement( R : RulePtr; St : StmtPtr );
Begin
  R^.RU_STMT := St
End;

{ rule's head }
Function Rule_GetHead( R : RulePtr ) : BTermPtr;
Begin
  Rule_GetHead := R^.RU_FBTR
End;

{ rule's queue }
Function Rule_GetQueue( R : RulePtr ) : BTermPtr;
Begin
  Rule_GetQueue := BTerms_GetNext(R^.RU_FBTR)
End;

{ rule's list of terms }
Function Rule_GetTerms( R : RulePtr ) : BTermPtr;
Begin
  Rule_GetTerms := R^.RU_FBTR
End;

{ set a rule's list of terms }
Procedure Rule_SetTerms( R : RulePtr; B : BTermPtr );
Begin
  R^.RU_FBTR := B
End;

{ rule's list of equations }
Function Rule_GetEqs( R : RulePtr ) : EqPtr;
Begin
  Rule_GetEqs := R^.RU_SYST
End;

{ set a rule's list of equations }
Procedure Rule_SetEqs( R : RulePtr; Eqs : EqPtr );
Begin
  R^.RU_SYST := Eqs
End;

{ rule's access }
Function Rule_GetAccess( R : RulePtr ) : IdPtr;
Begin
  Rule_GetAccess := R^.RU_ACCE
End;

{ set a rule's access identifier  }
Procedure Rule_SetAccess( R : RulePtr; I : IdPtr );
Begin
  R^.RU_ACCE := I
End;

{ rule's arity }
Function Rule_GetArity( R : RulePtr ) : TArity;
Begin
  Rule_GetArity := R^.RU_ARIT
End;

{ set a rule's arity }
Procedure Rule_SetArity( R : RulePtr; a : TArity );
Begin
  R^.RU_ARIT := a
End;

{ rule's arity }
Function Rule_GetType( R : RulePtr ) : TGoalType;
Begin
  Rule_GetType := R^.RU_TYPE
End;

{ set a rule's arity }
Procedure Rule_SetType( R : RulePtr; GoalType : TGoalType );
Begin
  R^.RU_TYPE := GoalType
End;

{-----------------------------------------------------------------------}
{ methods                                                               }
{-----------------------------------------------------------------------}

Procedure Rule_GetSignature( R : RulePtr; 
    Var Access : IdPtr; Var Arity : TArity );
Begin
  Access := Rule_GetAccess(R);
  Arity := Rule_GetArity(R)
End;

{ is the head of a rule valid? }
Function Rule_HeadIsValid( R : RulePtr ) : Boolean;
Begin
  Rule_HeadIsValid := (Rule_GetType(R) = GOAL_STD) And 
      (Rule_GetAccess(R) <> Nil)
End;

{ set a rule's head and queue, updating the rule's signature }
Procedure Rule_SetHeadAndQueue( R : RulePtr; B : BTermPtr );
Var
  GoalType : TGoalType;
  I : IdPtr;
  a : TArity;
Begin
  Rule_SetTerms(R,B);
  { update cached metadata }
  BTerm_GetMetadata(B,GoalType,I,a);
  Rule_SetType(R,GoalType);
  Rule_SetAccess(R,I);
  Rule_SetArity(R,a)
End;

End.