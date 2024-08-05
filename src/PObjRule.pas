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
  PObjDef,
  PObjBter;

Function Rule_New( y : TSyntax ) : RulePtr;

Function Rule_GetSyntax( R : RulePtr ) : TSyntax;
Procedure Rule_SetStatement( R : RulePtr; St : StmtPtr );
Function Rule_GetHead( R : RulePtr ) : BTermPtr;
Function Rule_GetQueue( R : RulePtr ) : BTermPtr;
Function Rule_GetTerms( R : RulePtr ) : BTermPtr;
Procedure Rule_SetTerms( R : RulePtr; B : BTermPtr );

Function Rule_Access( R : RulePtr ) : IdPtr;
Function Rule_Arity( R : RulePtr ) : PosInt;
Function Rule_HeadIsValid( R : RulePtr ) : Boolean;

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
  ptr := NewRegisteredPObject(RU,SizeOf(TObjRule),3,True,2);
  With R^ Do
  Begin
    RU_FBTR := Nil;
    RU_SYST := Nil;
    RU_STMT := Nil;
    RU_ACUT := False;
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

{-----------------------------------------------------------------------}
{ methods                                                               }
{-----------------------------------------------------------------------}

Function Rule_Access( R : RulePtr ) : IdPtr;
Begin
  Rule_Access := BTerm_GetAccessTerm(Rule_GetHead(R))
End;

Function Rule_Arity( R : RulePtr ) : PosInt;
Begin
  Rule_Arity := BTerm_GetArity(Rule_GetHead(R))
End;

{ test whether the head of a rule is valid }
Function Rule_HeadIsValid( R : RulePtr ) : Boolean;
Var
  B : BTermPtr;
Begin
  B := Rule_GetHead(R);
  Rule_HeadIsValid := (B <> Nil) And (BTerm_GetAccessTerm(B) <> Nil) And 
      (Not IdentifierIsCut(BTerm_GetAccessTerm(B)))
End;

End.