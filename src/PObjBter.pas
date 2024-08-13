{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObjBter.pas                                               }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{           P R O L O G   O B J E C T :   L I S T   O F   T E R M S          }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ term in a list, with access data }

Unit PObjBter;

Interface

Uses
  Num,
  Errs,
  Trace,
  Memory,
  PObj,
  PObjTerm,
  PObjFCVI,
  PObjStr,
  PObjDef,
  Tuple;

{ BTerm }
Function BTerm_New( T : TermPtr ) : BTermPtr;

Function BTerm_GetTerm( B : BTermPtr ) : TermPtr;
Function BTerm_GetHeader( B : BTermPtr ) : HeadPtr;
Procedure BTerm_SetHeader( B : BTermPtr; H : HeadPtr );

Function BTerm_GetType( B : BTermPtr ) : TGoalType;
Function BTerm_GetAccessTerm( B : BTermPtr ) : IdPtr;

Procedure BTerm_GetMetadata( B : BTermPtr; 
    Var GoalType : TGoalType; Var Access : IdPtr; Var Arity : TArity );


{ list }
Function BTerms_GetNext( B : BTermPtr ) : BTermPtr;
Procedure BTerms_SetNext( B : BTermPtr; B1 : BTermPtr );
Function BTerms_GetLast( B : BTermPtr ) : BTermPtr;
Procedure BTerms_SetCutHeader( B : BTermPtr; H : HeadPtr );

Implementation

{-----------------------------------------------------------------------}
{ constructor                                                           }
{-----------------------------------------------------------------------}

{ new block of type GoalType, access I, arity a, term T }
Function BTerm_New( T : TermPtr ) : BTermPtr;
Var 
  B : BTermPtr;
  ptr : TObjectPtr Absolute B;
Begin
  ptr := NewRegisteredPObject(BT,SizeOf(TObjBTerm),3,True,2);
  With B^ Do
  Begin
    BT_TERM := T;
    BT_NEXT := Nil;
    BT_HEAD := Nil
  End;
  BTerm_New := B
End;

{-----------------------------------------------------------------------}
{ get / set                                                             }
{-----------------------------------------------------------------------}

{ term }
Function BTerm_GetTerm( B : BTermPtr ) : TermPtr;
Begin
  CheckCondition(B <> Nil,'BTerm_GetTerm: Nil');
  BTerm_GetTerm := B^.BT_TERM
End;

{ get clock header }
Function BTerm_GetHeader( B : BTermPtr ) : HeadPtr;
Begin
  BTerm_GetHeader := B^.BT_HEAD
End;

{ set clock header }
Procedure BTerm_SetHeader( B : BTermPtr; H : HeadPtr );
Begin
  B^.BT_HEAD := H
End;

{-----------------------------------------------------------------------}
{ metadata                                                              }
{-----------------------------------------------------------------------}

Procedure BTerm_GetMetadata( B : BTermPtr; 
    Var GoalType : TGoalType; Var Access : IdPtr; Var Arity : TArity );
Var
  T : TermPtr;
Begin
  T := BTerm_GetTerm(B);
  Access := AccessIdentifier(T);
  Arity := GetArity(T);
  GoalType := IdentifierToGoalType(Access)  
End;

{ get type }
Function BTerm_GetType( B : BTermPtr ) : TGoalType;
Var 
  GoalType : TGoalType;
  Access : IdPtr; 
  Arity : TArity; 
Begin
  BTerm_GetMetadata(B,GoalType,Access,Arity);
  BTerm_GetType := GoalType
End;

{ access identifier of a block }
Function BTerm_GetAccessTerm( B : BTermPtr ) : IdPtr;
Var 
  GoalType : TGoalType;
  Access : IdPtr; 
  Arity : TArity; 
Begin
  BTerm_GetMetadata(B,GoalType,Access,Arity);
  BTerm_GetAccessTerm := Access
End;

{-----------------------------------------------------------------------}
{ list of BTerms                                                        }
{-----------------------------------------------------------------------}

{ next BTerm }
Function BTerms_GetNext( B : BTermPtr ) : BTermPtr;
Begin
  CheckCondition(B <> Nil,'BTerms_GetNext: Nil');
  BTerms_GetNext := B^.BT_NEXT
End;

{ make B points to B1 }
Procedure BTerms_SetNext( B : BTermPtr; B1 : BTermPtr );
Begin
  B^.BT_NEXT := B1
End;

{ get the last BTerm in a list }
Function BTerms_GetLast( B : BTermPtr ) : BTermPtr;
Begin
  While (BTerms_GetNext(B) <> Nil) Do
    B := BTerms_GetNext(B);
  BTerms_GetLast := B
End;

{ make all cut terms in the list B point back to header H }
Procedure BTerms_SetCutHeader( B : BTermPtr; H : HeadPtr );
Begin
  While B <> Nil Do
  Begin
    If BTerm_GetType(B) = GOAL_CUT Then
      BTerm_SetHeader(B,H);
    B := BTerms_GetNext(B)
  End
End;

End.