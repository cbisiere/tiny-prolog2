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
  Memory,
  PObj,
  PObjTerm,
  PObjFCVI,
  PObjStr,
  PObjDef,
  Tuple;

{ BTerm }
Function BTerm_New( T : TermPtr ) : BTermPtr;
Function BTerm_NewSpecial( GoalType : TGoalType; I : IdPtr; 
    H : HeadPtr ) : BTermPtr;

Function BTerm_GetTerm( B : BTermPtr ) : TermPtr;
Function BTerm_GetAccessTerm( B : BTermPtr ) : IdPtr;
Function BTerm_GetArity( B : BTermPtr ) : TArity;
Procedure BTerm_SetArity( B : BTermPtr; a : TArity );
Function BTerm_GetHeader( B : BTermPtr ) : HeadPtr;
Procedure BTerm_SetHeader( B : BTermPtr; H : HeadPtr );
Function BTerm_GetType( B : BTermPtr ) : TGoalType;
Procedure BTerm_SetType( B : BTermPtr; GoalType : TGoalType );

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
Function BTerm_Build( GoalType : TGoalType; I : IdPtr; a : TArity; 
    T : TermPtr ) : BTermPtr;
Var 
  B : BTermPtr;
  ptr : TObjectPtr Absolute B;
Begin
  ptr := NewRegisteredPObject(BT,SizeOf(TObjBTerm),4,True,3);
  With B^ Do
  Begin
    BT_TERM := T;
    BT_NEXT := Nil;
    BT_HEAD := Nil;
    BT_ACCE := I;
    BT_ARIT := a;
    BT_TYPE := GoalType
  End;
  BTerm_Build := B
End;

{ new block for term T }
Function BTerm_New( T : TermPtr ) : BTermPtr;
Var 
  I : IdPtr;
  a : TArity;
  GoalType : TGoalType;
Begin
  I := AccessIdentifier(T);
  a := Arity(T);
  GoalType := GOAL_STD;
  
  If I <> Nil Then
  Begin
    If IdentifierIsCut(I) Then
      GoalType := GOAL_CUT
    Else If IdentifierIsSyscall(I) Then
      GoalType := GOAL_SYS
  End;
  BTerm_New := BTerm_Build(GoalType,I,a,T)
End;

{ return a special BTerm with a back pointer, to handle findall/3 or block/2;
 identifier is for convenience (tracing) }
Function BTerm_NewSpecial( GoalType : TGoalType; I : IdPtr; 
    H : HeadPtr ) : BTermPtr;
Var
  B : BTermPtr;
Begin
  CheckCondition(GoalType In [GOAL_FIND,GOAL_BLOCK],'BTerm_NewSpecial: type');
  B := BTerm_Build(GoalType,I,0,TermPtr(I));
  BTerm_SetHeader(B,H);
  BTerm_NewSpecial := B
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

{ access identifier of a block }
Function BTerm_GetAccessTerm( B : BTermPtr ) : IdPtr;
Begin
  BTerm_GetAccessTerm := B^.BT_ACCE
End;

{ arity  }
Function BTerm_GetArity( B : BTermPtr ) : TArity;
Begin
  BTerm_GetArity := B^.BT_ARIT
End;

{ set arity }
Procedure BTerm_SetArity( B : BTermPtr; a : TArity );
Begin
  B^.BT_ARIT := a
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

{ get type }
Function BTerm_GetType( B : BTermPtr ) : TGoalType;
Begin
  BTerm_GetType := B^.BT_TYPE
End;

{ set clock header }
Procedure BTerm_SetType( B : BTermPtr; GoalType : TGoalType );
Begin
  B^.BT_TYPE := GoalType
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