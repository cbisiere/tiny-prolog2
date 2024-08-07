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
Function BTerm_NewFindAll : BTermPtr;

Function BTerm_GetTerm( B : BTermPtr ) : TermPtr;
Function BTerm_GetAccessTerm( B : BTermPtr ) : IdPtr;
Function BTerm_GetArity( B : BTermPtr ) : PosInt;
Function BTerm_GetHeader( B : BTermPtr ) : HeadPtr;
Procedure BTerm_SetHeader( B : BTermPtr; H : HeadPtr );
Function BTerm_GetType( B : BTermPtr ) : TGoalType;
Procedure BTerm_SetType( B : BTermPtr; GoalType : TGoalType );

{ list }
Function BTerms_GetNext( B : BTermPtr ) : BTermPtr;
Procedure BTerms_SetNext( B : BTermPtr; B1 : BTermPtr );
Function BTerms_GetLast( B : BTermPtr ) : BTermPtr;
Procedure BTerms_SetHeader( B : BTermPtr; H : HeadPtr );

Implementation

{-----------------------------------------------------------------------}
{ constructor                                                           }
{-----------------------------------------------------------------------}

{ new block of type GoalType, access I, arity a, term T }
Function BTerm_Build( GoalType : TGoalType; I : IdPtr; a : PosInt; 
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
  a : PosInt;
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

{ return a special BTerm to handle findall/3 }
Function BTerm_NewFindAll : BTermPtr;
Begin
  BTerm_NewFindAll := BTerm_Build(GOAL_FIND,Nil,0,Nil)
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

{ arity of a block }
Function BTerm_GetArity( B : BTermPtr ) : PosInt;
Begin
  BTerm_GetArity := B^.BT_ARIT
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

{ make all terms in B point back to header H }
Procedure BTerms_SetHeader( B : BTermPtr; H : HeadPtr );
Begin
  While B <> Nil Do
  Begin
    BTerm_SetHeader(B,H);
    B := BTerms_GetNext(B)
  End
End;

End.