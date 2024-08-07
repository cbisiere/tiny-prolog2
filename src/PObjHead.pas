{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObjHead.pas                                               }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{             P R O L O G   O B J E C T :   C L O C K   H E A D E R          }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Unit PObjHead;

Interface

Uses
  Errs,
  Memory,
  PObj,
  PObjStr,
  PObjDef,
  PObjTerm,
  PObjBter;

Function Header_New : HeadPtr;

Function Header_IsCut( H : HeadPtr ) : Boolean;
Function Header_IsSys( H : HeadPtr ) : Boolean;

Function Header_GetRule( H : HeadPtr ) : RulePtr;
Procedure Header_SetRule( H : HeadPtr; R : RulePtr );

Function Header_GetClock( H : HeadPtr ) : LongInt;

Function Header_GetOngoingFind( H : HeadPtr ) : Boolean;
Procedure Header_SetOngoingFind( H : HeadPtr; FindGoal : Boolean );

Function Header_IsCleared( H : HeadPtr ): Boolean ;
Procedure Header_SetCleared( H : HeadPtr; Clear : Boolean );

Function Header_GetSuccessCount( H : HeadPtr ) : TBranch;
Procedure Header_IncSuccessCount( H : HeadPtr );

Function Header_GetBranchNumber( H : HeadPtr ) : TBranch;
Procedure Header_IncBranchNumber( H : HeadPtr );

Function Header_GetGoalsToClear( H : HeadPtr ) : BTermPtr;

Function Header_GetTermToClear( H : HeadPtr ) : TermPtr;

Procedure Header_GetClearingInfo( H : HeadPtr; 
    Var R : RulePtr; Var isSys : Boolean; Var isCut : Boolean );
Procedure Header_SetClearingInfo( H : HeadPtr; 
    R : RulePtr; isSys, isCut : Boolean);

Function Header_GetMore( H : HeadPtr ): Boolean ;
Procedure Header_SetMore( H : HeadPtr; More : Boolean );

Function Header_IsUserLand( H : HeadPtr ) : Boolean;
Function Header_IsSolution( H : HeadPtr ) : Boolean;
Function Header_IsLeaf( H : HeadPtr ) : Boolean;
Procedure Header_InsertGoalsToClear( H : HeadPtr; B : BTermPtr );
Procedure Header_RemoveGoalToClear( H : HeadPtr );

Function Headers_GetNext( H : HeadPtr ) : HeadPtr;
Procedure Headers_PushNew( Var list : HeadPtr; Fbcl : BTermPtr; R : RulePtr; 
    isSys, isCut : Boolean );


Implementation

{-----------------------------------------------------------------------}
{ constructor                                                           }
{-----------------------------------------------------------------------}

{ new clock header }
Function Header_New : HeadPtr;
Var 
  H : HeadPtr;
  ptr : TObjectPtr Absolute H;
Begin
  ptr := NewRegisteredPObject(HE,SizeOf(TObjHead),7,True,0);
  With H^ Do
  Begin
    HH_NEXT := Nil;
    HH_RULE := Nil;
    HH_FBCL := Nil;
    HH_REST := Nil;
    HH_BACK := Nil;
    HH_CHOI := Nil;
    HH_CHOV := Nil;
    HH_SUCC := 0;
    HH_BRAN := 0;
    HH_CLEA := False;
    HH_CLOC := 0;
    HH_ISYS := False;
    HH_MORE := False;
    HH_FGOA := False;
    HH_ICUT := False
  End;
  Header_New := H
End;

{-----------------------------------------------------------------------}
{ get / set                                                             }
{-----------------------------------------------------------------------}

{ goal to clear is a cut }
Function Header_IsCut( H : HeadPtr ) : Boolean;
Begin
  Header_IsCut := H^.HH_ICUT
End;

{ goal to clear is a system call }
Function Header_IsSys( H : HeadPtr ) : Boolean;
Begin
  Header_IsSys := H^.HH_ISYS
End;

{ get the rule of a clock header }
Function Header_GetRule( H : HeadPtr ) : RulePtr;
Begin
  Header_GetRule := H^.HH_RULE
End;

{ set the rule of a clock header }
Procedure Header_SetRule( H : HeadPtr; R : RulePtr );
Begin
  H^.HH_RULE := R
End;

{ get clock time }
Function Header_GetClock( H : HeadPtr ) : LongInt;
Begin
  Header_GetClock := H^.HH_CLOC
End;

{ set the clock }
Procedure Header_SetClock( H : HeadPtr; Clock : LongInt );
Begin
  H^.HH_CLOC := Clock
End;

{ are we currently clearing a goal of a findall(V,G,L)?  }
Function Header_GetOngoingFind( H : HeadPtr ) : Boolean;
Begin
  Header_GetOngoingFind := H^.HH_FGOA
End;

{ set FindGoal status }
Procedure Header_SetOngoingFind( H : HeadPtr; FindGoal : Boolean );
Begin
  H^.HH_FGOA := FindGoal
End;

{ get clear status }
Function Header_IsCleared( H : HeadPtr ): Boolean ;
Begin
  Header_IsCleared := H^.HH_CLEA
End;

{ set the rule of a clock header }
Procedure Header_SetCleared( H : HeadPtr; Clear : Boolean );
Begin
  H^.HH_CLEA := Clear
End;

{ get number of successes so far  }
Function Header_GetSuccessCount( H : HeadPtr ) : TBranch;
Begin
  Header_GetSuccessCount := H^.HH_SUCC
End;

{ increase the number of successes }
Procedure Header_IncSuccessCount( H : HeadPtr );
Begin
  H^.HH_SUCC := H^.HH_SUCC + 1
End;

{ get current branch number }
Function Header_GetBranchNumber( H : HeadPtr ) : TBranch;
Begin
  Header_GetBranchNumber := H^.HH_BRAN
End;

{ next branch we are going to explore }
Procedure Header_IncBranchNumber( H : HeadPtr );
Begin
  H^.HH_BRAN := H^.HH_BRAN + 1
End;

{ get the list of goals to clear }
Function Header_GetGoalsToClear( H : HeadPtr ) : BTermPtr;
Begin
  Header_GetGoalsToClear := H^.HH_FBCL
End;

{ set the list of goals to clear }
Procedure Header_SetGoalsToClear( H : HeadPtr; B : BTermPtr);
Begin
  H^.HH_FBCL := B
End;

{ get the term to clear }
Function Header_GetTermToClear( H : HeadPtr ) : TermPtr;
Begin
  Header_GetTermToClear := BTerm_GetTerm(Header_GetGoalsToClear(H))
End;

{ get data about what to clear and how  }
Procedure Header_GetClearingInfo( H : HeadPtr; 
    Var R : RulePtr; Var isSys : Boolean; Var isCut : Boolean );
Begin
  R := H^.HH_RULE;
  isSys := H^.HH_ISYS;
  isCut := H^.HH_ICUT
End;

{ set the rule data of a clock header }
Procedure Header_SetClearingInfo( H : HeadPtr; R : RulePtr; isSys, isCut : Boolean );
Begin
  With H^ Do
  Begin
    HH_RULE := R;
    HH_ISYS := isSys;
    HH_ICUT := isCut
  End
End;

{ get more status }
Function Header_GetMore( H : HeadPtr ): Boolean ;
Begin
  Header_GetMore := H^.HH_MORE
End;

{ set the rule of a clock header }
Procedure Header_SetMore( H : HeadPtr; More : Boolean );
Begin
  H^.HH_MORE := More
End;

{-----------------------------------------------------------------------}
{ methods                                                               }
{-----------------------------------------------------------------------}

{ return True if the rule in H is a user land rule }
Function Header_IsUserLand( H : HeadPtr ) : Boolean;
Begin
  CheckCondition(Header_GetRule(H) <> Nil,'Header_IsUserLand: Nil');
  Header_IsUserLand := Rule_IsUserLand(Header_GetRule(H))
End;

{ return True if header H is a solution, that is, the (sole) goal of the 
 previous header has been cleared and the current header has no goals to clear }
Function Header_IsSolution( H : HeadPtr ) : Boolean;
Begin
  Header_IsSolution := Header_IsCleared(Headers_GetNext(H)) And 
      (Header_GetGoalsToClear(H) = Nil)
End;

{ return True if header H is a leaf in the search space, that is, H failed or 
 there are no goals left to clear }
Function Header_IsLeaf( H : HeadPtr ) : Boolean;
Begin
  Header_IsLeaf := Not Header_IsCleared(Headers_GetNext(H)) Or 
      (Header_GetGoalsToClear(H) = Nil)
End;

{ insert a list of goals to clear }
Procedure Header_InsertGoalsToClear( H : HeadPtr; B : BTermPtr );
Var
  BLast : BTermPtr;
Begin
  BLast := BTerms_GetLast(B);
  BTerms_SetNext(BLast,Header_GetGoalsToClear(H));
  Header_SetGoalsToClear(H,B)
End;

{ remove the current goal to clear }
Procedure Header_RemoveGoalToClear( H : HeadPtr );
Begin
  Header_SetGoalsToClear(H,BTerms_GetNext(Header_GetGoalsToClear(H)))
End;


{-----------------------------------------------------------------------}
{ list                                                                  }
{-----------------------------------------------------------------------}

{ next header }
Function Headers_GetNext( H : HeadPtr ) : HeadPtr;
Begin
  Headers_GetNext := H^.HH_NEXT
End;

{ set next header }
Procedure Headers_SetNext( H : HeadPtr; NextH : HeadPtr );
Begin
  H^.HH_NEXT := NextH
End;

{ append a clock header to a list (which may be Nil) }
Procedure Headers_Push( Var list : HeadPtr; H : HeadPtr );
Begin
  Headers_SetNext(H,list);
  list := H;
  If Headers_GetNext(H) <> Nil Then
    Header_SetClock(H,Header_GetClock(Headers_GetNext(H)) + 1)
End;

{ create and set a clock header on top of a list of headers; list now points to
 the new clock header }
Procedure Headers_PushNew( Var list : HeadPtr; Fbcl : BTermPtr; R : RulePtr; 
    isSys, isCut : Boolean );
Var 
  H : HeadPtr;
Begin
  H := Header_New;
  With H^ Do
  Begin
    HH_FBCL := Fbcl
  End;
  Header_SetClearingInfo(H,R,isSys,isCut);
  Headers_Push(list,H)
End;

End.