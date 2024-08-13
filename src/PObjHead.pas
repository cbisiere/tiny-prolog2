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
  PObjFCVI,
  PObjBter;

Function Header_New : HeadPtr;

{ header }

Function Header_GetClock( H : HeadPtr ) : TClock;
Procedure Header_SetClock( H : HeadPtr; Clock : TClock );
Function Header_ClockAtStart( H : HeadPtr ) : Boolean;

Function Header_GetCutTarget( H : HeadPtr ) : HeadPtr;
Procedure Header_SetCutTarget( H : HeadPtr; CutTarget : HeadPtr );

Function Header_GetSideCarTerm( H : HeadPtr ) : TermPtr;
Procedure Header_SetSideCarTerm( H : HeadPtr; T : TermPtr );

Function Header_GetSideCarObject( H : HeadPtr ) : Pointer;
Procedure Header_SetSideCarObject( H : HeadPtr; p : Pointer );

Function Header_GetRule( H : HeadPtr ) : RulePtr;
Procedure Header_SetRule( H : HeadPtr; R : RulePtr );

Function Header_IsDone( H : HeadPtr ) : Boolean ;
Procedure Header_SetIsDone( H : HeadPtr; Over : Boolean );

Function Header_GetOngoingFind( H : HeadPtr ) : Boolean;
Procedure Header_SetOngoingFind( H : HeadPtr; FindGoal : Boolean );

Function Header_GetBlockScope( H : HeadPtr ) : HeadPtr;
Procedure Header_SetBlockScope( H : HeadPtr; Hb : HeadPtr );

Function Header_IsCleared( H : HeadPtr ) : Boolean ;
Procedure Header_SetCleared( H : HeadPtr; Clear : Boolean );

Function Header_GetSuccessCount( H : HeadPtr ) : TBranch;
Procedure Header_IncSuccessCount( H : HeadPtr );

Function Header_GetBranchNumber( H : HeadPtr ) : TBranch;
Procedure Header_IncBranchNumber( H : HeadPtr );

Function Header_GetGoalsToClear( H : HeadPtr ) : BTermPtr;
Procedure Header_SetGoalsToClear( H : HeadPtr; Goals : BTermPtr);

Function Header_GetTermToClear( H : HeadPtr ) : TermPtr;

Function Header_GetMore( H : HeadPtr ): Boolean ;
Procedure Header_SetMore( H : HeadPtr; More : Boolean );

Procedure Header_GetGoalMetadata( H : HeadPtr; 
    Var GoalType : TGoalType; Var Access : IdPtr; Var Arity : TArity );
Function Header_IsUserLand( H : HeadPtr ) : Boolean;
Function Header_IsSolution( H : HeadPtr ) : Boolean;
Function Header_IsLeaf( H : HeadPtr ) : Boolean;
Procedure Header_InsertGoalsToClear( H : HeadPtr; Goals : BTermPtr );
Function Header_EndOfSearch( H : HeadPtr ) : Boolean;

{ list of headers }
Function Headers_GetNext( H : HeadPtr ) : HeadPtr;
Function Headers_GetLast( H : HeadPtr ) : HeadPtr;
Function Headers_PushNew( Ht : HeadPtr; Goals : BTermPtr ) : HeadPtr;

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
  ptr := NewRegisteredPObject(HE,SizeOf(TObjHead),8,True,0);
  With H^ Do
  Begin
    HH_NEXT := H;
    HH_RULE := Nil;
    HH_FBCL := Nil;
    HH_REST := Nil;
    HH_BACK := Nil;
    HH_BLOC := Nil;
    HH_CHOI := Nil;
    HH_CHOV := Nil;
    { other }
    HH_SUCC := 0;
    HH_BRAN := 0;
    HH_CLEA := False;
    HH_CLOC := 0;
    HH_MORE := False;
    HH_FGOA := False;
    HH_DONE := False
  End;
  Header_New := H
End;

{-----------------------------------------------------------------------}
{ goals                                                                 }
{-----------------------------------------------------------------------}

{ get the list of goals to clear }
Function Header_GetGoalsToClear( H : HeadPtr ) : BTermPtr;
Begin
  Header_GetGoalsToClear := H^.HH_FBCL
End;

{ set the list of goals to clear }
Procedure Header_SetGoalsToClear( H : HeadPtr; Goals : BTermPtr);
Begin
  H^.HH_FBCL := Goals
End;

{ get the term to clear }
Function Header_GetTermToClear( H : HeadPtr ) : TermPtr;
Begin
  Header_GetTermToClear := BTerm_GetTerm(Header_GetGoalsToClear(H))
End;

{-----------------------------------------------------------------------}
{ [CUT] cut target                                                      }
{-----------------------------------------------------------------------}

{ [CUT] documentation for cut:
 a cut target is the header that cleared a goal using a rule whose queue 
 contained a cut; the cut goal points back to that target, allowing to 
 backtrack several steps and try again to clear that same goal;

 For instance, consider the following query and rules:

   Q: yy aa zz;

   R0: yy ->;
   R1: aa -> bb ! cc;
   R2: aa ->;
   R3: bb ->;
   R4: cc ->;
   R5: zz ->;

  Sequence of headers: back [ header | rule to apply | list of goals to clear ]
      [ 0 | R0 | yy aa zz ]
      [ 1 | R1 | aa zz ]
      [ 2 | R3 | bb !<H1> cc zz ]  => '!' points to H1 ([CUT:1])
  <H1>[ 3 | !  | ! cc zz ]         => set back ptr of H3 to H1, using the <H1>
      [ 4 | R4 | cc zz ]              above ([CUT:2])
      [ 5 | R5 | zz ] 
      [ 6 | -- | Nil ]             => done: backtrack from H6 to H0 ([CUT:3])

  When clearing 'aa' using R1, '!' points back to header 1 and the 
  clock move forward; then, upon clearing '!', its clock pointer is set to H0, 
  the clock header *before* the one used to clear aa; further backtrack process 
  honors this back pointer, all the choices from (an including) 'aa' to the '!' 
  are 'forgotten'; this is done by artificially exhausting the choice points 
  until we reach the target.
 }

{ cut target }
Function Header_GetCutTarget( H : HeadPtr ) : HeadPtr;
Begin
  Header_GetCutTarget := H^.HH_BACK
End;

{ set cut target }
Procedure Header_SetCutTarget( H : HeadPtr; CutTarget : HeadPtr );
Begin
  H^.HH_BACK := CutTarget
End;

{-----------------------------------------------------------------------}
{ sidecar: fields to support complex predicates                         }
{-----------------------------------------------------------------------}

{ a sidecar term is used by:
 1) findall(V,G,L) to store one solution V before it is appended in list L
 2) block(T,G) to store the label of the block scope }

{ sidecar term }
Function Header_GetSideCarTerm( H : HeadPtr ) : TermPtr;
Begin
  Header_GetSideCarTerm := H^.HH_CHOV
End;

{ set sidecar term }
Procedure Header_SetSideCarTerm( H : HeadPtr; T : TermPtr );
Begin
  H^.HH_CHOV := T
End;

{ a sidecar Prolog object pointer is used by:
 1) findall(V,G,L) to accumulate solutions before conversion to a list V 
  by a second call to findall/3 }

{ sidecar term }
Function Header_GetSideCarObject( H : HeadPtr ) : Pointer;
Begin
  Header_GetSideCarObject := H^.HH_CHOI
End;

{ set sidecar term }
Procedure Header_SetSideCarObject( H : HeadPtr; p : Pointer );
Begin
  H^.HH_CHOI := p
End;


{-----------------------------------------------------------------------}
{ [FIND]                                                                }
{-----------------------------------------------------------------------}

{ [FIND] documentation for findall:
 1) [FIND:1] the syscall findall(T,G,L) is executed first, returning T,G,and L,
  and requesting to be called a second time (More=True)
 2) [FIND:2] then, upon returning from the syscall
  - G is inserted in the list of goals to clear
  at the new top header (H) level, along with a special goal F<H> pointing to H
  - a boolean is set in H to indicate that a findall is ongoing
  - the term T is saved in H; this terms will accumulate constraints as the
   search space for G keep going
 3) [FIND:3] while working on G
  - propagate upward the 'find' indicator; knowing we are currently clearing G 
  is useful to silent the 'no rule to clear' message for any goal involved in 
  the clearing
 4) [FIND:4] upon clearing the special goal F<H> (which means a solution to G 
  was found):
  - make a copy of the term T (which is now bound to a solution, and is stored
   in H, so we can reach it from the special goal F<H> )
  - append this T to the list of solutions, which is located in the header just 
   below H, that is, in the header whose first goal was the syscall
 5) [FIND:5] upon the second call to findall(T,G,L):
  - convert the list of solutions terms T to a Prolog list
  - unify it with L, passed back to the user as the solution to findall
}

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

{-----------------------------------------------------------------------}
{ [BLOCK]                                                               }
{-----------------------------------------------------------------------}

{ examples:

  aa -> bb block(1,cc) dd;

  clear bb, then cc; if cc executes a block_exit(1), go to dd immediately, and 
  then upon backtracking, backtrack until bb (forgetting all the choices made
  when clearing cc).
}

{ [BLOCK] documentation for block / block-exit:
 1) [BLOCK:1] the syscall block(T,G) is executed: 
  - it returns the label T and the goal G
 2) [BLOCK:2] then, upon returning from the syscall
  - G is inserted in the list of goals to clear
  at the new top header (H) level, along with a special goal B<H> pointing to H
  - a pointer to H is set in H to indicate that a block is currently open
  - the label T is saved in H
 3) [BLOCK:3] while working on G
  - propagate upward the 'block' back pointer (scope)
 At this point, either the special goal B<H> is cleared, meaning G has been 
  cleared w/o a block-exit, or a block-exit is executed 
 4) [BLOCK:4.1] if the special goal B<H> is cleared
  - the current scope is closed now: update the block scope indicator
  - succeed, moving forward, in order to clear the goals after block(T,G)
 5) [BLOCK:4.2] if we have to clear a syscall block_exit(T)
  - the syscall just returns the label T
 6) [BLOCK:5] upon returning from a syscall block_exit
  - we follow the 'block' back pointers to find the adequate target header, if 
   any; the target is the goal G set for clearing by the matching block(T,G)
  - we modify the list of goal of the top header H, to eliminate all the goals
   up to the special goal matching the target header; this will force getting 
   out (and forward) of the block, per design, e.g. given "block(1,aa) bb", any
   block_exit(1) during the execution of aa jumps to bb
  - we set a cut target to G, for later backtracking, in order to forget all the
   choices (inside the block scope) until the block_exit
}

{ header of the last opened block(T,G) }
Function Header_GetBlockScope( H : HeadPtr ) : HeadPtr;
Begin
  Header_GetBlockScope := H^.HH_BLOC
End;

{ set last opened block(T,G) }
Procedure Header_SetBlockScope( H : HeadPtr; Hb : HeadPtr );
Begin
  H^.HH_BLOC := Hb
End;

{-----------------------------------------------------------------------}
{ clock                                                                 }
{-----------------------------------------------------------------------}

{ get clock time }
Function Header_GetClock( H : HeadPtr ) : TClock;
Begin
  Header_GetClock := H^.HH_CLOC
End;

{ set the clock }
Procedure Header_SetClock( H : HeadPtr; Clock : TClock );
Begin
  H^.HH_CLOC := Clock
End;

{ return True if the clock time is (back to) its initial value }
Function Header_ClockAtStart( H : HeadPtr ) : Boolean;
Begin
  Header_ClockAtStart := Header_GetClock(H) = 0
End;

{-----------------------------------------------------------------------}
{ get / set                                                             }
{-----------------------------------------------------------------------}

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

{ get 'is done' status }
Function Header_IsDone( H : HeadPtr ) : Boolean ;
Begin
  Header_IsDone := H^.HH_DONE
End;

{ set choice node 'is done' status }
Procedure Header_SetIsDone( H : HeadPtr; Over : Boolean );
Begin
  H^.HH_DONE := Over
End;

{ get clear status }
Function Header_IsCleared( H : HeadPtr ) : Boolean ;
Begin
  Header_IsCleared := H^.HH_CLEA
End;

{ current goal has been cleared }
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

{ get metadata about the current goal; the current goal, if any, in header H, 
 is the first goal in the HH_FBCL list }
Procedure Header_GetGoalMetadata( H : HeadPtr; 
    Var GoalType : TGoalType; Var Access : IdPtr; Var Arity : TArity );
Begin
  BTerm_GetMetadata(Header_GetGoalsToClear(H),GoalType,Access,Arity)
End;

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

{ return True if we are done with the query, that is, the clock is back to
 initial state and there are no more branches to explore }
Function Header_EndOfSearch( H : HeadPtr ) : Boolean;
Begin
  Header_EndOfSearch := Header_ClockAtStart(H) And Header_IsDone(H)
End;

{-----------------------------------------------------------------------}
{ list                                                                  }
{-----------------------------------------------------------------------}

{ next header }
Function Headers_GetNext( H : HeadPtr ) : HeadPtr;
Begin
  Headers_GetNext := H^.HH_NEXT
End;

{ last in the list (that is, clock-zero header) }
Function Headers_GetLast( H : HeadPtr ) : HeadPtr;
Begin
  While Headers_GetNext(H) <> Nil Do
    H := Headers_GetNext(H);
  Headers_GetLast := H
End;

{ set next header }
Procedure Headers_SetNext( H : HeadPtr; NextH : HeadPtr );
Begin
  H^.HH_NEXT := NextH
End;

{-----------------------------------------------------------------------}
{ dynamics                                                              }
{-----------------------------------------------------------------------}

{ insert a list of goals to clear and update the header accordingly }
Procedure Header_InsertGoalsToClear( H : HeadPtr; Goals : BTermPtr );
Var
  LastGoal : BTermPtr;
Begin
  { create a new list old goals -> new goals }
  LastGoal := BTerms_GetLast(Goals);
  BTerms_SetNext(LastGoal,Header_GetGoalsToClear(H));
  { set H with the new list of goals}
  Header_SetGoalsToClear(H,Goals)
End;

{ create and set a clock header on top of a list of headers Ht; Goals
 can be Nil, as move forward always creates a new header, even when there was
 a single goal to clear }
Function Headers_PushNew( Ht : HeadPtr; Goals : BTermPtr ) : HeadPtr;
Var 
  H : HeadPtr;
Begin
  H := Header_New;
  { set goals and update header metadata }
  If Goals <> Nil Then
    Header_InsertGoalsToClear(H,Goals);
  { link }
  Headers_SetNext(H,Ht);
  { update clock }
  If Headers_GetNext(H) <> Nil Then
    Header_SetClock(H,Header_GetClock(Headers_GetNext(H)) + 1);

  Headers_PushNew := H
End;

End.