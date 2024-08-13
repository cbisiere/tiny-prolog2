{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Engine.pas                                                 }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                       P R O L O G   E N G I N E                            }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Unit Engine;

Interface

Uses
  ShortStr,
  Num,
  Errs,
  Readline,
  Files,
  Trace,
  Memory,
  PObj,
  PObjList,
  PObjTerm,
  PObjFCVI,
  PObjIO,
  PObjOp,
  PObjRest,
  PObjStr,
  PObjTok,
  PObjDict,
  PObjEq,
  PObjSys,
  PObjDef,
  PObjWrld,
  PObjBter,
  PObjRule,
  PObjQury,
  PObjHead,
  PObjProg,
  Encoding,
  Tokenize,
  Unparse,
  Reduc,
  Parse,
  Debug,
  Predef;

{ file suffix: for PII+ and Edinburgh, see PII+ doc p297 }
Type 
  TFileExt = Array[TSyntax] Of String[3];
Const 
  FileExt : TFileExt = ('p2c','pro','p2','p2E'); { TODO: rewrite to handle .pl cleanly }

Procedure ProcessCommandLine( P : ProgPtr );
Procedure LoadProgram( P : ProgPtr; s : StrPtr; TryPath : Boolean );

Implementation
{-----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{                                                                            }
{                  P R E D E F I N E D   P R E D I C A T E S                 }
{                                                                            }
{----------------------------------------------------------------------------}

{ insert("file") or insert('file'); 
 this predicate creates a giant dependency / execution loop:
 run -> exec insert -> clock -> exec insert -> run 
 and thus this large Engine unit }
Function ClearInsert( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  FileNamePtr : StrPtr;
Begin
  ClearInsert := False;
  { 1: filename (string or identifier, unquoted) }
  FileNamePtr := GetAtomArgAsStr(1,T,False);
  If FileNamePtr = Nil Then 
    Exit;
  { execute }
  LoadProgram(P,FileNamePtr,True);
  If Error Then 
    Exit;
  ClearInsert := True
End;

{ execute a system call syscall(Code,Arg1,...ArgN), meaning 
 Code(Arg1,...,ArgN); 
 - if the syscall is freeze or findall, this function sets the variable V and
   the goal G;
 - L is the undo stack of the header on top of the header for the syscall; 
   backtrackable syscalls (e.g. rule/2) need to use it; that way, variables
   in the syscall will be freed when backtracking from the top header, allowing 
   to move to the next solution for that syscall 
 - SuccessCount is the number of successes so far for the current goal; zero 
   means this is the initial call to the syscall 
 - Choices stores whatever is needed to represent the remaining 
   choices of a syscall that yields several results, e.g. rule/2; Choices is Nil
   on first call 
 - Predef is set to the predefined syscall
 - syscall set More to True if they want to be called again }
Function ExecutionSysCallOk( P : ProgPtr; Q : QueryPtr; T : TermPtr; 
    Var V,G : TermPtr; Var L : RestPtr; 
    SuccessCount : LongInt; Var Choices : Pointer; 
    Var Predef : TPP; Var More : Boolean ) : Boolean;
Var
  Ok : Boolean;
Begin
  ExecutionSysCallOk := False; { default is to fail }

  More := False; { by default, syscall are only called once }

  If Not PredefCallIsOk(P,T,Predef) Then
    Exit;

  If Predef = PP_INSERT Then
    Ok := ClearInsert(P,T)
  Else
    Ok := ClearPredef(Predef,P,Q,T,V,G,L,SuccessCount,Choices,More);
  ExecutionSysCallOk := Ok
End;


{----------------------------------------------------------------------------}
{                                                                            }
{                                C L O C K                                   }
{                                                                            }
{----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ trace                                                                      }
{----------------------------------------------------------------------------}

Type TTraceEvent = (TRACE_CALL,TRACE_REDO,TRACE_EXIT,TRACE_FAIL,TRACE_GOAL);

{ is a goal traceable? TODO:  restrict to userland }
Function Traceable( H : HeadPtr ) : Boolean;
Begin
  Traceable := True
End;

{ should a trace line be printed? }
Function MustTrace( P : ProgPtr; H : HeadPtr ) : Boolean;
Begin
  MustTrace := GetTrace(P) And Traceable(H)
End;

{ print the trace event }
Procedure PrintTraceEvent( P : ProgPtr; H : HeadPtr; TraceEvent : TTraceEvent );
Var
  Depth : TClock;
  Branch : TBranch;
  Tag : TString;
Begin
  Depth := Header_GetClock(H) + 1;
  Branch := Header_GetBranchNumber(H);

  { event tag as a short string }
  Case TraceEvent Of
  TRACE_CALL: Tag := 'Call';
  TRACE_REDO: Tag := 'Redo';
  TRACE_EXIT: Tag := 'Exit';
  TRACE_FAIL: Tag := 'Fail';
  TRACE_GOAL: Tag := 'Goal'
  End;

  OutTraceMessage(GetTraceStream(P),GetSyntax(P),Tag,Depth,Branch,
      Header_GetTermToClear(H))
End;

{----------------------------------------------------------------------------}
{ Prolog clock                                                               }
{----------------------------------------------------------------------------}

{ clear a query }
Procedure Clock( P : ProgPtr; Q : QueryPtr );
Var
  B           : BTermPtr;
  GCCount     : Integer;  { counter to trigger GC }

  { initialize the clock }
  Procedure InitClock;
  Begin
    { use a per-query clock: needed because Clock must be reentrant to
      support "insert/1" }
    Q^.QU_HEAD := Nil;
    GCCount := 0
  End;

  { display on Crt constraints only about the variables in the query }
  Procedure WriteQuerySolution;
  Begin
    OutQuerySolution(GetOutputConsole(P),Q);
    CWriteLn
  End;

  { emit a warning that the current goal cannot be cleared }
  Procedure WarningNoRuleToApply( I : IdPtr; a : TArity );
  Var
    s : StrPtr;
  Begin
    CWriteWarning('no rules to clear goal');
    If I <> Nil Then
    Begin
      s := IdentifierGetStr(I);
      CWrite(': ');
      Str_CWrite(s);
      CWrite('/');
      CWritePosInt(a)
    End;
    CWriteLn
  End;

  { rule to apply to clear a goal with given ident/arity, starting at rule R;
   return Nil if there is not rule to apply }
  Function FindRuleToApply( I : IdPtr; a : TArity; R : RulePtr ) : RulePtr;
  Begin
    FindRuleToApply := Nil;
    If I = Nil Then { no access, goal is probably a variable }
      Exit;
    R := FindRuleWithHeadAndArity(R,I,a,False);
    If R = Nil Then
      Exit;
    FindRuleToApply := R
  End;

  {----------------------------------------------------------------------------}
  { Set candidate rules                                                        }
  {----------------------------------------------------------------------------}

  { prepare header H for the initial attempt to clear its goal, by:
   1) setting its 'done' status to False if there is no way to clear it, 
   2) setting the rule to apply, if any;
   return False if there is chance to clear the goal; only standard goals
   may not be clearable so early, i.e., when no rule can be used;
   if Silent is True, do not print a warning if the goal is impossible to clear }
  Function FirstCandidate( H : HeadPtr; Silent : Boolean ) : Boolean;
  Var
    R : RulePtr;
    GoalType : TGoalType; 
    Access : IdPtr;
    Arity : TArity;
  Begin
    FirstCandidate := False;
    CheckCondition(Header_GetGoalsToClear(H) <> Nil,'FirstCandidate: Nil');
    If Error Then Exit;
    Header_SetIsDone(H,False);
    Header_GetGoalMetadata(H,GoalType,Access,Arity);
    If GoalType = GOAL_STD Then
    Begin
      R := FindRuleToApply(Access,Arity,FirstRule(P,False));
      If R = Nil Then
      Begin
        If Not Silent Then
          WarningNoRuleToApply(Access,Arity);
        Header_SetIsDone(H,True);
        Exit
      End;
      Header_SetRule(H,R);
    End;
    FirstCandidate := True
  End;

  { prepare header H with the next candidate rule, by:
   1) setting its 'done' status to True if it is no more possible to clear it, 
   2) setting the next rule to apply, if any;
   return True in only two cases: 
    1) standard rule, when another candidate rule with matching access and arity
       exists; in that case H's current rule is set to that candidate   
    2) system call that requested to be called again }
  Function NextCandidate( H : HeadPtr ) : Boolean;
  Var 
    R : RulePtr;
    GoalType : TGoalType; 
    Access : IdPtr;
    Arity : TArity;
  Begin
    NextCandidate := False;
    CheckCondition(Header_GetGoalsToClear(H) <> Nil,'NextCandidate: Nil');
    If Error Then Exit;
    { [CUT:3] artificially already exhausted header }
    If Header_IsDone(H) Then
      Exit;
    { assume exhaustion and exit when it is indeed the case }
    Header_SetIsDone(H,True);
    Header_GetGoalMetadata(H,GoalType,Access,Arity);
    Case GoalType Of
    GOAL_BLOCK,GOAL_CUT,GOAL_FIND: { cleared only once }
      Exit;
    GOAL_SYS:
      If Header_GetMore(H) Then { syscall requested additional calls }
        Header_SetIsDone(H,False)
      Else
        Exit;
    GOAL_STD:
      Begin
        R := Header_GetRule(H);
        CheckCondition(R <> Nil,'NextCandidate: rule is Nil');
        R := FindRuleToApply(Access,Arity,NextRule(R,False)); { OPTIMIZE: enforce rule grouping }
        { no next rule at all, R was the last rule in the Prolog program }
        If R = Nil Then
          Exit;
        Header_SetRule(H,R);
        Header_SetIsDone(H,False)
      End
    End;
    NextCandidate := True
  End;

{----------------------------------------------------------------------------}
{ Backtracks the Prolog clock to the last usable point of choice.            }
{ Backtracking is done in three steps:                                       }
{ (1) unstack the current clock header                                       }
{ (2) undo memory changes                                                    }
{ (3) find the next rule to apply; if no rules apply, backtrack again        }
{----------------------------------------------------------------------------}

  { backtracking can occur in three different contexts (H is top header, Hc is
   the header just below):
   1) H: no more goals to clear (Nil)
   2) Hc: goal failed to clear
   3) Hc: goal cleared; H: no way to clear (FirstCandidate returned False)  
   }
  Procedure Backtracking( Var H : HeadPtr );
  Var
    CutTarget : HeadPtr; { target header set by a cut }
    NextH : HeadPtr;
    Stop : Boolean; { target reached }
    HasNext : Boolean; { next branch exists }
  Begin
    CutTarget := Nil;
    Stop := False;
    While Not Stop And Not Header_EndOfSearch(H) Do
    Begin
      { trace; we trace before the actual backtracking to preserve bindings 
       for nice printing }
      NextH := Headers_GetNext(H);
      If MustTrace(P,NextH) Then
      Begin
        If Header_IsCleared(NextH) Then
          PrintTraceEvent(P,NextH,TRACE_EXIT)
        Else
          PrintTraceEvent(P,NextH,TRACE_FAIL)
      End;

      { backtrack one step, undoing changes to the reduced system }
      Rest_Restore(H^.HH_REST); { restore and free restore list }
      H^.HH_REST := Nil;
      H := Headers_GetNext(H);

      { [CUT:3] detect and honor the first target header }
      If (CutTarget = Nil) And (Header_GetCutTarget(H) <> Nil) Then
        CutTarget := Header_GetCutTarget(H);
      { we acquired a target: we artificially keep exhausting the remaining 
       choices at all levels downward including the target itself; that way we
       will keep backtracking, going back to the goal before the targeted rule 
       head  }
      If CutTarget <> Nil then
      Begin
        Header_SetIsDone(H,True); { will make HasNext False }
        If H = CutTarget Then
          CutTarget := Nil { cancel the target, to branch again the next header }
      End;
      
      { try to advance the new top header to the next candidate to clear the 
        goal at step H }
      HasNext := NextCandidate(H);

      { more rules to try, stop here }
      Stop := HasNext
    End
  End;


{------------------------------------------------------------------}
{ Advance the Prolog clock by one period, as follows:              }
{ (1) create a new header pointing to a copy of the candidate rule }
{ (2) add to the set of constraints the equation:                  }
{     first term to clear = head of the rule                       }
{ (3) replace in the list of terms to clear the first term to      }
{     clear with the queue of the candidate rule                   }
{------------------------------------------------------------------}

  Procedure MoveForward( Var H : HeadPtr );
  Var
    Cleared : Boolean;  { current goal can be cleared}
    Hc : HeadPtr; { current header }
    Hz : HeadPtr; { header collecting findall results }
    Ss : SysPtr;
    ZTerm,ZGoal : TermPtr; { term and goal set by frozen/2 and findall/3 }
    LTerm : ListPtr; { collected term (findall/3) }
    FrozenM : TermsPtr;
    Predef : TPP;
    More : Boolean;
    GoalToClear,B : BTermPtr;
    R : RulePtr;
    I : IdPtr;
  Begin
    { initialize value to return in cas of block end }
    GoalToClear := Header_GetGoalsToClear(H);

    CheckCondition(GoalToClear <> Nil,'MoveForward: No terms to clear');

    { take note that we explore a (possibly) new branch to clear the current goal }
    Header_SetCleared(H,False);
    Header_IncBranchNumber(H);

    { backup pointer to current header Hc }
    Hc := H;

    { new header H, without the current goal; this new header is not fully ready
     for the next forward step yet, as the rule is Nil; a call to 
     FirstCandidate is required to find the first matching rule }
    H := Headers_PushNew(Hc,BTerms_GetNext(GoalToClear));

    { code below seeks to: 
     1) clear GoalToClear, the current goal in Hc, 
     2) prepare the new top header H for the next forward step, in case of 
      successful clearing;
     * Work on the new H includes: 
     - setting up its list of goal, as the list of goal in Hc but with the 
      first goal replaces with with the queue of the rule in Hc (or with 
      special goals to handle findall or block) 
     - setting up the restoration stack that will be used to backtrack to the 
      previous header Hc; this restoration allows to 'undo' the unification of
      the goal in Hc and the head of the rule in Hc  
     * Work on the old Hc includes:
     - setting its 'cleared' state
     - cut: set the cut target in preparation of a further backtracking step;
      this will forces the backtracking to proceed until the goal that was just
      before the cut, forgetting about all the choices made after the cut
      }

    { [FIND:3] propagate findall search indicator upward }
    Header_SetOngoingFind(H,Header_GetOngoingFind(Hc));

    { [BLOCK:3] propagate pointer to last opened block upward }
    Header_SetBlockScope(H,Header_GetBlockScope(Hc));

    { try to clear the current goal in Hc }
    Case BTerm_GetType(GoalToClear) Of
    GOAL_BLOCK: { [BLOCK:4.1] block(T,G): G cleared, w/o a block_exit(T) }
      Begin
        { note that we are now out of scope of that block, and back in the
         scope of the outer block, if any }
        Hz := Header_GetBlockScope(H);
        Hz := Headers_GetNext(Hz); 
        Header_SetBlockScope(H,Header_GetBlockScope(Hz));
        { success }
        Cleared := True
      End;
    GOAL_FIND: { [FIND:4] findall(V,G,L): G cleared, collect a solution V in L }
      Begin
        { header where V is stored }
        Hz := BTerm_GetHeader(GoalToClear);
        { prepare the new collected item; in order to prevent backtracking from 
          undoing the bindings we make a copy of the ZTerm, which 
          was bound by unification steps; this prevents backtracking from 
          undoing those bindings }
        LTerm := List_New(TObjectPtr(CopyTerm(Header_GetSideCarTerm(Hz),False)));
        { append it to the target list, just below }
        Hz := Headers_GetNext(Hz);
        List_Chain(Header_GetSideCarObject(Hz),LTerm);
        Header_SetSideCarObject(Hz,LTerm);
        { make it a fail to look for the next solution }
        Cleared := False
      End;
    GOAL_CUT: { [CUT:2] set the target header, for later backtracking }
      Begin
        Header_SetCutTarget(Hc,BTerm_GetHeader(GoalToClear));
        Cleared := True
      End;
    GOAL_SYS: { system call }
      Begin
        ZTerm := Nil;
        ZGoal := Nil;
        Cleared := ExecutionSysCallOk(P,Q,Header_GetTermToClear(Hc),
            ZTerm,ZGoal,H^.HH_REST,Header_GetSuccessCount(Hc),
            Hc^.HH_CHOI,Predef,More);

        { not whether the syscall asked to be called again }
        Header_SetMore(Hc,More);

        If Cleared Then
        Begin
          { special post-clearing treatment }
          Case Predef Of 
          PP_BLOCK: { [BLOCK:2] block(T,G); insert goals in H: -> G -> B<H> }
            Begin
              { insert special goal }
              B := BTerm_New(EmitShortIdent(P,SPECIAL_IDENT_BLOCK,False));
              BTerm_SetHeader(B,H);
              Header_InsertGoalsToClear(H,B);
              { then, insert the goal to clear, as returned by the syscall }
              Header_InsertGoalsToClear(H,BTerm_New(ZGoal));
              { switch the engine to in-block mode }
              Header_SetBlockScope(H,H);
              { save label T }
              Header_SetSideCarTerm(H,ZTerm)
            End;
          PP_BLOCK_EXIT: { [BLOCK:5] block_exit(T) }
            Begin
              { 1) find the target header Hz }
              Hz := Header_GetBlockScope(Hc);
              While (Hz <> Nil) And 
                  Not Unifiable(Header_GetSideCarTerm(Hz),ZTerm,GetDebugStream(P)) Do 
              Begin
                { previous label, if any, is pointed by the header just below 
                 the special header; this is the header for the block(T,G); it
                 points to an outer block, if any }
                Hz := Headers_GetNext(Hz); 
                Hz := Header_GetBlockScope(Hz);
              End;
              If Hz = Nil Then
              Begin
                RuntimeError('block exit with no matching block');
                Header_SetCutTarget(Hc,Headers_GetLast(Hc));
                Cleared := False
              End
              Else
              Begin
                { 1) deletes any choice points waiting for all the goals
                 enclosed in the parentheses labeled T: prepare the header to 
                 backtrack to the target header; note that the effect of this 
                 cut will be to go just beyond the target of the cut itself, 
                 meaning we will go back to the opening block(T,G) itself; 
                 in turn, since that opening can only be called once, the
                 backtrack process will go back to the goal before block/2, if
                 any }
                Header_SetCutTarget(Hc,Hz);
                { 2) aborts execution of all goals enclosed by the pair of 
                 parentheses labeled T: in H, bypass all goals before BLOCK[Hz]; 
                 for instance, when clearing a block_exit to an outer block 
                 (of clock level 4), top two headers might have the following 
                 goals to clear:
                 
                 [top] cc2(v2) -> BLOCK[8] 
                        -> bb2(y2) -> BLOCK[4] -> aa2(z) ->  Nil

                 [top-1] syscall(sysblockexit,1) -> cc2(v2) -> BLOCK[8] 
                        -> bb2(y2) -> BLOCK[4] -> aa2(z) ->  Nil

                 The code below changes [top] to:

                 [top] BLOCK[4] -> aa2(z) ->  Nil

                 effectively setting the next goal to be the first one after
                 the end of the outer block 
                 }
                B := Header_GetGoalsToClear(H);
                While (B <> Nil) And ((BTerm_GetType(B) <> GOAL_BLOCK) Or
                    (BTerm_GetHeader(B) <> Hz)) Do
                  B := BTerms_GetNext(B);
                CheckCondition(B <> Nil,'block: cannot find special goal');
                Header_SetGoalsToClear(H,B)
              End
            End;
          PP_FIND_ALL: { [FIND:2] findall(V,G,L) }
            Begin
              If More Then { first call }
              Begin
                { insert the special goal meant to know when the goal G is 
                cleared; the goal point back to this header, where the
                handle variable V is stored }
                B := BTerm_New(EmitShortIdent(P,SPECIAL_IDENT_FINDALL,False));
                BTerm_SetHeader(B,H);
                Header_InsertGoalsToClear(H,B);
                { then, insert the goal to clear, as returned by the syscall }
                Header_InsertGoalsToClear(H,BTerm_New(ZGoal));
                { switch the engine to find mode }
                Header_SetOngoingFind(H,True);
                { save handle term V }
                Header_SetSideCarTerm(H,ZTerm)
              End
            End;
          PP_FREEZE: { freeze(V,G) }
            Begin
              If ZTerm <> Nil Then
              Begin
                CheckCondition(ZGoal <> Nil,'missing frozen term');
                If IsFree(ZTerm) Then { free var: goal clearing must be delayed }
                Begin
                  { create the list element }
                  FrozenM := List_New(TObjectPtr(ZGoal));
                  { prepend the frozen term to the existing list, which may be Nil }
                  List_SetNext(FrozenM,GetFrozenTerms(VarPtr(ZTerm)));
                  { attach to the frozen variable the updated list of frozen terms }
                  SetFrozenTerms(VarPtr(ZTerm),FrozenM)
                End
                Else { bounded term: goal must be cleared right now }
                Begin
                  { insert the (not frozen in the first place) goal, as returned 
                  by the syscall }
                  Header_InsertGoalsToClear(H,BTerm_New(ZGoal));
                End
              End
            End
          End
        End
      End;
    GOAL_STD:
      Begin
        Cleared := True;

        { rule to try }
        R := Header_GetRule(Hc);

        { if any, reduce the equations given as a system in the rule itself; 
          this must be done each time the rule is applied to take into account 
          global assignments; e.g. "go -> { test=1 )" may succeed or fail, 
          depending on the value of the identifier "test" if any; this value 
          will be 1 if a goal "assign(test,1)" has been cleared before;
          this reduction must be done *before* the rule is copied, such that
          the liaisons are copied as part of the rule itself }
        If Rule_GetEqs(R) <> Nil Then
        Begin
          Ss := Sys_New;
          Sys_CopyEqs(Ss,Rule_GetEqs(R));
          Cleared := ReduceSystem(Ss,True,H^.HH_REST,FrozenM,GetDebugStream(P))
        End;

        If Cleared Then
        Begin
          { copy the rule }
          R := RulePtr(DeepCopy(TObjectPtr(R)));

          { [CUT:1] each cut in the queue must point to the header whose rule is 
           the rule containing these cuts }
          BTerms_SetCutHeader(Rule_GetQueue(R),Hc);

          { constraint to reduce: term to clear = rule head }
          Ss := Sys_NewWithEq(BTerm_GetTerm(GoalToClear),BTerm_GetTerm(Rule_GetHead(R)));

          { insert the rule queue in the list of terms to clear }
          If Rule_GetQueue(R) <> Nil Then
            Header_InsertGoalsToClear(H,Rule_GetQueue(R));

          FrozenM := Nil;
          Cleared := ReduceSystem(Ss,True,H^.HH_REST,FrozenM,GetDebugStream(P));

          { insert unfrozen goals if any }
          If Cleared And (FrozenM <> Nil) Then
          Begin
            While FrozenM <> Nil Do { loop through all terms to unfreeze}
            Begin
              ZGoal := TermPtr(List_GetObject(FrozenM));
              { build and insert the goal in the list of goals to clear }
              Header_InsertGoalsToClear(H,BTerm_New(ZGoal));
              { trace; FIXME: should appear after the trace event below }
              If MustTrace(P,Hc) Then
                PrintTraceEvent(P,Hc,TRACE_GOAL);
              { next unfrozen term }
              FrozenM := List_GetNext(FrozenM)
            End
          End
        End
      End
    End; { of case }

    { set clear status}
    Header_SetCleared(Hc,Cleared);

    { update success counter }
    If Header_IsCleared(Hc) Then
      Header_IncSuccessCount(Hc);

    { trace; we do it late to use bindings for nice printing }
    If MustTrace(P,Hc) Then
    Begin
      If Header_GetBranchNumber(Hc) = 1 Then
        PrintTraceEvent(P,Hc,TRACE_CALL)
      Else
        PrintTraceEvent(P,Hc,TRACE_REDO)
    End
  End;

Begin { clock }
  InitClock;
  GarbageCollector;

  { try to reduce the system in the query, if any, and fail if it has 
    no solutions; note that the system is reduced before clearing any 
    goal, including goals that sets global variables; thus a query 
    like "assign(aa,1) { aa = 1 )" will fail right away  }
  If Query_GetSys(Q) <> Nil Then
    If Not ReduceEquations(Query_GetSys(Q),GetDebugStream(P)) Then
      Exit;

  B := Query_GetTerms(Q); { list of terms to clear }

  { no terms to clear: success }
  If B = Nil Then
  Begin
    WriteQuerySolution;
    Exit
  End;

  { first clock header; rule is set by the next statement  }
  Q^.QU_HEAD := Headers_PushNew(Nil,B);

  { from now, the top header is always pointed by the query, to ease debugging }
  With Q^ Do
  Begin
    { fail when there is no chance to clear the first goal }
    If Not FirstCandidate(QU_HEAD,False) Then
      Exit;

    { [CUT:1] cuts in the query must point back to this header }
    BTerms_SetCutHeader(B,QU_HEAD);

    Repeat
      MoveForward(QU_HEAD);

      If Header_IsLeaf(QU_HEAD) Then { a leaf: always backtrack }
      Begin
        If Header_IsSolution(QU_HEAD) Then
          WriteQuerySolution;
        Backtracking(QU_HEAD)
      End
      Else { not a leaf: try to find a first rule }
      Begin
        { note: clearing related to findall(X,G,L) is silent }
        If Not FirstCandidate(QU_HEAD,Header_GetOngoingFind(QU_HEAD)) Then
          Backtracking(QU_HEAD)
      End;

      { trigger GC after a certain number of steps }
      GCCount := GCCount + 1;
      If GCCount = 100 Then
      Begin
        GarbageCollector;
        GCCount := 0
      End;

      { give the user a chance to halt execution }
      If CtrlC Then
        UserInterrupt
    Until Header_EndOfSearch(QU_HEAD) Or Error
  End
End;

{----------------------------------------------------------------------------}
{                                                                            }
{                      E X E C U T E   Q U E R I E S                         }
{                                                                            }
{----------------------------------------------------------------------------}

{ execute query Q }
Procedure AnswerQuery( P : ProgPtr; Q : QueryPtr; Echo : Boolean );
Begin
  If Echo Then
    OutOneQuery(GetOutputConsole(P),Q);
  Clock(P,Q)
End;

{----------------------------------------------------------------------------}
{                                                                            }
{                                 P A R S E                                  }
{                                                                            }
{----------------------------------------------------------------------------}

{ parse and insert a sequence of rules, stopping at a token in StopTokens  }
Procedure ParseAndInsertRules( f : StreamPtr; P : ProgPtr; Var K : TokenPtr; 
    StopTokens : TTokenSet );
Var
  R : RulePtr;
Begin
  While Not (Token_GetType(K) In StopTokens) And (Not Error) Do
  Begin
    R := ParseOneRule(f,P,K);
    If Error Then Exit;
    ProgInsertRule(P,R)
  End
End;

{ process user input typed after the prompt; chars after the end-of-query 
 mark stay in the input buffer }  
Procedure ProcessCommandLine( P : ProgPtr );
Var
  K : TokenPtr;
  Q : QueryPtr;
  f : StreamPtr;
Begin
  f := GetInputConsole(P);
  K := ReadProgramToken(P,f);
  If Error Then Exit;
  If Token_GetType(K) <> TOKEN_END_OF_INPUT Then
  Begin
    Q := ParseOneQuery(f,P,K);
    If Error Then Exit;
    AnswerQuery(P,Q,False)
  End
End;

{ compile and execute a sequence of queries; if ContTokens is not empty, 
 each query must start with a token in this set; the sequence ends with a token 
  in StopTokens }
Procedure ParseAndExecuteQueries( f : StreamPtr; P : ProgPtr;
    Var K : TokenPtr; WithArrow : Boolean; 
    ContTokens, StopTokens : TTokenSet );
Var
  Q : QueryPtr;
  EchoQuery : Boolean;
Begin
  While ((ContTokens=[]) Or (Token_GetType(K) In ContTokens))
    And (Not (Token_GetType(K) In StopTokens)) And (Not Error) Do
  Begin
    If WithArrow Then
      VerifyToken(f,P,K,TOKEN_ARROW);
    If Error Then Exit;
    Q := ParseOneQuery(f,P,K);
    If Error Then Exit;
    EchoQuery := Not Stream_IsConsole(f) And Not Stream_GetEcho 
        And World_IsUserLand(GetCurrentWorld(P));
    AnswerQuery(P,Q,EchoQuery);
    If Error Then Exit;
    { now that the solution has been displayed, read the next token; this 
     sequencing is required to avoid the echo mode to mangle the output by
     e.g. displaying the arrow of the next goal before displaying the solution }
    K := ReadProgramToken(P,f)
  End
End;

{ parse and append rules and queries to a program; 
 top-level strings (that is, when a rule is expected) are taken to have the 
 value of a comment in all the supported Prolog syntaxes }
Procedure ProcessRulesAndQueries( f : StreamPtr; P : ProgPtr );
Var
  Stop : Boolean;
  K : TokenPtr;
  StopTokens : TTokenSet;
Begin
  K := ReadProgramToken(P,f);
  If Error Then Exit;
  Stop := False;
  { common tokens ending a series of queries or rules }
  StopTokens := [TOKEN_END_OF_INPUT,TOKEN_STRING];
  If GetSyntax(P) In [PrologIIc,PrologII] Then
    StopTokens := StopTokens + [TOKEN_SEMICOLON]; 
  Repeat
    Case Token_GetType(K) Of
    TOKEN_END_OF_INPUT:
      Stop := True;
    TOKEN_SEMICOLON:
      If GetSyntax(P) In [PrologIIc,PrologII] Then { Prolog II termination }
        Stop := True
      Else
        SyntaxError(TokenStr[TOKEN_SEMICOLON] + ' not expected here');
    TOKEN_STRING: { a comment }
      Begin
        ProgInsertComment(P,Token_GetStr(K));
        K := ReadProgramToken(P,f);
      End;
    TOKEN_ARROW: { a series of queries }
      Begin
        ParseAndExecuteQueries(f,P,K,True,[TOKEN_ARROW],StopTokens)
      End;
    Else { a series of rules }
      Begin
        ParseAndInsertRules(f,P,K,StopTokens + [TOKEN_ARROW]);
      End
    End
  Until Stop Or Error;
End;


{----------------------------------------------------------------------------}
{                                                                            }
{                   I N S E R T   P R O L O G   P R O G R A M S              }
{                                                                            }
{----------------------------------------------------------------------------}

{ try to set a Prolog file as input, making sure there are no loops }
Function TryPrologFileForInput( P : ProgPtr; Path : TPath; 
    RaiseAnError : Boolean ) : StreamPtr;
Var
  f : StreamPtr;
  ShortPath : TShortPath;
Begin
  TryPrologFileForInput := Nil;
  If Str_Length(Path) > StringMaxSize Then { path too long }
  Begin
    If RaiseAnError Then
      RuntimeError('path is too long: ''' + 
          Str_GetShortStringTruncate(Path) + '...''');
    Exit
  End;
  ShortPath := Str_AsShortString(Path);
  { check the file exists }
  If Not FileExistsOnDisk(ShortPath) Then
  Begin
    If RaiseAnError Then
      RuntimeError('file does not exist: ''' + ShortPath + '''');
    Exit
  End;
  { the file exists: check for errors }
  If GetStreamByPath(P,Path) <> Nil Then
  Begin
    If RaiseAnError Then
      RuntimeError('insertion loop: ''' + ShortPath + '''');
    Exit
  End;
  f := Stream_New(Path,Path,DEV_FILE,MODE_READ,True,False);
  If Not Stream_IsOpen(f) Then
  Begin
    If RaiseAnError Then
      RuntimeError('unable to open: ''' + ShortPath + '''');
    Exit
  End;
  PushStream(P,f);
  TryPrologFileForInput := f
End;

{ return a stream for filename Path in base directory BaseDir, using default 
 program extensions for the current syntax; BaseDir can be Nil. If it is not Nil
 it must end with a directory separator;
 NOTE: probably less TOCTOU-prone than looking for the file and then 
 setting it as input }
Function SetPrologFileForInput( P : ProgPtr; 
    BaseDir,Path : StrPtr; RaiseAnError : Boolean ) : StreamPtr;
Var
  f : StreamPtr;
  y : TSyntax;
  BasePath,OtherPath : StrPtr;
Begin
  SetPrologFileForInput := Nil;
  y := GetSyntax(P);
  { form the most natural filename to look for: dir + path }
  If BaseDir <> Nil Then
  Begin
    BasePath := Str_Clone(BaseDir);
    Str_Concat(BasePath,Path)
  End
  Else
    BasePath := Str_Clone(Path);
  { try as-is, not raising any error }
  f := TryPrologFileForInput(P,BasePath,False);
  { if not found, try with an extension }
  If (f = Nil) And (y = Edinburgh) Then
  Begin
    OtherPath := Str_Clone(BasePath);
    Str_Append(OtherPath,'.pl');
    f := TryPrologFileForInput(P,OtherPath,False)
  End;
  If f = Nil Then
  Begin
    OtherPath := Str_Clone(BasePath);
    Str_Append(OtherPath,'.' + FileExt[y]);
    f := TryPrologFileForInput(P,OtherPath,False)
  End;
  { failure, redo with the base filename so as to raise an error }
  If (f = Nil) And RaiseAnError Then
    f := TryPrologFileForInput(P,BasePath,True);
  SetPrologFileForInput := f
End;

{ load rules and execute queries (if nay) from a Prolog file; if TryPath is 
 True, try to use 1) if any, the directory of the program that contains this 
 insert; 2) the main program directory;
 Q is the query (if any) that triggered the loading, e.g. due to an "insert" 
 goal }
Procedure LoadProgram( P : ProgPtr; s : StrPtr; TryPath : Boolean );
Var 
  BaseDir : StrPtr;
  f : StreamPtr;
Begin
  f := Nil;
  If TryPath And Not Path_IsAbsolute(s) Then
  Begin
    { try the directory of the parent Prolog file, if any }
    BaseDir := Path_ExtractPath(Stream_GetPath(CurrentInput(P)));
    If Str_Length(BaseDir) > 0 Then
      f := SetPrologFileForInput(P,BaseDir,s,False);
    { try the directory of the Prolog file passed as parameter }
    If f = Nil Then
    Begin
      BaseDir := GetProgramPath(P);
      If Str_Length(BaseDir) > 0 Then
        f := SetPrologFileForInput(P,BaseDir,s,False)
    End
  End;
  { last attempt: OS's current directory }
  If f = Nil Then
    f := SetPrologFileForInput(P,Nil,s,True);
  If Error Then Exit;
  { reading a Prolog file can consume a lot of memory; we clean up before
    starting }
  GarbageCollector;
  { do insert }
  BeginInsertion(P);
  ProcessRulesAndQueries(f,P);
  EndInsertion(P);
  { do not close the input file in case of error, as the error handler 
    needs it to display an excerpt of the input data }
  If Error Then Exit;
  CloseAndDeleteStream(P,f)
End;

End.