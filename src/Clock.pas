{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Clock.pas                                                  }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                        P R O L O G   C L O C K                             }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ from Sys: }
Function ExecutionSysCallOk( T : TermPtr; P : ProgPtr; Q : QueryPtr ) : Boolean; Forward;
Function IdentifierIsSyscall( I : IdPtr ) : Boolean; Forward;

{----------------------------------------------------------------------------}
{ warning during execution                                                   }
{----------------------------------------------------------------------------}

{ warn user when a goal other than 'fail' fails }
Procedure WarnNoRuleToClearTerm( B : BTermPtr );
Var
  I : IdPtr;
  m,s : StrPtr;
Begin
  CheckCondition(B <> Nil,'WarnNoRuleToClearTerm: B is Nil');
  I := AccessIdentifier(B^.BT_TERM); { handle dynamic assignment of identifiers }
  If I = Nil Then
    Exit;
  s := IdentifierGetStr(I);
  If StrEqualTo(s,'fail') Then
    Exit;
  m := NewStringFrom('***WARNING: no rules match goal ''');
  StrConcat(m,s);
  StrAppend(m,'''');
  OutStringCR(m,False)
End;

{----------------------------------------------------------------------------}
{                                                                            }
{                                C L O C K                                   }
{                                                                            }
{----------------------------------------------------------------------------}

{ clear a query }
Procedure Clock( P : ProgPtr; Q : QueryPtr );

Var
  Head        : HeadPtr;  { backup of the current program head }
  Solvable    : Boolean;  { system has a solution? }
  EndOfClock  : Boolean;
  R           : RulePtr;
  B           : BTermPtr;
  isSys       : Boolean;
  isCut       : Boolean;
  GCCount     : Integer;  { counter to trigger GC }

  { init the clock }
  Procedure InitClock;
  Begin
    { backup the current head: needed because Clock must be reentrant to
      support "insert/1"; not that using P^.PP_HEAD helps debug, but a
      local head could be used instead }
    Head := P^.PP_HEAD;
    P^.PP_HEAD := Nil;
  
    EndOfClock := False;
    GCCount := 0
  End;

  { display constraints only about the variables in the query }
  Procedure WriteQuerySolution;
  Begin
    OutQuerySolution(Q,False);
    CWriteLn
  End;

  { are two terms possibly unifiable? if not, there is not point in copying
    a rule, etc.; note that since we make sure that a given constant value 
    (identifiers, numbers, strings) is represented by exactly one term,
    comparing pointers is fine even for constants }
  Function Unifiable( T1,T2 : TermPtr ) : Boolean;
  Var 
    Ok : Boolean;
  Begin
    CheckCondition((T1<>Nil) Or (T2<>Nil),
      'Call to Unifiable with two Nil terms'); { FIXME: is it really a problem?}
    Ok := (T1=T2) Or (T1=Nil) Or (T2=Nil);
    Unifiable := Ok
  End;

  { returns the rule following R, or Nil if R is the last rule in the query's
    scope }
  Function Next( R : RulePtr ) : RulePtr;
  Begin
    CheckCondition(R <> Nil,'cannot call Next on Nil');
    If R = LastRuleInQueryScope(Q) Then 
      Next := Nil
    Else
      Next := NextRule(R)
  End;

  { first rule that has a chance to unify with a term, starting with rule R }
  Function FirstCandidateRule( R : RulePtr; B : BTermPtr; Var isSys : Boolean ; Var isCut : Boolean ) : RulePtr;
  Var
    FirstR : RulePtr;
    I1 : IdPtr;
    TI1 : TermPtr Absolute I1;
    I2 : IdPtr;
    TI2 : TermPtr Absolute I2;
    Stop : Boolean;
  Begin
    FirstR := Nil;
    isSys := False;
    isCut := False;
    If B <> Nil Then
    Begin
      Stop := False;
      I1 := AccessIdentifier(B^.BT_TERM); { handle dynamic assignment of identifiers }
      If I1 <> Nil Then
      Begin
        If IdentifierIsSyscall(I1) Then
        Begin
          isSys := True;
          Stop := True
        End
        Else
        If IdentifierIsCut(I1) Then
        Begin
          isCut := True;
          Stop := True
        End
      End;
      While (R<>Nil) And Not Stop Do
      Begin
        I2 := AccessTerm(R^.RU_FBTR); { FIXME: check ident? Otherwise the rule head is a variable -- not parsable}
        If Unifiable(TI1,TI2) Then
        Begin
          FirstR := R;
          Stop := True
        End;
        R := Next(R)
      End
    End;
    FirstCandidateRule := FirstR
  End;

  {----------------------------------------------------------------------------}
  { Assuming term B was unifiable with the head of rule R, this function       }
  { returns the next rule whose head is unifiable with B, and Nil otherwise.   }
  {----------------------------------------------------------------------------}

  Function NextCandidateRule( R : RulePtr; B : BTermPtr; Var isSys : Boolean ; Var isCut : Boolean) : RulePtr;
  Var NextR : RulePtr;
  Begin
    If (R = Nil) Or (isSys) Or (isCut) Then
    Begin
      isSys := False;
      isCut := False;
      NextR := Nil
    End
    Else
      NextR := FirstCandidateRule(Next(R), B, isSys, isCut);
    NextCandidateRule := NextR
  End;

{----------------------------------------------------------------------------}
{ Backtracks the Prolog clock to the last usable point of choice. If there   }
{ are no more choices to consider, the function sets EndOfClock to True.     }
{ Backtracking is done in three steps:                                       }
{ (1) unstack the current clock header                                       }
{ (2) undo memory changes                                                    }
{ (3) find the next rule to apply; if no rules apply, backtrack again        }
{----------------------------------------------------------------------------}

  Procedure Backtracking( Var H : HeadPtr; Var NoMoreChoices : Boolean );
  Var
    CutH : HeadPtr; { target header set by a cut }
    OnTarget, Skip : Boolean;
    NextR : RulePtr;
    NextH : HeadPtr;
    isSys, isCut : Boolean;
    Stop : Boolean;
  Begin
    CutH := H^.HH_BACK;
    Repeat
      NoMoreChoices := H^.HH_CLOC = 0; { no previous choice point }
      Stop := NoMoreChoices;

      If Not Stop Then
      Begin
        { detect the first target header }
        If (CutH = Nil) And (H^.HH_BACK <> Nil) Then
          CutH := H^.HH_BACK;

        { a target header has been reached; skip that one 
          and then stop }
        OnTarget := H = CutH;
        Skip := (CutH <> Nil) And (Not OnTarget);
        If OnTarget Then
          CutH := Nil;

        { backtracks one step }
        NextH := H^.HH_NEXT;
        Restore(H^.HH_REST); { restore and free restore list }
        H^.HH_REST := Nil;
        H := NextH;

        NextR := Nil;
        isSys := False;
        isCut := False;

        { set next rule to apply, if any }
        NextR := NextCandidateRule(H^.HH_RULE,H^.HH_FBCL,isSys,isCut);

        Stop := (Not Skip) And ((NextR <> Nil) Or (isSys) Or (isCut))
      End
    Until Stop;
    If Not NoMoreChoices Then
      SetHeaderRule(H,NextR,isSys,isCut)
End;

{------------------------------------------------------------------}
{ set in header H the first rule whose head is unifiable with the  }
{ current goal and backtrack if none                               }
{------------------------------------------------------------------}

  Procedure SetFirstCandidateRuleOrBacktrack( Var H : HeadPtr );
  Var
    R : RulePtr;
    isSys, isCut : Boolean;
  Begin
    R := FirstCandidateRule(FirstRuleInQueryScope(Q),H^.HH_FBCL,isSys,isCut);
    SetHeaderRule(H,R,isSys,isCut);
    If (R = Nil) And (Not isSys) And (Not isCut) Then
    Begin
      WarnNoRuleToClearTerm(H^.HH_FBCL);
      Backtracking(H,EndOfClock)
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
    Hc : HeadPtr; { current header }
    ClearB, B : BTermPtr;
    ClearT : TermPtr;
    R : RulePtr;
    isCut, isSys : Boolean;
    Ss : SysPtr;
    RuleB : BTermPtr;
    PRuleB : TPObjPtr Absolute RuleB;
    CopyRuleP : TPObjPtr;
    BCopyRuleP : BTermPtr Absolute CopyRuleP;

  Begin
    GetHeaderRule(H,R,isSys,isCut); { rule to apply }

    CheckCondition(H^.HH_FBCL <> Nil,'MoveForward: No terms to clear');
    CheckCondition((R <> Nil) Or isSys Or isCut,'MoveForward: No rule to apply');

    ClearB := H^.HH_FBCL; { list of terms to clear }
    ClearT := ClearB^.BT_TERM; { current term to clear }

    { set the backward head pointer in case of a cut }
    If (H <> Nil) And (isCut) Then
      H^.HH_BACK := ClearB^.BT_HEAD;

    { backup pointer to current header }
    Hc := H;

    { new header; the "cut" indicator propagates }
    PushNewClockHeader(H,Nil,Nil,False,False);

    If isCut Then
    Begin
      Solvable := True; { "cut" is always clearable }
      H^.HH_FBCL := NextTerm(ClearB)
    End
    Else
    If isSys Then
    Begin
      Solvable := ExecutionSysCallOk(ClearT,P,Q);
      { remove the term from the list of terms to clear }
      H^.HH_FBCL := NextTerm(ClearB)
    End
    Else
    Begin
      Solvable := True;

      { if any, reduce the equations given as a system in the rule itself; 
        this must be done each time the rule is applied to take into account 
        global assignments; e.g. "go -> { test=1 )" may succeed or fail, 
        depending on the value of the identifier "test" if any; this value 
        will be 1 if a goal "assign(test,1)" has been cleared before;
        this reduction must be done *before* the rule is copied, such that
        the liaisons are copied as part of the rule itself }
      If R^.RU_SYST <> Nil Then
      Begin
        Ss := NewSystem;
        CopyAllEqInSys(Ss,R^.RU_SYST);
        Solvable := ReduceSystem(Ss,True,H^.HH_REST)
      End;

      If Solvable Then
      Begin
        { copy the terms of the target rule }
        RuleB := R^.RU_FBTR;
        CopyRuleP := DeepCopy(PRuleB);

        { link each term of the rule to the header pointing to that rule }
        SetTermsHeader(Hc,BCopyRuleP);

        { constraint to reduce: term to clear = rule head }
        Ss := NewSystemWithEq(ClearT,BCopyRuleP^.BT_TERM);

        { new list of terms to clear: rule queue + all previous terms 
          but the first }
        B := BCopyRuleP;
        While (NextTerm(B)<>Nil) Do
          B := NextTerm(B);
        B^.BT_NEXT := NextTerm(ClearB);
        H^.HH_FBCL := NextTerm(BCopyRuleP);

        Solvable := ReduceSystem(Ss,True,H^.HH_REST)
      End
    End
  End;

{------------------------------------------------------------------}
{ We are in a leaf of the exploration tree; if the set of          }
{ constraints is soluble, we have a solution; then backtrack       }
{------------------------------------------------------------------}

  Procedure MoveBackward( Var H : HeadPtr );
  Begin
    If Solvable Then
      WriteQuerySolution;
    Backtracking(H,EndOfClock)
  End;

Begin
  InitClock;
  GarbageCollector;

  { try to reduce the system in the query, if any, and fail if it has 
    no solutions; note that the system is reduced before clearing any 
    goal, including goals that sets global variables; thus a query 
    like "assign(aa,1) { aa = 1 )" will fail right away  }
  If Q^.QU_SYST <> Nil Then
    If Not ReduceEquations(Q^.QU_SYST) Then
      Exit;

  B := Q^.QU_FBTR; { list of terms to clear }

  { no terms to clear: success }
  If B = Nil Then
  Begin
    WriteQuerySolution;
    Exit
  End;

  R := FirstCandidateRule(FirstRuleInQueryScope(Q),B,isSys,isCut);

  { not even a candidate rule to try: fail }
  If (R = Nil) And (Not isSys) And (Not isCut) Then
  Begin
    WarnNoRuleToClearTerm(B);
    Exit
  End;

  PushNewClockHeader(P^.PP_HEAD,B,R,isSys,isCut);

  { terms to clear points to this header }
  SetTermsHeader(P^.PP_HEAD,B);

  Repeat
    MoveForward(P^.PP_HEAD);
    If (Not Solvable) Or    { system has no solution }
        (P^.PP_HEAD^.HH_FBCL = Nil) { no more terms to clear }
    Then
      MoveBackward(P^.PP_HEAD)
    Else
      SetFirstCandidateRuleOrBacktrack(P^.PP_HEAD);
    { trigger GC after a certain number of steps }
    GCCount := GCCount + 1;
    If GCCount = 100 Then
    Begin
      GarbageCollector;
      GCCount := 0
    End
  Until EndOfClock;
  P^.PP_HEAD := Head { restore current header }
End;
