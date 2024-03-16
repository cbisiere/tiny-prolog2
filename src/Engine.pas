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
  Errs,
  Files,
  IStream,
  IStack,
  OStack,
  Trace,
  Memory,
  PObj,
  PObjOp,
  PObjRest,
  PObjStr,
  PObjDict,
  PObjEq,
  PObjTerm,
  PObjProg,
  Encoding,
  Unparse,
  Reduc,
  Parse,
  Debug,
  Predef;

{ file suffix: for PII+ and Edinburgh, see PII+ doc p297 }
Type 
  TFileExt = Array[TSyntax] Of String[3];
Const 
  FileExt : TFileExt = ('pro','p2c','p2','p2E'); { TODO: rewrite to handle .pl }

Procedure AnswerQueries( P : ProgPtr; Echo : Boolean );
Procedure LoadProgram( P : ProgPtr; Q : QueryPtr; 
    s : StrPtr; TryPath : Boolean );

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
Function ClearInsert( P : ProgPtr; Q : QueryPtr; T : TermPtr ) : Boolean;
Var
  FileNamePtr : StrPtr;
  Qi, QLast : QueryPtr;
  HadNoRules : Boolean;
  Stop : Boolean;
Begin
  ClearInsert := False;
  { 1: filename (string or identifier, unquoted) }
  FileNamePtr := GetAtomArgAsStr(1,T,False);
  If FileNamePtr = Nil Then 
    Exit;
  { execute }
  HadNoRules := FirstProgramRule(P) = Nil;
  QLast := LastProgramQuery(P);
  LoadProgram(P,Q,FileNamePtr,True);
  If Error Then 
    Exit;
  { TWIST:
    if the program had no rules before these new rules were inserted,
    we must update the scopes of the current query and queries that follow, 
    up to the last query before the program was loaded;
    indeed, when reading a program *starting* with an insert/1, the scope
    of this insert will be nil; upon execution, the inserted rules will 
    not show up in the scope of this goal (and it may thus fail); the 
    situation is similar for the queries that follow insert/1 and are in 
    the same file: even if they have a non Nil scope, the first rule in
    the scope is not what it should be }
  If HadNoRules Then
  Begin
    Qi := Q;
    Stop := False;
    While Not Stop Do
    Begin
      SetFirstRuleInQueryScope(Qi,FirstProgramRule(P));
      If LastRuleInQueryScope(Qi) = Nil Then
        SetLastRuleInQueryScope(Qi,LastRuleInQueryScope(Q));
      Qi := NextQuery(Qi);
      Stop := (Qi=Nil) Or (Qi=QLast)
    End
  End;
  ClearInsert := True
End;

{ execute a system call syscall(Code,Arg1,...ArgN), meaning Code(Arg1,...,ArgN) }
Function ExecutionSysCallOk( P : ProgPtr; Q : QueryPtr; T : TermPtr ) : Boolean;
Var
  Ok : Boolean;
  Predef : TPP;
Begin
  ExecutionSysCallOk := False; { default is to fail }

  If Not PredefCallIsOk(P,T,Predef) Then
    Exit;

  If Predef = PP_INSERT Then
    Ok := ClearInsert(P,Q,T)
  Else
    Ok := ClearPredef(Predef,P,Q,T);
  ExecutionSysCallOk := Ok
End;


{----------------------------------------------------------------------------}
{                                                                            }
{                                C L O C K                                   }
{                                                                            }
{----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ warning during execution                                                   }
{----------------------------------------------------------------------------}

{ warn user when a goal other than 'fail' fails }
Procedure WarnNoRuleToClearTerm( B : BTermPtr );
Var
  I : IdPtr;
  s : StrPtr;
Begin
  CheckCondition(B <> Nil,'WarnNoRuleToClearTerm: B is Nil');
  I := AccessIdentifier(B^.BT_TERM); { handle dynamic assignment of identifiers }
  If I = Nil Then
    Exit;
  s := IdentifierGetStr(I);
  If StrEqualTo(s,'fail') Then
    Exit;
  CWriteWarning('no rules match goal');
  CWrite(' ''');
  StrWrite(s);
  CWrite('''');
  CWriteLn
End;

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
    PRuleB : TObjectPtr Absolute RuleB;
    CopyRuleP : TObjectPtr;
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
      Solvable := ExecutionSysCallOk(P,Q,ClearT);
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


{----------------------------------------------------------------------------}
{                                                                            }
{       L O A D   R U L E S   A N D   E X E C U T E   Q U E R I E S          }
{                                                                            }
{----------------------------------------------------------------------------}

{ reset the input/output system, closing all open files, but preserving the
 input console buffer }
Procedure ResetIO;
Begin
  ResetIFileStack;
  ResetOFileStack
End;

{ execute query Q }
Procedure AnswerQuery( P : ProgPtr; Q : QueryPtr; Echo : Boolean );
Begin
  If Echo Then
    OutOneQuery(Q,False);
  Clock(P,Q);
  ResetIO
End;

{ execute queries starting at the current insertion level in P }
Procedure AnswerQueries( P : ProgPtr; Echo : Boolean );
Var
  Q : QueryPtr;
Begin
  Q := FirstQueryToExecute(P);
  While Q <> Nil Do
  Begin
    AnswerQuery(P,Q,Echo);
    Q := NextQuery(Q)
  End;
  RemoveQueries(P)
End;

{ return a stream for filename fn, using default program extensions for 
 syntax y;
 NOTE: probably less TOCTOU-prone than looking for the file and then setting it  
 as input }
Function SetPrologFileForInput( y : TSyntax; fn : TPath; 
    Var FileDesc : TFileDescriptor ) : TIStreamPtr;
Var
  f : TIStreamPtr;
Begin
  f := SetFileForInput(FileDesc,fn,fn,False);
  If f = Nil Then
  Begin
    If y = Edinburgh Then
      f := SetFileForInput(FileDesc,fn,fn + '.pl',False);
    If f = Nil Then
      f := SetFileForInput(FileDesc,fn,fn + '.' + FileExt[y],False)
  End;
  SetPrologFileForInput := f
End;

{ load rules and queries from a Prolog file, and execute the queries it 
 contains, if any; if TryPath is True, try to use the main program dir first;
 Q is the query (if any) that triggered the loading, e.g. due to an "insert" 
 goal }
Procedure LoadProgram( P : ProgPtr; Q : QueryPtr; s : StrPtr; 
    TryPath : Boolean );
Var 
  y : TSyntax;
  FileName, Path : TPath;
  f : TIStreamPtr;
  FileDesc : TFileDescriptor;
Begin
  y := GetSyntax(P);
  If StrLength(s) <= StringMaxSize Then
  Begin
    FileName := StrGetString(s);
    Path := GetProgramPath(P);
    f := Nil;
    If TryPath And (path <> '') And 
        (Length(Path) + Length(FileName) <= StringMaxSize) Then
      f := SetPrologFileForInput(y,Path + FileName,FileDesc);
    If f = Nil Then
      f := SetPrologFileForInput(y,FileName,FileDesc);
    If f <> Nil Then
    Begin
      BeginInsertion(P);
      ParseRulesAndQueries(f,P,Q,GetRuleType(P));
      If Error Then Exit;
      CloseInputByFileDescriptor(FileDesc); { TODO: close(f) }
      If Error Then Exit;
      { clearing goals only after closing the input file is the right way to  
        do it, as calls to input_is, etc. must not consider the program file 
        as an input file }
      AnswerQueries(P,GetRuleType(P)=RTYPE_USER);
      EndInsertion(P)
    End
    Else
      RuntimeError('Cannot open file ')
  End
  Else
    RuntimeError('filename is too long');
End;

{ initialize the input/output system }
Begin
  InitIFileStack;
  InitOFileStack
End.