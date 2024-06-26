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
 Code(Arg1,...,ArgN); if the syscall is freeze, set the frozen variable V and
 the frozen goal G }
Function ExecutionSysCallOk( P : ProgPtr; Q : QueryPtr; T : TermPtr; 
    Var V,G : TermPtr ) : Boolean;
Var
  Ok : Boolean;
  Predef : TPP;
Begin
  ExecutionSysCallOk := False; { default is to fail }

  If Not PredefCallIsOk(P,T,Predef) Then
    Exit;

  If Predef = PP_INSERT Then
    Ok := ClearInsert(P,T)
  Else
    Ok := ClearPredef(Predef,P,Q,T,V,G);
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

{ clear a query }
Procedure Clock( P : ProgPtr; Q : QueryPtr );
Var
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
    { use a per-query clock: needed because Clock must be reentrant to
      support "insert/1" }
    Q^.QU_HEAD := Nil;
  
    EndOfClock := False;
    GCCount := 0
  End;

  { display on Crt constraints only about the variables in the query }
  Procedure WriteQuerySolution;
  Begin
    OutQuerySolution(Nil,Q);
    CWriteLn
  End;

  { access whether B has a chance to be cleared, that is, 1) there is a rule Rc 
   that has a chance to unify with a term B, starting with rule R; R could be 
   Nil; or 2) B is a cut; or 3) B is the system call syscall;
   if WarnNone is True, print a warning if the goal is impossible to clear }
  Function PossibleToClear( R : RulePtr; B : BTermPtr; 
      Var Rc : RulePtr; Var isSys : Boolean ; Var isCut : Boolean; 
      WarnNone : Boolean ) : Boolean;
  Var
    Possible : Boolean;
    Tc : TermPtr; { term to clear }
    I : IdPtr; { access of Tc }
    a : PosInt; { arity of Tc }
    s : StrPtr;
  Begin
    Rc := Nil;
    isSys := False;
    isCut := False;
    If B = Nil Then { FIXME: ? }
      Exit;
    Tc := BTerm_GetTerm(B);
    I := AccessIdentifier(Tc); { slow but handle assignment }
    a := Arity(Tc);
    If I <> Nil Then
    Begin
      If IdentifierIsSyscall(I) Then
        isSys := True
      Else If IdentifierIsCut(I) Then
        isCut := True
      Else
        Rc := FindRuleWithHeadAndArity(R,I,a,False)
    End;
    Possible := (Rc <> Nil) Or isSys Or isCut;
    If WarnNone And Not Possible Then
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
    PossibleToClear := Possible
  End;

  {----------------------------------------------------------------------------}
  { Assuming term B was unifiable with the head of rule R, this function       }
  { returns the next rule whose head is unifiable with B, and Nil otherwise.   }
  {----------------------------------------------------------------------------}

  Function NextCandidateRule( R : RulePtr; B : BTermPtr; Var isSys : Boolean; 
      Var isCut : Boolean) : RulePtr;
  Var 
    NextR : RulePtr;
    Possible : Boolean;
  Begin
    If (R = Nil) Or (isSys) Or (isCut) Then { no "next" rule }
    Begin
      isSys := False;
      isCut := False;
      NextR := Nil
    End
    Else
      Possible := PossibleToClear(NextRule(R,False),B,NextR,isSys,isCut,False);
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
        Rest_Restore(H^.HH_REST); { restore and free restore list }
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
      Header_SetRule(H,NextR,isSys,isCut)
End;

{------------------------------------------------------------------}
{ set in header H the first rule whose head is unifiable with the  }
{ current goal and backtrack if none                               }
{------------------------------------------------------------------}

  Procedure SetFirstCandidateRuleOrBacktrack( Var H : HeadPtr );
  Var
    Possible : Boolean;
    R : RulePtr;
    isSys, isCut : Boolean;
  Begin
    Possible := PossibleToClear(FirstRule(P,False),H^.HH_FBCL,R,isSys,isCut,True);
    Header_SetRule(H,R,isSys,isCut);
    If Not Possible Then
      Backtracking(H,EndOfClock)
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
    FrozenV,FrozenT : TermPtr; { frozen variable and goal }
    FrozenB : BTermPtr;
    FrozenM : TermsPtr;
  Begin
    Header_GetRule(H,R,isSys,isCut); { rule to apply }

    CheckCondition(H^.HH_FBCL <> Nil,'MoveForward: No terms to clear');
    CheckCondition((R <> Nil) Or isSys Or isCut,'MoveForward: No rule to apply');

    ClearB := H^.HH_FBCL; { list of terms to clear }
    ClearT := BTerm_GetTerm(ClearB); { current term to clear }

    { set the backward head pointer in case of a cut }
    If (H <> Nil) And (isCut) Then
      H^.HH_BACK := BTerm_GetHeader(ClearB);

    { backup pointer to current header }
    Hc := H;

    { new header; the "cut" indicator propagates }
    Headers_PushNew(H,Nil,Nil,False,False);

    If isCut Then { cut }
    Begin
      Solvable := True; { "cut" is always clearable }
      H^.HH_FBCL := BTerms_GetNext(ClearB)
    End
    Else
    If isSys Then { system call }
    Begin
      FrozenV := Nil;
      FrozenT := Nil;
      Solvable := ExecutionSysCallOk(P,Q,ClearT,FrozenV,FrozenT);

      If Solvable Then
      Begin
        { handle freeze }
        If FrozenV <> Nil Then { syscall was 'freeze' }
        Begin
          CheckCondition(FrozenT <> Nil,'missing frozen term');
          If IsFree(FrozenV) Then { free var: goal clearing must be delayed }
          Begin
            { create the list element }
            FrozenM := List_New(TObjectPtr(FrozenT));
            { prepend the frozen term to the existing list, which may be Nil }
            List_SetNext(FrozenM,GetFrozenTerms(VarPtr(FrozenV)));
            { attach to the frozen variable the updated list of frozen terms }
            SetFrozenTerms(VarPtr(FrozenV),FrozenM)
          End
          Else { bounded term: goal must be cleared right now }
          Begin
            { insert the goal after the cleared one, that will be removed 
             below }
            FrozenB := BTerm_New(FrozenT);
            BTerms_SetNext(FrozenB,BTerms_GetNext(ClearB));
            BTerms_SetNext(ClearB,FrozenB)
          End
        End;

        { remove the cleared term from the list of terms to clear }
        H^.HH_FBCL := BTerms_GetNext(ClearB)
      End
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
        Ss := Sys_New;
        Sys_CopyEqs(Ss,R^.RU_SYST);
        Solvable := ReduceSystem(Ss,True,H^.HH_REST,FrozenM,GetDebug(P))
      End;

      If Solvable Then
      Begin
        { copy the terms of the target rule }
        RuleB := Rule_GetTerms(R);
        CopyRuleP := DeepCopy(PRuleB);

        { link each term of the rule to the header pointing to that rule }
        BTerms_SetHeader(BCopyRuleP,Hc);

        { constraint to reduce: term to clear = rule head }
        Ss := Sys_NewWithEq(ClearT,BTerm_GetTerm(BCopyRuleP));

        { new list of terms to clear: rule queue + all previous terms 
          but the first }
        B := BTerms_GetLast(BCopyRuleP);
        B^.BT_NEXT := BTerms_GetNext(ClearB);
        H^.HH_FBCL := BTerms_GetNext(BCopyRuleP);

        FrozenM := Nil;
        Solvable := ReduceSystem(Ss,True,H^.HH_REST,FrozenM,GetDebug(P));

        { insert unfrozen goals if any }
        If Solvable And (FrozenM <> Nil) Then
        Begin
          While FrozenM <> Nil Do { loop through all terms to unfreeze}
          Begin
            FrozenT := TermPtr(List_GetObject(FrozenM));
            If GetDebug(P) Then
            Begin
              CWrite('UNFROZEN: ');
              OutTerm(Nil,GetSyntax(P),FrozenT); { FIXME: syntax }
              CWriteLn
            End;
            { build and insert the goal in the list of goals to clear }
            FrozenB := BTerm_New(FrozenT);
            BTerms_SetNext(FrozenB,H^.HH_FBCL);
            H^.HH_FBCL := FrozenB;
            { next unfrozen term }
            FrozenM := List_GetNext(FrozenM)
          End
        End
      End
    End;

    { trace: print goal that has been cleared }
    If GetTrace(P) And (Solvable Or GetDebug(P)) Then
    Begin
      If GetDebug(P) Then 
        If Solvable Then 
          CWrite('+') 
        Else 
          CWrite('-');
      CWriteLongInt(Header_GetClock(H));
      CWrite(': ');
      OutTerm(Nil,GetSyntax(P),ClearT); { FIXME: syntax }
      CWriteLn
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
  If Query_GetSys(Q) <> Nil Then
    If Not ReduceEquations(Query_GetSys(Q),GetDebug(P)) Then
      Exit;

  B := Query_GetTerms(Q); { list of terms to clear }

  { no terms to clear: success }
  If B = Nil Then
  Begin
    WriteQuerySolution;
    Exit
  End;

  { fail when there is no chance to clear B }
  If Not PossibleToClear(FirstRule(P,False),B,R,isSys,isCut,True) Then
    Exit;

  Headers_PushNew(Q^.QU_HEAD,B,R,isSys,isCut);

  { terms to clear points to this header }
  BTerms_SetHeader(B,Q^.QU_HEAD);

  Repeat
    MoveForward(Q^.QU_HEAD);
    If (Not Solvable) Or    { system has no solution }
        (Q^.QU_HEAD^.HH_FBCL = Nil) { no more terms to clear }
    Then
      MoveBackward(Q^.QU_HEAD)
    Else
      SetFirstCandidateRuleOrBacktrack(Q^.QU_HEAD);
    { trigger GC after a certain number of steps }
    GCCount := GCCount + 1;
    If GCCount = 100 Then
    Begin
      GarbageCollector;
      GCCount := 0
    End
  Until EndOfClock
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
    OutOneQuery(Nil,Q);
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
Function TryPrologFileForInput( P : ProgPtr; Path : TPath ) : StreamPtr;
Var
  f : StreamPtr;
Begin
  TryPrologFileForInput := Nil;
  f := Nil;
  { just return Nil if the file does not exist }
  If Not FileExistsOnDisk(Path) Then
    Exit;
  { the file exists: check for errors }
  If GetStreamByPath(P,Path) <> Nil Then
  Begin
    RuntimeError('insertion loop: ''' + Path + '''');
    Exit
  End;
  f := Stream_New(Path,Path,DEV_FILE,MODE_READ,True,False);
  If Not Stream_IsOpen(f) Then
  Begin
    f := Nil;
    RuntimeError('unable to open: ''' + Path + '''');
    Exit
  End;
  PushStream(P,f);
  TryPrologFileForInput := f
End;

{ return a stream for filename Path in base directory BaseDir, using default 
 program extensions for the current syntax; BaseDir can be ''. If it is not ''
 it must end with a directory separator;
 NOTE: probably less TOCTOU-prone than looking for the file and then 
 setting it as input }
Function SetPrologFileForInput( P : ProgPtr; BaseDir,Path : TPath ) : StreamPtr;
Var
  f : StreamPtr;
  y : TSyntax;
  Ext : TPath;
Begin
  SetPrologFileForInput := Nil;
  If Length(BaseDir) + Length(Path) > StringMaxSize Then
    Exit;
  y := GetSyntax(P);
  f := TryPrologFileForInput(P,BaseDir + Path);
  If Error Then Exit;
  If (f = Nil) And (y = Edinburgh) Then
  Begin
    Ext := '.pl';
    If Length(BaseDir) + Length(Path) + Length(Ext) <= StringMaxSize Then
      f := TryPrologFileForInput(P,BaseDir + Path + Ext);
    If Error Then Exit
  End;
  If f = Nil Then
  Begin
    Ext := '.' + FileExt[y];
    If Length(BaseDir) + Length(Path) + Length(Ext) <= StringMaxSize Then
      f := TryPrologFileForInput(P,BaseDir + Path + Ext);
    If Error Then Exit
  End;
  SetPrologFileForInput := f
End;

{ load rules and execute queries (if nay) from a Prolog file; if TryPath is 
 True, try to use 1) if any, the directory of the program that contains this 
 insert; 2) the main program directory;
 Q is the query (if any) that triggered the loading, e.g. due to an "insert" 
 goal }
Procedure LoadProgram( P : ProgPtr; s : StrPtr; TryPath : Boolean );
Var 
  Filename,BaseDir : TPath;
  f : StreamPtr;
Begin
  If Str_Length(s) <= StringMaxSize Then
  Begin
    Filename := Str_AsShortString(s);
    f := Nil;
    If TryPath Then
    Begin
      { try the directory of the parent Prolog file, if any }
      BaseDir := ExtractPath(Stream_GetPath(CurrentInput(P)));
      If BaseDir <> '' Then
        f := SetPrologFileForInput(P,BaseDir,Filename);
      { try the directory of the Prolog file passed as parameter }
      If f = Nil Then
      Begin
        BaseDir := GetProgramPath(P);
        If BaseDir <> '' Then
          f := SetPrologFileForInput(P,BaseDir,Filename)
      End
    End;
    If Error Then Exit;
    { last attempt: OS's current directory }
    If f = Nil Then
      f := SetPrologFileForInput(P,'',Filename);
    If Error Then Exit;
    If f <> Nil Then
    Begin
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
    End
    Else
      RuntimeError('cannot open file: ''' + Filename + '''')
  End
  Else
    RuntimeError('filename is too long');
End;

End.