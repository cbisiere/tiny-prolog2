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
  Trace,
  Memory,
  PObj,
  PObjIO,
  PObjOp,
  PObjRest,
  PObjStr,
  PObjTok,
  PObjDict,
  PObjEq,
  PObjTerm,
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
  FileExt : TFileExt = ('pro','p2c','p2','p2E'); { TODO: rewrite to handle .pl }

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
    Ok := ClearInsert(P,T)
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
  I := AccessIdentifier(BTerm_GetTerm(B)); { handle dynamic assignment of identifiers }
  If I = Nil Then
    Exit;
  s := IdentifierGetStr(I);
  If Str_EqualToString(s,'fail') Then
    Exit;
  CWriteWarning('no rules match goal');
  CWrite(' ''');
  Str_CWrite(s);
  CWrite('''');
  CWriteLn
End;

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

  { first rule that has a chance to unify with a term, starting with rule R;
   R could be Nil }
  Function FirstCandidateRule( R : RulePtr; B : BTermPtr; 
      Var isSys : Boolean ; Var isCut : Boolean ) : RulePtr;
  Var
    I : IdPtr;
  Begin
    FirstCandidateRule := Nil;
    isSys := False;
    isCut := False;
    If B = Nil Then
      Exit;
    I := AccessIdentifier(BTerm_GetTerm(B)); { slow but handle assignment }
    If I = Nil Then
      Exit;
    If IdentifierIsSyscall(I) Then
      isSys := True
    Else If IdentifierIsCut(I) Then
      isCut := True
    Else 
      FirstCandidateRule := FindRuleWithHead(R,I,False) 
  End;

  {----------------------------------------------------------------------------}
  { Assuming term B was unifiable with the head of rule R, this function       }
  { returns the next rule whose head is unifiable with B, and Nil otherwise.   }
  {----------------------------------------------------------------------------}

  Function NextCandidateRule( R : RulePtr; B : BTermPtr; Var isSys : Boolean; 
      Var isCut : Boolean) : RulePtr;
  Var NextR : RulePtr;
  Begin
    If (R = Nil) Or (isSys) Or (isCut) Then
    Begin
      isSys := False;
      isCut := False;
      NextR := Nil
    End
    Else
      NextR := FirstCandidateRule(NextRule(R,False), B, isSys, isCut);
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
      Header_SetRule(H,NextR,isSys,isCut)
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
    R := FirstCandidateRule(FirstRule(P,False),H^.HH_FBCL,isSys,isCut);
    Header_SetRule(H,R,isSys,isCut);
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
    Header_GetRule(H,R,isSys,isCut); { rule to apply }

    CheckCondition(H^.HH_FBCL <> Nil,'MoveForward: No terms to clear');
    CheckCondition((R <> Nil) Or isSys Or isCut,'MoveForward: No rule to apply');

    ClearB := H^.HH_FBCL; { list of terms to clear }
    ClearT := BTerm_GetTerm(ClearB); { current term to clear }

    { set the backward head pointer in case of a cut }
    If (H <> Nil) And (isCut) Then
      H^.HH_BACK := ClearB^.BT_HEAD;

    { backup pointer to current header }
    Hc := H;

    { new header; the "cut" indicator propagates }
    Headers_PushNew(H,Nil,Nil,False,False);

    If isCut Then
    Begin
      Solvable := True; { "cut" is always clearable }
      H^.HH_FBCL := BTerms_GetNext(ClearB)
    End
    Else
    If isSys Then
    Begin
      Solvable := ExecutionSysCallOk(P,Q,ClearT);
      { remove the term from the list of terms to clear }
      H^.HH_FBCL := BTerms_GetNext(ClearB)
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
        RuleB := Rule_GetTerms(R);
        CopyRuleP := DeepCopy(PRuleB);

        { link each term of the rule to the header pointing to that rule }
        BTerms_SetHeader(BCopyRuleP,Hc);

        { constraint to reduce: term to clear = rule head }
        Ss := NewSystemWithEq(ClearT,BTerm_GetTerm(BCopyRuleP));

        { new list of terms to clear: rule queue + all previous terms 
          but the first }
        B := BCopyRuleP;
        While (BTerms_GetNext(B)<>Nil) Do
          B := BTerms_GetNext(B);
        B^.BT_NEXT := BTerms_GetNext(ClearB);
        H^.HH_FBCL := BTerms_GetNext(BCopyRuleP);

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
  If Query_GetSys(Q) <> Nil Then
    If Not ReduceEquations(Query_GetSys(Q)) Then
      Exit;

  B := Query_GetTerms(Q); { list of terms to clear }

  { no terms to clear: success }
  If B = Nil Then
  Begin
    WriteQuerySolution;
    Exit
  End;

  R := FirstCandidateRule(FirstRule(P,False),B,isSys,isCut);

  { not even a candidate rule to try: fail }
  If (R = Nil) And (Not isSys) And (Not isCut) Then
  Begin
    WarnNoRuleToClearTerm(B);
    Exit
  End;

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
  If Token_GetType(K) <> TOKEN_END_OF_INPUT Then
  Begin
    Q := ParseOneQuery(f,P,K,False);
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
Begin
  While ((ContTokens=[]) Or (Token_GetType(K) In ContTokens))
    And (Not (Token_GetType(K) In StopTokens)) And (Not Error) Do
  Begin
    If WithArrow Then
      VerifyToken(f,P,K,TOKEN_ARROW);
    If Error Then Exit;
    Q := ParseOneQuery(f,P,K,True);
    If Error Then Exit;
    AnswerQuery(P,Q,World_IsUserLand(GetCurrentWorld(P)))
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
  If GetSyntax(P) = PrologII Then
    StopTokens := StopTokens + [TOKEN_SEMICOLON]; 
  Repeat
    Case Token_GetType(K) Of
    TOKEN_END_OF_INPUT:
      Stop := True;
    TOKEN_SEMICOLON:
      If GetSyntax(P) = PrologII Then { old Prolog II termination }
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
  If GetStreamByPath(P,Path) <> Nil Then
  Begin
    RuntimeError('insertion loop: ''' + Path + '''');
    Exit
  End;
  f := NewStream(Path,Path,DEV_FILE,MODE_READ,True,False);
  PushStream(P,f);
  TryPrologFileForInput := f
End;

{ return a stream for filename Path, using default program extensions for 
 the current syntax; 
 NOTE: probably less TOCTOU-prone than looking for the file and then 
 setting it as input }
Function SetPrologFileForInput( P : ProgPtr; Path : TPath ) : StreamPtr;
Var
  f : StreamPtr;
  y : TSyntax;
Begin
  SetPrologFileForInput := Nil;
  y := GetSyntax(P);
  f := TryPrologFileForInput(P,Path);
  If Error Then Exit;
  If (f = Nil) And (y = Edinburgh) Then
  Begin
    f := TryPrologFileForInput(P,Path + '.pl');
    If Error Then Exit
  End;
  If f = Nil Then
  Begin
    f := TryPrologFileForInput(P,Path + '.' + FileExt[y]);
    If Error Then Exit
  End;
  SetPrologFileForInput := f
End;

{ load rules and execute queries (if nay) from a Prolog file; if TryPath is 
 True, try to use the main program dir first;
 Q is the query (if any) that triggered the loading, e.g. due to an "insert" 
 goal }
Procedure LoadProgram( P : ProgPtr; s : StrPtr; TryPath : Boolean );
Var 
  FileName, Path : TPath;
  f : StreamPtr;
Begin
  If Str_Length(s) <= StringMaxSize Then
  Begin
    FileName := Str_AsString(s);
    Path := GetProgramPath(P);
    f := Nil;
    If TryPath And (path <> '') And 
        (Length(Path) + Length(FileName) <= StringMaxSize) Then
      f := SetPrologFileForInput(P,Path + FileName);
    If f = Nil Then
      f := SetPrologFileForInput(P,FileName);
    If f <> Nil Then
    Begin
      BeginInsertion(P);
      ProcessRulesAndQueries(f,P);
      StreamClose(f);
      EndInsertion(P);
      If Error Then Exit
    End
    Else
      RuntimeError('Cannot open file ')
  End
  Else
    RuntimeError('filename is too long');
End;

End.