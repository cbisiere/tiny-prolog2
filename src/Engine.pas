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
  Strings,
  Errs,
  Chars,
  Crt2,
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
  IStack,
  Encoding,
  Unparse,
  Reduc,
  Expr,
  Parse,
  Debug;

{ file suffix: for PII+ and Edinburgh, see PII+ doc p297 }
Type 
  TFileExt = Array[TSyntax] Of String[3];
Const 
  FileExt : TFileExt = ('pro','p2c','p2','p2E'); { TODO: rewrite to handle .pl }

Procedure RegisterPredefined( P : ProgPtr );
Procedure AnswerQueries( P : ProgPtr; Echo : Boolean );
Procedure LoadProgram( P : ProgPtr; Q : QueryPtr; 
    s : StrPtr; TryPath : Boolean );

Implementation
{-----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{                                                                            }
{                           S Y S T E  M   C A L L S                         }
{                                                                            }
{----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ predefined predicates                                                      }
{----------------------------------------------------------------------------}

Const
  NB_PP = 26;
  MAX_PP_LENGHT = 21; { max string length }
  SYSCALL_IDENT_AS_STRING = 'syscall'; 
Type
  TPP = (
    PP_INPUT_IS,PP_INPUT,PP_CLOSE_CURRENT_INPUT,PP_CLOSE_INPUT,PP_CLEAR_INPUT,
    PP_IN_TERM,PP_IN_CHAR,
    PP_OUTPUT_IS,PP_OUTPUT,PP_CLOSE_CURRENT_OUTPUT,PP_CLOSE_OUTPUT,PP_FLUSH,
    PP_QUIT,PP_INSERT,PP_LIST,
    PP_OUT,PP_OUTM,PP_LINE,
    PP_BACKTRACE,PP_CLRSRC,PP_EVAL,PP_OP,PP_ASSIGN,PP_DUMP,
    PP_DIF,PP_UNIV);
  TPPRec = Record
    I : TPP; { identifier }
    S : String[MAX_PP_LENGHT]; { identifier as string }
    N : Byte; { number of arguments }
  End;
  TPPArray = Array[1..NB_PP] Of TPPRec;

{ predefined predicates; use identifiers that are portable 
 across all the supported syntaxes: lowercase letters with at least two
 letters }

Const 
  PPArray : TPPArray = (
    (I:PP_INPUT_IS;S:'sysinputis';N:1),
    (I:PP_INPUT;S:'sysinput';N:1),
    (I:PP_CLOSE_CURRENT_INPUT;S:'sysclosecurrentinput';N:0),
    (I:PP_CLOSE_INPUT;S:'syscloseinput';N:1),
    (I:PP_CLEAR_INPUT;S:'sysclearinput';N:0),
    (I:PP_IN_TERM;S:'sysinterm';N:1),
    (I:PP_IN_CHAR;S:'sysinchar';N:1),
    (I:PP_OUTPUT_IS;S:'sysoutputis';N:1),
    (I:PP_OUTPUT;S:'sysoutput';N:1),
    (I:PP_CLOSE_CURRENT_OUTPUT;S:'sysclosecurrentoutput';N:0),
    (I:PP_CLOSE_OUTPUT;S:'syscloseoutput';N:1),
    (I:PP_FLUSH;S:'sysflush';N:0),
    (I:PP_QUIT;S:'sysquit';N:0),
    (I:PP_INSERT;S:'sysinsert';N:1),
    (I:PP_LIST;S:'syslist';N:0),
    (I:PP_OUT;S:'sysout';N:1),
    (I:PP_OUTM;S:'sysoutm';N:1),
    (I:PP_LINE;S:'sysline';N:0),
    (I:PP_BACKTRACE;S:'sysbacktrace';N:0),
    (I:PP_CLRSRC;S:'sysclrsrc';N:0),
    (I:PP_EVAL;S:'syseval';N:2),
    (I:PP_OP;S:'sysop';N:4), { TODO: 3-arg version }
    (I:PP_ASSIGN;S:'sysassign';N:2),
    (I:PP_DUMP;S:'sysdump';N:0),
    (I:PP_DIF;S:'sysdif';N:2),
    (I:PP_UNIV;S:'sysuniv';N:2) { '=..', Edinburgh only, p.221 }
  );

{ lookup for a predefined predicates; set the found record; 
  return True if found  }
Function LookupPP( str : TString; Var rec : TPPRec) : Boolean;
Var 
  i : 0..NB_PP;
  Found : Boolean;
Begin
  i := 0;
  Found := False;
  While (Not Found) And (i < NB_PP) Do
  Begin
    i := i + 1;
    If PPArray[i].S = str Then
    Begin
      Found := True;
      rec := PPArray[i]
    End
  End;
  LookupPP := Found
End;


{ is an identifier a syscall? }
Function IdentifierIsSyscall( I : IdPtr ) : Boolean;
Begin
  IdentifierIsSyscall := IdentifierEqualTo(I,SYSCALL_IDENT_AS_STRING)
End;

{ install all predefined, persistent constants }
Procedure RegisterPredefined( P : ProgPtr );
Var I : IdPtr;
Begin
  I := InstallIdentifier(P^.PP_DCON,NewStringFrom(SYSCALL_IDENT_AS_STRING),True)
End;


{----------------------------------------------------------------------------}
{ helpers for syscall's arguments                                            }
{----------------------------------------------------------------------------}

{ get n-th argument of the predicate represented by tuple U }
Function GetPArg( n : Byte; U : TermPtr ) : TermPtr;
Begin
  GetPArg := TupleArgN(2+n,U)
End;

{ return as a string pointer (or Nil) argument n of a predicate, supposed to be
 a filename; filename can be a string or an identifier, and are always returned 
 unquoted }
Function GetFilenameArgAsStr( n : Byte; T : TermPtr ) : StrPtr;
Var
  I : IdPtr;
  C : ConstPtr;
  s : StrPtr;
Begin
  s := Nil;
  I := EvaluateToIdentifier(GetPArg(n,T));
  If I <> Nil Then
    s := GetIdentAsString(I,False)
  Else
  Begin
    C := EvaluateToString(GetPArg(n,T));
    If C <> Nil Then
      s := GetConstAsString(C,False)
  End;
  GetFilenameArgAsStr := s
End;

{ return True if argument n of a predicate can be assigned to Pascal
 string str }
Function GetFilenameArgAsString( n : Byte; T : TermPtr; 
    Var str : TString ) : Boolean;
Var
  s : StrPtr;
Begin
  s := GetFilenameArgAsStr(n,T);
  If s <> Nil Then
    str := StrGetFirstData(s);
  GetFilenameArgAsString := s <> Nil
End;


{----------------------------------------------------------------------------}
{ syscall                                                                    }
{----------------------------------------------------------------------------}

{ execute a system call syscall(Code,Arg1,...ArgN), meaning Code(Arg1,...,ArgN) }
Function ExecutionSysCallOk( T : TermPtr; P : ProgPtr; Q : QueryPtr ) : Boolean;
Var
  Ok : Boolean;
  y : TSyntax;
  Ident : StrPtr;
  NbArgs : Integer;
  SysCallCode : StrPtr;
  NbPar : Integer; { number of parameters of the system call }
  T1 : TermPtr;
  IT1 : IdPtr Absolute T1;
  VT1 : VarPtr Absolute T1;
  T2 : TermPtr;
  IT2 : IdPtr Absolute T2;
  VT2 : VarPtr Absolute T2;
  C1,C3 : ConstPtr;
  rec : TPPRec;
  str : TString;
  ch : TChar;
  I,I2,I3,I4 : IdPtr;
  TI : TermPtr Absolute I;
  v,code : Integer;
  Id2,Id3,Id4 : TString;
  Qi, QLast : QueryPtr;
  Stop : Boolean;
  FileName : TString;
  FileNamePtr : StrPtr;
  o : OpPtr;
  ot : TOpType;
  L : TermPtr;
  HadNoRules : Boolean;

Begin
  ExecutionSysCallOk := False; { default is to fail }
  y := GetSyntax(P);

  { coded as a functional symbol }
  CheckCondition(TypeOfTerm(T) = FuncSymbol,'syscall: functional symbol expected');
  
  { first parameter is syscall }
  T1 := TupleArgN(1,T);
  CheckCondition(TypeOfTerm(T1) = Identifier,'syscall: constant expected');
  SysCallCode := IdentifierGetStr(IT1);
  CheckCondition(StrEqualTo(SysCallCode,SYSCALL_IDENT_AS_STRING),'Not a syscall');

  { there are at least two arguments: 'syscall' and the identifier }
  NbArgs := TupleArgCount(T);
  Ok := NbArgs >= 2;  
  If Ok Then
  Begin
    T2 := TupleArgN(2,T);
    Ident := IdentifierGetStr(IT2);
    Ok := StrLength(Ident) <= MAX_PP_LENGHT
  End;

  { predicate is known and has the correct number of parameters }
  If Ok Then
  Begin
    str := StrGetString(Ident);
    Ok := LookupPP(str,rec);
    If Ok Then
    Begin
      NbPar := NbArgs - 2;
      Ok := NbPar = rec.N
    End;
  End;

  { clear the predicate }
  If Ok Then
  Begin
    Case rec.I Of
    PP_DIF:
      Begin
        T1 := GetPArg(1,T);
        T2 := GetPArg(2,T);
        Ok := ReduceOneIneq(T1,T2)
      End;
    PP_ASSIGN: { assign(file_name, "myfile.txt") }
      Begin
        { note: re-assignments are tricky to handle, as the reduced system, after
          e.g. "assign(test,1)", contains i=1 (w/o any remaining reference to the 
          identifier) }
        { get the identifier }
        T1 := GetPArg(1,T);
        Case TypeOfTerm(T1) Of
        Identifier: { an identifier, thus unbound (first assignment) }
          Begin
            DictSetGlobal(IT1^.TV_DVAR,True); { dict entry is now persistent }
            I := IT1
          End;
        Variable:
          Begin
            I := VT1^.TV_IRED;
            If I = Nil Then { variable has never been bound to an identifier }
              I := EvaluateToIdentifier(GetPArg(1,T));
            Ok := I <> Nil;
            If Ok Then
            Begin
              { unbound the variable (which was bounded to the term) }
              UnbindVar(VT1)
            End
          End;
        Else
          Ok := False
        End;
        If Ok Then
        Begin
          { neutralize its role (if any) in the reduced system as variable-like, 
            assigned identifier }
          UnbindVar(I);
          I^.TV_ASSI := True;
          { second parameter }
          T2 := GetPArg(2,T);
          { assign }
          Ok := ReduceOneEq(TI,T2) { "ident = term" }
        End
      End;
    PP_EVAL: { val(100,x) }
      Begin
        { evaluate the term; it may includes variables and constraints;
          thus, our assign is similar to "cassign(i,t)";
          see p113 of the PrologII+ documentation }
        T1 := EvaluateExpression(GetPArg(1,T),P); { FIXME: do a copy and unbound variables? }
        Ok := T1 <> Nil;
        If Ok Then
        Begin
          T2 := GetPArg(2,T);
          Ok := ReduceOneEq(T2,T1) { FIXME: shouldn't it be backtrackable? }
        End
      End;
    PP_UNIV: { '=..'(foo(a,b),[foo,a,b]) }
      Begin
        T1 := RepresentativeOf(GetPArg(1,T));
        T2 := RepresentativeOf(GetPArg(2,T));
        { create a new list from T1 }
        Case TypeOfTerm(T1) Of
        Constant,Identifier:
          L := NewList2(P,T1,Nil); { foo =.. Y gives Y = [foo] }
        FuncSymbol:
          L := TupleToList(P,T1); { foo(a,b) =.. Y gives Y = [foo,a,b] }
        Else
          L := T1
        End;
        { create a new tuple from list T2 }
        Case TypeOfTerm(T2) Of
        FuncSymbol:
          T := ListToTuple(T2); { X =.. [foo,a,b] gives X = foo(a,b) }
        Else
          T := T2
        End;
        Ok := ReduceOneEq(T,L) { TODO: backtrackable? }
      End;
    PP_OP: { op(700,xfx,"<",inf) } { TODO: implement full specs PII+ p137}
      Begin
        { 1: precedence (integer value between 1 and 1200) }
        C1 := EvaluateToInteger(GetPArg(1,T));
        If C1 = Nil Then
          Exit;
        str := ConstGetPStr(C1);
        If Length(str) > 4 Then
          Exit;
        Val(str,v,code);
        If code <> 0 Then
          Exit;
        If (v < 0) Or (v > 1200) Then
          Exit;
        { 2: type of operator (identifier in a list) }
        I2 := EvaluateToIdentifier(GetPArg(2,T));
        If I2 = Nil Then
          Exit;
        Id2 := IdentifierGetPStr(I2);
        If Not IsOpTypeString(Id2) Then
          Exit;
        ot := PStrToOpType(Id2);
        { 3: operator (string or identifier) }
        I3 := EvaluateToIdentifier(GetPArg(3,T));
        If I3 <> Nil Then
          Id3 := IdentifierGetPStr(I3)
        Else
        Begin
          C3 := EvaluateToString(GetPArg(3,T));
          If C3 = Nil Then
            Exit;
          Id3 := ConstGetPStr(C3) { limits to StringMaxSize chars }
        End;
        { 4: functional symbol (identifier) }
        I4 := EvaluateToIdentifier(GetPArg(4,T));
        If I4 = Nil Then
          Exit;
        Id4 := IdentifierGetPStr(I4);
        { not allowed: existing function with same number of parameters }
        o := OpLookup(P^.PP_OPER,'',Id4,[],TOpTypeToArity(ot),1200);
        If o <> Nil Then { TODO: do not fail when both declarations match }
          Exit;
        { register the new operator }
        o := OpAppend(P^.PP_OPER,Id3,Id4,ot,v);
        Ok := True
      End;
    PP_QUIT:
      SetQuitOn(0);
    PP_INSERT: { insert("file") or insert('file') }
      Begin
        { 1: filename (string or identifier, unquoted) }
        FileNamePtr := GetFilenameArgAsStr(1,T);
        If FileNamePtr = Nil Then Exit;
        { execute }
        HadNoRules := FirstProgramRule(P) = Nil;
        QLast := LastProgramQuery(P);
        LoadProgram(P,Q,FileNamePtr,True);
        If Error Then Exit;
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
        End
      End;
    PP_INPUT: { input("buffer") }
      Begin
        If Not GetFilenameArgAsString(1,T,FileName) Then 
          Exit;
        CloseOutput(FileName); { close the file if it was already open for output }
        Ok := SetFileForInput(FileName) { TODO: warn when length > 255 }
      End;
    PP_INPUT_IS: { input_is(s) }
      Begin
        T1 := EmitConst(P,NewStringFrom(InputIs),CS,False);
        Ok := ReduceOneEq(GetPArg(1,T),T1)
      End;
    PP_CLOSE_CURRENT_INPUT: { close_input }
      Begin
        Ok := True;
        CloseCurrentInput
      End;
    PP_CLOSE_INPUT: { close_input("buffer") }
      Begin
        If Not GetFilenameArgAsString(1,T,FileName) Then 
          Exit;
        CloseInputByName(FileName) { TODO: warn when length > 255 }
      End;
    PP_CLEAR_INPUT: { clear_input }
      Begin
        Ok := True;
        ClearInput
      End;
    PP_OUTPUT: { output("buffer") }
      Begin
        If Not GetFilenameArgAsString(1,T,FileName) Then 
          Exit;
        CloseInputByName(FileName); { close the file if it was already open for input }
        Ok := SetFileForOutput(FileName) { TODO: warn when length > 255 }
      End;
    PP_OUTPUT_IS: { output_is(s) }
      Begin
        T1 := EmitConst(P,NewStringFrom(OutputIs),CS,False);
        Ok := ReduceOneEq(GetPArg(1,T),T1)
      End;
    PP_CLOSE_CURRENT_OUTPUT: { close_output }
      Begin
        Ok := True;
        CloseCurrentOutput
      End;
    PP_CLOSE_OUTPUT: { close_output("buffer") }
      Begin
        If Not GetFilenameArgAsString(1,T,FileName) Then 
          Exit;
        CloseOutput(FileName) { TODO: warn when length > 255 }
      End;
    PP_FLUSH: { flush }
      Begin
        Ok := True;
        FlushCurrentOutput
      End;
    PP_LIST:
      Begin
        Ok := True;
        OutRuleRange(FirstRuleInQueryScope(Q),LastRuleInQueryScope(Q),
            RTYPE_USER,False)
      End;
    PP_OUT:
      Begin
        Ok := True;
        OutTerm(y,GetPArg(1,T),True)
      End;
    PP_OUTM:
      Begin
        Ok := True;
        OutTermBis(y,GetPArg(1,T),False,False,True)
      End;
    PP_LINE:
      Begin
        Ok := True;
        OutCR(True)
      End;
    PP_CLRSRC:
      Begin
        Ok := True;
        If OutputIsTerminal Then
          CrtClrSrc
      End;
    PP_IN_TERM:
      Begin
        CheckConsoleInput(True);
        T1 := ParseOneTerm(P);
        Ok := Not Error;
        If Ok Then
          Ok := ReduceOneEq(GetPArg(1,T),T1)
      End;
    PP_IN_CHAR:
      Begin
        CheckConsoleInput(False);
        str := GetChar(ch);
        Ok := Not Error;
        If Ok Then
        Begin
          T1 := EmitConst(P,NewStringFrom(str),CS,False);
          Ok := ReduceOneEq(GetPArg(1,T),T1)
        End
      End;
    PP_BACKTRACE:
      Begin
        Ok := True;
        DumpBacktrace
      End;
    PP_DUMP:
      Begin
        Ok := True;
        DumpRegisteredObject
      End
    End
  End;
    
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

{ return True if filename fn can be set to be an input file, using
 default program extensions for syntax y;
 NOTE: probably less TOCTOU-prone than looking for the file and then setting it  
 as input
 FIXME: where to convert through OSFilename/1 and which name to store in the IO
 stream objects is a question that need to be solved; maybe two members? 
 (the user provided name, without alteration, and the actual path to the file)}
Function SetPrologFileForInput( y : TSyntax; fn : TString ) : Boolean;
Var
  Found : Boolean;
Begin
  Found := SetFileForInput(fn);
  If Not Found Then
  Begin
    If y = Edinburgh Then
      Found := SetFileForInput(fn + '.pl');
    If Not Found Then
      Found := SetFileForInput(fn + '.' + FileExt[y])
  End;
  SetPrologFileForInput := Found
End;

{ load rules and queries from a Prolog file, and execute the queries it 
 contains, if any; if TryPath is True, try to use the main program dir first;
 Q is the query (if any) that triggered the loading, e.g. due to an "insert" 
 goal }
Procedure LoadProgram( P : ProgPtr; Q : QueryPtr; s : StrPtr; 
    TryPath : Boolean );
Var 
  y : TSyntax;
  FileName, Path : TString;
  Opened : Boolean;
Begin
  y := GetSyntax(P);
  If StrLength(s) <= StringMaxSize Then
  Begin
    FileName := StrGetString(s);
    Path := GetProgramPath(P);
    Opened := False;
    If TryPath And (path <> '') And 
        (Length(Path) + Length(FileName) <= StringMaxSize) Then
      Opened := SetPrologFileForInput(y,Path + FileName);
    If Not Opened Then
      Opened := SetPrologFileForInput(y,FileName);
    If Opened Then
    Begin
      BeginInsertion(P);
      ParseRulesAndQueries(P,Q,GetRuleType(P));
      If Error Then Exit;
      CloseCurrentInput;
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