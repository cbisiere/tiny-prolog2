{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Debug.pas                                                  }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                              D E B U G G I N G                             }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Unit Debug;

Interface

Uses
  ShortStr,
  Num,
  Errs,
  Trace,
  Memory,
  PObj,
  PObjIO,
  PObjTerm,
  PObjFCVI,
  PObjRest,
  PObjOp,
  PObjStr,
  PObjTok,
  PObjDict,
  PObjEq,
  PObjSys,
  PObjDef,
  PObjBter,
  PObjQury,
  PObjHead,
  PObjProg,
  PObjRule,
  Unparse;

Procedure SetCurrentProgram( P : ProgPtr );
Procedure DumpBacktrace( f : StreamPtr; Q : QueryPtr );
Procedure CoreDumpProg( P : ProgPtr; Message : TString; Trace : Boolean );
Procedure CoreDump( Message : TString; Trace : Boolean );

Implementation
{-----------------------------------------------------------------------------}

Var
  CurrentProgram : ProgPtr; { Prolog program to debug }
  OngoingCoreDump : Boolean;

Procedure SetCurrentProgram( P : ProgPtr );
Begin
  CurrentProgram := P
End;

{----------------------------------------------------------------------------}
{ DUMP                                                                       }
{----------------------------------------------------------------------------}

Procedure DumpToken( f : StreamPtr; K : TokenPtr );
Begin
  Stream_WriteShortString(f,'(');
  Stream_WriteShortString(f,IntToShortString(K^.TK_LINE));
  Stream_WriteShortString(f,',');
  Stream_WriteShortString(f,IntToShortString(K^.TK_CHAR));
  Stream_WriteShortString(f,') ');
  Stream_WriteShortString(f,Token_GetTypeAsShortString(K));
  If Token_GetStr(K) <> Nil Then
  Begin
    Stream_WriteShortString(f,': ');
    OutString(f,Token_GetStr(K))
  End
End;

{ Write extra data in a Prolog object }
Procedure WriteExtraData( f : StreamPtr; p : TObjectPtr );
Var 
  PRp : ProgPtr Absolute p;
  Tp : TermPtr Absolute p;
  Cp : ConstPtr Absolute p;
  Vp : VarPtr Absolute p;
  Ip : IdPtr Absolute p;
  Fp : FuncPtr Absolute p;
  Dp : DictPtr Absolute p;
  E : EqPtr Absolute p;
  Sda : StrDataPtr Absolute p;
  S : StrPtr Absolute p;
  Rp : RestPtr Absolute p;
  Hp : HeadPtr Absolute p;
  Bp : BTermPtr Absolute p;
  Rup : RulePtr Absolute p;
  Kp : TokenPtr Absolute p;
  Opp : OpPtr Absolute p;
  y : TSyntax;
Begin
  CheckCondition(CurrentProgram <> Nil,'WriteExtraData: program not set');
  y := PrologIIc; { syntax for debug output }
  Case PObjectType(p) Of
  PR:
    Begin
      Stream_WriteShortString(f,IntToShortString(PRp^.PP_LEVL));
      Stream_WriteShortString(f,' ');
      y := GetSyntax(PRp);
      If y = PrologIIc Then
        Stream_WriteShortString(f,'PrologII version 1')
      Else If y = PrologII Then
        Stream_WriteShortString(f,'PrologII version 2')
      Else If y = PrologIIp Then
        Stream_WriteShortString(f,'PrologII+')
      Else If y = Edinburgh Then
        Stream_WriteShortString(f,'Edinburgh')
    End;
  RU:
    Begin
      Stream_WriteShortString(f,'Head: ');
      OutTerm(f,y,BTerm_GetTerm(Rule_GetHead(Rup)))
    End;
  QU:
    Begin
    End;
  SY:
    Begin
    End;
  EQ:
    Begin
      OutOneEquation(f,y,E)
    End;
  BT:
    Begin
      OutTerm(f,y,BTerm_GetTerm(Bp));
    End;
  CO:
    Begin
      Stream_WriteShortString(f,'''');
      OutConst(f,Cp);
      Stream_WriteShortString(f,'''')
    End;
  ID:
    Begin
      Stream_WriteShortString(f,BoolToShortString(Ip^.TI_ASSI));
      Stream_WriteShortString(f,' ');
      OutIdentifier(f,Ip)
    End;
  FU:
    Begin
      OutTerm(f,y,Tp);
      If (FRed(Fp) <> Nil) Then
      Begin
        Stream_WriteShortString(f,'==');
        OutTerm(f,y,FRed(Fp))
      End
    End;
  VA:
    Begin
      OutVarName(f,Vp);
      If (VRed(Vp) <> Nil) Then
      Begin
        Stream_WriteShortString(f,'==');
        OutTerm(f,y,VRed(Vp))
      End
    End;
  DE:
    Begin
      Stream_WriteShortString(f,BoolToShortString(Dict_IsGlobal(Dp)));
      Stream_WriteShortString(f,' "');
      OutString(f,Dict_GetStr(Dp));
      Stream_WriteShortString(f,'"')
    End;
  HE:
    Begin
      Stream_WriteShortString(f,LongIntToShortString(Header_GetClock(Hp)));
      Stream_WriteShortString(f,' Sys:');
      Stream_WriteShortString(f,BoolToShortString(Header_GetGoalType(Hp) = GOAL_SYS));
      Stream_WriteShortString(f,' Cut:');
      Stream_WriteShortString(f,BoolToShortString(Header_GetGoalType(Hp) = GOAL_CUT))
    End;
  ST:
    Begin
      OutString(f,s);
      Stream_WriteShortString(f,' (' + LongIntToShortString(Str_Length(s)) + ')')
    End;
  SD:
    Begin
      Stream_WriteShortString(f,' (' + IntToShortString(Sda^.SD_DATA.Len) + ')')
    End;
  RE:
    Begin
      Stream_WriteShortString(f,PtrToName(Rp^.RE_ADDR^) + ' ');
      Stream_WriteShortString(f,BoolToShortString(Rp^.RE_DONE))
    End;
  OP:
    Begin
      Stream_WriteShortString(f,'"' + Op_GetOperator(Opp) + '"');
      Stream_WriteShortString(f,' ');
      Stream_WriteShortString(f,'"' + Op_GetFunction(Opp) + '"')
    End;
  TK:
    DumpToken(f,Kp)
  End
End;

{ display a Prolog clock header }
Procedure DumpHeader( f : StreamPtr; H : HeadPtr );
Var 
  y : TSyntax;
  U : RestPtr;
  B : BTermPtr;
Begin
  y := PrologIIc; { syntax for header output }
  Stream_WritelnShortString(f,'*** Header level ' + LongIntToShortString(Header_GetClock(H)) + ' ***');
  { goal type }
  Stream_WriteShortString(f,'  Type: ');
  Case Header_GetGoalType(H) Of
    GOAL_UNDEFINED: Stream_WritelnShortString(f,'GOAL_UNDEFINED');
    GOAL_BLOCK: Stream_WritelnShortString(f,'GOAL_BLOCK');
    GOAL_FIND: Stream_WritelnShortString(f,'GOAL_FIND');
    GOAL_CUT: Stream_WritelnShortString(f,'GOAL_CUT');
    GOAL_SYS: Stream_WritelnShortString(f,'GOAL_SYS');
    GOAL_STD: Stream_WritelnShortString(f,'GOAL_STD')
  End;

  { access / arity }
  If Header_GetGoalAccess(H) <> Nil Then
  Begin
    Stream_WriteShortString(f,'  Header access: ');
    Stream_WriteShortString(f,IdentifierGetShortString(Header_GetGoalAccess(H)));
    Stream_WriteShortString(f,'/');
    Stream_WriteShortString(f,PosIntToShortString(Header_GetGoalArity(H)));
    Stream_Writeln(f)
  End;

  { terms to clear }
  Stream_WriteShortString(f,'  Terms: -> ');
  B := Header_GetGoalsToClear(H);
  While B <> Nil Do
  Begin
    If BTerm_GetType(B) = GOAL_FIND Then
      Stream_WriteShortString(f,'{FIND}') 
    Else If BTerm_GetType(B) = GOAL_BLOCK Then
      Stream_WriteShortString(f,'{BLOCK}') 
    Else
      OutTerm(f,y,BTerm_GetTerm(B));
    If BTerm_GetHeader(B) <> Nil Then
      Stream_WriteShortString(f,'[' + LongIntToShortString(Header_GetClock(BTerm_GetHeader(B))) + ']');
    Stream_WriteShortString(f,' -> ');
    B := BTerms_GetNext(B)
  End;
  Stream_WritelnShortString(f,' Nil');

  { access / arity }
  B := Header_GetGoalsToClear(H);
  If B <> Nil Then
  Begin
    If BTerm_GetAccessTerm(B) <> Nil Then
    Begin
      Stream_WriteShortString(f,'  First goal access: ');
      Stream_WriteShortString(f,IdentifierGetShortString(BTerm_GetAccessTerm(B)));
      Stream_WriteShortString(f,'/');
      Stream_WriteShortString(f,PosIntToShortString(BTerm_GetArity(B)));
      Stream_Writeln(f)
    End
  End;

  { cleared? }
  Stream_WriteShortString(f,'  Cleared: ');
  If Header_IsCleared(H) Then
    Stream_WritelnShortString(f,'Yes')
  Else
    Stream_WritelnShortString(f,'No');

  { branch }
  Stream_WritelnShortString(f,'  Branch: ' + LongIntToShortString(Header_GetBranchNumber(H)));
  { over? }
  Stream_WriteShortString(f,'  Over: ');
  If Header_IsDone(H) Then
    Stream_WritelnShortString(f,'Yes')
  Else
    Stream_WritelnShortString(f,'No');

  { more? }
  Stream_WriteShortString(f,'  More: ');
  If Header_GetMore(H) Then
    Stream_WritelnShortString(f,'Yes')
  Else
    Stream_WritelnShortString(f,'No');
  
  { rule }
  Stream_WriteShortString(f,'  Rule: ');
  If (Header_GetRule(H) = Nil) Then
    Stream_WritelnShortString(f,'Nil')
  Else
  Begin
    OutOneRule(f,Header_GetRule(H));
    Stream_Writeln(f)
  End;

  { cut target, if any }
  If Header_GetCutTarget(H) <> Nil Then
    Stream_WritelnShortString(f,'  Back cut: ' + LongIntToShortString(Header_GetClock(Header_GetCutTarget(H))));

  { block target, if any }
  If Header_GetBlockScope(H) <> Nil Then
    Stream_WritelnShortString(f,'  Back block: ' + LongIntToShortString(Header_GetClock(Header_GetBlockScope(H))));

  { ZTerm, if any }
  If Header_GetSideCarTerm(H) <> Nil Then
  Begin
    Stream_WriteShortString(f,'  CHOIV: ');
    OutTerm(f,y,Header_GetSideCarTerm(H));
    Stream_Writeln(f)
  End;

  { restore }
  If H^.HH_REST <> Nil Then
  Begin
    Stream_WriteShortString(f,'  Rest_Restore: ');
    U := H^.HH_REST;
    While U <> Nil Do
      With U^ Do
      Begin
        Stream_WriteShortString(f,'.');
        U := RE_NEXT
      End;
    Stream_Writeln(f)
  End
End;

{ display the call stack until header H, included }
Procedure Backtrace( f : StreamPtr; H : HeadPtr );
Begin
  Stream_WritelnShortString(f,'HEADER DUMP:');
  While H <> Nil Do
  Begin
    DumpHeader(f,H);
    H := Headers_GetNext(H)
  End
End;

{ display the call stack }
Procedure DumpBacktrace( f : StreamPtr; Q : QueryPtr );
Begin
  Backtrace(f,Query_GetHead(Q))
End;

{ display variable identifiers in a dictionary }
Procedure DumpDictVar( f : StreamPtr; y : TSyntax; D : DictPtr );
Var 
  e : DictPtr;
  V : VarPtr;
  TV : TermPtr Absolute V;
Begin
  Stream_WritelnShortString(f,'Variables:');
  e := D;
  while e <> Nil Do
  Begin
    TV := Dict_GetTerm(e);
    Stream_WriteShortString(f,' ');
    If Dict_IsGlobal(e) Then
      Stream_WriteShortString(f,'*')
    Else
      Stream_WriteShortString(f,' ');
    OutVarName(f,V);
    If VRed(V) <> Nil Then
    Begin
      Stream_WriteShortString(f,' = ');
      OutTerm(f,y,VRed(V))
    End;
    If WatchIneq(V) <> Nil Then
    Begin
      Stream_WriteShortString(f,', ');
      OutOneEquation(f,y,WatchIneq(V))
    End;
    Stream_Writeln(f);
    e := Dict_GetNext(e)
  End
End;

{ display the dictionary of constants }
Procedure DumpDictConst( f : StreamPtr; e : DictPtr; title : TString; 
    Ident: Boolean );
Var
  s : StrPtr;
Begin
  Stream_WritelnShortString(f,title);
  while (e<>Nil) Do
  Begin
    If Dict_IsGlobal(e) Then
      Stream_WriteShortString(f,'*')
    Else
      Stream_WriteShortString(f,' ');
    If Ident Then
      s := GetIdentAsStr(IdPtr(Dict_GetTerm(e)),True)
    Else
      s := Dict_GetStr(e);
    OutlnString(f,s);
    e := Dict_GetNext(e)
  End
End;

{ core dump a Prolog program }
Procedure CoreDumpProg( P : ProgPtr; Message : TString; Trace : Boolean );
Var
  reg : Boolean;
  f : StreamPtr;
Begin
  { suspend objects registration to avoid messing up the core dump with 
    objects created during the trace }
  reg := GetRegistrationState;
  f := GetOutputConsole(P);
  SetRegistration(False);
  If Not OngoingCoreDump Then
  Begin
    OngoingCoreDump := True;
    Stream_WritelnShortString(f,'Begin Core Dump: "' + Message + '"');
    PrintMemoryStats;
    DumpGCRoots;
    DumpRegisteredObject;
    If P <> Nil Then
    Begin
      DumpDictConst(f,P^.PP_DCON,'Constants:',False);
      DumpDictConst(f,P^.PP_DIDE,'Identifiers:',True);
      If Trace Then
        Backtrace(f,Query_GetHead(GetCurrentQuery(P)))
    End;
    Stream_WritelnShortString(f,'End Code Dump: "' + Message + '"');
    OngoingCoreDump := False
  End;
  SetRegistration(reg)
End;

{ core dump the current Prolog program }
Procedure CoreDump( Message : TString; Trace : Boolean );
Begin
  CheckCondition(CurrentProgram <> Nil,'CoreDump: program not set');
  CoreDumpProg(CurrentProgram,Message,Trace);
End;

{ initialize the unit }
Begin
  OngoingCoreDump := False;
  CurrentProgram := Nil
End.