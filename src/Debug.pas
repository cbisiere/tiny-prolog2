{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Debug.pas                                                  }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022-2026                                                  }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                              D E B U G G I N G                             }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }

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
    Stream_WriteLongString(f,Token_GetStr(K))
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
  y := PrologIIv1; { syntax for debug output }
  Case PObjectType(p) Of
  PR:
    Begin
      Stream_WriteShortString(f,IntToShortString(PRp^.PP_LEVL));
      Stream_WriteShortString(f,' ');
      y := GetSyntax(PRp);
      If y = PrologIIv1 Then
        Stream_WriteShortString(f,'PrologIIv2 version 1')
      Else If y = PrologIIv2 Then
        Stream_WriteShortString(f,'PrologIIv2 version 2')
      Else If y = PrologIIp Then
        Stream_WriteShortString(f,'PrologII+')
      Else If y = Edinburgh Then
        Stream_WriteShortString(f,'Edinburgh')
    End;
  RU:
    Begin
      Stream_WriteShortString(f,'Head: ');
      PutTerm(f,y,BTerm_GetTerm(Rule_GetHead(Rup)))
    End;
  QU:
    Begin
    End;
  SY:
    Begin
    End;
  EQ:
    Begin
      PutOneEquation(f,y,E)
    End;
  BT:
    Begin
      PutTerm(f,y,BTerm_GetTerm(Bp));
    End;
  CO:
    Begin
      Stream_WriteShortString(f,'''');
      PutConst(f,y,Cp);
      Stream_WriteShortString(f,'''')
    End;
  ID:
    Begin
      Stream_WriteShortString(f,BoolToShortString(Ip^.TI_ASSI));
      Stream_WriteShortString(f,' ');
      PutIdentifier(f,y,Ip)
    End;
  FU:
    Begin
      PutTerm(f,y,Tp);
      If (FRed(Fp) <> Nil) Then
      Begin
        Stream_WriteShortString(f,'==');
        PutTerm(f,y,FRed(Fp))
      End
    End;
  VA:
    Begin
      PutVarName(f,y,Vp);
      If (VRed(Vp) <> Nil) Then
      Begin
        Stream_WriteShortString(f,'==');
        PutTerm(f,y,VRed(Vp))
      End
    End;
  DE:
    Begin
      Stream_WriteShortString(f,BoolToShortString(Dict_IsGlobal(Dp)));
      Stream_WriteShortString(f,' "');
      Stream_WriteLongString(f,Dict_GetStr(Dp));
      Stream_WriteShortString(f,'"')
    End;
  HE:
    Begin
      Stream_WriteShortString(f,LongIntToShortString(Header_GetClock(Hp)))
    End;
  ST:
    Begin
      Stream_WriteLongString(f,s);
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
  GoalType : TGoalType; 
  Access : IdPtr;
  Arity : TArity;
Begin
  y := PrologIIv1; { syntax for header output }
  Stream_WritelnShortString(f,'*** Header level ' + LongIntToShortString(Header_GetClock(H)) + ' ***');

  B := Header_GetGoalsToClear(H);
  If B <> Nil Then
    BTerm_GetMetadata(B,GoalType,Access,Arity);

  { goal type }
  If B <> Nil Then
  Begin
    Stream_WriteShortString(f,'  Type: ');
    Case GoalType Of
      GOAL_BLOCK: Stream_WritelnShortString(f,'GOAL_BLOCK');
      GOAL_FIND: Stream_WritelnShortString(f,'GOAL_FIND');
      GOAL_CUT: Stream_WritelnShortString(f,'GOAL_CUT');
      GOAL_SYS: Stream_WritelnShortString(f,'GOAL_SYS');
      GOAL_STD: Stream_WritelnShortString(f,'GOAL_STD')
    End
  End;

  { access / arity }
  If B <> Nil Then
  Begin
    If Access <> Nil Then
    Begin
      Stream_WriteShortString(f,'  Access: ');
      Stream_WriteShortString(f,IdentifierGetShortString(Access));
      Stream_WriteShortString(f,'/');
      Stream_WriteShortString(f,PosIntToShortString(Arity));
      Stream_LineBreak(f)
    End
  End;

  { terms to clear }
  Stream_WriteShortString(f,'  Terms: -> ');
  While B <> Nil Do
  Begin
    PutTerm(f,y,BTerm_GetTerm(B));
    If BTerm_GetHeader(B) <> Nil Then
      Stream_WriteShortString(f,'[' + LongIntToShortString(Header_GetClock(BTerm_GetHeader(B))) + ']');
    Stream_WriteShortString(f,' -> ');
    B := BTerms_GetNext(B)
  End;
  Stream_WritelnShortString(f,' Nil');

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
    PutOneRule(f,y,Header_GetRule(H));
    Stream_LineBreak(f)
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
    PutTerm(f,y,Header_GetSideCarTerm(H));
    Stream_LineBreak(f)
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
    Stream_LineBreak(f)
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
    PutVarName(f,y,V);
    If VRed(V) <> Nil Then
    Begin
      Stream_WriteShortString(f,' = ');
      PutTerm(f,y,VRed(V))
    End;
    If WatchIneq(V) <> Nil Then
    Begin
      Stream_WriteShortString(f,', ');
      PutOneEquation(f,y,WatchIneq(V))
    End;
    Stream_LineBreak(f);
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
    Stream_WriteLongString(f,s);
    Stream_LineBreak(f);
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