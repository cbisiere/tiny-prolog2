{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Dumper.pas                                                 }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022-2026                                                  }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                        D E B U G G I N G  ( U G L Y )                      }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }

Unit Dumper;

Interface

Uses
  ShortStr,
  Num,
  Errs,
  Chars,
  Dump,
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
Procedure DumpBacktrace( Q : QueryPtr );
Procedure CoreDumpProg( P : ProgPtr; Message : TString; 
    WithBackTrace : Boolean );
Procedure CoreDump( Message : TString; WithBackTrace : Boolean );

Implementation
{-----------------------------------------------------------------------------}

Var
  TargetEncoding : TEncoding;
  CurrentProgram : ProgPtr; { Prolog program to debug }
  OngoingCoreDump : Boolean;

Procedure SetCurrentProgram( P : ProgPtr );
Begin
  CurrentProgram := P
End;

{----------------------------------------------------------------------------}
{ write a long string to the dump file                                       }
{----------------------------------------------------------------------------}

{ write a single TChar to the dump file, honoring line break soft marks, 
 ignoring other soft marks }
Procedure WriteCharToDumpFile( cc : TChar );
Begin
  If TCharIsEol(cc) Then
    WriteLineBreakToDumpFile
  Else If Not TCharIsSoftMark(cc) Then
    WriteToDumpFile(TCharGetBytes(cc))
End;

{ write a long string to the dump file }
Procedure WriteLongStringToDumpFile( s : StrPtr );
Var
  Iter : StrIter;
  cc : TChar;
Begin
  StrIter_ToStart(Iter,s);
  While StrIter_NextChar(Iter,cc) Do
    WriteCharToDumpFile(cc)
End;

{----------------------------------------------------------------------------}
{ source dump                                                                }
{----------------------------------------------------------------------------}

{ dump one constant }
Procedure DumpConst( y : TSyntax; C : ConstPtr );
Var 
  s : StrPtr;
Begin
  s := ConstToLongString(TargetEncoding,y,C);
  WriteLongStringToDumpFile(s)
End;

{ dump one identifier }
Procedure DumpIdentifier( y : TSyntax; I : IdPtr );
Var 
  s : StrPtr;
Begin
  s := IdentifierToLongString(TargetEncoding,y,I);
  WriteLongStringToDumpFile(s)
End;

{ dump one variable name }
Procedure DumpVarName( y : TSyntax; V : VarPtr );
Var 
  s : StrPtr;
Begin
  s := VarNameToLongString(TargetEncoding,y,V);
  WriteLongStringToDumpFile(s)
End;

{ dump a term }
Procedure DumpTerm( y : TSyntax; T : TermPtr );
Var 
  s : StrPtr;
Begin
  s := TermToLongString(TargetEncoding,y,T);
  WriteLongStringToDumpFile(s)
End;

{ dump a term, unquoted }
Procedure DumpTermUnquoted( y : TSyntax; T : TermPtr );
Var 
  s : StrPtr;
Begin
  s := TermUnquotedToLongString(TargetEncoding,y,T);
  WriteLongStringToDumpFile(s)
End;

{ dump one equation }
Procedure DumpOneEquation( y : TSyntax; E : EqPtr );
Var 
  s : StrPtr;
Begin
  s := OneEquationToLongString(TargetEncoding,y,E);
  WriteLongStringToDumpFile(s)
End;

{ dump the reduced system for the variables in the current query (engine) }
Procedure DumpQuerySolution( y : TSyntax; Q : QueryPtr );
Var
  s : StrPtr;
Begin
  s := QuerySolutionToLongString(TargetEncoding,y,Q);
  WriteLongStringToDumpFile(s)
End;

{ dump one rule, using its native syntax (list/1) }
Procedure DumpOneRule( y : TSyntax; R : RulePtr );
Var 
  s : StrPtr;
Begin
  s := OneRuleToLongString(TargetEncoding,y,R);
  WriteLongStringToDumpFile(s)
End;

{ dump one query, using its native syntax }
Procedure DumpOneQuery( y : TSyntax; Q : QueryPtr );
Var 
  s : StrPtr;
Begin
  s := OneQueryToLongString(TargetEncoding,y,Q);
  WriteLongStringToDumpFile(s)
End;

{ dump one comment (list/1) }
Procedure DumpOneComment( y : TSyntax; C : CommPtr );
Var 
  s : StrPtr;
Begin
  s := OneCommentToLongString(TargetEncoding,y,C);
  WriteLongStringToDumpFile(s)
End;

{----------------------------------------------------------------------------}
{ detailed dump                                                              }
{----------------------------------------------------------------------------}

{ Write extra data in a Prolog object }
Procedure DumpExtraData( p : TObjectPtr );
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
  CheckCondition(CurrentProgram <> Nil,'DumpExtraData: program not set');
  y := PrologIIv1; { syntax for dump }
  Case PObjectType(p) Of
  PR:
    Begin
      WriteToDumpFile(IntToShortString(PRp^.PP_LEVL));
      WriteToDumpFile(' ');
      y := GetSyntax(PRp);
      If y = PrologIIv1 Then
        WriteToDumpFile('PrologIIv2 version 1')
      Else If y = PrologIIv2 Then
        WriteToDumpFile('PrologIIv2 version 2')
      Else If y = PrologIIp Then
        WriteToDumpFile('PrologII+')
      Else If y = Edinburgh Then
        WriteToDumpFile('Edinburgh')
    End;
  RU:
    Begin
      WriteToDumpFile('Head: ');
      DumpTerm(y,BTerm_GetTerm(Rule_GetHead(Rup)))
    End;
  QU:
    Begin
    End;
  SY:
    Begin
    End;
  EQ:
    Begin
      DumpOneEquation(y,E)
    End;
  BT:
    Begin
      DumpTerm(y,BTerm_GetTerm(Bp));
    End;
  CO:
    Begin
      WriteToDumpFile('''');
      DumpConst(y,Cp);
      WriteToDumpFile('''')
    End;
  ID:
    Begin
      WriteToDumpFile(BoolToShortString(Ip^.TI_ASSI));
      WriteToDumpFile(' ');
      DumpIdentifier(y,Ip)
    End;
  FU:
    Begin
      DumpTerm(y,Tp);
      If (FRed(Fp) <> Nil) Then
      Begin
        WriteToDumpFile('==');
        DumpTerm(y,FRed(Fp))
      End
    End;
  VA:
    Begin
      DumpVarName(y,Vp);
      If (VRed(Vp) <> Nil) Then
      Begin
        WriteToDumpFile('==');
        DumpTerm(y,VRed(Vp))
      End
    End;
  DE:
    Begin
      WriteToDumpFile(BoolToShortString(Dict_IsGlobal(Dp)));
      WriteToDumpFile(' "');
      Str_Dump(Dict_GetStr(Dp));
      WriteToDumpFile('"')
    End;
  HE:
    Begin
      WriteToDumpFile(LongIntToShortString(Header_GetClock(Hp)))
    End;
  ST:
    Begin
      Str_Dump(s);
      WriteToDumpFile(' (' + LongIntToShortString(Str_Length(s)) + ')')
    End;
  SD:
    Begin
      WriteToDumpFile(' (' + IntToShortString(Sda^.SD_DATA.Len) + ')')
    End;
  RE:
    Begin
      WriteToDumpFile(PtrToName(Rp^.RE_ADDR^) + ' ');
      WriteToDumpFile(BoolToShortString(Rp^.RE_DONE))
    End;
  OP:
    Begin
      WriteToDumpFile('"' + Op_GetOperator(Opp) + '"');
      WriteToDumpFile(' ');
      WriteToDumpFile('"' + Op_GetFunction(Opp) + '"')
    End;
  TK:
    Token_Dump(Kp)
  End
End;

{ display a Prolog clock header }
Procedure DumpHeader( H : HeadPtr );
Var 
  y : TSyntax;
  U : RestPtr;
  B : BTermPtr;
  GoalType : TGoalType; 
  Access : IdPtr;
  Arity : TArity;
Begin
  y := PrologIIv1; { syntax for header output }
  WritelnToDumpFile('*** Header level ' + LongIntToShortString(Header_GetClock(H)) + ' ***');

  B := Header_GetGoalsToClear(H);
  If B <> Nil Then
    BTerm_GetMetadata(B,GoalType,Access,Arity);

  { goal type }
  If B <> Nil Then
  Begin
    WriteToDumpFile('  Type: ');
    Case GoalType Of
      GOAL_BLOCK: WritelnToDumpFile('GOAL_BLOCK');
      GOAL_FIND: WritelnToDumpFile('GOAL_FIND');
      GOAL_CUT: WritelnToDumpFile('GOAL_CUT');
      GOAL_SYS: WritelnToDumpFile('GOAL_SYS');
      GOAL_STD: WritelnToDumpFile('GOAL_STD')
    End
  End;

  { access / arity }
  If B <> Nil Then
  Begin
    If Access <> Nil Then
    Begin
      WriteToDumpFile('  Access: ');
      WriteToDumpFile(IdentifierGetShortString(Access));
      WriteToDumpFile('/');
      WriteToDumpFile(PosIntToShortString(Arity));
      WriteLineBreakToDumpFile
    End
  End;

  { terms to clear }
  WriteToDumpFile('  Terms: -> ');
  While B <> Nil Do
  Begin
    DumpTerm(y,BTerm_GetTerm(B));
    If BTerm_GetHeader(B) <> Nil Then
      WriteToDumpFile('[' + LongIntToShortString(Header_GetClock(BTerm_GetHeader(B))) + ']');
    WriteToDumpFile(' -> ');
    B := BTerms_GetNext(B)
  End;
  WritelnToDumpFile(' Nil');

  { cleared? }
  WriteToDumpFile('  Cleared: ');
  If Header_IsCleared(H) Then
    WritelnToDumpFile('Yes')
  Else
    WritelnToDumpFile('No');

  { branch }
  WritelnToDumpFile('  Branch: ' + LongIntToShortString(Header_GetBranchNumber(H)));
  { over? }
  WriteToDumpFile('  Over: ');
  If Header_IsDone(H) Then
    WritelnToDumpFile('Yes')
  Else
    WritelnToDumpFile('No');

  { more? }
  WriteToDumpFile('  More: ');
  If Header_GetMore(H) Then
    WritelnToDumpFile('Yes')
  Else
    WritelnToDumpFile('No');
  
  { rule }
  WriteToDumpFile('  Rule: ');
  If (Header_GetRule(H) = Nil) Then
    WritelnToDumpFile('Nil')
  Else
  Begin
    DumpOneRule(y,Header_GetRule(H));
    WriteLineBreakToDumpFile
  End;

  { cut target, if any }
  If Header_GetCutTarget(H) <> Nil Then
    WritelnToDumpFile('  Back cut: ' + LongIntToShortString(Header_GetClock(Header_GetCutTarget(H))));

  { block target, if any }
  If Header_GetBlockScope(H) <> Nil Then
    WritelnToDumpFile('  Back block: ' + LongIntToShortString(Header_GetClock(Header_GetBlockScope(H))));

  { ZTerm, if any }
  If Header_GetSideCarTerm(H) <> Nil Then
  Begin
    WriteToDumpFile('  CHOIV: ');
    DumpTerm(y,Header_GetSideCarTerm(H));
    WriteLineBreakToDumpFile
  End;

  { restore }
  If H^.HH_REST <> Nil Then
  Begin
    WriteToDumpFile('  Rest_Restore: ');
    U := H^.HH_REST;
    While U <> Nil Do
      With U^ Do
      Begin
        WriteToDumpFile('.');
        U := RE_NEXT
      End;
    WriteLineBreakToDumpFile
  End
End;

{ display the call stack until header H, included }
Procedure Backtrace( H : HeadPtr );
Begin
  WritelnToDumpFile('HEADER DUMP:');
  While H <> Nil Do
  Begin
    DumpHeader(H);
    H := Headers_GetNext(H)
  End
End;

{ display the call stack }
Procedure DumpBacktrace( Q : QueryPtr );
Begin
  Backtrace(Query_GetHead(Q))
End;

{ display variable identifiers in a dictionary }
Procedure DumpDictVar( y : TSyntax; D : DictPtr );
Var 
  e : DictPtr;
  V : VarPtr;
  TV : TermPtr Absolute V;
Begin
  WritelnToDumpFile('Variables:');
  e := D;
  while e <> Nil Do
  Begin
    TV := Dict_GetTerm(e);
    WriteToDumpFile(' ');
    If Dict_IsGlobal(e) Then
      WriteToDumpFile('*')
    Else
      WriteToDumpFile(' ');
    DumpVarName(y,V);
    If VRed(V) <> Nil Then
    Begin
      WriteToDumpFile(' = ');
      DumpTerm(y,VRed(V))
    End;
    If WatchIneq(V) <> Nil Then
    Begin
      WriteToDumpFile(', ');
      DumpOneEquation(y,WatchIneq(V))
    End;
    WriteLineBreakToDumpFile;
    e := Dict_GetNext(e)
  End
End;

{ display the dictionary of constants }
Procedure DumpDictConst( e : DictPtr; title : TString; 
    Ident: Boolean );
Var
  s : StrPtr;
Begin
  WritelnToDumpFile(title);
  while (e<>Nil) Do
  Begin
    If Dict_IsGlobal(e) Then
      WriteToDumpFile('*')
    Else
      WriteToDumpFile(' ');
    If Ident Then
      s := GetIdentAsStr(IdPtr(Dict_GetTerm(e)),True)
    Else
      s := Dict_GetStr(e);
    Str_Dump(s);
    WriteLineBreakToDumpFile;
    e := Dict_GetNext(e)
  End
End;

{ core dump a Prolog program }
Procedure CoreDumpProg( P : ProgPtr; Message : TString; 
    WithBackTrace : Boolean );
Var
  reg : Boolean;
Begin
  { suspend objects registration to avoid messing up the core dump with 
    objects created during the trace }
  reg := GetRegistrationState;
  SetRegistration(False);
  If Not OngoingCoreDump Then
  Begin
    OngoingCoreDump := True;
    WritelnToDumpFile('Begin Core Dump: "' + Message + '"');
    PrintMemoryStats;
    DumpGCRoots;
    DumpRegisteredObject;
    If P <> Nil Then
    Begin
      DumpDictConst(P^.PP_DCON,'Constants:',False);
      DumpDictConst(P^.PP_DIDE,'Identifiers:',True);
      If WithBackTrace Then
        Backtrace(Query_GetHead(GetCurrentQuery(P)))
    End;
    WritelnToDumpFile('End Code Dump: "' + Message + '"');
    OngoingCoreDump := False
  End;
  SetRegistration(reg)
End;

{ core dump the current Prolog program }
Procedure CoreDump( Message : TString; WithBackTrace : Boolean );
Begin
  CheckCondition(CurrentProgram <> Nil,'CoreDump: program not set');
  CoreDumpProg(CurrentProgram,Message,WithBackTrace);
End;

{ initialize the unit }
Begin
  TargetEncoding := GetSystemEncoding;
  OngoingCoreDump := False;
  CurrentProgram := Nil
End.