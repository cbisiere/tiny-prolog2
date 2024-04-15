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
  PObjRest,
  PObjOp,
  PObjStr,
  PObjTok,
  PObjDict,
  PObjEq,
  PObjTerm,
  PObjDef,
  PObjBter,
  PObjHead,
  PObjProg,
  PObjRule,
  Unparse;

Procedure SetCurrentProgram( P : ProgPtr );
Procedure DumpBacktrace;
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

Procedure DumpToken( K : TokenPtr );
Begin
  CWrite('(');
  CWriteInt(K^.TK_LINE);
  CWrite(',');
  CWriteInt(K^.TK_CHAR);
  CWrite(') ');
  CWrite(Token_GetTypeAsString(K));
  If Token_GetStr(K) <> Nil Then
  Begin
    CWrite(': ');
    OutString(Nil,Token_GetStr(K))
  End
End;

Procedure DumpTokens( K : TokenPtr );
Begin
  While K <> Nil Do
  Begin
    DumpToken(K);
    CWriteLn;
    K := Token_GetNext(K)
  End;
  CWriteLn
End;

{ Write extra data in a Prolog object }
Procedure WriteExtraData( p : TObjectPtr );
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
  Rp : RestorePtr Absolute p;
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
      CWriteInt(PRp^.PP_LEVL);
      CWrite(' ');
      y := GetSyntax(PRp);
      If y = PrologII Then
        CWrite('PrologII')
      Else If y = PrologIIc Then
        CWrite('PrologII w/ constraints')
      Else If y = PrologIIp Then
        CWrite('PrologII+')
      Else If y = Edinburgh Then
        CWrite('Edinburgh')
    End;
  RU:
    Begin
      CWrite('Head: ');
      OutTerm(Nil,y,BTerm_GetTerm(Rule_GetHead(Rup)))
    End;
  QU:
    Begin
    End;
  SY:
    Begin
    End;
  EQ:
    Begin
      OutOneEquation(Nil,y,E)
    End;
  BT:
    Begin
      OutTerm(Nil,y,BTerm_GetTerm(Bp));
    End;
  CO:
    Begin
      CWrite('''');
      OutConst(Nil,Cp);
      CWrite('''')
    End;
  ID:
    Begin
      CWriteBool(Ip^.TV_ASSI);
      CWrite(' ');
      OutIdentifier(Nil,Ip)
    End;
  FU:
    Begin
      OutTerm(Nil,y,Tp);
      If (FRed(Fp) <> Nil) Then
      Begin
        CWrite('==');
        OutTerm(Nil,y,FRed(Fp))
      End
    End;
  VA:
    Begin
      OutVarName(Nil,Vp);
      If (VRed(Vp) <> Nil) Then
      Begin
        CWrite('==');
        OutTerm(Nil,y,VRed(Vp))
      End
    End;
  DE:
    Begin
      CWriteBool(Dict_IsGlobal(Dp));
      CWrite(' "');
      OutString(Nil,Dict_GetStr(Dp));
      CWrite('"')
    End;
  HE:
    Begin
      CWrite(LongIntToStr(Hp^.HH_CLOC));
      CWrite(' Sys:');
      CWriteBool(Hp^.HH_ISYS);
      CWrite(' Cut:');
      CWriteBool(Hp^.HH_ICUT)
    End;
  ST:
    Begin
      OutString(Nil,s);
      CWrite(' (' + LongIntToStr(Str_Length(s)) + ')')
    End;
  SD:
    Begin
      CWrite('"' + Sda^.SD_DATA + '"');
      CWrite(' (' + IntToStr(Length(Sda^.SD_DATA)) + ')')
    End;
  RE:
    Begin
      CWrite(PtrToName(Rp^.RE_ADDR^) + ' ');
      CWriteBool(Rp^.RE_DONE)
    End;
  OP:
    Begin
      CWrite('"' + Op_GetOperator(Opp) + '"');
      CWrite(' ');
      CWrite('"' + Op_GetFunction(Opp) + '"')
    End;
  TK:
    DumpToken(Kp)
  End
End;

{ display a Prolog clock header }
Procedure DumpHeader( H : HeadPtr );
Var 
  y : TSyntax;
  R : RulePtr;
  isSys : Boolean;
  isCut : Boolean;
  U : RestorePtr;
  B : BTermPtr;
Begin
  y := PrologIIc; { syntax for header output }
  CWrite('*** Header level ' + LongIntToStr(H^.HH_CLOC) + ' ***');
  CWriteLn;
  If H^.HH_BACK <> Nil Then
  Begin
    CWrite(' Back: ' + LongIntToStr(H^.HH_BACK^.HH_CLOC));
    CWriteLn
  End;
  CWrite('  Terms: ');
  B := H^.HH_FBCL;
  While B <> Nil Do
  Begin
    OutTerm(Nil,y,BTerm_GetTerm(B));
    If B^.BT_HEAD <> Nil Then
      CWrite('[' + LongIntToStr(B^.BT_HEAD^.HH_CLOC) + ']');
    CWrite(' ');
    B := BTerms_GetNext(B)
  End;
  CWriteLn;
  CWrite('  Rule: ');
  Header_GetRule(H,R,isSys,isCut);
  if (isSys) Then
  Begin
    CWrite('SYS');
    CWriteLn
  End
  Else if (isCut) Then
  Begin
    CWrite('!');
    CWriteLn
  End
  Else If (R = Nil) Then
  Begin
    CWrite('Nil');
    CWriteLn
  End
  Else
  Begin
    CWrite('Rule has cut? ' + BoolToStr(R^.RU_ACUT));
    CWriteLn;
    OutOneRule(Nil,R);
  End;
  CWrite('  Restore: ');
  U := H^.HH_REST;
  While U<>Nil Do
    With U^ Do
    Begin
      CWrite('.');
      U := RE_NEXT
    End;
  CWriteLn
End;

{ display the call stack until header H, included }
Procedure Backtrace( H : HeadPtr );
Begin
  While H <> Nil Do
  Begin
    DumpHeader(H);
    H := H^.HH_NEXT
  End
End;

{ display the call stack }
Procedure DumpBacktrace;
Begin
  If CurrentProgram<>Nil Then
    Backtrace(CurrentProgram^.PP_HEAD)
End;

{ display variable identifiers in a dictionary }
Procedure DumpDictVar( y : TSyntax; D : DictPtr );
Var 
  e : DictPtr;
  V : VarPtr;
  TV : TermPtr Absolute V;
Begin
  CWrite('Variables:');
  CWriteLn;
  e := D;
  while e <> Nil Do
  Begin
    TV := Dict_GetTerm(e);
    CWrite(' ');
    If Dict_IsGlobal(e) Then
      CWrite('*')
    Else
      CWrite(' ');
    OutVarName(Nil,V);
    If VRed(V) <> Nil Then
    Begin
      CWrite(' = ');
      OutTerm(Nil,y,VRed(V))
    End;
    If WatchIneq(V) <> Nil Then
    Begin
      CWrite(', ');
      OutOneEquation(Nil,y,WatchIneq(V))
    End;
    CWriteLn;
    e := Dict_GetNext(e)
  End
End;

{ display the dictionary of constants }
Procedure DumpDictConst( e : DictPtr; title : TString );
Begin
  CWrite(title);
  CWriteLn;
  while (e<>Nil) Do
  Begin
    If Dict_IsGlobal(e) Then
      CWrite('*')
    Else
      CWrite(' ');
    OutStringCR(Nil,Dict_GetStr(e));
    e := Dict_GetNext(e)
  End
End;

{ core dump a Prolog program }
Procedure CoreDumpProg( P : ProgPtr; Message : TString; Trace : Boolean );
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
    CWrite('Begin Core Dump: "' + Message + '"');
    CWriteLn;
    PrintMemoryStats;
    DumpGCRoots;
    DumpRegisteredObject;
    If P <> Nil Then
    Begin
      DumpDictConst(P^.PP_DCON,'Constants:');
      DumpDictConst(P^.PP_DIDE,'Identifiers:');
      If Trace Then
        Backtrace(P^.PP_HEAD)
    End;
    CWrite('End Code Dump: "' + Message + '"');
    CWriteLn;
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