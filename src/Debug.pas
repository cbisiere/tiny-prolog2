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
  PObjProg,
  Unparse;

Procedure SetCurrentProgram( P : ProgPtr );
Procedure DumpBacktrace;

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
  CWrite(TokenTypeAsString(K));
  If K^.TK_STRI <> Nil Then
  Begin
    CWrite(': ');
    OutString(K^.TK_STRI,False)
  End
End;

Procedure DumpTokens( K : TokenPtr );
Begin
  While K <> Nil Do
  Begin
    DumpToken(K);
    CWriteLn;
    K := NextToken(K)
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
      OutTerm(y,Rup^.RU_FBTR^.BT_TERM,False)
    End;
  QU:
    Begin
    End;
  SY:
    Begin
    End;
  EQ:
    Begin
      OutOneEquation(y,E,False)
    End;
  BT:
    Begin
      OutTerm(y,Bp^.BT_TERM,False);
    End;
  CO:
    Begin
      CWrite('''');
      OutConst(Cp,False);
      CWrite('''')
    End;
  ID:
    Begin
      CWriteBool(Ip^.TV_ASSI);
      CWrite(' ');
      OutIdentifier(Ip,False)
    End;
  FU:
    Begin
      OutTerm(y,Tp,False);
      If (FRed(Fp) <> Nil) Then
      Begin
        CWrite('==');
        OutTerm(y,FRed(Fp),False)
      End
    End;
  VA:
    Begin
      OutVarName(Vp,False);
      If (VRed(Vp) <> Nil) Then
      Begin
        CWrite('==');
        OutTerm(y,VRed(Vp),False)
      End
    End;
  DE:
    Begin
      CWriteBool(Dp^.DE_GLOB);
      CWrite(' "');
      OutString(Dp^.DE_STRI,False);
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
      OutString(s,False);
      CWrite(' (' + LongIntToStr(s^.ST_NDAT) + ',' + LongIntToStr(StrLength(s)) + ')')
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
      CWrite('"' + Opp^.OP_OPER + '"');
      CWrite(' ');
      CWrite('"' + Opp^.OP_FUNC + '"')
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
    OutTerm(y,B^.BT_TERM,False);
    If B^.BT_HEAD <> Nil Then
      CWrite('[' + LongIntToStr(B^.BT_HEAD^.HH_CLOC) + ']');
    CWrite(' ');
    B := NextTerm(B)
  End;
  CWriteLn;
  CWrite('  Rule: ');
  GetHeaderRule(H,R,isSys,isCut);
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
    OutOneRule(R,False);
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

{ display variable identifiers from start to stop (excluding stop) }
Procedure DumpDictVar( y : TSyntax; start,stop : DictPtr );
Var 
  e : DictPtr;
  V : VarPtr;
  TV : TermPtr Absolute V;
Begin
  CWrite('Variables:');
  CWriteLn;
  e := start;
  while (e<>Nil) And (e<>stop) Do
  Begin
    TV := e^.DE_TERM;
    CWrite(' ');
    If DictIsGlobal(e) Then
      CWrite('*')
    Else
      CWrite(' ');
    OutVarName(V,False);
    If VRed(V) <> Nil Then
    Begin
      CWrite(' = ');
      OutTerm(y,VRed(V),False)
    End;
    If WatchIneq(V) <> Nil Then
    Begin
      CWrite(', ');
      OutOneEquation(y,WatchIneq(V),False)
    End;
    CWriteLn;
    e := e^.DE_NEXT
  End
End;

{ display the dictionary of constants }
Procedure DumpDictConst( e : DictPtr; title : TString );
Begin
  CWrite(title);
  CWriteLn;
  while (e<>Nil) Do
  Begin
    If DictIsGlobal(e) Then
      CWrite('*')
    Else
      CWrite(' ');
    OutStringCR(e^.DE_STRI,False);
    e := e^.DE_NEXT
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
      DumpDictVar(GetSyntax(P),P^.PP_DVAR,Nil);
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