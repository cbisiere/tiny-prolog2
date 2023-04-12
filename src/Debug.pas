{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : init.pas                                                   }
{   Author      : Christophe Bisière                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                              D E B U G G I N G                             }
{                                                                            }
{----------------------------------------------------------------------------}


{----------------------------------------------------------------------------}
{ Write extra data of a Prolog object.                                       }
{----------------------------------------------------------------------------}

Procedure WriteExtraData; (*( p : TPObjPtr );*)
Var 
  Cp : ConstPtr Absolute p;
  Vp : VarPtr Absolute p;
  Fp : FuncPtr Absolute p;
  Dv : DictVarPtr Absolute p;
  Dc : DictConstPtr Absolute p;
  E : EqPtr Absolute p;
  Sda : StrDataPtr Absolute p;
  S : StrPtr Absolute p;
Begin
  Case PObjectType(p) Of
  PR:
    Begin
    End;
  RU:
    Begin
    End;
  QU:
    Begin
    End;
  SY:
    Begin
    End;
  EQ:
    Begin
      OutOneEquation(E)
    End;
  BT:
    Begin
    End;
  CO:
    Begin
      OutConst(Cp)
    End;
  FU:
    Begin
      If (FRed(Fp) <> Nil) Then
        Write('***');
    End;
  VA:
    Begin
      OutVarName(Vp);
      If (VRed(Vp) <> Nil) Then
      Begin
        Write(' = ...');
      End
    End;
  CV:
    Begin
      StrWrite(Dc^.DC_CVAL)
    End;
  VV:
    Begin
      OutVarName(Dv^.DV_PVAR)
    End;
  HE:
    Begin
    End;
  ST:
    Begin
      StrWrite(s);
      Write(' (',LongIntToStr(s^.ST_NDAT),',',LongIntToStr(StrLength(s)),')')
    End;
  SD:
    Begin
      Write('"',Sda^.SD_DATA,'"');
      Write(' (',Length(Sda^.SD_DATA),')')
    End;
  RE:
    Begin
    End
  End
End;

{----------------------------------------------------------------------------}
{ Affiche l'entête de l'horloge.                                             }
{----------------------------------------------------------------------------}

Procedure DumpHeader( H : HeadPtr );
Var 
  R : RulePtr;
  isSys : Boolean;
  isCut : Boolean;
  U : RestorePtr;
Begin
  WriteLn('Header level ',LongIntToStr(H^.HH_CLOC));
  Write('  Terms: ');
  OutTerms(H^.HH_FBCL,True);
  WriteLn;
  Write('  Rule: ');
  GetHeaderRule(H,R,isSys,isCut);
  If (R = Nil) Then
    WriteLn('Nil')
  Else if (isSys) Then
    WriteLn('SYS')
  Else if (isCut) Then
    WriteLn('!')
  Else
    OutOneRule(R);
  Write('  Restore: ');
  U := H^.HH_REST;
  While U<>Nil Do
    With U^ Do
    Begin
      Write('.');
      U := RE_NEXT
    End;
  Writeln;
  Write('  Cut: ');
  WriteLn(H^.HH_ACUT)
End;

{----------------------------------------------------------------------------}
{ Affiche la pile d'appels de l'horloge jusqu'au header H compris.           }
{----------------------------------------------------------------------------}

Procedure Backtrace( H : HeadPtr );
Begin
  While H <> Nil Do
  Begin
    DumpHeader(H);
    H := H^.HH_NEXT
  End
End;

{----------------------------------------------------------------------------}
{ Affiche la pile d'appels de l'horloge.                                     }
{----------------------------------------------------------------------------}

Procedure DumpBacktrace;
Begin
  If CurrentProgram<>Nil Then
    Backtrace(CurrentProgram^.PP_HEAD)
End;

{----------------------------------------------------------------------------}
{ display variable identifiers from start to stop (excluding stop)           }
{----------------------------------------------------------------------------}

Procedure DumpDictVar( start,stop : DictVarPtr );
Var 
  e : DictVarPtr;
  V : VarPtr;
Begin
  WriteLn('Variables:');
  e := start;
  while (e<>Nil) And (e<>stop) Do
  Begin
    V := e^.DV_PVAR;
    Write('  ');
    OutVarName(V);
    If VRed(V) <> Nil Then
    Begin
      Write(' = ');
      OutTerm(VRed(V))
    End;
    If VWatchIneq(V) <> Nil Then
    Begin
      Write(', ');
      OutOneEquation(VWatchIneq(V))
    End;    
    WriteLn;
    e := e^.DV_NEXT
  End
End;

{----------------------------------------------------------------------------}
{ Affiche le dictionnaire des constantes.                                    }
{----------------------------------------------------------------------------}

Procedure DumpDictConst( P : ProgPtr );
Var e : DictConstPtr;
Begin
  WriteLn('Constants:');
  e := P^.PP_DCON;
  while (e<>Nil) Do
  Begin
    StrWriteln(e^.DC_CVAL);
    e := e^.DC_NEXT
  End
End;

{----------------------------------------------------------------------------}
{ core dump a Prolog program                                                 }
{----------------------------------------------------------------------------}

Procedure CoreDump; (* ( P : ProgPtr; Message : AnyStr; Trace : Boolean ); *)
Begin
  WriteLn('Begin Core Dump: "',Message,'"');
  DumpDictConst(P);
  DumpDictVar(P^.PP_DVAR,Nil);
  If Trace Then
    Backtrace(P^.PP_HEAD);
  WriteLn('End Code Dump: "',Message,'"')
End;
