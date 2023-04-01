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
{ String representation of a Prolog object.                                  }
{----------------------------------------------------------------------------}

Function ToString; (* ( p : TPObjPtr ) : AnyStr; *)
Var 
  s : AnyStr;
  Cp : ConstPtr Absolute p;
  Vp : VarPtr Absolute p;
  Fp : FuncPtr Absolute p;
  Dv : DictVarPtr Absolute p;
  Dc : DictConstPtr Absolute p;
  E : EqPtr Absolute p;
Begin
  s := '';
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
      WriteOneEquation(E)
    End;
  BT:
    Begin
    End;
  CO:
    Begin
      s := GetConstAsString(Cp, True)
    End;
  FU:
    Begin
      If (Fp^.TF_TRED <> Nil) Then
        s := '***'
    End;
  VA:
    Begin
      s := GetVarNameAsString(Vp);
      If (Vp^.TV_TRED <> Nil) Then
      Begin
        s := s + ' = ...';
      End
    End;
  CV:
    Begin
      s := Dc^.DC_CVAL
    End;
  VV:
    Begin
      s := GetVarNameAsString(Dv^.DV_PVAR)
    End;
  HE:
    Begin
    End;
  RE:
    Begin
    End
  End;
  ToString := s
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
  WriteLn('Header level ',H^.HH_CLOC);
  Write('  Terms: ');
  UnparseTerms(H^.HH_FBCL,True);
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
    UnparseOneRule(R);
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
    WriteVarName(V);
    If V^.TV_TRED <> Nil Then
    Begin
      Write(' = ');
      WriteTerm(V^.TV_TRED)
    End;
    If V^.TV_FWAT <> Nil Then
    Begin
      Write(', ');
      WriteOneEquation(V^.TV_FWAT)
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
    WriteLn(e^.DC_CVAL);
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
