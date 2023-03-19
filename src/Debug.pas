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
Var s : AnyStr;
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
  EQ:
    Begin
    End;
  BT:
    Begin
    End;
  CO:
    Begin
      s := GetConstAsString(ConstPtr(p), True);
    End;
  FU:
    Begin
      If (FuncPtr(p)^.TF_TRED <> Nil) Then
        s := '***'
    End;
  VA:
    Begin
      s := GetVarNameAsString(VarPtr(p));
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
  WriteLn('Header at : ');
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
      U := RE_NEXT
    End;
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
  End;
  WriteLn('*EoB*');
End;

{----------------------------------------------------------------------------}
{ Affiche la pile d'appels de l'horloge.                                     }
{----------------------------------------------------------------------------}

Procedure DumpBacktrace;
Begin
End;

{----------------------------------------------------------------------------}
{ Affiche le dictionnaire des variables.                                     }
{----------------------------------------------------------------------------}

Procedure DumpDictVar;
Var 
  K : Integer;
  V : VarPtr;
Begin
  If NbVar > 0 Then
  Begin
    WriteLn('Variables:');
    For K := 1 To NbVar Do
    Begin
      V := DictVar[K].Ptr;
      WriteVarName(V);
      If V^.TV_TRED <> Nil Then
      Begin
        Write(' = ');
        WriteTerm(V^.TV_TRED)
      End;
      WriteLn
    End
  End
End;

{----------------------------------------------------------------------------}
{ Affiche le dictionnaire des constantes.                                    }
{----------------------------------------------------------------------------}

Procedure DumpDictConst;
Var K : Integer;
Begin
  If NbConst > 0 Then
  Begin
    WriteLn('Constants:');
    For K := 1 To NbConst Do
      WriteLn(K:4,': ',DictConst[K])
  End
End;

{----------------------------------------------------------------------------}
{ Affiche les variables globales d'état.                                     }
{----------------------------------------------------------------------------}

Procedure DumpState;
Begin
  WriteLn('NbVar = ',NbVar);
  WriteLn('NbConst = ',NbConst);
End;

{----------------------------------------------------------------------------}
{ Affiche l'état complet de la machine Prolog.                               }
{----------------------------------------------------------------------------}

Procedure CoreDump( Message : AnyStr; Trace : Boolean );
Begin
  WriteLn('Begin Core Dump: "',Message,'"');
  DumpState;
  DumpDictConst;
  DumpDictVar;
  If Trace Then
    DumpBacktrace;
  WriteLn('End Code Dump: "',Message,'"')
End;
