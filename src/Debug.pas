{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : init.pas                                                   }
{   Author      : Christophe Bisière                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                              D E B U G G I N G                             }
{                                                                            }
{----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ Affiche la valeur Val.                                                     }
{----------------------------------------------------------------------------}

Function Disp( Val : Integer ) : AnyStr;
Var S : AnyStr;
Begin
  Case Val Of
  Undefined:
    S := '?';
  TERM_F:
    S := 'F';
  TERM_C:
    S := 'C';
  TERM_V:
    S := 'V';
  REL_EQUA:
    S := '=';
  REL_INEQ:
    S := '<>';
  NULL:
    S := '.';
  NO:
    S := '[F]';
  YES:
    S := '[T]';
  SYS_CALL:
    S := 'SYS';
  SYS_CUT:
    S := '!';
  RTYPE_AUTO:
    S := 'AUTO';
  RTYPE_USER:
    S := 'USER'
  Else
    Str(Val, S)
  End;
  Disp := S
End;

{----------------------------------------------------------------------------}
{ Affiche l'entête de l'horloge.                                             }
{----------------------------------------------------------------------------}

Procedure DumpHeader( H : Integer );
Var R : Integer;
Begin
  Writeln('Header at : ', H);
  Write('  Terms: ');
  UnparseTerms(Memory[H+HH_FBCL],True);
  Writeln;
  Write('  Rule: ');
  R := Memory[H+HH_RULE];
  If (R = NULL) Or (R = SYS_CALL) Or (R = SYS_CUT) Then
    Writeln(Disp(R))
  Else
    UnparseOneRule(R);
  Write('  Restore: ');
  Writeln(Disp(Memory[H+HH_REST]));
  Write('  PrevPtrLeft: ');
  Writeln(Disp(Memory[H+HH_STAC]));
  Write('  BackHeader: ');
  Writeln(Disp(Memory[H+HH_PREV]));
  Write('  Cut: ');
  Writeln(Disp(Memory[H+HH_ACUT]))
End;

{----------------------------------------------------------------------------}
{ Affiche la pile d'appels de l'horloge jusqu'au header H compris.           }
{----------------------------------------------------------------------------}

Procedure Backtrace( H : Integer );
Begin
  Writeln('BACKTRACE AT ', H, ' ClockTime = ', ClockTime);
  While H <> NULL Do
  Begin
    DumpHeader(H);
    H := Memory[H+HH_PREV]
  End;
  Writeln('*EoB*');
End;

{----------------------------------------------------------------------------}
{ Affiche la pile d'appels de l'horloge.                                     }
{----------------------------------------------------------------------------}

Procedure DumpBacktrace;
Begin
  Backtrace(PtrLeft-HH_length+1)
End;

{----------------------------------------------------------------------------}
{ Affiche le contenu de la mémoire principale.                               }
{----------------------------------------------------------------------------}

Procedure DumpMem;
Const
  NbPerLine = 10;
  Head = 5;
  Val = 6;
Var K : Integer;
Begin
  Writeln('Left stack:');
  Write(0:Head, ':', ' ':Val);
  For K := 1 To PtrLeft Do
  Begin
    If K Mod NbPerLine = 0 Then
    Begin
      writeln;
      write((K Div NbPerLine):Head,':');
    End;
    Write(Disp(Memory[K]):Val)
  End;
  Writeln;
  Writeln('Right stack:');
  K := HiMemAddr;
  While K > PtrRight Do
  Begin
    Write(Disp(Memory[K+ZZ_TYPE]):Val,' ');
    WriteTerm(Memory[K+ZZ_LTER]);
    Write(' ');
    WriteTerm(Memory[K+ZZ_RTER]);
    K := K - ZZ_length;
  End;
  Writeln
End;

{----------------------------------------------------------------------------}
{ Affiche le dictionnaire des variables.                                     }
{----------------------------------------------------------------------------}

Procedure DumpDictVar;
Var K,V : Integer;
Begin
  If NbVar > 0 Then
  Begin
    Writeln('Variables:');
    For K := 1 To NbVar Do
    Begin
      V := DictVar[K].Ptr;
      Write(K:3,': ',V:5,': ');
      WriteVarName(V);
      If Memory[V+TV_IRED] = YES Then
      Begin
        Write(' = ');
        WriteTerm(Memory[V+TV_TRED])
      End;
      Writeln
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
    Writeln('Constants:');
    For K := 1 To NbConst Do
      Writeln(K:4,': ',DictConst[K])
  End
End;

{----------------------------------------------------------------------------}
{ Affiche les variables globales d'état.                                     }
{----------------------------------------------------------------------------}

Procedure DumpState;
Begin
  Writeln('PtrLeft = ',Disp(PtrLeft));
  Writeln('NbVar = ',NbVar);
  Writeln('NbConst = ',NbConst);
  Writeln('PtrRight = ',Disp(PtrRight))
End;

{----------------------------------------------------------------------------}
{ Affiche l'état complet de la machine Prolog.                               }
{----------------------------------------------------------------------------}

Procedure CoreDump;(* ( Message : AnyStr; Trace : Boolean ); *)
Begin
  Writeln('Begin Core Dump: "',Message,'"');
  DumpState;
  DumpMem;
  DumpDictConst;
  DumpDictVar;
  If Trace Then
    DumpBacktrace;
  Writeln('End Code Dump: "',Message,'"')
End;
