{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Main.pas                                                   }
{   Author      : Christophe Bisière                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                           M A I N  P R O G R A M                           }
{                                                                            }
{----------------------------------------------------------------------------}

{$C-} { TP3: Ctrl-C during I/O does not interrupt program execution }
{$U-} { TP3: Ctrl-C does not interrupt program execution }

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{$I TP3.pas     }  { TP3.pas: Turbo Pascal 3; FPC.pas: Free Pascal Compiler }

{$I Memory.pas  }  { Module : Gestion de la mémoire principale   }
{$I Dict.pas    }  { Module : Gestion des dictionnaires          }
{$I Restore.pas }  { Module : Gestion de la pile de restauration }
{$I Keyboard.pas}  { Module : Read from keyboard w/ history      }
{$I Input.pas   }  { Module : Lecture du flot d'entrée           }
{$I Parse.pas   }  { Module : Codage des objets                  }
{$I Unparse.pas }  { Module : Décodage des objets                }
{$I Sys.pas     }  { Module : System calls                       }
{$I Reduc.pas   }  { Module : Algorithme de réduction            }
{$I Clock.pas   }  { Module : L'horloge Prolog                   }

{$I Init.pas    }  { Module : Initialisations                    }
{$I Debug.pas }

{----------------------------------------------------------------------------}
{ Réinitialise la machine Prolog.                                            }
{----------------------------------------------------------------------------}

Function ResetMachine : Integer;
Var P : Integer;
Begin
  Initialisation;
  P := CreateEmptyProgram;
  InstallPredefinedConstants;
  ResetMachine := P
End;

{----------------------------------------------------------------------------}
{ Code le programme utilisateur et lance l'interpréteur Prolog sur chaque    }
{ question posée.                                                            }
{----------------------------------------------------------------------------}

Procedure Main;
var
  P : Integer;      { Adresse du programme }
  Q : Integer;      { Adresse de la question }
  FileName : AnyStr;
Begin
  P := ResetMachine;
  LoadProgram(P,'start.pro',RTYPE_AUTO);
  If ParamCount = 1 Then
  Begin
    FileName := ParamStr(1);
    LoadProgram(P,FileName,RTYPE_USER);
    if Not Error Then
      Writeln('Program "' + FileName + '" loaded')
  End;
  if Not Error Then
    AnswerProgramQueries(P);
  InitHistory;
  Repeat
    Error := False;
    Write('> ');
    LireCommande;
    Q := CompileCommandLineQueries(P);
    If Not Error Then
      AnswerQueries(Q);
    RemoveCommandLineQueries(P);
  Until False
End;

{----------------------------------------------------------------------------}
{ Programme Principal;                                                       }
{----------------------------------------------------------------------------}

Begin
  Main
End.
