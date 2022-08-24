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

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{$I Memory.pas  }  { Module : Gestion de la mémoire principale   }
{$I Dict.pas    }  { Module : Gestion des dictionnaires          }
{$I Restore.pas }  { Module : Gestion de la pile de restauration }
{$I Input.pas   }  { Module : Lecture du flot d'entrée           }
{$I Parse.pas   }  { Module : Codage des objets                  }
{$I Unparse.pas }  { Module : Décodage des objets                }
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
  If ParamCount = 1 Then
  Begin
    FileName := ParamStr(1);
    LoadProgram(P,FileName);
    if Not Error Then
    Begin
      Writeln('Program "' + FileName + '" loaded');
      AnswerProgramQueries(P)
    End
  End;
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
