{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   Fichier     : Main.pas                                                    }
{   Auteur      : Christophe BISIERE                                         }
{   Date        : 07/01/88                                                   }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                P R O G R A M M E   P R I N C I P A L                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{ P. InitPointeurs;                                                          }
{ P. EnleverQuestion;                                                        }
{ P. Temporise;                                                              }
{                                                                            }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Directive de Compilation : Verifier les Indices de Tableaux.      }
{$V-} { Directive de Compilation : Ne pas verifier la taille des Chaines. }

{ Inclusion des Modules : }

{$I Input.pas   }  { Module : Lecture du Flot d'Entrée           }
{$I Dico.pas    }  { Module : Gestion des Dictionnaires          }
{$I Memoire.pas }  { Module : Gestion de la mémoire principale   }
{$I Restore.pas }  { Module : Gestion de la pile de restauration }
{$I Reduc.pas   }  { Module : Algorithme de réduction            }
{$I Coder.pas   }  { Module : Codage des Objets                  }
{$I Decoder.pas }  { Module : Decodage des Objets                }
{$I Horloge.pas }  { Module : Horloge Prolog                     }

{$I Init.pas    }  { Module : Initialisations                    }


{----------------------------------------------------------------------------}
{ Procedure InitPointeurs;                                                   }
{----------------------------------------------------------------------------}
{ Initialisation des pointeurs qui permettent de traiter plusieurs questions }
{ pour un meme programme codé.                                               }
{----------------------------------------------------------------------------}

Procedure InitPointeurs;
Begin
  TopVar     := NbVar   + 1;  { Debut de la recherche dans DicoVar         }
  FirstVar   := NbVar   + 1;  { Premiere variable définie dans la Question }
  FirstConst := NbConst + 1;  { Première constante locale à la question    }
  SommetRegles    := PtrLeft; { Pointeur vers le sommet des règles codées  }
  SommetProgramme := PtrLeft; { Actuel sommet du programme complet         }
End;


{----------------------------------------------------------------------------}
{ Procedure EnleverQuestion;                                                 }
{----------------------------------------------------------------------------}
{ Enleve la question qui est au sommet de la pile (si il y en a une).        }
{----------------------------------------------------------------------------}

Procedure EnleverQuestion;
Begin                          { On depile :                               }
  PtrLeft := SommetRegles;     {   - le code de la question                }
  NbVar   := FirstVar   - 1;   {   - les variables de cette question       }
  NbConst := FirstConst - 1;   {   - les constantes définies dans celle-ci }
End;


{----------------------------------------------------------------------------}
{ Procedure Temporise;                                                       }
{----------------------------------------------------------------------------}
{ Prévient l'utilisateur de la fin du travail de l'interpreteur et attend un }
{ caractère au clavier pour passer à la question suivante.                   }
{----------------------------------------------------------------------------}

Procedure Temporise;
Var Car : Char;
Begin
  Writeln;
  Write('Ok ');
  Read(kbd,Car);
  Writeln;
End;


{----------------------------------------------------------------------------}
{ Programme Principal;                                                       }
{----------------------------------------------------------------------------}
{ Code le programme utilisateur et lance l'interpreteur Prolog sur chaque    }
{ question posée.                                                            }
{----------------------------------------------------------------------------}

Begin
  Initialisation;                { Initialise le Programme                   }
  CompilerProgramme;             { Code le Programme Prolog                  }
  InitPointeurs;                 { Initialise l'environnement de la question }
  RestituerProgramme;            { Le restitue à l'écran                     }
  If Not Error Then
    Repeat
      EnleverQuestion;            { Enleve la question précedente            }
      CompilerQuestion;           { Code la nouvelle question                }
      SommetProgramme := PtrLeft; { Note ou est le sommet du Pgm complet     }
      Writeln;
      RestituerQuestion;
      Writeln;
      If Not Error Then Horloge;
      Temporise
    Until Calu = '.'
End.




