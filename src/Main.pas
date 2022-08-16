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

{$R+} { Directive de compilation : Vérifier les indices des tableaux.     }
{$V-} { Directive de compilation : Ne pas vérifier la taille des chaînes. }

{ Inclusion des Modules : }

{$I Input.pas   }  { Module : Lecture du flot d'entrée           }
{$I Dico.pas    }  { Module : Gestion des dictionnaires          }
{$I Memoire.pas }  { Module : Gestion de la mémoire principale   }
{$I Restore.pas }  { Module : Gestion de la pile de restauration }
{$I Reduc.pas   }  { Module : Algorithme de réduction            }
{$I Coder.pas   }  { Module : Codage des objets                  }
{$I Decoder.pas }  { Module : Décodage des objets                }
{$I Horloge.pas }  { Module : Horloge Prolog                     }

{$I Init.pas    }  { Module : Initialisations                    }


{----------------------------------------------------------------------------}
{ Procedure InitPointeurs;                                                   }
{----------------------------------------------------------------------------}
{ Initialisation des pointeurs qui permettent de traiter plusieurs questions }
{ pour un même programme codé.                                               }
{----------------------------------------------------------------------------}

Procedure InitPointeurs;
Begin
  TopVar     := NbVar   + 1;  { Début de la recherche dans DicoVar         }
  FirstVar   := NbVar   + 1;  { Première variable définie dans la question }
  FirstConst := NbConst + 1;  { Première constante locale à la question    }
  SommetRegles    := PtrLeft; { Pointeur vers le sommet des règles codées  }
  SommetProgramme := PtrLeft; { Actuel sommet du programme complet         }
End;


{----------------------------------------------------------------------------}
{ Procedure EnleverQuestion;                                                 }
{----------------------------------------------------------------------------}
{ Enlève la question qui est au sommet de la pile (si il y en a une).        }
{----------------------------------------------------------------------------}

Procedure EnleverQuestion;
Begin                          { On dépile :                               }
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
Begin
  Writeln;
  Write('Ok ');
  Readln;
  Writeln;
End;


{----------------------------------------------------------------------------}
{ Procedure Main;                                                       }
{----------------------------------------------------------------------------}
{ Code le programme utilisateur et lance l'interpreteur Prolog sur chaque    }
{ question posée.                                                            }
{----------------------------------------------------------------------------}

Procedure Main;
var Butee : Integer;  { Butee droite fin des travaux }
    P : Integer;      { Adresse du programme }
    Q : Integer;      { Adresse de la question }
Begin
  Initialisation;                     { Initialise le programme              }
  P := CompilerProgramme; { Code le programme Prolog             }
  InitPointeurs;                      { Initialise l'env. de la question     }
  RestituerProgramme(P);  { Le restitue à l'écran                }
  If Not Error Then
    Repeat
      EnleverQuestion;            { Enlève la question précédente            }
      Butee := PtrRight;
      Q := CompilerQuestion; { Code la nouvelle question          }
      Entete(Q,P,0,0);
      SommetProgramme := PtrLeft; { Note où est le sommet du Pgm complet     }
      Writeln;
      RestituerQuestion(Q);
      Writeln;
      If Not Error Then Horloge(P, Butee);
      Temporise
    Until Calu = '.'
End;

{----------------------------------------------------------------------------}
{ Programme Principal;                                                       }
{----------------------------------------------------------------------------}

Begin
  Main
End.




