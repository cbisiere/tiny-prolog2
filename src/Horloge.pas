{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   Fichier     : Horloge.pas                                                 }
{   Auteur      : Christophe BISIERE                                         }
{   Date        : 07/01/88                                                   }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                     H O R L O G E   P R O L O G                            }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{  PRE-UNIFICATION                                                           }
{                                                                            }
{     F. Unifiable (A1,A2 : Integer) : Boolean;                              }
{     F. PremiereRegleOk( FirstR, T : Integer ) : Integer;                   }
{     F. NextRegleOk( R,T : Integer ) : Integer;                             }
{                                                                            }
{  RECOPIE DES REGLES                                                        }
{                                                                            }
{     F. Stocke( T : Integer ) : Boolean;                                    }
{     P. Add( T : Integer );                                                 }
{     F. PushRegle( R : Integer ) : Integer;                                 }
{     P. MiseAJour( Ad,Depl : Integer );                                     }
{     P. MiseAJourInequation( E,d : Integer );                               }
{     P. MiseAJourTerme( T,d : Integer );                                    }
{     F. NewRegle( R : Integer ) : Integer;                                  }
{                                                                            }
{  HORLOGE                                                                   }
{                                                                            }
{     P. Horloge( FirstR, FirstT, ButeeDroite : Integer );                   }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Directive de compilation : Vérifier les indices des tableaux.     }
{$V-} { Directive de compilation : Ne pas vérifier la taille des chaînes. }


Var Temps : Integer;                          { Temps de l'horloge PROLOG    }

{----------------------------------------------------------------------------}
{                                                                            }
{                   P R E - U N I F I C A T I O N                            }
{                                                                            }
{----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ Function Unifiable (A1,A2 : Integer) : Boolean;                            }
{----------------------------------------------------------------------------}
{ Unifiable détermine si les deux termes A1 et A2 sont a priori unifiables.  }
{ Ils ne le sont pas lorsque ils ont pour accès une constante différente.    }
{ Ce test permet d'économiser un grand nombre de recopies de règles.         }
{----------------------------------------------------------------------------}

Function Unifiable( A1,A2 : Integer) : Boolean;
Begin
  Unifiable := (A1=A2) Or (A1=0) Or (A2=0) Or
               ( (Typ(A1)=Constante) And (Typ(A2)=Constante) And
                 (Memoire[A1+1]=Memoire[A2+1]) )
End;


{----------------------------------------------------------------------------}
{ Function PremiereRegleOk( FirstR, T : Integer ) : Integer;                 }
{----------------------------------------------------------------------------}
{ Retourne un pointeur vers la première règle qui a une chance de s'unifier  }
{ avec le terme T (ou 0 si aucune règle ne peut s'unifier avec T), FirstR    }
{ étant l'adrresse de la première règle du programme Prolog.                 }
{----------------------------------------------------------------------------}

Function PremiereRegleOk( FirstR, T : Integer ) : Integer;
Var R,A1,A2  : Integer;
    Ok       : Boolean;
Begin
  R  := FirstR; { La première candidate est la première règle du programme. }
  Ok := False;
  A1 := AccesTerme(T);
  While (R<>0) And (Not Ok) Do
  Begin
    A2 := AccesTerme(R+4);
    Ok := Unifiable(A1,A2);
    If Not Ok Then R := RegleSuivante(R);
  End;
  PremiereRegleOk := R
End;


{----------------------------------------------------------------------------}
{ Function NextRegleOk( R,T : Integer ) : Integer;                           }
{----------------------------------------------------------------------------}
{ Sachant que le terme T pouvait s'unifier avec la tête de la règle R, la    }
{ fonction NextRegleOk retourne un pointeur vers la règle qui suit R, si la  }
{ tête de cette dernière peut s'unifier avec le terme T, et 0 dans le cas    }
{ contraire.                                                                 }
{ Cette logique impose qu'un ensemble de règles ayant même accès doit être   }
{ écrit à la suite dans le fichier source.                                   }
{----------------------------------------------------------------------------}

Function NextRegleOk( R,T : Integer ) : Integer;
Var A1,A2  : Integer;
Begin
  R  := RegleSuivante(R);
  A1 := AccesTerme(T);
  If R <> 0 Then
  Begin
    A2 := AccesTerme(R+4);
    If Not Unifiable(A1,A2) Then R := 0
  End;
  NextRegleOk := R
End;


{----------------------------------------------------------------------------}
{                                                                            }
{                 R E C O P I E   D ' U N E   R E G L E                      }
{                                                                            }
{----------------------------------------------------------------------------}

Type Tdico = Array[1..100] Of Integer;  { Dico d'Integers                    }

Var DicoAdr : Tdico;           { Dico des adresses des objets déjà recopiés  }
    PtrDico : Integer;         { Stack Pointer pour ce dictionnaire          }


{----------------------------------------------------------------------------}
{ Function Stocke( T : Integer ) : Boolean;                                  }
{----------------------------------------------------------------------------}
{ Stocke regarde si l'adresse T est déjà dans le dictionnaire DicoAdr. Si    }
{ oui elle retourne True, sinon elle retourne False.                         }
{----------------------------------------------------------------------------}

Function Stocke( T : Integer ) : Boolean;
Var I     : Integer;
    Find  : Boolean;
Begin
  I    := 1;
  Find := False;
  While (Not Find) And (I<=PtrDico) Do
    Begin
      If DicoAdr[I] = T Then Find := True;
      I := I + 1
    End;
  Stocke := Find
End;


{----------------------------------------------------------------------------}
{ Procedure Add( T : Integer );                                              }
{----------------------------------------------------------------------------}
{ Add ajoute au dictionnaire DicoAdr l'adresse T.                            }
{----------------------------------------------------------------------------}

Procedure Add( T : Integer );
Begin
  PtrDico := PtrDico+1;
  DicoAdr[PtrDico] := T
End;


{----------------------------------------------------------------------------}
{ Function PushRegle( R : Integer ) : Integer;                               }
{----------------------------------------------------------------------------}
{ PushRegle réalise une recopie rapide de la suite de termes qui composent   }
{ la règle pointée par R. Elle retourne l'adresse où a été recopiée cette    }
{ suite.                                                                     }
{----------------------------------------------------------------------------}

Function PushRegle( R : Integer ) : Integer;
Var SizeRegle : Integer;
Begin
  SizeRegle := Memoire[R+1];
  PushRegle := PtrLeft + 1;
  Move(Memoire[R+4],Memoire[PtrLeft+1],SizeRegle*2); {Primitive Turbo-Pascal}
  PtrLeft := PtrLeft + SizeRegle
End;


{----------------------------------------------------------------------------}
{ Procedure MiseAJour( Ad,Depl : Integer );                                  }
{----------------------------------------------------------------------------}
{ MiseAJour ajoute au contenu de la case d'adresse Ad la valeur Depl. On ne  }
{ met bien sûr pas à jour le pointeur nul.                                   }
{----------------------------------------------------------------------------}

Procedure MiseAJour( Ad,Depl : Integer );
Begin
  If Memoire[Ad] <> 0 Then Memoire[Ad] := Memoire[Ad] + Depl
End;


{----------------------------------------------------------------------------}
{ Procedure MiseAJourInequation( E,d : Integer );                            }
{----------------------------------------------------------------------------}
{ MiseAJourInequation met à jour une chaîne d'inéquations pointée par E avec }
{ le déplacement d.                                                          }
{----------------------------------------------------------------------------}

Procedure MiseAJourInequation( E,d : Integer );
Begin
  If E <> 0 Then
    Begin
      MiseAJour(E,d);
      MiseAJour(E+1,d);
      MiseAJour(E+2,d);
      MiseAJourInequation(Memoire[E+2],d)
    End
End;


{----------------------------------------------------------------------------}
{ Procedure MiseAJourTerme( T,d : Integer );                                 }
{----------------------------------------------------------------------------}
{ MiseAJourTerme met à jour le terme T avec la valeur d. Elle doit faire     }
{ attention à ne pas mettre à jour une variable qui l'a déjà été. En effet   }
{ une seule allocation mémoire est réalisée pour une variable, et celle-ci   }
{ peut donc être pointée plusieurs fois. La fonction doit donc s'aider d'un  }
{ dictionnaire (DicoAdr) des adresses des variables déjà mises à jour.       }
{----------------------------------------------------------------------------}

Procedure MiseAJourTerme( T,d : Integer );
Begin
  If T <> 0 Then
  Case Typ(T) Of
    SymboleF : Begin
                 MiseAJour(T+2,d);
                 MiseAJour(T+3,d);
                 MiseAJourTerme(Memoire[T+2],d);
                 MiseAJourTerme(Memoire[T+3],d)
               End;
    Variable : Begin
                 If Not Stocke( T ) Then
                   Begin
                     MiseAJour(T+4,d);
                     MiseAJour(T+5,d);
                     Add(T);
                     If Memoire[T+3] = 1 Then
                       MiseAJourInequation(Memoire[T+5],d)
                   End
               End
  End
End;


{----------------------------------------------------------------------------}
{ Function NewRegle( R : Integer ) : Integer;                                }
{----------------------------------------------------------------------------}
{ C'est la fonction de recopie et mise à jour d'une règle. La nécessité de   }
{ la mise à jour des adresses utilisées dans la règle provient de l'utili-   }
{ sation d'adresses absolues plutôt que d'adresses relatives.                }
{ NewRegle retourne l'adresse de la copie de la règle R.                     }
{----------------------------------------------------------------------------}

Function NewRegle( R : Integer ) : Integer;
Var Adr,Depl,T : Integer;
Begin
  Adr     := PushRegle(R);
  Depl    := Adr - (R+4);
  T       := Adr;
  PtrDico := 0;
  Repeat                              { Pour chaque bloc-terme }
    MiseAJour(T,Depl);                {  - Ptr Terme           }
    MiseAJour(T+1,Depl);              {  - Ptr Terme Suivant   }
    MiseAJour(T+2,Depl);              {  - Ptr Acces           }
    MiseAJourTerme(Memoire[T],Depl);  {  - Terme pointé        }
    T := Memoire[T+1]
  Until T = 0;
  NewRegle := Adr
End;


{----------------------------------------------------------------------------}
{                                                                            }
{                             H O R L O G E                                  }
{                                                                            }
{----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ Procedure Horloge( FirstR, FirstT, ButeeDroite : Integer );                }
{----------------------------------------------------------------------------}
{ Lance l'horloge pour effacer la liste de termes FirstT en utilisant la     }
{ liste de règles FirstR d'un programme Prolog, avec au départ un éventuel   }
{ système à résoudre stocké entre PtrRight et ButeeDroite (système spécifié  }
{ dans la  question).                                                        }
{----------------------------------------------------------------------------}

Procedure Horloge( FirstR, FirstT, ButeeDroite : Integer );

Var PtrLeftSave : Integer;  { Sauvegarde sommet de pile                     }
    Soluble     : Boolean;  { Système de contraintes soluble ?              }
    Butee       : Integer;  { Délimite les équations à réduire dans la pile }
    PtrT,PtrR,T : Integer;  { Divers pointeurs                              }
    Fin         : Boolean;  { Fin de l'horloge ?                            }


{------------------------------------------------------------------}
{ Procedure InitHorloge;                                           }
{------------------------------------------------------------------}
{ Initialise l'horloge Prolog;                                     }
{------------------------------------------------------------------}

  Procedure InitHorloge;
  Begin
    Fin   := False;             { Ce n'est pas encore la fin !              }
    InitRestore;                { Initialise la pile de restauration        }
    Temps := 0;                 { Temps 0                                   }
    Butee := ButeeDroite;       { Système présent dans la question          }
  End;

{------------------------------------------------------------------}
{ Procedure LancerReduction;                                       }
{------------------------------------------------------------------}
{ LancerReduction fait un appel à ReductionSysteme pour tester la  }
{ solvabilité de l'ensemble de contraintes et s'occupe en plus de  }
{ deux problèmes particuliers :                                    }
{                                                                  }
{ (1) La procédure de réduction a peut-être créé des inéquations,  }
{     et dans ce cas ces triplets <Tg,Td,Next> ont été créés après }
{     les quatre pointeurs qui se trouvaient en tête de pile.      }
{     Il faut alors remettre ce bloc de quatre pointeurs en tête.  }
{                                                                  }
{ (2) Il faut positionner le pointeur de restauration pour pouvoir }
{     retrouver l'état antérieur.                                  }
{                                                                  }
{------------------------------------------------------------------}

  Procedure LancerReduction;
  Begin
    PtrLeftSave := PtrLeft;
    Soluble     := ReductionSysteme(Butee,True);
    If PtrLeftSave <> PtrLeft Then { Des inéquations ont été créées }
      Begin
        Push(Memoire[PtrLeftSave-3]);   { On remet la tête en tête ! }
        Push(Memoire[PtrLeftSave-2]);
        Push(Memoire[PtrLeftSave-1]);
        Push(Memoire[PtrLeftSave  ]);
      End;
    Memoire[PtrLeft-1] := PtrRestore; { Sauve pointeur de restauration }
  End;


{------------------------------------------------------------------}
{ Procedure EcrireSolution;                                        }
{------------------------------------------------------------------}
{ EcrireSolution affiche à l'écran la partie intéressante du       }
{ système de contraintes. Cette partie ne concerne que les         }
{ variables de la question.                                        }
{------------------------------------------------------------------}

  Procedure EcrireSolution;
  Begin
    EcrireSysteme(FirstVar,NbVar);
    Writeln
  End;


{----------------------------------------------------------------------------}
{ Procedure BackTracking( Var Fin : Boolean );                               }
{----------------------------------------------------------------------------}
{ La procédure BackTracking remet l'horloge Prolog au dernier point de choix }
{ utilisable. Si il n'y a plus de choix à envisager, elle positionne le      }
{ booléen Fin à True.                                                        }
{                                                                            }
{ Un retour en arrière se fait en trois étapes :                             }
{                                                                            }
{  (1) Dépiler la règle qui est en tête de pile grâce au pointeur qui est    }
{      au sommet.                                                            }
{                                                                            }
{  (2) Restaurer la mémoire à l'état antérieur grâce au pointeur de restau-  }
{      ration de la nouvelle règle de tête.                                  }
{                                                                            }
{  (3) Positionner le pointeur de règle à appliquer sur la règle suivante    }
{      à appliquer.                                                          }
{                                                                            }
{ Si ce dernier pointeur est nul (épuisement des règles applicables) et      }
{ que ce n'est pas la fin (Temps=0), on recommence l'opération.              }
{                                                                            }
{----------------------------------------------------------------------------}

  Procedure BackTracking( Var Fin : Boolean );
  Var Next : Integer;
  Begin
    Fin := False;
    Repeat
      If Temps = 0 Then
        Begin
          Fin := True;
          Exit
        End;
      PtrLeft := Memoire[PtrLeft];  { BackTracking }
      Restore(Memoire[PtrLeft-1]);
      Temps := Temps - 1;
      Next := NextRegleOk(Memoire[PtrLeft-2],Memoire[PtrLeft-3]);
    Until Next <> 0;
    Memoire[PtrLeft-2] := Next
  End;


{------------------------------------------------------------------}
{ Procedure PremiereRegle;                                         }
{------------------------------------------------------------------}
{ PremiereRegle tente d'initialiser le pointeur de règle à appli-  }
{ quer avec la première règle dont la tête est peut-être unifiable }
{ avec le premier terme à effacer. Si ce terme n'a aucune chance   }
{ d'être effacé, la procédure fait un appel à Backtracking;        }
{------------------------------------------------------------------}

  Procedure PremiereRegle;
  Begin
    Memoire[PtrLeft-2] := PremiereRegleOk(FirstR, Memoire[PtrLeft-3]);
    If Memoire[PtrLeft-2] = 0 Then BackTrackIng(Fin)
  End;


{------------------------------------------------------------------}
{ Procedure Avancer;                                               }
{------------------------------------------------------------------}
{ Avancer fait avancer l'horloge Prolog d'une période. Les diffé-  }
{ rentes opérations à effectuer sont les suivantes :               }
{                                                                  }
{ (1) Recopier en tête de pile la règle courante à appliquer ;     }
{ (2) Créer les quatre pointeurs de tête ;                         }
{ (3) Ajouter à l'ensemble des contraintes l'équation              }
{                                                                  }
{    < Premier terme à effacer  =  tête de la règle recopiée >;    }
{                                                                  }
{ (4) Positionner le pointeur de termes à effacer de la manière    }
{     suivante :                                                   }
{                                                                  }
{      * Chercher le dernier bloc-terme de la règle ;              }
{      * Positionner le pointeur bloc-terme-suivant de ce bloc-    }
{        terme vers le bloc-terme suivant du premier bloc-terme    }
{        de l'ancienne suite de termes à effacer ;                 }
{      * Positionner le pointeur de termes à effacer vers le       }
{        bloc-terme suivant de la tête de la règle ;               }
{                                                                  }
{ (5) Positionner le pointeur de retour en arrière vers l'ancien   }
{     sommet de pile (avant recopie de la règle).                  }
{                                                                  }
{------------------------------------------------------------------}

  Procedure Avancer;
  Begin
    Temps := Temps + 1;
    PtrLeftSave := PtrLeft;
    PtrT := Memoire[PtrLeft-3];           { Sauve pointeur de termes    }
    PtrR := NewRegle(Memoire[PtrLeft-2]); { Endroit où a été copié la R }
    Entete(0,0,0,0);
    Butee := PtrRight;
    AjouteTravail('=',Memoire[PtrT],Memoire[PtrR]);
    T := PtrR;
    While (TermeSuivant(T)<> 0) Do T := TermeSuivant(T);
    Memoire[T+1] := TermeSuivant(PtrT);
    Memoire[PtrLeft-3] := TermeSuivant(PtrR);
    Memoire[PtrLeft] := PtrLeftSave;      { Pour futur backtrack }
  End;


{------------------------------------------------------------------}
{ Procedure Reculer;                                               }
{------------------------------------------------------------------}
{ On est ici au bout d'une feuille de l'arbre développé par        }
{ l'horloge. Si l'ensemble de contraintes est soluble c'est une    }
{ solution. Dans tous les cas on retourne au dernier point de      }
{ choix.                                                           }
{------------------------------------------------------------------}

  Procedure Reculer;
  Begin
    If Soluble Then EcrireSolution;
    BackTracking(Fin)
  End;


Begin
  InitHorloge;
  Entete(FirstT,FirstR,0,0); { Entête pour réduction du système de la question }
  Repeat
    LancerReduction;
    If (Not Soluble) Or             { Système de contraintes non soluble }
       (Memoire[PtrLeft-3] = 0)     { Plus de terme à effacer            }
    Then
      Reculer
    Else
      PremiereRegle;
    If Not Fin Then Avancer
  Until Fin
End;



