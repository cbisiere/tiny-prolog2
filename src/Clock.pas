{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Clock.pas                                                  }
{   Author      : Christophe Bisière                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                        P R O L O G   C L O C K                             }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Var ClockTime : Integer;                     { Le temps de l'horloge Prolog }

  {-----------------------------------------------------------------------}
  {                                                                       }
  {  LE SOMMET DE LA PILE PRINCIPALE µ :                                  }
  {                                                                       }
  {               |-------|                                               }
  {               |       |                                               }
  {         µ     | FBCL  |---> Pointe vers la suite des termes à         }
  {               |       |     effacer.                                  }
  {               |-------|                                               }
  {               |       |                                               }
  {         µ+1   | RULE  |---> Pointe vers la règle à appliquer.         }
  {               |       |                                               }
  {               |-------|                                               }
  {               |       |                                               }
  {         µ+2   | REST  |---> Pointe, avant le lancement de la          }
  {               |       |     procédure de réduction, sur le sommet de  }
  {               |-------|     la pile de restauration.                  }
  {               |       |                                               }
  {         µ+3   | STAC  |---> Pointe dans la pile principale vers le    }
  {               |       |     sommet de pile à l'étape précédente.      }
  {               |-------|                                               }
  {               |       |                                               }
  {         µ+4   | PREV  |---> Pointe dans la pile principale vers       }
  {               |       |     l'entête de l'étape précédente (ou 0      }
  {               |-------|     si pas d'étape précédente).               }
  {                                                                       }
  {-----------------------------------------------------------------------}

  Const
    HH_length = 5;
    HH_FBCL = 0;
    HH_RULE = 1;
    HH_REST = 2;
    HH_STAC = 3;
    HH_PREV = 4;

  {-----------------------------------------------------------------------}
  { Crée une entête.                                                      }
  {-----------------------------------------------------------------------}

  Function NewClockHeader(Fbcl,Rule,Rest,Stac,Prev : Integer ) : Integer;
  Var H : Integer;
  Begin
    H := Alloc(HH_length);
    Memory[H+HH_FBCL] := Fbcl;               { Suite des termes à effacer }
    Memory[H+HH_RULE] := Rule;               { Première règle             }
    Memory[H+HH_REST] := Rest;               { Ptr Pile Restore           }
    Memory[H+HH_STAC] := Stac;               { Pointeur back              }
    Memory[H+HH_PREV] := Prev;               { Pointeur back              }
    NewClockHeader := H
  End;

{----------------------------------------------------------------------------}
{                                                                            }
{                 R E C O P I E   D ' U N E   R E G L E                      }
{                                                                            }
{----------------------------------------------------------------------------}

Const MaxCopyAdr = 100;
Type TDictAdr = Array[1..MaxCopyAdr] Of Integer;  { Dict d'adresses          }

Var
  DictAdr : TDictAdr;        { Dict des adresses des objets déjà recopiés  }
  PtrDict : Integer;         { Stack Pointer pour ce dictionnaire          }

{----------------------------------------------------------------------------}
{ Regarde si l'adresse T est déjà dans le dictionnaire DictAdr.              }
{ Si oui elle retourne True, sinon elle retourne False.                      }
{----------------------------------------------------------------------------}

Function StoreAdr( T : Integer ) : Boolean;
Var
  I     : Integer;
  Found : Boolean;
Begin
  I    := 1;
  Found := False;
  While (Not Found) And (I<=PtrDict) Do
  Begin
    If DictAdr[I] = T Then Found := True;
    I := I + 1
  End;
  StoreAdr := Found
End;

{----------------------------------------------------------------------------}
{ Ajoute au dictionnaire DictAdr l'adresse T.                                }
{----------------------------------------------------------------------------}

Procedure Add( T : Integer );
Begin
  PtrDict := PtrDict + 1;
  CheckCondition(PtrDict <= MaxCopyAdr,'Memory exhausted while copying a rule');
  DictAdr[PtrDict] := T
End;

{----------------------------------------------------------------------------}
{ Réalise une recopie rapide de la suite de termes qui composent             }
{ la règle pointée par R. Elle retourne l'adresse où a été recopiée cette    }
{ suite.                                                                     }
{----------------------------------------------------------------------------}

Function PushRule( R : Integer ) : Integer;
Var
  Size  : Integer;
  CopyR : Integer;
Begin
  Size := Memory[R+RU_SIZE];
  CopyR := Alloc(Size);
  Move(Memory[R+RU_FBTR],Memory[CopyR],Size*SizeOf(Integer)); {Turbo Pascal}
  PushRule := CopyR
End;

{----------------------------------------------------------------------------}
{ Ajoute au contenu de la case d'adresse A la valeur Offset. On ne           }
{ met bien sûr pas à jour le pointeur nul.                                   }
{----------------------------------------------------------------------------}

Procedure ShiftValueAt( A,Offset : Integer );
Begin
  If (Memory[A] <> 0) And (Memory[A] <> NULL) Then
    Memory[A] := Memory[A] + Offset
End;

{----------------------------------------------------------------------------}
{ Met à jour une chaîne d'inéquations pointée par E avec                     }
{ le déplacement d.                                                          }
{----------------------------------------------------------------------------}

Procedure ShiftInequation( E,d : Integer );
Begin
  If E <> NULL Then
  Begin
    ShiftValueAt(E+EQ_LTER,d);
    ShiftValueAt(E+EQ_RTER,d);
    ShiftValueAt(E+EQ_NEXT,d);
    ShiftInequation(Memory[E+EQ_NEXT],d)
  End
End;

{----------------------------------------------------------------------------}
{ Met à jour le terme T avec la valeur d. Elle doit faire                    }
{ attention à ne pas mettre à jour une variable qui l'a déjà été. En effet   }
{ une seule allocation mémoire est réalisée pour une variable, et celle-ci   }
{ peut donc être pointée plusieurs fois. La fonction doit donc s'aider d'un  }
{ dictionnaire (DictAdr) des adresses des variables déjà mises à jour.       }
{----------------------------------------------------------------------------}

Procedure ShiftTerm( T,d : Integer );
Begin
  If T <> NULL Then
    Case TypeOfTerm(T) Of
    FuncSymbol :
      Begin
         ShiftValueAt(T+TF_LTER,d);
         ShiftValueAt(T+TF_RTER,d);
         ShiftTerm(Memory[T+TF_LTER],d);
         ShiftTerm(Memory[T+TF_RTER],d)
       End;
    Variable :
      Begin
       If Not StoreAdr( T ) Then
       Begin
         Memory[T+TV_COPY] := YES;
         ShiftValueAt(T+TV_TRED,d);
         ShiftValueAt(T+TV_FWAT,d);
         Add(T);
         If Memory[T+TV_IWAT] = YES Then
           ShiftInequation(Memory[T+TV_FWAT],d)
       End
     End
  End
End;

{----------------------------------------------------------------------------}
{ C'est la fonction de recopie et mise à jour d'une règle. La nécessité de   }
{ la mise à jour des adresses utilisées dans la règle provient de l'utili-   }
{ sation d'adresses absolues plutôt que d'adresses relatives.                }
{ Retourne l'adresse de la copie des blocs de la règle R.                    }
{----------------------------------------------------------------------------}

Function CopyTermsOfRule( R : Integer ) : Integer;
Var RuleB,Offset,B : Integer;
Begin
  RuleB := PushRule(R);
  Offset := RuleB - (R+RU_FBTR);
  B := RuleB;
  PtrDict := 0;
  Repeat                                   { Pour chaque bloc-terme }
    ShiftValueAt(B+BT_TERM,Offset);        {  - Ptr Terme           }
    ShiftValueAt(B+BT_NEXT,Offset);        {  - Ptr Terme Suivant   }
    ShiftValueAt(B+BT_CONS,Offset);        {  - Ptr Acces           }
    ShiftTerm(Memory[B+BT_TERM],Offset);   {  - Terme pointé        }
    B := Memory[B+BT_NEXT]
  Until B = NULL;
  CopyTermsOfRule := RuleB
End;

{----------------------------------------------------------------------------}
{                                                                            }
{                                C L O C K                                   }
{                                                                            }
{----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ Lance l'horloge pour effacer la question Q .                               }
{----------------------------------------------------------------------------}

Procedure Clock( Q, RightStopper : Integer );

Var
  PtrLeftSave : Integer;  { Sauvegarde sommet de pile                     }
  Soluble     : Boolean;  { Système de contraintes soluble ?              }
  Stopper     : Integer;  { Délimite les équations à réduire dans la pile }
  EndOfClock  : Boolean;  { Fin de l'horloge ?                            }
  H           : Integer;  { Entête                                        }
  R,B         : Integer;

{------------------------------------------------------------------}
{ Initialise l'horloge Prolog;                                     }
{------------------------------------------------------------------}

  Procedure InitClock;
  Begin
    EndOfClock := False;        { Ce n'est pas encore la fin !              }
    InitRestore;                { Initialise la pile de restauration        }
    ClockTime := 0;             { Le temps initial                          }
    Stopper := RightStopper     { Système présent dans la question          }
  End;

{------------------------------------------------------------------}
{ Fait un appel à ReduceSystem pour tester la                      }
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

  Procedure TryToReduce( Var H : Integer );
  Begin
    PtrLeftSave := PtrLeft;
    Soluble := ReduceSystem(Stopper,True);
    If PtrLeftSave <> PtrLeft Then { Des inéquations ont été créées }
      H := NewClockHeader(Memory[H+HH_FBCL],Memory[H+HH_RULE],
        Memory[H+HH_REST],Memory[H+HH_STAC],Memory[H+HH_PREV]);
    Memory[H+HH_REST] := PtrRestore { Sauve pointeur de restauration }
  End;

{------------------------------------------------------------------}
{ Affiche à l'écran la partie intéressante du                      }
{ système de contraintes. Cette partie ne concerne que les         }
{ variables de la question.                                        }
{------------------------------------------------------------------}

  Procedure WriteSolution;
  Begin
    WriteSystem(Memory[Q+QU_FVAR],Memory[Q+QU_LVAR],True);
    Writeln
  End;

  {----------------------------------------------------------------------------}
  { Détermine si les deux termes T1 et T2 sont a priori unifiables.            }
  { Ils ne le sont pas lorsque ils ont pour accès une constante différente.    }
  { Ce test permet d'économiser un grand nombre de recopies de règles.         }
  {----------------------------------------------------------------------------}

  Function Unifiable( T1,T2 : Integer) : Boolean;
  Var Ok : Boolean;
  Begin
    CheckCondition((T1<>NULL) Or (T2<>NULL),'Call to Unifiable with two NULL terms');
    Ok := (T1=T2) Or (T1=NULL) Or (T2=NULL);
    If Not Ok Then
      Ok := (TypeOfTerm(T1)=Constant) And (TypeOfTerm(T2)=Constant) And
          (Memory[T1+TC_CONS]=Memory[T2+TC_CONS]);
    Unifiable := Ok
  End;

  {----------------------------------------------------------------------------}
  { Returns the rule following R, or NULL if R is the last rule in the query's }
  { scope.                                                                     }
  {----------------------------------------------------------------------------}
  Function Next( R : Integer ) : Integer;
  Begin
    If (R = NULL) Or (R = Memory[Q+QU_LRUL]) Then
      Next := NULL
    Else
      Next := Memory[R+RU_NEXT]
  End;

  {----------------------------------------------------------------------------}
  { Retourne un pointeur vers la première règle ayant une chance de s'unifier  }
  { avec le terme B (ou NULL si aucune règle ne peut s'unifier avec B), R      }
  { étant l'addresse de la première règle à examiner.        .                 }
  {----------------------------------------------------------------------------}

  Function FirstCandidateRule( R,B : Integer ) : Integer;
  Var
    FirstR,T1,T2 : Integer;
    Stop : Boolean;
  Begin
    FirstR := NULL;
    If B <> NULL Then
    Begin
      Stop := False;
      T1 := AccessTerm(B);
      While (R<>NULL) And Not Stop Do
      Begin
        T2 := AccessTerm(R+RU_FBTR);
        If Unifiable(T1,T2) Then
        Begin
          FirstR := R;
          Stop := True
        End;
        R := Next(R)
      End
    End;
    FirstCandidateRule := FirstR
  End;

  {----------------------------------------------------------------------------}
  { Sachant que le terme B pouvait s'unifier avec la tête de la règle R, la    }
  { fonction  retourne un pointeur vers la règle qui suit R, si la             }
  { tête de cette dernière peut s'unifier avec le terme B, et 0 dans le cas    }
  { contraire.                                                                 }
  { Cette logique impose qu'un ensemble de règles ayant même accès doit être   }
  { écrit à la suite dans le fichier source.                                   }
  {----------------------------------------------------------------------------}

  Function NextCandidateRule( R,B : Integer ) : Integer;
  Begin
    NextCandidateRule := FirstCandidateRule(Next(R),B)
  End;

{----------------------------------------------------------------------------}
{ Remet l'horloge Prolog au dernier point de choix                           }
{ utilisable. Si il n'y a plus de choix à envisager, elle positionne le      }
{ booléen EndOfClock à True.                                                 }
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
{ que ce n'est pas la fin (ClockTime=0), on recommence l'opération.          }
{                                                                            }
{----------------------------------------------------------------------------}

  Procedure Backtracking( Var H : Integer; Var NoMoreChoices : Boolean );
  Var
    NextR : Integer;
    NewPtrLeft : Integer;
  Begin
    NoMoreChoices := False;
    Repeat
      If ClockTime = 0 Then
      Begin
        NoMoreChoices := True;
        Exit
      End;
      NewPtrLeft := Memory[H+HH_STAC];  { Backtracking }
      H := Memory[H+HH_PREV];
      PtrLeft := NewPtrLeft;
      Restore(Memory[H+HH_REST]);
      ClockTime := ClockTime - 1;
      NextR := NextCandidateRule(Memory[H+HH_RULE],Memory[H+HH_FBCL]);
    Until NextR <> NULL;
    Memory[H+HH_RULE] := NextR;
End;

{------------------------------------------------------------------}
{ Tente d'initialiser le pointeur de règle à appliquer             }
{ avec la première règle dont la tête est peut-être unifiable      }
{ avec le premier terme à effacer. Si ce terme n'a aucune chance   }
{ d'être effacé, la procédure fait un appel à Backtracking;        }
{------------------------------------------------------------------}

  Procedure FirstRule( H : Integer );
  Begin
    Memory[H+HH_RULE] := FirstCandidateRule(Memory[Q+QU_FRUL], Memory[H+HH_FBCL]);
    If Memory[H+HH_RULE] = NULL Then
      Backtracking(H,EndOfClock)
  End;

{------------------------------------------------------------------}
{ MoveForward fait avancer l'horloge Prolog d'une période. Les diffé-  }
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

  Procedure MoveForward( Var H : Integer );
  Var ClearB, RuleB, B, PtrLeftSave : Integer;
  Begin
    CheckCondition(Memory[H+HH_FBCL] <> NULL,'MoveForward: No terms to clear');
    CheckCondition(Memory[H+HH_RULE] <> NULL,'MoveForward: No rule to apply');
    ClockTime := ClockTime + 1;
    PtrLeftSave := PtrLeft;
    ClearB := Memory[H+HH_FBCL];          { Sauve pointeur de termes    }
    RuleB := CopyTermsOfRule(Memory[H+HH_RULE]);
    H := NewClockHeader(NULL,NULL,0,PtrLeftSave,H);
    Stopper := PtrRight;
    { Contrainte à réduire : terme à réduire = tête de la règle }
    PushOneEquationToSolve(REL_EQUA,Memory[ClearB+BT_TERM],Memory[RuleB+BT_TERM]);
    { Crée la nouvelle liste de blocs à effacer :
      queue +  anciens termes à effacer sauf le premier }
    B := RuleB;
    While (NextTerm(B)<>NULL) Do
      B := NextTerm(B);
    Memory[B+BT_NEXT] := NextTerm(ClearB);
    Memory[H+HH_FBCL] := NextTerm(RuleB)
  End;

{------------------------------------------------------------------}
{ On est ici au bout d'une feuille de l'arbre développé par        }
{ l'horloge. Si l'ensemble de contraintes est soluble c'est une    }
{ solution. Dans tous les cas on retourne au dernier point de      }
{ choix.                                                           }
{------------------------------------------------------------------}

  Procedure MoveBackward( Var H : Integer );
  Begin
    If Soluble Then
      WriteSolution;
    Backtracking(H,EndOfClock)
  End;

Begin
  InitClock;
  B := Memory[Q+QU_FBTR];
  If B = NULL Then { no terms to clear in the query }
  Begin
    WriteSolution;
    Exit
  End;
  R := FirstCandidateRule(Memory[Q+QU_FRUL], B);
  If R = NULL Then
  Begin
    Exit
  End;
  H := NewClockHeader(B,R,0,PtrLeft,NULL);
  Repeat
    MoveForward(H);
    TryToReduce(H);
    If (Not Soluble) Or           { Système de contraintes non soluble }
       (Memory[H+HH_FBCL] = NULL) { Plus de terme à effacer            }
    Then
      MoveBackward(H)
    Else
      FirstRule(H)
  Until EndOfClock
End;

{----------------------------------------------------------------------------}
{ Répond à la question Q.                                                    }
{----------------------------------------------------------------------------}

Procedure AnswerQuery( Q : Integer );
Var Stopper : Integer;
Begin
  UnparseOneQuery(Q);
  Stopper := PtrRight;
  Clock(Q, Stopper)
End;

{----------------------------------------------------------------------------}
{ Répond à toutes les questions de la liste Q.                               }
{----------------------------------------------------------------------------}

Procedure AnswerQueries( Q : Integer );
Begin
  While Q <> NULL Do
  Begin
    AnswerQuery(Q);
    Q := Memory[Q+QU_NEXT]
  End
End;

{----------------------------------------------------------------------------}
{ Répond à toutes les questions du programme P.                              }
{----------------------------------------------------------------------------}

Procedure AnswerProgramQueries( P : Integer );
Begin
  AnswerQueries(Memory[P+PP_FQRY])
End;
