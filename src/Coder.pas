{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   Fichier     : Coder.pas                                                  }
{   Auteur      : Christophe BISIERE                                         }
{   Date        : 07/01/88                                                   }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                 C O D A G E   D E S   O B J E T S                          }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{     F. InstalConst( Ch : StrIdent ) : Integer;                             }
{     F. InstalSymb ( AdrG,AdrD : Integer ) : Integer;                       }
{     F. InstalVar (Ch :StrIdent) : Integer;                                 }
{     F. GetArgument( Fin : Char ) : Integer;                                }
{     F. LireTerme : Integer;                                                }
{     F. LireEquation : Integer;                                             }
{     F. LireSysteme : Integer;                                              }
{     F. Acces (T : Integer) : Integer;                                      }
{     F. CompilerTerme : Integer;                                            }
{     F. CompilerSuiteDeTermes( StopCar : CharSet ) : Integer;               }
{     P. CompilerRegle;                                                      }
{     F. CompilerSuiteDeRegles( StopCar : CharSet ) : Integer;               }
{     F. CompilerProgramme : Integer; ;                                      }
{     P. Entete(a,b,c,d);                                                    }
{     F. CompilerQuestion : Integer;                                         }
{                                                                            }
{----------------------------------------------------------------------------}

{ $R+} { Directive de compilation : Vérifier les indices des tableaux.     }
{ $V-} { Directive de compilation : Ne pas vérifier la taille des chaînes. }


{----------------------------------------------------------------------------}
{                                                                            }
{  La syntaxe adoptée pour ce mini PROLOG II est la suivante (en B.N.F.) :   }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{    variable ::= <lettre> [<chiffre>]* [']*                                 }
{                                                                            }
{    identificateur ::= <lettre> <lettre> [<lettre>]* [<chiffre>]*           }
{                                                                            }
{    entier ::= <chiffre> [<chiffre>]*                                       }
{                                                                            }
{    constante ::= <identificateur> | <entier>                               }
{                                                                            }
{                                                                            }
{                                                                            }
{    terme ::= <terme simple> [.<terme>]                                     }
{                                                                            }
{    terme simple ::= <constante> |                                          }
{                     <variable>  |                                          }
{                     <identificateur> ( <terme> [,<terme>]* ) |             }
{                     < <terme> [,<terme>]* > |                              }
{                     ( <terme> )                                            }
{                                                                            }
{    contrainte ::= <terme> = <terme> |                                      }
{                   <terme> <> <terme>                                       }
{                                                                            }
{    système ::= { <contrainte> [,<contrainte>]* }                          {}
{                                                                            }
{    règle ::= <terme> -> [<terme>]* [<système>] ;                           }
{                                                                            }
{    question ::= > [<terme>]* [système] ?                                           }
{                                                                            }
{    programme-et-questions ::= [<règle>]*  [<question>]*                }
{                                                                            }
{                                                                            }
{----------------------------------------------------------------------------}


Const Lettre   : CharSet = ['a'..'z','A'..'Z'];   { Ensemble des lettres    }
      Chiffre  : CharSet = ['0'..'9'];            { Ensemble des chiffres   }

Var SommetRegles    : Integer;  { Pointeur sommet des règles compilées       }
    SommetProgramme : Integer;  { Pointeur sommet du programme (Règles + Q ) }
    TopVar      : Integer;      { Début de recherche (localité des Vars)     }
    FirstVar    : Integer;   { Adresse Dico première variable de la question }
    FirstConst  : Integer;   { Adresse Dico premiere constante allouée dans Q}

Function LireTerme : Integer; Forward;

{-----------------------------------------------------------------------}
{                                                                       }
{  UNE CONSTANTE C :                                                    }
{                                                                       }
{               |-------|                                               }
{               |       |                                               }
{         C     | Type  |---> Code for 'Constante' : TERM_C.            }
{               |       |                                               }
{               |-------|                                               }
{               |       |                                               }
{         C+1   | NumC  |---> Numéro de la constante dans le diction-   }
{               |       |     naire des constantes.                     }
{               |-------|                                               }
{                                                                       }
{      Pour chaque rencontre d'une constante, une allocation est        }
{   réalisée. Cela implique que la comparaison de deux constantes ne    }
{   se réduit pas à la comparaison de deux pointeurs.                   }
{   Cette solution a été adoptée pour simplifier la procédure de reco-  }
{   pie des règles.                                                     }
{                                                                       }
{-----------------------------------------------------------------------}


{-----------------------------------------------------------------------}
{ Function InstalConst( Ch : StrIdent ) : Integer;                      }
{-----------------------------------------------------------------------}
{ La fonction instalConst réalise une allocation mémoire pour la        }
{ constante Ch. Elle retourne l'adresse où a été installée cette        }
{ constante.                                                            }
{-----------------------------------------------------------------------}

Function InstalConst( Ch : StrIdent ) : Integer;
Var Adr : Integer;
Begin
  Adr := AllocLeft(2);
  Memoire[Adr+0] := TERM_C;        {      1 : Code 'Constante'          }
  Memoire[Adr+1] := NumConst(Ch);  {      2 : Pointeur dans DicoConst   }
  InstalConst := Adr
End;


{-----------------------------------------------------------------------}
{                                                                       }
{  UN SYMBOLE FONCTIONNEL (BINAIRE) F :                                 }
{                                                                       }
{               |-------|                                               }
{               |       |                                               }
{         F     | Type  |---> Code 'Symbole Fonctionnel' : TERM_F       }
{               |       |                                               }
{               |-------|                                               }
{               |       |                                               }
{         F+1   | PtrS  |---> Pointe vers le membre droit auquel est    }
{               |       |     associé ce symbole fonctionnel dans le    }
{               |-------|     système réduit (0 sinon).                 }
{               |       |                                               }
{         F+2   | PtrL  |---> Pointe vers le fils gauche.               }
{               |       |                                               }
{               |-------|                                               }
{               |       |                                               }
{         F+3   | PtrR  |---> Pointe vers le fils droit.                }
{               |       |                                               }
{               |-------|                                               }
{                                                                       }
{   L'unique symbole fonctionnel du système sert à coder les prédicats  }
{   Prolog de la manière suivante :                                     }
{                                                                       }
{           F                                                           }
{          / \               (1) Dans le cas d'un N-uplet, le nom du    }
{       nom   F                  prédicat est le premier argument.      }
{            / \                                                        }
{         Arg1  F            (2) Il est bien entendu que Arg1 .. ArgN   }
{              / \               peuvent aussi être des prédicats.      }
{           Arg2   ...                                                  }
{                   \        (3) Le '.' (liste) est considéré comme     }
{                    F           un prédicat quelconque et stocké comme }
{                  /  \          tel.                                   }
{               ArgN    0                                               }
{                                                                       }
{-----------------------------------------------------------------------}


{-----------------------------------------------------------------------}
{ Function InstalSymb ( AdrG,AdrD : Integer ) : Integer;                }
{-----------------------------------------------------------------------}
{ La fonction instalSymb réalise une allocation mémoire pour un symbole }
{ fonctionnel binaire dont le fils gauche est AdrG et le fils droit est }
{ AdrD. Elle retourne un pointeur vers l'allocation.                    }
{-----------------------------------------------------------------------}

Function InstalSymb( AdrG,AdrD : Integer ) : Integer;
Var Adr : Integer;
Begin
  Adr := AllocLeft(4);
  Memoire[Adr+0] := TERM_F;     {      1 : Code 'Symbole Fonct.'     }
  Memoire[Adr+1] := 0;          {      2 : Pointeur futur mbre droit }
  Memoire[Adr+2] := AdrG;       {      3 : Pointeur premier terme    }
  Memoire[Adr+3] := AdrD;       {      4 : Pointeur deuxième terme   }
  InstalSymb := Adr
End;


{-----------------------------------------------------------------------}
{                                                                       }
{  UNE VARIABLE V :                                                     }
{                                                                       }
{               |-------|                                               }
{               |       |                                               }
{         V     | Type  |---> Code for 'Variable' : TERM_V.             }
{               |       |                                               }
{               |-------|                                               }
{               |       |                                               }
{         V+1   | NumV  |---> Numéro de la variable dans le diction-    }
{               |       |     naire des variables.                      }
{               |-------|                                               }
{               |       |                                               }
{         V+2   | Equ?  |---> Booléen qui indique si cette variable est }
{               |       |     actuellement le membre gauche d'une équa- }
{               |-------|     tion du système réduit.                   }
{               |       |                                               }
{         V+3   | Sur?  |---> Booléen qui indique si cette variable     }
{               |       |     surveille actuellement une ou des inéqua- }
{               |-------|     tions.                                    }
{               |       |                                               }
{         V+4   | PtrD  |---> Pointe vers le membre droit du système    }
{               |       |     réduit. N'est significatif que si Equ =   }
{               |-------|     True.                                     }
{               |       |                                               }
{         V+5   | PtrI  |---> Pointe vers la première inéquation que    }
{               |       |     cette variable surveille. N'est signifi-  }
{               |-------|     catif que si Sur = True.                  }
{                                                                       }
{   Remarques :                                                         }
{                                                                       }
{     (1) Dans l'algorithme original de réduction d'un système d'équa-  }
{         tions et d'inéquations, la présence simultanée des deux in-   }
{         dicateurs Equ et Sur est indispensable parce que l'Etape2 de  }
{         la résolution d'inéquations se fait après l'Etape1. Il nous   }
{         faut savoir à l'Etape2 si une variable est à la fois membre   }
{         gauche du système réduit et gardienne d'une inéquation.       }
{         Dans l'interpreteur Prolog, l'Etape2 de la résolution d'iné-  }
{         quations a été intégrée dans la résolution d'équations. La    }
{         présence simultanée des deux indicateurs a été conservée tout }
{         de même pour des raisons de clarté.                           }
{                                                                       }
{     (2) Une même variable n'est allouée qu'une seule fois, sauf si    }
{         elle se trouve dans deux règles différentes (localité des     }
{         variables par rapport aux règles).                            }
{                                                                       }
{                                                                       }
{     (3) Une inéquation dans le système réduit sera codée de la façon  }
{         suivante :                                                    }
{                                                                       }
{               |-------|                                               }
{               |       |                                               }
{         I     | PtrL  |---> Pointe vers le membre gauche.             }
{               |       |                                               }
{               |-------|                                               }
{               |       |                                               }
{         I+1   | PtrR  |---> Pointe vers le membre droit.              }
{               |       |                                               }
{               |-------|                                               }
{               |       |                                               }
{         I+2   | PtrS  |---> Pointe vers l'inéquation suivante (0 si   }
{               |       |     cette inéquation est la dernière de la    }
{               |-------|     chaîne).                                  }
{                                                                       }
{                                                                       }
{-----------------------------------------------------------------------}


{-----------------------------------------------------------------------}
{ Function InstalVar (Ch :StrIdent) : Integer;                          }
{-----------------------------------------------------------------------}
{ La fonction instalVar réalise une allocation mémoire pour la variable }
{ Ch, si celle-ci n'a pas déjà été allouée pour la règle courante. Dans }
{ tous les cas elle retourne un pointeur vers son allocation.           }
{-----------------------------------------------------------------------}

Function InstalVar( Ch : StrIdent ) : Integer;
Var Adr : Integer;
    PosInDicoVar : Integer;
Begin
  PosInDicoVar := Position(TopVar,Ch);
  If PosInDicoVar = 0 Then
    Begin
      Adr := AllocLeft(6);
      PosInDicoVar := NewVar(Ch,Adr);
      Memoire[Adr+0] := TERM_V;       { 1 : Code 'Variable'           }
      Memoire[Adr+1] := PosInDicoVar; { 2 : Pointeur dans Dicovar     }
      Memoire[Adr+2] := 0;            { 3 : Pas d'équation            }
      Memoire[Adr+3] := 0;            { 4 : Pas d'inéquation          }
      Memoire[Adr+4] := 0;            { 5 : partie droite équation    }
      Memoire[Adr+5] := 0;            { 6 : Première inéquation       }
      InstalVar := Adr
    End
  Else
    InstalVar := GetVarPtr(PosInDicoVar) { adresse d'implantation }
End;


{-----------------------------------------------------------------------}
{ Function GetArgument( Fin : Char ) : Integer;                         }
{-----------------------------------------------------------------------}
{ La fonction GetArgument lit et code les arguments d'un prédicat (Fin= }
{ ')') ou d'un N-Uplet (Fin='>'). Elle retourne un pointeur vers la     }
{ structure qui représente ces arguments.                               }
{-----------------------------------------------------------------------}

Function GetArgument( Fin : Char ) : Integer;
Var AdrG : Integer;
Begin
  AdrG := Lireterme;
  Calu := GetCharNb(Calu);
  If Calu =  ',' Then GetArgument := InstalSymb(AdrG,GetArgument(Fin))
  Else
  If Calu = Fin  Then
    Begin
      GetArgument := InstalSymb(AdrG,0);
      UnGetChar(GetCharNb(Calu))
    End
  Else
    Begin
      Erreur(Fin+' expected');
      GetArgument := 0
    End
End;


{-----------------------------------------------------------------------}
{ Function LireTerme : Integer;                                         }
{-----------------------------------------------------------------------}
{ LireTerme lit un terme en entrée, le code en mémoire et retourne      }
{ l'adresse où il a été implanté.                                       }
{-----------------------------------------------------------------------}

Function LireTerme;
Var Adr : Integer;
    Ch  : StrIdent;
Begin
  Ch   := '';
  UnGetChar(GetCharNb(Calu));
  Get(Ch,Lettre);
  Case Length(Ch) Of
    0 : Begin
          If Calu In Chiffre Then     { un entier }
            Begin
              Get(Ch,Chiffre);
              Adr := InstalConst(Ch);
            End
          Else
          If Calu ='(' Then          { une forme ( <terme> ) }
            Begin
              Calu := GetChar(Calu);
              Adr  := LireTerme;
              If GetCharNb(Calu) <> ')' Then Erreur(' ) expected ')
              Else UnGetChar(GetCharNb(Calu))
            End
          Else
          If Calu = '<' Then        { un N-uplet }
            Begin
              Calu := GetChar(Calu);
              Adr  := GetArgument('>')
            End
          Else
            Erreur('Syntax error')
        End;
    1 : Begin                      { une variable }
          Get(Ch,Chiffre);
          Get(Ch,['''']);
          Adr := InstalVar(Ch);
        End;
    Else
      Begin
        Get(Ch,Lettre+Chiffre+['-']);     { un identificateur }
        If Calu = '(' Then                { => un prédicat    }
          Begin
            Calu := GetChar(Calu);
            Adr  := InstalSymb(InstalConst(Ch),GetArgument(')'));
          End
        Else Adr := InstalConst(Ch)       { => une constante  }
      End
  End;
  If Calu = '.' Then    { un élément de liste }
    Begin
      Calu := GetChar(Calu);
      Adr  := InstalSymb( InstalConst('.'),
                          InstalSymb(Adr,InstalSymb(LireTerme,0)) )
    End;
  LireTerme := Adr    { Retourne adresse d'implantation de ce terme }
End;

{-----------------------------------------------------------------------}
{                                                                       }
{  UNE EQUATION OU UNE INEQUATION (PILE DROITE) E :                     }
{                                                                       }
{               |-------|                                               }
{               |       |                                               }
{         E     |  Code |---> Code REL_EQUA pour une équation,          }
{               |       |     Code REL_INEQ pour une inéquation.        }
{               |-------|                                               }
{               |       |                                               }
{         E-1   | PtrL  |---> Pointe vers le membre gauche.             }
{               |       |                                               }
{               |-------|                                               }
{               |       |                                               }
{         E-2   | PtrR  |---> Pointe vers le membre droit.              }
{               |       |                                               }
{               |-------|                                               }
{                                                                       }
{                                                                       }
{-----------------------------------------------------------------------}


{-----------------------------------------------------------------------}
{ Function LireEquation : Integer;                                      }
{-----------------------------------------------------------------------}
{ LireEquation lit une équation ou une inéquation et la stocke dans la  }
{ pile droite de la mémoire principale.                                 }
{-----------------------------------------------------------------------}

Function LireEquation : Integer;
Var Adr : Integer;
Begin
  Adr := AllocRight(3); { Adresse d'implantation dans pile droite }
  Memoire[Adr-1] := LireTerme;   {      2 : Pointeur 1er terme (pile gauche) }
  If Not Error Then
  Begin
    Calu := GetCharNb(Calu);
    Case Calu Of
      '=' : Memoire[Adr] := REL_EQUA;               { 1 : Code 'Equation'   }
      '<' : If GetCharNb(Calu) <> '>' Then Erreur('> expected')
            Else
              Memoire[Adr] := REL_INEQ;             { 1 : Code 'Inéquation' }
      Else Erreur('= or <> expected')
    End;
  End;
  If Not Error Then
    Memoire[Adr-2] := LireTerme;  {      3 : Pointeur 2nd terme (pile gauche) }
  LireEquation := Adr
End;


{-----------------------------------------------------------------------}
{                                                                       }
{  UN SYSTEME :                                                         }
{                                                                       }
{    Le système est simplement codé sous forme d'une suite d'équations  }
{  ou d'inéquations dans la pile droite.                                }
{                                                                       }
{                                                                       }
{-----------------------------------------------------------------------}


{-----------------------------------------------------------------------}
{ Function LireSysteme : Integer;                                       }
{-----------------------------------------------------------------------}
{ LireSysteme lit un système d'équations et d'inéquations et la stocke  }
{ dans la pile droite de la mémoire principale.                         }
{-----------------------------------------------------------------------}

Function LireSysteme : Integer;
Var Adr, Loc : Integer;
    First : Boolean;
Begin
  Loc := 0;
  If GetCharNb(Calu) <> '{' Then Erreur('Missing {')
  Else
    Begin
      First := True;
      Repeat
        Adr := LireEquation;
        If First Then
          Begin
            Loc := Adr;
            First := False
          End
      Until (Error) Or (GetCharNb(Calu) <> ',');
      If (Not Error) And (Calu <> '}') Then Erreur('Missing }')
    End;
  LireSysteme := Loc
End;

{-----------------------------------------------------------------------}
{                                                                       }
{  UN BLOC-TERME B :                                                    }
{                                                                       }
{               |-------|                                               }
{               |       |                                               }
{         B     | PrtT  |---> Pointe vers le terme T associé au bloc.   }
{               |       |                                               }
{               |-------|                                               }
{               |       |                                               }
{         B+1   | PtrS  |---> Pointe vers le terme suivant, vaut 0 si   }
{               |       |     le bloc B est le dernier de la chaîne.    }
{               |-------|                                               }
{               |       |                                               }
{         B+2   | PtrA  |---> Pointe vers la constante d'accès utilisée }
{               |       |     par la pré-unification. Vaut 0 si ce      }
{               |-------|     terme n'a pas une constante pour accès.   }
{                                                                       }
{-----------------------------------------------------------------------}


{-----------------------------------------------------------------------}
{ Function Acces (T : Integer) : Integer;                               }
{-----------------------------------------------------------------------}
{ Acces retourne l'accès du terme T (pour pré-unification). Si le terme }
{ T n'a pas une constante pour accès, la fonction retourne 0.           }
{-----------------------------------------------------------------------}

Function Acces( T : Integer ) : Integer;
Begin
  Acces := 0;
  Case Typ(T) Of
    Constante : Acces := T;
    SymboleF  : Case Typ(Memoire[T+2]) Of
                  Constante : Acces := Memoire[T+2];
                  SymboleF  : Begin
                              End
                End
  End
End;


{-----------------------------------------------------------------------}
{ Function CompilerTerme : Integer                                      }
{-----------------------------------------------------------------------}
{ CompilerTerme code un bloc-terme en mémoire.                          }
{-----------------------------------------------------------------------}

Function CompilerTerme : Integer;
Var Adr : Integer;
Begin
  Adr := AllocLeft(3);
  Memoire[Adr+0]  := LireTerme;
  Memoire[Adr+1]  := 0;                    { terme suivant  }
  Memoire[Adr+2]  := Acces(Memoire[Adr]);
  CompilerTerme := Adr
End;


{-----------------------------------------------------------------------}
{ Function CompilerSuiteDeTermes( StopCar : CharSet ) : Integer;        }
{-----------------------------------------------------------------------}
{ CompilerSuiteDeTermes code une suite de termes. Dès qu'un caractère   }
{ contenu dans StopCar est rencontré, le processus s'arrête.            }
{-----------------------------------------------------------------------}

Function CompilerSuiteDeTermes( StopCar : CharSet ) : Integer;
Var Adr : Integer;
Begin
  UnGetChar(GetCharNb(Calu));
  If (Not (Calu In StopCar)) And (Not Error) Then
    Begin
      Adr := AllocLeft(3);
      Memoire[Adr+0] := LireTerme;
      Memoire[Adr+2] := Acces(Memoire[Adr]);
      Memoire[Adr+1] := CompilerSuiteDeTermes(StopCar)
    End
  Else Adr := 0;
  CompilerSuiteDeTermes := Adr
End;

{-----------------------------------------------------------------------}
{                                                                       }
{  UNE REGLE R :                                                        }
{                                                                       }
{               |-------|                                               }
{               |       |                                               }
{         R     | PrtS  |---> Pointe vers la règle suivante, vaut 0 si  }
{               |       |     la règle R est la dernière du programme.  }
{               |-------|                                               }
{               |       |                                               }
{         R+1   | Size  |---> Taille occupée par le code de l'ensemble  }
{               |       |     des termes qui composent cette règle.     }
{               |-------|                                               }
{               |       |                                               }
{         R+2   | First |---> Adresse dans le dictionnaire de la        }
{               |       |     premiere variable de cette règle.         }
{               |-------|                                               }
{               |       |                                               }
{         R+3   | Last  |---> Adresse dans le dictionnaire de la        }
{               |       |     dernière variable de cette règle.         }
{               |-------|                                               }
{                                                                       }
{    Le premier bloc-terme de cette règle (tête) commence en R+4.       }
{                                                                       }
{-----------------------------------------------------------------------}


{-----------------------------------------------------------------------}
{ Procedure CompilerRegle;                                              }
{-----------------------------------------------------------------------}
{ CompilerRegle code une règle en mémoire.                              }
{-----------------------------------------------------------------------}

Procedure CompilerRegle;
Var Adr,AdrT,Butee : Integer;
Begin
  UnGetChar(GetCharNb(Calu));
  Adr := AllocLeft(3);
  Memoire[Adr+1] := NbVar + 1;  { Première variable dans DicoVar  }
  AdrT := CompilerTerme;        { tête                            }
  Verifier('->');
  If Not Error Then
    Begin
      Memoire[AdrT+1] := CompilerSuiteDeTermes(['{',';']);
      If Calu = '{' Then
        Begin
          Butee := PtrRight;
          LireSysteme;
          If Not ReductionSysteme(Butee,False) Then
            Erreur('Constraint cannot be satisfied')
        End;
      Verifier(';');
      Memoire[Adr+0] := PtrLeft - Adr; { Taille de la règle en Integer  }
      Memoire[Adr+2] := NbVar;         { Dernière variable dans DicoVar }
      TopVar := NbVar + 1;                    { Var locales à une règle }
    End
End;


{-----------------------------------------------------------------------}
{ Function CompilerSuiteDeRegles( StopCar : CharSet ) : Integer;       }
{-----------------------------------------------------------------------}
{ CompilerSuiteDeRegles code une suite de règles. Dès qu'un caractère   }
{ contenu dans StopCar est rencontré, le processus s'arrête.            }
{-----------------------------------------------------------------------}

Function CompilerSuiteDeRegles( StopCar : CharSet ) : Integer;
Var Adr : Integer;
Begin
  UnGetChar(GetCharNb(Calu));
  If (Not (Calu In StopCar)) And (Not Error) Then
    Begin
      Adr := AllocLeft(1);
      CompilerRegle;
      Memoire[Adr] := CompilerSuiteDeRegles(StopCar) { Règle suivante }
    End
  Else Adr := 0;
  CompilerSuiteDeRegles := Adr
End;


{-----------------------------------------------------------------------}
{ Function CompilerProgramme : Integer;                                 }
{-----------------------------------------------------------------------}
{ CompilerProgramme code un programme Prolog.                           }
{-----------------------------------------------------------------------}

Function CompilerProgramme : Integer;
Begin
  UnGetChar(GetCharNb(Calu));
  CompilerProgramme := CompilerSuiteDeRegles(['>'])
End;

{-----------------------------------------------------------------------}
{                                                                       }
{  LE SOMMET DE LA PILE PRINCIPALE µ :                                  }
{                                                                       }
{               |-------|                                               }
{               |       |                                               }
{         µ     | PrtS  |---> Pointe vers la suite des termes à         }
{               |       |     effacer.                                  }
{               |-------|                                               }
{               |       |                                               }
{         µ+1   | PtrR  |---> Pointe vers la règle à appliquer.         }
{               |       |                                               }
{               |-------|                                               }
{               |       |                                               }
{         µ+2   | PtrP  |---> Pointe, avant le lancement de la          }
{               |       |     procédure de réduction, sur le sommet de  }
{               |-------|     la pile de restauration.                  }
{               |       |                                               }
{         µ+3   | PtrB  |---> Pointe dans la pile principale vers le    }
{               |       |     sommet de l'étape précédente.             }
{               |-------|                                               }
{                                                                       }
{-----------------------------------------------------------------------}


{-----------------------------------------------------------------------}
{ Procedure Entete(a,b,c,d);                                            }
{-----------------------------------------------------------------------}
{ Entete met en entête de la pile les quatre valeurs a,b,c et d.        }
{-----------------------------------------------------------------------}

Procedure Entete(a,b,c,d : Integer );
Begin
  Push(a);               { Suite des termes à effacer }
  Push(b);               { Première règle             }
  Push(c);               { Ptr Pile Restore           }
  Push(d);               { Pointeur back              }
End;


{-----------------------------------------------------------------------}
{ Function CompilerQuestion : Integer;                                  }
{-----------------------------------------------------------------------}
{ CompilerQuestion code une question après le code des règles et met    }
{ en place l'entête.                                                    }
{-----------------------------------------------------------------------}

Function CompilerQuestion : Integer;
Var Adr : Integer;
Begin
  If GetCharNb(Calu) = '>' Then
    Begin
      FirstVar   := NbVar   + 1;
      FirstConst := NbConst + 1;
      Adr := CompilerSuiteDeTermes(['{','?']);
      If Calu = '{' Then LireSysteme;
      Verifier('?');
    End
  Else
    Erreur('No queries!');
  UnGetChar(GetCharNb(Calu));
  CompilerQuestion := Adr
End;


