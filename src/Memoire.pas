{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   Fichier     : Memoire.pas                                                }
{   Auteur      : Christophe BISIERE                                         }
{   Date        : 07/01/88                                                   }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{     G E S T I O N   D E   L A   M E M O I R E   P R I N C I P A L E        }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{      F. AllocLeft ( I : Integer ) : Integer;   Allocation pile gauche      }
{      P. Push ( V : Integer );                  Empiler sur pile gauche     }
{      P. Pop ( Var V : Integer );               Dépiler pile gauche         }
{      F. AllocRight( I : Integer ) : Integer;   Allocation pile droite      }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Directive de compilation : Vérifier les indices des tableaux.     }
{$V-} { Directive de compilation : Ne pas vérifier la taille des chaînes. }


{----------------------------------------------------------------------------}
{                                                                            }
{      Le tableau Memoire représente le memoire Principale utilisée par      }
{      l'interpréteur Prolog. Il contiendra notamment :                      }
{                                                                            }
{   (1) Dans sa partie gauche :                                              }
{                                                                            }
{         - Les règles du programme ;                                        }
{         - Le système réduit (codé dans les termes des règles) ;            }
{         - La question posée par l'utilisateur ;                            }
{         - Toutes les recopies des règles nécessaires à l'horloge Prolog ;  }
{         - Les doublets Terme = Terme, éliminés après chaque réduction ;    }
{                                                                            }
{   (1) Dans sa partie droite :                                              }
{                                                                            }
{         - Les équations et inéquations que la procédure de réduction devra }
{           traiter ;                                                        }
{                                                                            }
{----------------------------------------------------------------------------}


Const SizeMem   = 15000;                     { Taille de la mémoire          }
      Undefined = -1;                        { Valeur indéfinie              }
      TERM_F = SizeMem + 1;                  { Value for 'type func. symb.'' }
      TERM_C = SizeMem + 2;                  { Value for 'type constant'     }
      TERM_V = SizeMem + 3;                  { Value for 'type variable'     }
      REL_EQUA = SizeMem + 4;                { Value for 'equation'          }
      REL_INEQ = SizeMem + 5;                { Value for 'equation'          }

Var Memoire  : Array[1..SizeMem] Of Integer; { Mémoire principale            }
    PtrLeft  : Integer;                      { Pointeur pile gauche          }
    PtrRight : Integer;                      { Pointeur pile droite          }


{----------------------------------------------------------------------------}
{ Function AllocLeft ( I : Integer ) : Integer;                              }
{----------------------------------------------------------------------------}
{ AllocLeft alloue dans la partie gauche du tableau Memoire I cases.         }
{----------------------------------------------------------------------------}

Function AllocLeft( I : Integer ) : Integer;
Var K : Integer;
Begin
  AllocLeft := PtrLeft + 1;
  PtrLeft := PtrLeft + I;
  For K :=  PtrLeft-I+1 To PtrLeft Do
    Memoire[K] := Undefined;
End;


{----------------------------------------------------------------------------}
{ Procedure Push ( V : Integer );                                            }
{----------------------------------------------------------------------------}
{ Push ajoute au sommet de la partie gauche du tableau Memoire la valeur V.  }
{----------------------------------------------------------------------------}

Procedure Push( V : Integer );
Begin
  PtrLeft := PtrLeft + 1;
  Memoire[PtrLeft] := V
End;


{----------------------------------------------------------------------------}
{ Function AllocRight( I : Integer ) : Integer;                              }
{----------------------------------------------------------------------------}
{ AllocRight alloue dans la partie droite du tableau Memoire I cases.        }
{----------------------------------------------------------------------------}

Function AllocRight( I : Integer ) : Integer;
Var K : Integer;
Begin
  AllocRight := PtrRight - 1;
  PtrRight := PtrRight - I;
  For K :=  PtrRight To PtrRight+I-1 Do
    Memoire[K] := Undefined;
End;

