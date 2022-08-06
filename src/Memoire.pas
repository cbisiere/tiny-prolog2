{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   Fichier     : Memoire.pas                                                 }
{   Auteur      : Christophe BISIERE                                         }
{   Date        : 07/01/88                                                   }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{     G E S T I O N   D E   L A   M E M O I R E   P R I N C I P A L E        }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{      P. AllocLeft ( I : Integer );        Allocation pile gauche           }
{      P. Push ( V : Integer );             Empiler sur pile gauche          }
{      P. Pop ( Var V : Integer );          Dépiler pile gauche              }
{      P. AllocRight( I : Integer );        Allocation pile droite           }
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

Var Memoire  : Array[1..SizeMem] Of Integer; { Mémoire principale            }
    PtrLeft  : Integer;                      { Pointeur pile gauche          }
    PtrRight : Integer;                      { Pointeur pile droite          }


{----------------------------------------------------------------------------}
{ Procedure AllocLeft ( I : Integer );                                       }
{----------------------------------------------------------------------------}
{ AllocLeft alloue dans la partie gauche du tableau Memoire I cases.         }
{----------------------------------------------------------------------------}

Procedure AllocLeft( I : Integer );
Begin
  PtrLeft := PtrLeft + I;
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
{ Procedure Pop ( Var V : Integer );                                         }
{----------------------------------------------------------------------------}
{ Pop retourne la valeur qui est au sommet de la partie gauche du tableau    }
{ Memoire, puis décrémente PtrLeft.                                          }
{----------------------------------------------------------------------------}

Procedure Pop( Var V : Integer );
Begin
  V := Memoire[PtrLeft];
  PtrLeft := PtrLeft - 1
End;


{----------------------------------------------------------------------------}
{ Procedure AllocRight( I : Integer );                                       }
{----------------------------------------------------------------------------}
{ AllocRight alloue dans la partie droite du tableau Memoire I cases.        }
{----------------------------------------------------------------------------}

Procedure AllocRight( I : Integer );
Begin
  PtrRight := PtrRight - I;
End;

