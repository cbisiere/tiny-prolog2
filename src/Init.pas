{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   Fichier     : init.pas                                                   }
{   Auteur      : Christophe BISIERE                                         }
{   Date        : 07/01/88                                                   }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                     I N I T I A L I S A T I O N S                          }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{     P. Initialisation;                                                     }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Directive de compilation : Vérifier les indices des tableaux.     }
{$V-} { Directive de compilation : Ne pas vérifier la taille des chaînes. }


Procedure Initialisation;
Begin
  FicOpen  := False;                   { Le Fichier est Fermé        }
  Error    := False;                   { Il n'y a pas d'erreur       }
  PtrIn    := 0;                       { Init Pointeur Buffer        }
  PtrLeft  := 0;                       { Init Pointeur pile gauche   }
  PtrRight := SizeMem+1;               { Init Pointeur pile droite   }
  NbVar    := 0;                       { Dico Variables vide         }
  TopVar   := 1;                       { Recherche au début du Dico  }
  NbConst  := 0;                       { Dico Constantes vide        }
End;

