{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : init.pas                                                   }
{   Author      : Christophe Bisière                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                     I N I T I A L I S A T I O N S                          }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Procedure Initialisation;
Begin
  FileIsOpen  := False;                { Le Fichier est Fermé        }
  Error    := False;                   { Il n'y a pas d'erreur       }
  PtrIn    := 0;                       { Init Pointeur Buffer        }
  PtrLeft  := LoMemAddr-1;             { Init Pointeur pile gauche   }
  PtrRight := HiMemAddr+1;             { Init Pointeur pile droite   }
  NbVar    := 0;                       { DictVar vide                }
  NbConst  := 0;                       { DictConst vide              }
End;
