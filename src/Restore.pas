{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   Fichier     : Restore.pas                                                 }
{   Auteur      : Christophe BISIERE                                         }
{   Date        : 07/01/88                                                   }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{    G E S T I O N   D E   L A   P I L E   D E   R E S T A U R A T I O N     }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{      P. InitRestore;                   Initialisation pile                 }
{      P. PushRestore (A,V : Integer);   Empile doublet (Adresse,Valeur)     }
{      P. SetMem( A,V : Integer );       Affecte et sauve                    }
{      P. Restore( P : Integer );        Restaure mémoire                    }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Directive de compilation : Vérifier les indices des tableaux.     }
{$V-} { Directive de compilation : Ne pas vérifier la taille des chaînes. }


Const MaxSizeRestore = 3000;                   { Taille pile de restauration }

Var PileRestore : Array[1..MaxSizeRestore] Of  { Pile de restauration        }
      Record
        Ad    : Integer;                       { Sauve adresse               }
        Value : Integer                        { Sauve valeur                }
      End;

Var PtrRestore : Integer;                      { Sommet de la pile           }

{----------------------------------------------------------------------------}
{ Procedure InitRestore;                                                     }
{----------------------------------------------------------------------------}
{ Initialise la pile de restauration (pile vide).                            }
{----------------------------------------------------------------------------}

Procedure InitRestore;
Begin
  PtrRestore := 0
End;


{----------------------------------------------------------------------------}
{ Procedure PushRestore (A,V : Integer);                                     }
{----------------------------------------------------------------------------}
{ Met un doublet (Adresse,Valeur) au sommet de la pile de restauration.      }
{----------------------------------------------------------------------------}

Procedure PushRestore( A,V : Integer );
Begin
  PtrRestore := PtrRestore + 1;
  PileRestore[PtrRestore].Ad    := A;
  PileRestore[PtrRestore].Value := V
End;


{----------------------------------------------------------------------------}
{ Procedure SetMem( A,V : Integer );                                         }
{----------------------------------------------------------------------------}
{ SetMem affecte la case A du tableau Memoire avec la valeur V, en sauvant   }
{ préalablement l'ancienne valeur de la case A dans la pile de restauration. }
{----------------------------------------------------------------------------}

Procedure SetMem( A,V : Integer );
Begin
  PushRestore(A,Memoire[A]);
  Memoire[A] := V
End;


{----------------------------------------------------------------------------}
{ Procedure Restore( P : Integer );                                          }
{----------------------------------------------------------------------------}
{ Restore restaure le tableau Memoire en utilisant tous les doublets (A,V),  }
{ du sommet de la pile de restauration à P+1.                                }
{----------------------------------------------------------------------------}

Procedure Restore( P : Integer);
Begin
  While PtrRestore > P Do
    Begin
      Memoire[PileRestore[PtrRestore].Ad] := PileRestore[PtrRestore].Value;
      PtrRestore := PtrRestore - 1
    End
End;

