{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Restore.pas                                                }
{   Author      : Christophe Bisière                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                  R E S T O R A T I O N   S T A C K                         }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Const MaxSizeRestore = 1000;                   { Taille pile de restauration }

Var PileRestore : Array[1..MaxSizeRestore] Of  { Pile de restauration        }
  Record
    Ad    : Integer;                       { Sauve adresse               }
    Value : Integer                        { Sauve valeur                }
  End;

Var PtrRestore : Integer;                      { Sommet de la pile           }

{----------------------------------------------------------------------------}
{ Initialise la pile de restauration (pile vide).                            }
{----------------------------------------------------------------------------}

Procedure InitRestore;
Begin
  PtrRestore := 0
End;

{----------------------------------------------------------------------------}
{ Met un doublet (Adresse,Valeur) au sommet de la pile de restauration.      }
{----------------------------------------------------------------------------}

Procedure PushRestore( A,V : Integer );
Begin
  PtrRestore := PtrRestore + 1;
  CheckCondition(PtrRestore <= MaxSizeRestore,'Maximum number of restore addresses reached');
  PileRestore[PtrRestore].Ad    := A;
  PileRestore[PtrRestore].Value := V
End;

{----------------------------------------------------------------------------}
{ Affecte la case A du tableau mémoire avec la valeur V, en sauvant          }
{ si demandé préalablement l'ancienne valeur de la case A dans la pile de    }
{ restauration.                                                              }
{----------------------------------------------------------------------------}

Procedure SetMem( A,V : Integer; Backtrackable : Boolean);
Begin
  If Backtrackable Then PushRestore(A,Memory[A]);
  Memory[A] := V
End;

{----------------------------------------------------------------------------}
{ Restaure le tableau mémoire en utilisant tous les couples (A,Val),         }
{ du sommet de la pile de restauration à A+1.                                }
{----------------------------------------------------------------------------}

Procedure Restore( A : Integer);
Begin
  While PtrRestore > A Do
  Begin
    Memory[PileRestore[PtrRestore].Ad] := PileRestore[PtrRestore].Value;
    PtrRestore := PtrRestore - 1
  End
End;
