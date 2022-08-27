{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Memory.pas                                                 }
{   Author      : Christophe Bisière                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                           M A I N   M E M O R Y                            }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{----------------------------------------------------------------------------}
{                                                                            }
{      Le tableau Memory représente le memoire principale utilisée par      }
{      l'interpréteur Prolog. Il contiendra notamment :                      }
{                                                                            }
{   (1) Dans sa partie gauche :                                              }
{                                                                            }
{         - Les règles du programme ;                                        }
{         - Le système réduit (codé dans les termes des règles) ;            }
{         - Les questions posées par l'utilisateur ;                         }
{         - Toutes les recopies des règles nécessaires à l'horloge Prolog ;  }
{         - Les doublets Terme = Terme, éliminés après chaque réduction ;    }
{                                                                            }
{   (1) Dans sa partie droite :                                              }
{                                                                            }
{         - Les équations et inéquations que la procédure de réduction devra }
{           traiter ;                                                        }
{                                                                            }
{----------------------------------------------------------------------------}

Type  AnyStr    = String[254];

Procedure DumpState; Forward;
Procedure DumpHeader( H : Integer ); Forward;
Procedure CoreDump( Message : AnyStr; Trace : Boolean ); Forward;
Procedure CheckCondition( Cond : Boolean; Message : AnyStr ); Forward;

Const
  SizeMem   = 20000;               { Taille de la mémoire          }
  Undefined = -1;                  { Valeur indéfinie              }
  TERM_F = 20001;                  { Value for 'type func. symb.'' }
  TERM_C = 20002;                  { Value for 'type constant'     }
  TERM_V = 20003;                  { Value for 'type variable'     }
  REL_EQUA = 20004;                { Value for 'equation'          }
  REL_INEQ = 20005;                { Value for 'equation'          }
  NULL = 20006;
  NO = 20007;
  YES = 20008;
  SYS_CALL = 20009;                { Special rule ptr: system call }
  RTYPE_AUTO = 20010;              { Rule type: auto-loaded (system calls) }
  RTYPE_USER = 20011;              { Rule type: user }

Var
  Memory  : Array[1..SizeMem] Of Integer; { Mémoire principale            }
  PtrLeft  : Integer;                      { Pointeur pile gauche         }
  PtrRight : Integer;                      { Pointeur pile droite         }

{----------------------------------------------------------------------------}
{ Alloue dans la partie gauche du tableau mémoire Count cases.               }
{----------------------------------------------------------------------------}

Function Alloc( Count : Integer ) : Integer;
Var K : Integer;
Begin
  Alloc := PtrLeft + 1;
  PtrLeft := PtrLeft + Count;
  CheckCondition(PtrLeft < PtrRight, 'Memory exhausted');
  For K :=  PtrLeft-Count+1 To PtrLeft Do
    Memory[K] := Undefined;
End;

{----------------------------------------------------------------------------}
{ Ajoute au sommet de la partie gauche du tableau Memory la valeur Val.     }
{----------------------------------------------------------------------------}

Procedure Push( Val : Integer );
Var Adr : Integer;
Begin
  Adr := Alloc(1);
  Memory[Adr] := Val
End;

{----------------------------------------------------------------------------}
{ Retire et retourne la valeur au sommet de la pile gauche.                  }
{----------------------------------------------------------------------------}

Function Pop( Var Val : Integer ) : Integer;
Begin
  CheckCondition(PtrLeft-1 >= 0, 'Cannot pop from an empty stack');
  Val := Memory[PtrLeft];
  PtrLeft := PtrLeft - 1;
  Pop := Val
End;

{----------------------------------------------------------------------------}
{ Echange le contenu de deux variables entières.                             }
{----------------------------------------------------------------------------}

Procedure Swap( Var Val1,Val2 : Integer );
Var Tmp : Integer;
Begin
  Tmp := Val1;
  Val1 := Val2;
  Val2 := Tmp
End;

{----------------------------------------------------------------------------}
{ Echange le contenu de deux cases mémoire.                                  }
{----------------------------------------------------------------------------}

Procedure SwapMem( A1,A2 : Integer );
Begin
  Swap(Memory[A1],Memory[A2])
End;

{----------------------------------------------------------------------------}
{ Alloue dans la partie droite du tableau mémoire Count cases.               }
{----------------------------------------------------------------------------}

Function AllocRight( Count : Integer ) : Integer;
Var K : Integer;
Begin
  CheckCondition(Count>0,'Right allocation of size zero requested');
  AllocRight := PtrRight - 1;
  PtrRight := PtrRight - Count;
  CheckCondition(PtrLeft < PtrRight, 'Memory exhausted');
  For K :=  PtrRight To PtrRight+Count-1 Do
    Memory[K] := Undefined;
End;
