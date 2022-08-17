{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   Fichier     : Decoder.pas                                                }
{   Auteur      : Christophe BISIERE                                         }
{   Date        : 07/01/88                                                   }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                 D E C O D A G E   D E S   O B J E T S                      }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{ F. AccesTerme( T : Integer ) : Integer;                                    }
{ F. RegleSuivante( R : Integer ) : Integer;                                 }
{ F. TermeSuivant( T : Integer ) : Integer;                                  }
{ F. FirstV (R : Integer) : Integer : Integer;                               }
{ F. LastV  (R : Integer) : Integer : Integer;                               }
{ P. InitIneq;                                                               }
{ P. AddIneq (E : Integer );                                                 }
{ P. EcrireArgument (T : Integer);                                           }
{ P. EcrireTermeBis( T : Integer ; ArgList : Boolean);                       }
{ P. EcrireInequation( E : Integer; Var Before : Boolean );                  }
{ P. EcrireInequations( Var Before : Boolean );                              }
{ P. EcrireTerme( T : Integer );                                             }
{ P. EcrireSysteme( First,Last : Integer );                                  }
{ P. RestituerTerme( T : Integer );                                          }
{ P. RestituerSuiteDeTermes( T : Integer; CR : Boolean );                    }
{ P. RestituerRegle( R : Integer );                                          }
{ P. RestituerSuiteDeRegles( R : Integer );                                  }
{ P. RestituerProgramme(P : Integer);                                        }
{ P. RestituerQuestion(Q : Integer);                                         }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Directive de compilation : Vérifier les indices des tableaux.     }
{$V-} { Directive de compilation : Ne pas vérifier la taille des chaînes. }


Type StakIneq = Array[1..1000] Of Integer; { Pile sauvegarde inéquations     }

Var Ineq    : StakIneq;                    { Pile pour affichage des inéq.   }
    PtrIneq : Integer;                     { Pointeur pile des inéq.         }


{----------------------------------------------------------------}
{ Function  AccesTerme( T : Integer ) : Integer;                 }
{----------------------------------------------------------------}
{ Retourne un pointeur vers l'accès du bloc-terme T.             }
{----------------------------------------------------------------}

Function AccesTerme( T : Integer ) : Integer;
Begin
  AccesTerme := Memoire[T+2]
End;


{----------------------------------------------------------------}
{ Function  RegleSuivante( R : Integer ) : Integer;              }
{----------------------------------------------------------------}
{ Retourne un pointeur vers la règle qui suit la règle R.        }
{----------------------------------------------------------------}

Function RegleSuivante( R : Integer ) : Integer;
Begin
  RegleSuivante := Memoire[R];
End;


{----------------------------------------------------------------}
{ Function  TermeSuivant( T : Integer ) : Integer;               }
{----------------------------------------------------------------}
{ Retourne un pointeur vers le bloc-terme qui suit le bloc T.    }
{----------------------------------------------------------------}

Function TermeSuivant( T : Integer ) : Integer;
Begin
  TermeSuivant := Memoire[T+1];
End;


{----------------------------------------------------------------}
{ Function  FirstV (R : Integer) : Integer : Integer;            }
{----------------------------------------------------------------}
{ Retourne un pointeur dans DicoVar vers la première variable    }
{ locale à la règle R.                                           }
{----------------------------------------------------------------}

Function FirstV( R : Integer ) : Integer;
Begin
  FirstV := Memoire[R+2]
End;


{----------------------------------------------------------------}
{ Function  LastV  (R : Integer) : Integer : Integer;            }
{----------------------------------------------------------------}
{ Retourne un pointeur dans DicoVar vers la dernière variable    }
{ locale à la règle R.                                           }
{----------------------------------------------------------------}

Function LastV( R : Integer ) : Integer;
Begin
  LastV := Memoire[R+3]
End;

{----------------------------------------------------------------------------}
{ Procedure InitIneq;                                                        }
{----------------------------------------------------------------------------}
{ IntIneq initialise la pile des inéquations. Cette pile sert à stocker      }
{ toutes les inéquations rencontrées pour un affichage ultérieur.            }
{----------------------------------------------------------------------------}

Procedure InitIneq;
Begin
  PtrIneq := 0
End;


{----------------------------------------------------------------------------}
{ Procedure AddIneq (E : Integer );                                          }
{----------------------------------------------------------------------------}
{ AddIneq ajoute à la pile des inéquations le pointeur E qui pointe vers     }
{ une inéquation (triplet <Tg,Td,Next>).                                     }
{----------------------------------------------------------------------------}

Procedure AddIneq( E : Integer );
Begin
  PtrIneq := PtrIneq + 1;
  Ineq[PtrIneq] := E
End;


Procedure EcrireTermeBis( T : Integer ; ArgList : Boolean); Forward;


{----------------------------------------------------------------------------}
{ Procedure EcrireArgument (T : Integer);                                    }
{----------------------------------------------------------------------------}
{ EcrireArgument écrit une suite d'arguments séparés par des virgules.       }
{ (Voir Codage d'une suite d'arguments).                                     }
{----------------------------------------------------------------------------}

Procedure EcrireArgument( T : Integer );
Begin
  EcrireTermeBis(Memoire[T+2],False);
  If Memoire[T+3] <> 0 Then
    Begin
      Write(',');
      EcrireArgument(Memoire[T+3])
    End
End;


{----------------------------------------------------------------------------}
{ Procedure EcrireTermeBis( T : Integer ; ArgList : Boolean);                }
{----------------------------------------------------------------------------}
{ EcrireTermeBis écrit le terme T tout en prenant en compte le fait que      }
{ ce terme est ou n'est pas (booléen ArgList) un argument de prédicat        }
{ (nécessaire pour le parenthésage des listes imbriquées).                   }
{----------------------------------------------------------------------------}

Procedure EcrireTermeBis;
Var P     : Integer;
    Ident : StrIdent;
Begin
  Case Typ(T) Of
    Constante : Write(DicoConst[Memoire[T+1]]);
    Variable  : Begin
                  If T <= SommetProgramme Then
                    Write(DicoVar[Memoire[T+1]].Nom)
                  Else
                    If Memoire[T+2] <> 1 Then
                      Write(DicoVar[Memoire[T+1]].Nom,T)
                    Else
                      EcrireTermeBis(Memoire[T+4],False);
                  If Memoire[T+3] = 1 Then AddIneq(Memoire[T+5])
                End;
    SymboleF  : Begin
                  P := Memoire[T+2];
                  If (Typ(P) = Constante) Then
                    Begin
                      If Not (DicoConst[Memoire[P+1]][1] In Chiffre) Then
                        Begin
                          Ident := DicoConst[Memoire[P+1]];
                          If Ident = '.' Then
                            Begin
                              If ArgList Then Write('(');
                              T := Memoire[T+3];
                              EcrireTermeBis(Memoire[T+2],True);
                              Write('.');
                              T := Memoire[T+3];
                              EcrireTermeBis(Memoire[T+2],False);
                              If ArgList Then Write(')');
                            End
                          Else
                            Begin
                              Write(Ident);
                              Write('(');
                              EcrireArgument(Memoire[T+3]);
                              Write( ')');
                            End;
                        End
                    End
                  Else
                    Begin
                      Write('<');
                      EcrireArgument(T);
                      Write( '>');
                    End;
                End;
  End;
End;

Procedure EcrireTerme( T : Integer ); Forward;

{----------------------------------------------------------------------------}
{ Procedure EcrireInequation( E : Integer; Var Before : Boolean );           }
{----------------------------------------------------------------------------}
{ Restitution des Inequations de la chaîne pointée par E.                    }
{----------------------------------------------------------------------------}

Procedure EcrireInequation( E : Integer; Var Before : Boolean );
Begin
  If E <> 0 Then
    Begin
      If Before Then Write(' , ');
      Before := True;
      EcrireTerme(Memoire[E]);
      Write(' <> ');
      EcrireTerme(Memoire[E+1]);
      EcrireInequation(Memoire[E+2],Before)
    End
End;


{----------------------------------------------------------------------------}
{ Procedure EcrireInequations( Var Before : Boolean );                       }
{----------------------------------------------------------------------------}
{ Restitution de toutes les inéquations de la pile Ineq.                     }
{----------------------------------------------------------------------------}

Procedure EcrireInequations( Var Before : Boolean );
Var I,K : Integer;
Begin
  K := PtrIneq;
  For I := 1 To K Do EcrireInequation(Ineq[I],Before)
End;


{----------------------------------------------------------------------------}
{ Procedure EcrireTerme( T : Integer );                                      }
{----------------------------------------------------------------------------}
{ Ecriture d'un terme qui n'est pas un argument de prédicat.                 }
{----------------------------------------------------------------------------}

Procedure EcrireTerme;
Begin
  EcrireTermeBis(T,False)
End;


{----------------------------------------------------------------------------}
{  Procedure EcrireSysteme( First,Last : Integer );                          }
{----------------------------------------------------------------------------}
{ EcrireSysteme restitue une partie intéressante du système réduit. La       }
{ partie intéressante est définie comme la partie qui concerne les variables }
{ du dictionnaire comprises entre First et Last.                             }
{----------------------------------------------------------------------------}

Procedure EcrireSysteme( First,Last : Integer );
Var I       : Integer;
    P       : Integer;
    Before  : Boolean;
    Printed  : Boolean;

    Procedure CurlyBrace;
    Begin
      If not Printed Then
        Begin
          Printed := True;
          Write('{ ')
        End
    End;

Begin
  Printed := False;
  InitIneq;
  Before  := False;
  For I := First To Last Do
    Begin
      P := DicoVar[I].Ptr;
      If Memoire[P+2] = 1 Then
        Begin
          CurlyBrace;
          If Before Then Write(', ');
          Write(DicoVar[I].Nom,' = ');
          Before := True;
          EcrireTerme(Memoire[P+4])
        End;
      If Memoire[P+3] = 1 Then
        Begin
          CurlyBrace;
          AddIneq(Memoire[P+5])
        End
    End;
  EcrireInequations(Before);
  If Printed Then
    Write(' }')
End;


{----------------------------------------------------------------------------}
{ Procedure RestituerTerme( T : Integer );                                   }
{----------------------------------------------------------------------------}
{ Restitution du terme d'un bloc-terme pointé par T.                         }
{----------------------------------------------------------------------------}

Procedure RestituerTerme( T : Integer );
Begin
  EcrireTerme(Memoire[T])
End;


{----------------------------------------------------------------------------}
{ Procedure RestituerSuiteDeTermes( T : Integer; CR : Boolean );             }
{----------------------------------------------------------------------------}
{ Restitution des termes d'une suite de bloc-termes.                         }
{----------------------------------------------------------------------------}

Procedure RestituerSuiteDeTermes( T : Integer; CR : Boolean );
Begin
  If T <> 0 Then
    Begin
      If CR Then
        Begin
          Writeln;
          Write('        ')
        End;
      RestituerTerme(T);
      Write(' ');
      RestituerSuiteDeTermes(Memoire[T+1],CR)
    End
End;


{----------------------------------------------------------------------------}
{ RestituerRegle( R : Integer );                                             }
{----------------------------------------------------------------------------}
{ Restitution de la règle pointée par R.                                     }
{----------------------------------------------------------------------------}

Procedure RestituerRegle( R : Integer );
Begin
  InitIneq;
  RestituerTerme(R+4);
  Write(' ->  ');
  RestituerSuiteDeTermes(Memoire[R+5],True);
  EcrireSysteme(FirstV(R),LastV(R));
  Writeln(';');
  Writeln;
End;


{----------------------------------------------------------------------------}
{ Procedure RestituerSuiteDeRegles( R : Integer );                           }
{----------------------------------------------------------------------------}
{ Restitution de la suite de règles pointée par R.                           }
{----------------------------------------------------------------------------}

Procedure RestituerSuiteDeRegles( R : Integer );
Begin
  If R <> 0 Then
    Begin
      RestituerRegle(R);
      RestituerSuiteDeRegles(Memoire[R])
    End
End;


{----------------------------------------------------------------------------}
{ Procedure RestituerProgramme(P : Integer);                                 }
{----------------------------------------------------------------------------}
{ Restitution du programme Prolog stockée à l'adresse P.                     }
{----------------------------------------------------------------------------}

Procedure RestituerProgramme(P : Integer);
Const Separ = '--------------------------------------------------------------';
Begin
  Writeln(Separ);
  Writeln;
  RestituerSuiteDeRegles(P);
  Writeln(Separ)
End;


{----------------------------------------------------------------------------}
{ Procedure RestituerQuestion(Q : Integer);                                  }
{----------------------------------------------------------------------------}
{ Restitution d'une question stockée à l'adresse Q.                          }
{----------------------------------------------------------------------------}

Procedure RestituerQuestion(Q : Integer);
Begin
  InitIneq;
  RestituerSuiteDeTermes(Q+2,False);
  EcrireSysteme(Memoire[Q],Memoire[Q+1]);
  Writeln(' ?')
End;

