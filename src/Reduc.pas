{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   Fichier     : Reduc.pas                                                   }
{   Auteur      : Christophe BISIERE                                         }
{   Date        : 07/01/88                                                   }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{         A L G O R I T H M E   D E   R E D U C T I O N   D ' U N            }
{                                                                            }
{  S Y S T E M E   D ' E Q U A T I O N S   E T   D ' I N E Q U A T I O N S   }
{                                                                            }
{      D'après "Equations et Inéquations sur les arbres finis et infinis"    }
{                                                                            }
{                      Alain COLMERAUER - Mai 1984                           }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{           F. Typ (T : Integer ) : Tterme;                                  }
{           P. Swap (Var T1,T2 : Integer);                                   }
{           P. AjouteTravail (Car : Char; T1,T2 : Integer );                 }
{           P. TrierSysteme( ButeeDroite : Integer );                        }
{           F. ReductionEquation(     BreakIt     : Boolean;                 }
{                                 Var VarProd     : Integer;                 }
{                                     ButeeDroite : Integer ) : Boolean;     }
{           F. ReductionSysteme( ButeeDroite : Integer ) : Boolean;          }
{                                                                            }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Directive de compilation : Vérifier les indices des tableaux.     }
{$V-} { Directive de compilation : Ne pas vérifier la taille des chaînes. }


Type Tterme = (Variable,Constante,SymboleF,Dummy); { Types de termes }

{-----------------------------------------------------------------------}
{ Function Typ (T : Integer ) : Tterme;                                 }
{-----------------------------------------------------------------------}
{ La fonction Typ retourne le type du terme pointé par T.               }
{-----------------------------------------------------------------------}

Function Typ( T : Integer ) : Tterme;
Begin
  Typ := Dummy;
  If T <> 0 Then
    Case Chr(Memoire[T]) Of
      'C' : Typ := Constante;
      '*' : Typ := Variable;
      'F' : Typ := SymboleF
    End;
End;


{----------------------------------------------------------------------------}
{ Procedure Swap (Var T1,T2 : Integer);                                      }
{----------------------------------------------------------------------------}
{ Swap échange le contenu de deux variables Turbo-Pascal.                    }
{----------------------------------------------------------------------------}

Procedure Swap( Var T1,T2 : Integer );
Var Ts : Integer;
Begin
  Ts := T1;
  T1 := T2;
  T2 := Ts
End;


{----------------------------------------------------------------------------}
{ Procedure AjouteTravail (Car : Char; T1,T2 : Integer );                    }
{----------------------------------------------------------------------------}
{ AjouteTravail crée une nouvelle équation (Car='=') ou inéquation (Car='<') }
{ de forme T1 = T2 dans la pile droite de la mémoire principale.             }
{                                                                            }
{----------------------------------------------------------------------------}

Procedure AjouteTravail( Car : Char; T1,T2 : Integer );
Var Adr : Integer;
Begin
  Adr := PtrRight - 1;
  AllocRight(3);
  Memoire[Adr  ] := Ord(Car);
  Memoire[Adr-1] := T1;
  Memoire[Adr-2] := T2;
End;


{----------------------------------------------------------------------------}
{ Procedure TrierSysteme( ButeeDroite : Integer );                           }
{----------------------------------------------------------------------------}
{ TrierSysteme modifie les équations et inéquations du sommet de la pile     }
{ droite (sans dépasser ButeeDroite) de telle sorte que les équations        }
{ se trouvent toutes avant les inéquations. Cette procédure est indispen-    }
{ sable parce que la procédure ReductionEquation peut, au cours de son       }
{ travail, ajouter des inéquations (remise en cause d'inéquations) à droite. }
{----------------------------------------------------------------------------}

Procedure TrierSysteme( ButeeDroite : Integer );
Var Stop : Boolean;
    I    : Integer;
Begin
  Repeat
    Stop := True;
    I := ButeeDroite - 3;
    While ( I > PtrRight) Do
      Begin
        If (Chr(Memoire[I+2]) = '=') And (Chr(Memoire[I+2-3]) = '<') Then
          Begin
            Swap(Memoire[I  ],Memoire[I - 3]);
            Swap(Memoire[I+1],Memoire[I - 2]);
            Swap(Memoire[I+2],Memoire[I - 1]);
            Stop := False;
          End;
        I := I - 3
      End;
  Until Stop;
End;

{----------------------------------------------------------------------------}
{                                                                            }
{     Algorithme de réduction (SYSTEME D'EQUATIONS)                           }
{                                                                            }
{         On veut réduire un système fini de la forme "S U T", où "S" est    }
{     déjà réduit et où "T" est l'ensemble des équations apparaissant dans   }
{     une suite finie "Z" d'équations. On considère le couple "<S,Z>" et on  }
{     le modifie autant de fois que possible par l'opération de base qui     }
{     suit. Si dans ce processus une opération de base se déroule anormale-  }
{     ment, le système initial "S U T" est insoluble. Sinon on aboutit à un  }
{     couple finial de la forme "< S' , () >". Le sous-ensemble des équa-    }
{     tions de "S" dont les membres gauches sont des variables constitue     }
{     alors un système réduit, équivalent au système initial "S U T" et      }
{     contenant toujours le système initial "S".                             }
{                                                                            }
{----------------------------------------------------------------------------}

Function ReductionEquation(     BreakIt     : Boolean;
                            Var VarProd     : Integer;
                                ButeeDroite : Integer ) : Boolean;

Var Possible    : Boolean;
    Anormal     : Boolean;
    PtrLeftSave : Integer;

        {---------------------------------------------------------}
        {                                                         }
        {  OPERATION DE BASE :                                    }
        {                                                         }
        {      Choisir dans "Z" une occurence de contrainte s'=t',}
        {  et l'enlever. Poser s = rep[s',S] et t = rep[t',S].    }
        {  Si s=t l'operation est finie, sinon trois cas se pre-  }
        {  sentent :                                              }
        {                                                         }
        {      . L'un au moins des termes de "s" où "t" est une   }
        {  variable ; on ajoute alors à "S" l'une des équations   }
        {  "s=t" où "t=s", pourvu que le membre gauche de l'équa- }
        {  tion ajoutée soit une variable ;                       }
        {                                                         }
        {      . Les termes "s" et "t" sont respectivement de la  }
        {  forme "f s1...sn" et "f t1...tn", avec "n>=1" ; On     }
        {  ajoute alors à "S" l'une des équations "s=t" ou "t=s"  }
        {  et l'on intercale, n'importe où dans "Z", la suite     }
        {  d'équations "s1=t1 ... sn=tn";                         }
        {                                                         }
        {      . Les termes "s" et "t" sont respectivement de la  }
        {  forme "f s1...sm" et "g t1...tn", "f" et "g" étant des }
        {  symboles fonctionnels distincts, avec "m>=1" et        }
        {  "n>=1"; dans ce seul cas le déroulement de l'opération }
        {  est considéré comme anormal.                           }
        {                                                         }
        {---------------------------------------------------------}

  Procedure OperationDeBase;
  Var Tg,Td : Integer;

    { Unification de deux termes }

    Procedure Unifier( Tg,Td : Integer );
    Var T1,T2 : Integer;

      { Calcul du représentant du terme pointé par Ter}

      Function Representant( Ter : Integer ) : Integer;
      Begin
        If Ter = 0 Then Representant := 0
        Else
          Case Typ(Ter) Of
            Constante : Representant := Ter;
            Variable  : If Memoire[Ter+2] = 1 Then
                          Representant := Representant(Memoire[Ter+4])
                        Else Representant := Ter;
            SymboleF  : If Memoire[Ter+1] <> 0 Then
                          Representant := Representant(Memoire[Ter+1])
                        Else Representant := Ter;
          End;
      End;

      { Créer une équation, de forme T1 = T2, dans le système réduit }

      Procedure CreerLiaison( T1,T2 : Integer );
      Var P : Integer;
      Begin
        If Typ(T1) = Variable Then
          Begin
            SetMem(T1+2,1);   { Une Equation }
            SetMem(T1+4,T2);  { Liaison      }

            { L'Etape2 de la résolution de système est traitée ici }

            If Memoire[T1+3] = 1 Then { x surveillait déjà une liaison ! }
              Begin
                SetMem(T1+3,0);
                P := Memoire[T1 + 5];
                Repeat
                  AjouteTravail('<',Memoire[P],Memoire[P+1]);
                  P := Memoire[P + 2];
                Until P = 0
              End;

          End
        Else
          Memoire[T1+1] := T2;
      End;


      { Création d'une liaison dans le système réduit de la forme x = terme }

      Procedure Production(T1,T2 : Integer);
      Begin
        If BreakIt Then  { Breaker la réduction si production x = terme }
          VarProd := T1
        Else
          CreerLiaison(T1,T2);
      End;

      { Test de deux pointeurs }

      Function Test( T1,T2 : Integer ) : Integer;
      Begin
        If (Typ(T1) = Variable) Then
          If (Typ(T2) = Variable) Then Test := 3  { Deux variables        }
          Else Test := 1                          { Une seule variable T1 }
        Else
          If (Typ(T2) = Variable) Then Test := 2  { Une seule variable T2 }
          Else Test := 0 ;                        { Pas de variable       }
      End;

    Begin     { Unifier }
      T1 := Representant(Tg);  { Représentant du premier terme  }
      T2 := Representant(Td);  { Représentant du deuxième terme }
      If (T1<>T2) And
         Not( (Typ(T1)=Constante) And (Typ(T2)=Constante) And
            (Memoire[T1+1] = Memoire[T2+1]) ) Then    { Deux termes différents }
        Case Test(T1,T2) Of
          0 : If (Typ(T1) = SymboleF) And (Typ(T2) = SymboleF) Then
                Begin
                  CreerLiaison(T1,T2);{ Créer l'équation dans le système réduit }
                  Push(T1);           { Sauve liaison terme = terme }
                  AjouteTravail('=',Memoire[T1+3],Memoire[T2+3] ); { Nouvelle équation  }
                  AjouteTravail('=',Memoire[T1+2],Memoire[T2+2] ); { Nouvelle équation  }
                End
              Else                     { Deux constantes différentes }
                Anormal := True;
          1 : Production(T1,T2);
          2 : Production(T2,T1);
          3 : Begin
                If T2 < T1 Then Swap(T1,T2); { Ordonne T1 et T2 }
                Production(T1,T2)
              End
        End;
    End;      { End Unifier }

  Begin                          { OperationDeBase }
    Tg := Memoire[PtrRight+1];
    Td := Memoire[PtrRight  ];
    PtrRight := PtrRight + 3;
    Unifier(Tg,Td);
  End;                           { End OperationDeBase }

Begin
  VarProd     := 0;
  PtrLeftSave := PtrLeft;
  Anormal     := False;
  Repeat
    Possible := PtrRight <> ButeeDroite ;
    TrierSysteme(ButeeDroite); { Because Etape2 traitée en même temps ici }
    If Possible Then
      Possible := Chr(Memoire[PtrRight+2]) = '=';
    If Possible Then
      OperationDeBase
  Until Not(Possible) Or (Anormal) Or (BreakIt And (VarProd<>0));
  While (PtrLeft > PtrLeftSave) Do
    Begin
      Memoire[Memoire[PtrLeft]+1] := 0; { Défait liaison }
      PtrLeft := PtrLeft - 1
    End;
  ReductionEquation := Not Anormal;
End;

{----------------------------------------------------------------------------}
{                                                                            }
{     Algorithme de réduction (SYSTEME D'EQUATIONS ET D'INEQUATIONS)         }
{                                                                            }
{         On veut réduire un ensemble fini de contraintes qui est de la      }
{     forme ( S= U S<> ) U ( Z'= U Z'<> ), où ( S= U S<> ) est déjà réduit,  }
{     où Z'= est l'ensemble des équations apparaissant dans une suite finie  }
{     Z= d'équations et où Z'<> est l'ensemble des contraintes du type <>    }
{     apparaissant dans une suite finie Z<> de contraintes de type <>.       }
{                                                                            }
{         L'algorithme se déroule en trois étapes :                          }
{                                                                            }
{----------------------------------------------------------------------------}

Function ReductionSysteme( ButeeDroite : Integer ) : Boolean;
Var Echec : Boolean;

{---------------------------------------------------------}
{                                                         }
{  ETAPE 1 : On modifie l'ensemble S= en appliquant       }
{  l'algorithme de réduction d'équations sur le couple    }
{  < S= , Z= >. Si l'algorithme échoue, le système        }
{  initial n'est pas soluble et le processus s'arrête là. }
{                                                         }
{---------------------------------------------------------}

  Procedure Etape1;
  Var DummyVar : Integer;
  Begin
    If Not ReductionEquation(False,DummyVar,ButeeDroite) Then Echec := True
  End;

{---------------------------------------------------------}
{                                                         }
{  ETAPE 2 : On modifie l'ensemble S<> et la suite Z<>    }
{  comme suit : de l'ensemble S<> on retire chaque        }
{  contrainte de la forme s <x> t, avec x membre gauche   }
{  d'une équation de S=, et on l'insère sous la forme     }
{  s <> t dans Z<>.                                       }
{                                                         }
{---------------------------------------------------------}

{ L'étape 2 est actuellement rejetée dans la résolution d'équations }

  Procedure Etape2;
  Begin
  End;

{---------------------------------------------------------}
{                                                         }
{  ETAPE 3 : On considère le couple < S= U S<> , Z<> > et }
{  on le modifie autant de fois que possible par l'opé-   }
{  ration de base définie ci-dessous. Si dans le proces-  }
{  sus une opération de base se déroule anormalement, le  }
{  système initial est insoluble. Sinon on aboutit à un   }
{  couple final de la forme < S= U S<> , ^ >. Le système  }
{  S= U S<> constitue alors un système réduit équivalent  }
{  au système initial.                                    }
{                                                         }
{---------------------------------------------------------}

    Procedure Etape3;
    Var Possible,
        Anormal   : Boolean;

        {---------------------------------------------------------}
        {                                                         }
        {  OPERATION DE BASE :                                    }
        {                                                         }
        {      Choisir dans Z<> une occurence de contrainte s<>t, }
        {  l'enlever et appliquer l'algorithme de réduction       }
        {  d'équations sur le couple <S=,(s=t)>. Trois situations }
        {  peuvent se présenter :                                 }
        {                                                         }
        {      (1) L'algorithme de réduction d'équations termine  }
        {  sur un échec : l'operation de base est terminée.       }
        {                                                         }
        {      (2) L'algorithme de réduction d'équations est      }
        {  amené à ajouter à S= une équation de la forme x = r :  }
        {  au lieu de cela on ajoute à S<> la contrainte s<x>t.   }
        {                                                         }
        {      (3) Ni la situation (1), ni la situation (2) ne    }
        {  se présentent et on aboutit à la configuration finale  }
        {  < S=,^ > : l'opération de base se déroule anormal-     }
        {  ement.                                                 }
        {                                                         }
        {---------------------------------------------------------}

      Procedure OperationDeBase;
        Var    P            : Integer;
               Tg,Td        : Integer;
               VarProd      : Integer;
               Butee        : Integer;
               Ok           : Boolean;
      Begin
        Memoire[PtrRight+2] := Ord('='); { Transforme dernière inéq. en éq. }
        Tg := Memoire[PtrRight+1];
        Td := Memoire[PtrRight];
        Butee    := PtrRight + 3;           { Traite juste une équation }
        Ok       := ReductionEquation(True,VarProd,Butee);
        PtrRight := Butee;
        If Ok Then
        Begin
          If VarProd<>0 Then
            Begin
              If Memoire[VarProd+3] <> 1 Then
                Begin
                  SetMem(VarProd+3,1); { Une inéquation }
                  SetMem(VarProd+5,0);
                End;
              P := VarProd + 3;
              While(Memoire[P+2]<>0) Do P := Memoire[P+2];
              SetMem(P+2,PtrLeft + 1);
              Push(Tg);
              Push(Td);
              Push(0)
            End
          Else Anormal := True;
        End
      End;

    Begin
      Anormal  := False;
      Repeat
        Possible := PtrRight <> ButeeDroite;
        If Possible Then
          OperationDeBase
      Until Not(Possible) Or Anormal;
      Echec    := Anormal;
    End;

Begin
  TrierSysteme( ButeeDroite );
  Echec := False;
  Etape1;
  If Not Echec Then
    Begin
      Etape2;
      Etape3
    End;
  PtrRight := ButeeDroite;
  ReductionSysteme := Not Echec;
End;

