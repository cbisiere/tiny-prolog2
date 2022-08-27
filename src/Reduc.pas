{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Reduc.pas                                                  }
{   Author      : Christophe Bisière                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{         A L G O R I T H M   F O R   S O L V I N G   S Y S T E M S          }
{                                                                            }
{          O F   E Q U A T I O N S   A N D   I N E Q U A T I O N S           }
{                                                                            }
{     Based on "Equations and Inequations on Finite and Infinite Trees"      }
{                                                                            }
{                         Alain Colmerauer - 1984                            }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{----------------------------------------------------------------------------}
{                                                                            }
{     Algorithme de réduction (SYSTEME D'EQUATIONS)                          }
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

Function Reduce(      BreakIt       : Boolean;
                  Var VarProd       : Integer;
                      RightStopper  : Integer;
                      Backtrackable : Boolean) : Boolean;

Var
  Possible    : Boolean;
  Abnormal    : Boolean;
  PtrLeftSave : Integer;
  Z           : Integer;
  F           : Integer;

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

  Procedure BasicOperation;
  Var
    Code : Integer;
    Tg,Td : Integer;

    { Unification de deux termes }

    Procedure Unify( Tg,Td : Integer );
    Var T1,T2 : Integer;

      { Calcul du représentant d'un terme }

      Function Representant( T : Integer ) : Integer;
      Begin
        If T = NULL Then
          Representant := NULL
        Else
          Case TypeOfTerm(T) Of
          Constant :
            Representant := T;
          Variable  :
            If Memory[T+TV_IRED] = YES Then
              Representant := Representant(Memory[T+TV_TRED])
            Else
              Representant := T;
          FuncSymbol  :
            If Memory[T+TF_TRED] <> NULL Then
              Representant := Representant(Memory[T+TF_TRED])
            Else
              Representant := T
          End
      End;

      { Are two terms two identical constants? }

      Function AreTwoIdenticalConst( T1,T2 : Integer ) : Boolean;
      Var Dif : Boolean;
      Begin
        Dif := (TypeOfTerm(T1)<>Constant) Or (TypeOfTerm(T2)<>Constant);
        If Not Dif Then
          Dif := (Memory[T1+TC_CONS] <> Memory[T2+TC_CONS]);
        AreTwoIdenticalConst := Not Dif
      End;

      { Créer une équation, de forme T1 = T2, dans le système réduit }

      Procedure CreateLiaison( T1,T2 : Integer );
      Begin
        If TypeOfTerm(T1) = Variable Then
        Begin
          SetMem(T1+TV_IRED,YES,Backtrackable); { Une Equation }
          SetMem(T1+TV_TRED,T2,Backtrackable);  { Liaison      }

          { L'étape 2 de la résolution de système est traitée ici }

          If Memory[T1+TV_IWAT] = YES Then { x surveillait déjà une liaison }
          Begin
            SetMem(T1+TV_IWAT,NO,Backtrackable);
            PushEquationsToSolve(Memory[T1+TV_FWAT])
          End
        End
        Else
          Memory[T1+TF_TRED] := T2;
      End;

      { Création d'une liaison dans le système réduit de la forme x = terme }

      Procedure Production(T1,T2 : Integer);
      Begin
        If BreakIt Then  { Breaker la réduction si production x = terme }
          VarProd := T1
        Else
          CreateLiaison(T1,T2)
      End;

      { Test de deux pointeurs }

      Function Test( T1,T2 : Integer ) : Integer;
      Begin
        If (TypeOfTerm(T1) = Variable) Then
          If (TypeOfTerm(T2) = Variable) Then
            Test := 3  { Deux variables }
          Else
            Test := 1                          { Une seule variable }
        Else
          If (TypeOfTerm(T2) = Variable) Then
            Test := 2  { Une seule variable }
          Else
            Test := 0                          { Pas de variable }
      End;


    Begin     { Unify }
      T1 := Representant(Tg);  { Représentant du premier terme  }
      T2 := Representant(Td);  { Représentant du deuxième terme }
      If (T1<>T2) And Not AreTwoIdenticalConst(T1,T2) Then { Deux termes dif }
        Case Test(T1,T2) Of
        0 :
          If (TypeOfTerm(T1) = FuncSymbol)
          And (TypeOfTerm(T2) = FuncSymbol) Then
          Begin
            CreateLiaison(T1,T2);{ Créer l'équation ds le système réduit }
            Push(T1);           { Sauve liaison terme = terme }
            If (Memory[T1+TF_RTER] <> NULL) And (Memory[T2+TF_RTER] <> NULL) Then
              PushOneEquationToSolve(REL_EQUA,Memory[T1+TF_RTER],
                Memory[T2+TF_RTER] ); { Nouvelle équation  }
            If (Memory[T1+TF_LTER] <> NULL) And (Memory[T2+TF_LTER] <> NULL) Then
              PushOneEquationToSolve(REL_EQUA,Memory[T1+TF_LTER],
                Memory[T2+TF_LTER] ); { Nouvelle équation  }
          End
          Else                     { Deux constantes différentes }
            Abnormal := True;
        1 :
          Production(T1,T2);
        2 :
          Production(T2,T1);
        3 :
          Begin
            If T2 < T1 Then Swap(T1,T2); { Ordonne T1 et T2 }
            Production(T1,T2)
          End
        End
    End;      { End Unify }

  Begin                          { BasicOperation }
    PopEquationToSolve(Code,Tg,Td);
    CheckCondition(Code=REL_EQUA,'Object of type REL_EQUA expected');
    Unify(Tg,Td)
  End;                           { End BasicOperation }

Begin
  VarProd     := 0;
  PtrLeftSave := PtrLeft;
  Abnormal    := False;
  Repeat
    Possible := PtrRight <> RightStopper ;
    SortEquationsToSolve(RightStopper); { Car étape 2 traitée ici }
    If Possible Then
    Begin
      Z := TopEquationToSolve;
      Possible := Memory[Z+ZZ_TYPE] = REL_EQUA
    End;
    If Possible Then
      BasicOperation
  Until Not(Possible) Or (Abnormal) Or (BreakIt And (VarProd<>0));
  While (PtrLeft > PtrLeftSave) Do
  Begin
    F := Pop(F);
    Memory[F+TF_TRED] := NULL; { Défait liaison }
  End;
  Reduce := Not Abnormal;
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

Function ReduceSystem; (* (RightStopper  : Integer;
                       Backtrackable : Boolean ) : Boolean; *)
Var Fails : Boolean;

{---------------------------------------------------------}
{                                                         }
{  ETAPE 1 : On modifie l'ensemble S= en appliquant       }
{  l'algorithme de réduction d'équations sur le couple    }
{  < S= , Z= >. Si l'algorithme échoue, le système        }
{  initial n'est pas soluble et le processus s'arrête là. }
{                                                         }
{---------------------------------------------------------}

  Procedure Step1;
  Var DummyVar : Integer;
  Begin
    If Not Reduce(False,DummyVar,RightStopper,Backtrackable) Then
      Fails := True
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

  Procedure Step2;
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

  Procedure Step3;
  Var Possible, Abnormal   : Boolean;

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

    Procedure BasicOperation;
      Var
        Z            : Integer;
        NewE, E      : Integer;
        Tg,Td        : Integer;
        VarProd      : Integer;
        Stopper      : Integer;
        Ok           : Boolean;
    Begin
      Z := TopEquationToSolve;
      CheckCondition(Memory[Z+ZZ_TYPE]=REL_INEQ,'Object of type REL_INEQ expected');
      Memory[Z+ZZ_TYPE] := REL_EQUA; { Transforme dernière inéq. en éq. }
      Tg := Memory[Z+ZZ_LTER];
      Td := Memory[Z+ZZ_RTER];
      Stopper := PtrRight + ZZ_length;      { Traite juste une équation }
      Ok := Reduce(True,VarProd,Stopper,Backtrackable);
      PtrRight := Stopper;
      If Ok Then
      Begin
        If VarProd<>0 Then
        Begin
          NewE := PushEquation(REL_INEQ,Tg,Td);
          { Note que la variable sureille cette inéquation }
          If Memory[VarProd+TV_IWAT] = NO Then
          Begin
            { première surveillance }
            SetMem(VarProd+TV_IWAT,YES,Backtrackable);
            SetMem(VarProd+TV_FWAT,NewE,Backtrackable)
          End
          Else
          Begin
            { ajout en fin de liste }
            E := Memory[VarProd+TV_FWAT];
            While(Memory[E+EQ_NEXT] <> NULL) Do E := Memory[E+EQ_NEXT];
            SetMem(E+EQ_NEXT,NewE,Backtrackable)
          End
        End
        Else
          Abnormal := True
      End
    End;

  Begin
    Abnormal := False;
    Repeat
      Possible := PtrRight <> RightStopper;
      If Possible Then
        BasicOperation
    Until Not(Possible) Or Abnormal;
    Fails := Abnormal;
  End;

Begin
  SortEquationsToSolve( RightStopper );
  Fails := False;
  Step1;
  If Not Fails Then
  Begin
    Step2;
    Step3
  End;
  PtrRight := RightStopper;
  ReduceSystem := Not Fails;
End;
