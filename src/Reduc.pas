{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Reduc.pas                                                  }
{   Author      : Christophe Bisière                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
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

Function Reduce(S : SysPtr;
                      BreakIt       : Boolean;
                  Var VarProd       : VarPtr;
                      Backtrackable : Boolean;
                  Var L : RestorePtr) : Boolean;

Var
  Abnormal : Boolean;
  Uf : RestorePtr; { to undo "f = g" equations in the reduced system }

        {---------------------------------------------------------}
        {                                                         }
        {  OPERATION DE BASE :                                    }
        {                                                         }
        {      Choisir dans "Z" une occurence de contrainte s'=t',}
        {  et l'enlever. Poser s = rep[s',S] et t = rep[t',S].    }
        {  Si s=t l'operation est finie, sinon trois cas se pre-  }
        {  sentent :                                              }
        {                                                         }
        {     (1) L'un au moins des termes de "s" où "t" est une   }
        {  variable ; on ajoute alors à "S" l'une des équations   }
        {  "s=t" où "t=s", pourvu que le membre gauche de l'équa- }
        {  tion ajoutée soit une variable ;                       }
        {                                                         }
        {    (2) Les termes "s" et "t" sont respectivement de la  }
        {  forme "f s1...sn" et "f t1...tn", avec "n>=1" ; On     }
        {  ajoute alors à "S" l'une des équations "s=t" ou "t=s"  }
        {  et l'on intercale, n'importe où dans "Z", la suite     }
        {  d'équations "s1=t1 ... sn=tn";                         }
        {                                                         }
        {    (3) Les termes "s" et "t" sont respectivement de la  }
        {  forme "f s1...sm" et "g t1...tn", "f" et "g" étant des }
        {  symboles fonctionnels distincts, avec "m>=1" et        }
        {  "n>=1"; dans ce seul cas le déroulement de l'opération }
        {  est considéré comme anormal.                           }
        {                                                         }
        {---------------------------------------------------------}

  Procedure BasicOperation;
  Var
    E : EqPtr;

    { unify two terms }
    Procedure Unify( Tg,Td : TermPtr );
    Var 
      T1,T2 : TermPtr;
      F1,F2 : FuncPtr;
      E : EqPtr;

      { representative of a term }
      Function RepresentativeOf( T : TermPtr ) : TermPtr;
      Begin
        If T = Nil Then
          RepresentativeOf := Nil
        Else
          Case TypeOfTerm(T) Of
          Constant :
            RepresentativeOf := T;
          Variable  :
            If VarPtr(T)^.TV_TRED <> Nil Then
              RepresentativeOf := RepresentativeOf(VarPtr(T)^.TV_TRED)
            Else
              RepresentativeOf := T;
          FuncSymbol  :
            If FuncPtr(T)^.TF_TRED <> Nil Then
              RepresentativeOf := RepresentativeOf(FuncPtr(T)^.TF_TRED)
            Else
              RepresentativeOf := T
          End
      End;

      { return true if T1 and T2 are equal }
      Function SameTerms( T1,T2 : TermPtr ) : Boolean;
      Var Same : Boolean;
      Begin
        Same := T1 = T2;
        If (Not Same) And (TypeOfTerm(T1)=Constant) And (TypeOfTerm(T2)=Constant) Then
          Same := ConstPtr(T1)^.TC_CONS = ConstPtr(T2)^.TC_CONS;
        SameTerms := Same
      End;

      { swap two terms }
      Procedure SwapTerms( Var T1, T2 : TermPtr );
      Var Tmp : TermPtr;
      Begin
        Tmp := T1;
        T1 := T2;
        T2 := Tmp
      End;

      { add an equation T1 = T2 in the reduced system }
      Procedure CreateLiaison( V1 : VarPtr; T2 : TermPtr );
      Begin
        SetMem(L,Addr(V1^.TV_TRED),T2,Backtrackable);  { add v=t in the reduced system }

        { step 2 of system solving is handled here}
        If V1^.TV_FWAT <> Nil Then { x already watched a liaison }
        Begin
          CopyAllEqInSys(S,V1^.TV_FWAT);
          SetMem(L,Addr(V1^.TV_FWAT),Nil,Backtrackable)
        End
      End;

      { create a liaison "x = term" in the reduced system }
      Procedure Production( V1 : VarPtr; T2 : TermPtr );
      Begin
        If BreakIt Then  { break the reduction when a liaison "x = term" is created }
          VarProd := V1
        Else
          CreateLiaison(V1,T2)
      End;

    Begin     { Unify }
      T1 := RepresentativeOf(Tg);
      T2 := RepresentativeOf(Td);
      If Not SameTerms(T1,T2) Then 
      Begin
        { ordering: variables always first, and arbitrary order on variables (memory) }
        If (TypeOfTerm(T2)=Variable) And Not ((TypeOfTerm(T1)=Variable) And (T1<T2)) Then
          SwapTerms(T1,T2);

        { left term is a variable, thus at least one of the terms is a variable (thanks to sorting) }
        If (TypeOfTerm(T1)=Variable) Then
        Begin
          Production(VarPtr(T1),T2)
        End
        { two functional symbols }
        Else If (TypeOfTerm(T1)=FuncSymbol) And (TypeOfTerm(T2)=FuncSymbol) Then
        Begin
          { for convenience }
          F1 := FuncPtr(T1);
          F2 := FuncPtr(T2);
          { add "f = f" to the reduced system }
          SetMem(Uf,Addr(F1^.TF_TRED),T2,Backtrackable);
          { insert in the unreduced system l1=l2 and r1=r2 }
          If (RightArg(F1) <> Nil) And (RightArg(F2) <> Nil) Then
          Begin
            E := NewEq(REL_EQUA,RightArg(F1),RightArg(F2));
            InsertOneEqInSys(S,E)
          End;
          If (LeftArg(F1) <> Nil) And (LeftArg(F2) <> Nil) Then
          Begin
            E := NewEq(REL_EQUA,LeftArg(F1),LeftArg(F2));
            InsertOneEqInSys(S,E)
          End
        End
        Else
        Begin
          { cannot be unified }
          Abnormal := True
        End
      End
    End; { Unify }

  Begin { BasicOperation }
    E := RemoveOneEqFromSys(S,REL_EQUA);
    CheckCondition(E<>Nil,'Object of type REL_EQUA expected');
    Unify(E^.EQ_LTER,E^.EQ_RTER);
    FreeEq(E)
  End; { BasicOperation }

Begin { Reduce }
  VarProd := Nil;
  Uf := Nil;
  Abnormal := False;
  While (Not Abnormal) And HasEqInSys(S,REL_EQUA) And Not (BreakIt And (VarProd<>Nil)) Do
    BasicOperation;
  { remove "f = f" equations from the reduced system }
  Restore(Uf);
  Reduce := Not Abnormal;
End; { Reduce }

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

Function ReduceSystem; (* (S : SysPtr;
                       Backtrackable : Boolean; Var L : RestorePtr ) : Boolean; *)
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
  Var DummyVar : VarPtr;
  Begin
    If Not Reduce(S,False,DummyVar,Backtrackable,L) Then
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

{ step 2 is handled in the equation reduction part }
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
  Var Abnormal   : Boolean;

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
        NewE, E      : EqPtr;
        Tg,Td        : TermPtr;
        VarProd      : VarPtr;
        Ok           : Boolean;
        Ss : SysPtr;
    Begin
      { extract from Z an inequation and transform it into an equation }
      E := RemoveOneEqFromSys(S,REL_INEQ);
      CheckCondition(E<>Nil,'Object of type REL_INEQ expected');
      E^.EQ_TYPE := REL_EQUA;

      Tg := E^.EQ_LTER;
      Td := E^.EQ_RTER;

      { put it into its own little system }
      Ss := NewSys;
      InsertOneEqInSys(Ss,E);
      Ok := Reduce(Ss,True,VarProd,Backtrackable,L);
      FreeSys(Ss);

      If Ok Then
      Begin
        If VarProd<>Nil Then
        Begin
          NewE := PushEquation(REL_INEQ,Tg,Td);
          { this variable now watches this inequation }
          If VarProd^.TV_FWAT = Nil Then
          Begin
            { first watch }
            SetMem(L,Addr(VarProd^.TV_FWAT),NewE,Backtrackable)
          End
          Else
          Begin
            { add a watch }
            E := VarProd^.TV_FWAT;
            While(E^.EQ_NEXT <> Nil) Do E := E^.EQ_NEXT;
            SetMem(L,Addr(E^.EQ_NEXT),NewE,Backtrackable)
          End
        End
        Else
          Abnormal := True
      End
    End;

  Begin
    Abnormal := False;
    While HasEqInSys(S,REL_INEQ) And Not Abnormal Do
      BasicOperation;
    Fails := Abnormal;
  End;

Begin
  Fails := False;
  Step1;
  If Not Fails Then
  Begin
    Step2;
    Step3
  End;
  ReduceSystem := Not Fails
End;
