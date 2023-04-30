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
      VT1 : VarPtr Absolute T1;
      FT1 : FuncPtr Absolute T1;
      FT2 : FuncPtr Absolute T2;
      E : EqPtr;

      { add an equation V1 = T2 in the reduced system }
      Procedure CreateLiaison( V1 : VarPtr; T2 : TermPtr ); {xxx take note if V1 is an (assignable) ident?}
      Begin
        SetMem(L,V1^.TV_TRED,T2,Backtrackable);  { add v=t in the reduced system }

        { step 2 of system solving is handled here}
        If WatchIneq(V1) <> Nil Then { x already watched a liaison }
        Begin
          CopyAllEqInSys(S,WatchIneq(V1));
          SetMemEq(L,V1^.TV_FWAT,Nil,Backtrackable)
        End;

        { assigned identifier feature: keep track of var = ident unification }
        TrackAssignment(Tg,Td)
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
        { ordering: variables always first; for two variables, an arbitrary order is 
          given by the memory management system }
        OrderTerms(T1,T2);

        { left term is a variable, thus at least one of the terms is a variable 
          (thanks to sorting) }
        If IsVariable(T1) Then
        Begin
          Production(VT1,T2)
        End
        { two functional symbols }
        Else If (TypeOfTerm(T1)=FuncSymbol) And (TypeOfTerm(T2)=FuncSymbol) Then 
        Begin
          If OneIsNil(FRightArg(FT1),FRightArg(FT2)) Or OneIsNil(FLeftArg(FT1),FLeftArg(FT2)) Then
              Abnormal := True
          Else
          Begin
            { add "f = f" to the reduced system }
            SetMem(Uf,FT1^.TF_TRED,T2,Backtrackable);
            { insert in the unreduced system l1=l2 and r1=r2 }
            If (FRightArg(FT1) <> Nil) And (FRightArg(FT2) <> Nil) Then
            Begin
              E := NewEquation(REL_EQUA,FRightArg(FT1),FRightArg(FT2));
              InsertOneEqInSys(S,E)
            End;
            If (FLeftArg(FT1) <> Nil) And (FLeftArg(FT2) <> Nil) Then
            Begin
              E := NewEquation(REL_EQUA,FLeftArg(FT1),FLeftArg(FT2));
              InsertOneEqInSys(S,E)
            End
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
    Unify(E^.EQ_LTER,E^.EQ_RTER)
  End; { BasicOperation }

Begin { Reduce }
  VarProd := Nil;
  Uf := Nil;
  Abnormal := False;
  While (Not Abnormal) And HasEqInSys(S,REL_EQUA) And Not (BreakIt And (VarProd<>Nil)) Do
    BasicOperation;
  { remove "f = f" equations from the reduced system }
  Restore(Uf);
  Uf := Nil; { free this restoration stack }
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

Function ReduceSystem( S : SysPtr;
    Backtrackable : Boolean; Var L : RestorePtr ) : Boolean;
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
      {  BASIC OPERATION:                                       }
      {                                                         }
      {  Remove from Z an occurrence of an inequation s<>t,     }
      {  and apply the reduction algorithm on the pair          }
      {  <S=,(s=t)>. Three cases are possible:                  }
      {                                                         }
      {  (1) the reduction algorithm fails to produce a reduced }
      {    system: in this case the operation terminates.       }
      {                                                         }
      {  (2) the reduced system added to S= an equation x=r.    }
      {    instead, we add to S<> the contraint s<x>t.          }
      {                                                         }
      {  (3) otherwise the system is < S=,^ >. in this case     }
      {  the execution of the basic operation is abnormal.      }
      {                                                         }
      {---------------------------------------------------------}

    Procedure BasicOperation;
      Var
        E : EqPtr;
        VarProd : VarPtr;
        Ok : Boolean;
        Ss : SysPtr;
    Begin
      { extract from Z an inequation s<>t }
      E := RemoveOneEqFromSys(S,REL_INEQ);
      CheckCondition(E<>Nil,'Object of type REL_INEQ expected');

      { check whether the corresponding equation s=t can be 
        inserted into S}
      Ss := NewSystemWithEq(E^.EQ_LTER,E^.EQ_RTER);
      Ok := Reduce(Ss,True,VarProd,Backtrackable,L);

      If Ok Then
      Begin
        If VarProd<>Nil Then
          { this variable now watches this inequation }
          AddWatch(VarProd,E,Backtrackable,L)
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

{ reduce a list of equations and inequations E; non backtrackable }
Function ReduceEquations( E : EqPtr ) : Boolean;
Var
  S : SysPtr;
  U : RestorePtr;
Begin
  S := NewSystem;
  CopyAllEqInSys(S,E);
  U := Nil;
  ReduceEquations := ReduceSystem(S,False,U)
End;

{ reduce a single equation; reduced equations may be already attached 
  to elements in T1 or T2; non backtrackable }
Function ReduceOneEq( T1,T2 : TermPtr ) : Boolean;
Var   
  E : EqPtr;
Begin
  E := NewEquation(REL_EQUA,T1,T2);
  ReduceOneEq := ReduceEquations(E)
End;
