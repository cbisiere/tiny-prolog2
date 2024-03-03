{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Reduc.pas                                                  }
{   Author      : Christophe Bisiere                                         }
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
{               (quoted citations below are from this paper)                 }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Unit Reduc;

Interface

Uses
  Errs,
  Memory,
  PObj,
  PObjRest,
  PObjEq,
  PObjTerm;

Function ReduceSystem( S : SysPtr;
    Backtrackable : Boolean; Var L : RestorePtr ) : Boolean;
Function ReduceEquations( E : EqPtr ) : Boolean;
Function ReduceOneEq( T1,T2 : TermPtr ) : Boolean;
Function ReduceOneIneq( T1,T2 : TermPtr ) : Boolean;

Implementation
{-----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{                                                                            }
{     Reduction algorithm of a system of equations on trees:                 }
{                                                                            }
{     "It is proposed to reduce a finite system of the form S U T, where     }
{     S is already reduced and where T is the set of equations occurring     }
{     in a finite sequence Z of equations. The pair <S,Z> is repeatedly      }
{     modified using the "basic operation" which is defined below.           }
{     If in the way, a basic operation is not executed normally then the     }
{     initial system S U T is unsolvable. Otherwise the final result is a    }
{     pair of the form  < S',() >. The subset of S', constituted from the    }
{     equations whose left hand sides are variables, is then a reduced       }
{     system equivalent to the initial system S U T and contains the         }
{     original system S." (p.94)                                             }
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
        {  BASIC OPERATION:                                       }
        {                                                         }
        { "Basic operation: Choose in Z any occurrence of an      }
        { equation s'=t' and remove it. Let s = rep[s',S] and     }
        { t = rep[t',S]. If s=t the operation terminates,         }
        { otherwise there are 3 cases:                            }
        { - at least one of the terms s and t is a variable;      }
        {   in this case add to S one of the equations s=t or     }
        {   t=s, provided that the left hand side of the added    }
        {   equation is a variable;                               }
        { - the terms s and t are respectively of the form        }
        {   f s1...sm and g t1...tn, with n>=1; in this case add  }
        {   to S one of the equations s=t or t=s and insert,      }
        {   anywhere in Z, the sequence of equations              }
        {   s1=t1 ... sn=tn;                                      }
        { - the terms s and t are respectively of the form        }
        {   f s1...sm and g t1...tn, where f and g are distinct   }
        {   functional symbols, with m>=1 and n>=1; in this case  }
        {   alone, we consider the execution of the basic         }
        {   operation to be abnormal." (p.94)                     }
        {                                                         }
        {---------------------------------------------------------}

  Procedure BasicOperation;
  Var
    E : EqPtr;

    { unify two terms }
    Procedure Unify( Tg,Td : TermPtr );
    Var 
      T1,T2 : TermPtr;
      OT1 : TObjectPtr Absolute T1;
      VT1 : VarPtr Absolute T1;
      FT1 : FuncPtr Absolute T1;
      FT2 : FuncPtr Absolute T2;
      E : EqPtr;

      { add an equation V1 = T2 in the reduced system }
      Procedure CreateLiaison( V1 : VarPtr; T2 : TermPtr );
      Var
        OV1 : TObjectPtr Absolute V1;
      Begin
        SetMem(L,OV1,V1^.TV_TRED,T2,Backtrackable);  { add v=t in the reduced system }

        { step 2 of system solving is handled here}
        If WatchIneq(V1) <> Nil Then { x already watched a liaison }
        Begin
          CopyAllEqInSys(S,WatchIneq(V1));
          SetMemEq(L,OV1,V1^.TV_FWAT,Nil,Backtrackable)
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
            { add "f = f" to the reduced system; must always be undone }
            SetMem(Uf,OT1,FT1^.TF_TRED,T2,True);
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
{   Reduction algorithm of a system of equations and inequations on trees:   }
{                                                                            }
{   "Simplification algorithm: We wish to simplify a system of the form      }
{   (Se U Si) U (Te U Ti), where the system Se U Si is already simplified    }
{    and where Te U Ti is the complete set of equations and inequations      }
{   occurring in a finite sequence Ze of equations and a finite sequence Zi  }
{   of inequations. The indices e and i are systematically used to specify   }
{   respectively equation parts and inequation parts. The algorithm consists }
{   of transforming the pair <Se U Si,Te U Ti> in three steps:" (p.97)       }
{                                                                            }
{----------------------------------------------------------------------------}

Function ReduceSystem( S : SysPtr; Backtrackable : Boolean; 
    Var L : RestorePtr ) : Boolean;
Var Fails : Boolean;

{---------------------------------------------------------}
{                                                         }
{  STEP 1:                                                }
{                                                         }
{  "We process first the equations by applying the        }
{  reduction algorithm to the pair <Se,Ze›. We obtain a   }
{  reduced system Se'. If this reduced system Se' does    }
{  not exist, the initial system (Se U Si) U (Te U Ti)    }
{  is unsolvable and the algorithm terminates. Otherwise  }
{  the pair <Se' U Si,Zi> remains to be processed."       }
{  (p.98)                                                 }
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
{  STEP 2:                                                }
{                                                         }
{  "We remove from the set Si all inequations s<>t such   }
{  that the system Se'U(s<>t) is not in a simplified      }
{  form, and we insert them anywhere in the sequence Zi.  }
{  Let Si' and Zi' be the new resulting set and the new   }
{  resulting sequence. The pair <Se' U Si',Zi'> remains   }
{  to be processed." (p.97)                               }
{                                                         }
{  We modify the set Si and the sequence Zi as follows:   }
{  we remove from Si each constraint of the form s <x> t, }
{  where x is the left hand of an equation in Se, and we  }
{  insert s <> t in Zi.                                   }
{                                                         }
{---------------------------------------------------------}

{ step 2 is handled in the equation reduction part }
  Procedure Step2;
  Begin
  End;

{---------------------------------------------------------}
{                                                         }
{  STEP 3:                                                }
{                                                         }
{  "We consider the pair <Se' U Si', Zi'> and repeatedly  }
{  modify it by the "basic operation" which is defined    }
{  below. If a basic operation executes abnormally the    }
{  initial system (Se U Si) U (Te U Ti) is unsolvable.    }
{  Otherwise we obtain a final pair of the form           }
{  <Se'USi',()>. The system Se'USi' then constitutes a    }
{  simplified system equivalent to the initial system     }
{  (Se U Si) U (Te U Ti)." (p.97)                         }
{                                                         }
{---------------------------------------------------------}

  Procedure Step3;
  Var Abnormal   : Boolean;

      {---------------------------------------------------------}
      {                                                         }
      {  BASIC OPERATION:                                       }
      {                                                         }
      {  "Basic operation: Remove an occurrence of an           }
      {  inequation s<>t from the sequence Zi' and apply the    }
      {  reduction algorithm on the pair <Se',(s=t)>.           }
      {  Three cases are possible:                              }
      {  (1) the reduction algorithm fails to produce a reduced }
      {  system: in this case the operation terminates;         }
      {  (2) the reduced system generated is of the form        }
      {  Se' U T, with T disjoint from Se' and of the form      }
      {  T = (y1=t1, ..., Yn-tn) with n>=1: in this case we add }
      {  to the set Si' the inequation                          }
      {  b..byn...y1<>b..btn...t1;                              }
      {  (3) the reduced system generated is Se': in this case  }
      {  alone we consider the execution of the basic operation }
      {  to be abnormal." (p.97)                                }
      {                                                         }
      {  Note about (2): if the algorithm added to Se an        }
      {  equation x=r, we add to Si the constraint s<x>t.       }
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

{ reduce a single equation or inequation; reduced equations may be 
  already attached to elements in T1 or T2; non backtrackable }
Function ReduceOne( EType : EqType; T1,T2 : TermPtr ) : Boolean;
Var   
  E : EqPtr;
Begin
  E := NewEquation(EType,T1,T2);
  ReduceOne := ReduceEquations(E)
End;

{ reduce a single equation }
Function ReduceOneEq( T1,T2 : TermPtr ) : Boolean;
Begin
  ReduceOneEq := ReduceOne(REL_EQUA,T1,T2)
End;

{ reduce a single inequation }
Function ReduceOneIneq( T1,T2 : TermPtr ) : Boolean;
Begin
  ReduceOneIneq := ReduceOne(REL_INEQ,T1,T2)
End;

End.