{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Clock.pas                                                  }
{   Author      : Christophe Bisière                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                        P R O L O G   C L O C K                             }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Function ExecutionSysCallOk( T : TermPtr; P : ProgPtr; Q : QueryPtr ) : Boolean; Forward;

{----------------------------------------------------------------------------}
{                                                                            }
{                                C L O C K                                   }
{                                                                            }
{----------------------------------------------------------------------------}

{ clear a query }
Procedure Clock( P : ProgPtr; Q : QueryPtr );

Var
  Solvable    : Boolean;  { system has a solution? }
  EndOfClock  : Boolean;
  R           : RulePtr;
  B           : BTermPtr;
  isSys       : Boolean;
  isCut       : Boolean;
  GCCount     : Integer;  { counter to trigger GC }

  { init the clock }
  Procedure InitClock;
  Begin
    P^.PP_HEAD := Nil;
    EndOfClock := False;
    GCCount := 0
  End;

  { display constraints only about the variables in the query }
  Procedure WriteQuerySolution;
  Begin
    OutQuerySolution(Q);
    WriteLn
  End;

  { are two terms possibly unifiable? if not, there is not point in copying
    a rule, etc.; note that since we make sure that a given constant value 
    (identifiers, numbers, strings) is represented by exactly one term,
    comparing pointers is fine even for constants }
  Function Unifiable( T1,T2 : TermPtr ) : Boolean;
  Var 
    Ok : Boolean;
  Begin
    CheckCondition((T1<>Nil) Or (T2<>Nil),'Call to Unifiable with two Nil terms'); { FIXME: is it really a problem?}
    Ok := (T1=T2) Or (T1=Nil) Or (T2=Nil);
    Unifiable := Ok
  End;

  { returns the rule following R, or Nil if R is the last rule in the query's
    scope }
  Function Next( R : RulePtr ) : RulePtr;
  Begin
    CheckCondition(R <> Nil,'cannot call Next on Nil');
    If R = Q^.QU_LRUL Then 
      Next := Nil
    Else
      Next := NextRule(R)
  End;

  { first rule that has a chance to unify with a term, starting with rule R;
    assumes B is not a cut or a system call }
  Function FirstCandidateRule( R : RulePtr; B : BTermPtr; Var isSys : Boolean ; Var isCut : Boolean ) : RulePtr;
  Var
    FirstR : RulePtr;
    I1 : IdPtr;
    TI1 : TermPtr Absolute I1;
    I2 : IdPtr;
    TI2 : TermPtr Absolute I2;
    Stop : Boolean;
  Begin
    FirstR := Nil;
    isSys := False;
    isCut := False;
    If B <> Nil Then
    Begin
      Stop := False;
      I1 := AccessIdentifier(B^.BT_TERM); { use Access to tackle dynamic assignment of identifiers }
      If I1 <> Nil Then
      Begin
        If IdentifierEqualTo(I1,'SYSCALL') Then
        Begin
          isSys := True;
          Stop := True
        End
        Else
        If IdentifierEqualTo(I1,'!') Then
        Begin
          isCut := True;
          Stop := True
        End
      End;
      While (R<>Nil) And Not Stop Do
      Begin
        I2 := AccessTerm(R^.RU_FBTR); { FIXME: check ident? Otherwise the rule head is a variable -- not parsable}
        If Unifiable(TI1,TI2) Then
        Begin
          FirstR := R;
          Stop := True
        End;
        R := Next(R)
      End
    End;
    FirstCandidateRule := FirstR
  End;

  {----------------------------------------------------------------------------}
  { Sachant que le terme B pouvait s'unifier avec la tête de la règle R, la    }
  { fonction  retourne un pointeur vers la règle qui suit R, si la             }
  { tête de cette dernière peut s'unifier avec le terme B, et 0 dans le cas    }
  { contraire.                                                                 }
  { Cette logique impose qu'un ensemble de règles ayant même accès doit être   }
  { écrit à la suite dans le fichier source.                                   }
  {----------------------------------------------------------------------------}

  Function NextCandidateRule( R : RulePtr; B : BTermPtr; Var isSys : Boolean ; Var isCut : Boolean) : RulePtr;
  Var NextR : RulePtr;
  Begin
    If (R = Nil) Or (isSys) Or (isCut) Then
    Begin
      isSys := False;
      isCut := False;
      NextR := Nil
    End
    Else
      NextR := FirstCandidateRule(Next(R), B, isSys, isCut);
    NextCandidateRule := NextR
  End;

{----------------------------------------------------------------------------}
{ Remet l'horloge Prolog au dernier point de choix                           }
{ utilisable. Si il n'y a plus de choix à envisager, elle positionne le      }
{ booléen EndOfClock à True.                                                 }
{                                                                            }
{ Un retour en arrière se fait en trois étapes :                             }
{                                                                            }
{  (1) Dépiler la règle qui est en tête de pile grâce au pointeur qui est    }
{      disponible dans l'entête H courante.                                  }
{                                                                            }
{  (2) Restaurer la mémoire à l'état antérieur grâce au pointeur de restau-  }
{      ration de la nouvelle règle de tête.                                  }
{                                                                            }
{  (3) Positionner le pointeur de règle à appliquer sur la règle suivante    }
{      à appliquer.                                                          }
{                                                                            }
{ Si ce dernier pointeur est nul (épuisement des règles applicables) et      }
{ que ce n'est pas la fin (Clock=0), on recommence l'opération.              }
{                                                                            }
{----------------------------------------------------------------------------}

  Procedure Backtracking( Var H : HeadPtr; Var NoMoreChoices : Boolean );
  Var
    NextR : RulePtr;
    NextH : HeadPtr;
    isSys, isCut : Boolean;
  Begin
    NoMoreChoices := False;
    NextR := Nil;
    isSys := False;
    isCut := False;
    Repeat
      If (H^.HH_CLOC > 0) And (Not H^.HH_ACUT) Then
      Begin
        { backtracks one step }
        NextH := H^.HH_NEXT;
        Restore(H^.HH_REST); { restore and free restore object }
        H^.HH_REST := Nil;
        H := NextH;
        { set next rule to apply, if any }
        NextR := NextCandidateRule(H^.HH_RULE,H^.HH_FBCL,isSys,isCut)
      End
      Else
        NoMoreChoices := True
    Until NoMoreChoices Or (NextR <> Nil) Or isSys Or isCut;
    SetHeaderRule(H,NextR,isSys,isCut)
End;

{------------------------------------------------------------------}
{ Tente d'initialiser le pointeur de règle à appliquer             }
{ avec la première règle dont la tête est peut-être unifiable      }
{ avec le premier terme à effacer. Si ce terme n'a aucune chance   }
{ d'être effacé, la procédure fait un appel à Backtracking;        }
{------------------------------------------------------------------}

  Procedure FirstRule( Var H : HeadPtr );
  Var
    R : RulePtr;
    isSys, isCut : Boolean;
  Begin
    R := FirstCandidateRule(Q^.QU_FRUL,H^.HH_FBCL,isSys,isCut);
    SetHeaderRule(H,R,isSys,isCut);
    If (R = Nil) And (Not isSys) And (Not isCut) Then
      Backtracking(H,EndOfClock)
  End;

{------------------------------------------------------------------}
{ MoveForward fait avancer l'horloge Prolog d'une période. Les diffé-  }
{ rentes opérations à effectuer sont les suivantes :               }
{                                                                  }
{ (1) Recopier en tête de pile la règle courante à appliquer ;     }
{ (2) Créer les quatre pointeurs de tête ;                         }
{ (3) Ajouter à l'ensemble des contraintes l'équation              }
{                                                                  }
{    < Premier terme à effacer  =  tête de la règle recopiée >;    }
{                                                                  }
{ (4) Positionner le pointeur de termes à effacer de la manière    }
{     suivante :                                                   }
{                                                                  }
{      * Chercher le dernier bloc-terme de la règle ;              }
{      * Positionner le pointeur bloc-terme-suivant de ce bloc-    }
{        terme vers le bloc-terme suivant du premier bloc-terme    }
{        de l'ancienne suite de termes à effacer ;                 }
{      * Positionner le pointeur de termes à effacer vers le       }
{        bloc-terme suivant de la tête de la règle ;               }
{                                                                  }
{ (5) Positionner le pointeur de retour en arrière vers l'ancien   }
{     sommet de pile (avant recopie de la règle).                  }
{                                                                  }
{------------------------------------------------------------------}

  Procedure MoveForward( Var H : HeadPtr );
  Var
    ClearB, B : BTermPtr;
    ClearT : TermPtr;
    R : RulePtr;
    isCut, isSys : Boolean;
    Ss : SysPtr;
    RuleB : BTermPtr;
    PRuleB : TPObjPtr Absolute RuleB;
    CopyRuleP : TPObjPtr;
    BCopyRuleP : BTermPtr Absolute CopyRuleP;

  Begin
    GetHeaderRule(H,R,isSys,isCut); { rule to apply }

    CheckCondition(H^.HH_FBCL <> Nil,'MoveForward: No terms to clear');
    CheckCondition((R <> Nil) Or isSys Or isCut,'MoveForward: No rule to apply');

    ClearB := H^.HH_FBCL; { list of terms to clear }
    ClearT := ClearB^.BT_TERM; { current term to clear }

    PushNewClockHeader(H,Nil,Nil,False,False,False);

    If isCut Then
    Begin
      Solvable := True; { "cut" is always clearable }
      H^.HH_ACUT := True;
      H^.HH_FBCL := NextTerm(ClearB)
    End
    Else
    If isSys Then
    Begin
      Solvable := ExecutionSysCallOk(ClearT,P,Q);
      { remove the term from the list of terms to clear }
      H^.HH_FBCL := NextTerm(ClearB)
    End
    Else
    Begin
      Solvable := True;

      { if any, reduce the equations given as a system in the rule itself; 
        this must be done each time the rule is applied to take into account 
        global assignments; e.g. "go -> { test=1 )" may succeed or fail, 
        depending on the value of the identifier "test" if any; this value 
        will be 1 if a goal "assign(test,1)" has been cleared before;
        this reduction must be done *before* the rule is copied, such that
        the liaisons are copied as part of the rule itself }
      If R^.RU_SYST <> Nil Then
      Begin
        Ss := NewSystem;
        CopyAllEqInSys(Ss,R^.RU_SYST);
        Solvable := ReduceSystem(Ss,True,H^.HH_REST)
      End;

      If Solvable Then
      Begin
        { copy the terms of the target rule }
        RuleB := R^.RU_FBTR;
        CopyRuleP := DeepCopy(PRuleB);

        { contraint to reduce: term to clear = rule head }
        Ss := NewSystemWithEq(ClearT,BCopyRuleP^.BT_TERM);

        { new list of terms to clear: rule queue + previous terms but the first }
        B := BCopyRuleP;
        While (NextTerm(B)<>Nil) Do
          B := NextTerm(B);
        B^.BT_NEXT := NextTerm(ClearB);
        H^.HH_FBCL := NextTerm(BCopyRuleP);

        Solvable := ReduceSystem(Ss,True,H^.HH_REST)
      End
    End
  End;

{------------------------------------------------------------------}
{ On est ici au bout d'une feuille de l'arbre développé par        }
{ l'horloge. Si l'ensemble de contraintes est soluble c'est une    }
{ solution. Dans tous les cas on retourne au dernier point de      }
{ choix.                                                           }
{------------------------------------------------------------------}

  Procedure MoveBackward( Var H : HeadPtr );
  Begin
    If Solvable Then
      WriteQuerySolution;
    Backtracking(H,EndOfClock)
  End;

Begin
  InitClock;

  { try to reduce the system in the query, if any, and fail if it has 
    no solutions; note that the system is reduced before clearing any 
    goal, including goals that sets global variables; thus a query 
    like "assign(aa,1) { aa = 1 )" will fail right away  }
  If Q^.QU_SYST <> Nil Then
    If Not ReduceEquations(Q^.QU_SYST) Then
      Exit;

  B := Q^.QU_FBTR; { list of terms to clear }

  { no terms to clear: success }
  If B = Nil Then
  Begin
    WriteQuerySolution;
    Exit
  End;

  R := FirstCandidateRule(Q^.QU_FRUL, B, isSys, isCut);

  { not even a candidate rule to try: fail }
  If (R = Nil) And (Not isSys) And (Not isCut) Then
    Exit;

  PushNewClockHeader(P^.PP_HEAD,B,R,False,isSys,isCut);
  Repeat
    MoveForward(P^.PP_HEAD);
    If (Not Solvable) Or    { system has no solution }
        (P^.PP_HEAD^.HH_FBCL = Nil) { no more terms to clear }
    Then
      MoveBackward(P^.PP_HEAD)
    Else
      FirstRule(P^.PP_HEAD);
    { trigger GC after a certain number of steps }
    GCCount := GCCount + 1;
    If GCCount = 100 Then
    Begin
      GarbageCollector;
      GCCount := 0
    End
  Until EndOfClock;
  GarbageCollector
End;
