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
  Solvable    : Boolean;  { Système de contraintes soluble ?              }
  EndOfClock  : Boolean;  { Fin de l'horloge ?                            }
  R           : RulePtr;
  B           : BTermPtr;
  isSys       : Boolean;
  isCut       : Boolean;
  GCCount     : Integer;  { counter to trigger GC }

  { init the clock }
  Procedure InitClock;
  Begin
    P^.PP_HEAD := Nil;
    EndOfClock := False;        { Ce n'est pas encore la fin !              }
    GCCount := 0
  End;

  { display constraints only about the variables in the query }
  Procedure WriteSolution;
  Begin
    InitIneq;
    OutSystem(Q^.QU_FVAR,Q^.QU_LVAR,True);
    WriteLn
  End;

  { are two terms possibly unifiable? if not, there is not point in copying
  a rule, etc. }
  Function Unifiable( T1,T2 : TermPtr ) : Boolean;
  Var 
    Ok : Boolean;
    CT1 : ConstPtr Absolute T1;
    CT2 : ConstPtr Absolute T2;
  Begin
    CheckCondition((T1<>Nil) Or (T2<>Nil),'Call to Unifiable with two Nil terms');
    Ok := (T1=T2) Or (T1=Nil) Or (T2=Nil);
    If Not Ok Then
      If (TypeOfTerm(T1)=Constant) And (TypeOfTerm(T2)=Constant) Then
        Ok := CT1^.TC_DCON = CT2^.TC_DCON;
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
    C1 : ConstPtr;
    TC1 : TermPtr Absolute C1;
    C2 : ConstPtr;
    TC2 : TermPtr Absolute C2;
    Stop : Boolean;
  Begin
    FirstR := Nil;
    isSys := False;
    isCut := False;
    If B <> Nil Then
    Begin
      Stop := False;
      C1 := AccessTerm(B);
      If TypeOfTerm(TC1) = Constant Then { FIXME: if it is not a constant: the query is a variable "x" -- do we want to handle this?}
      Begin
        If ConstEqualTo(C1,'SYSCALL') Then
        Begin
          isSys := True;
          Stop := True
        End
        Else
        If ConstEqualTo(C1,'!') Then
        Begin
          isCut := True;
          Stop := True
        End
      End;
      While (R<>Nil) And Not Stop Do
      Begin
        C2 := AccessTerm(R^.RU_FBTR); { FIXME: check constant? Otherwise the rule head is a variable -- not parsable}
        If Unifiable(TC1,TC2) Then
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
    E : EqPtr;
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
      WriteSolution;
    Backtracking(H,EndOfClock)
  End;

Begin
  InitClock;
  B := Q^.QU_FBTR;
  If B = Nil Then { no terms to clear in the query }
  Begin
    WriteSolution;
    Exit
  End;
  R := FirstCandidateRule(Q^.QU_FRUL, B, isSys, isCut); 
  If (R = Nil) And (Not isSys) And (Not isCut) Then
  Begin
    Exit
  End;
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
