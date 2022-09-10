{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Unparse.pas                                                }
{   Author      : Christophe Bisière                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                              U N P A R S I N G                             }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Const MaxIneq = 1000;

Type StakIneq = Array[1..MaxIneq] Of Integer; { Pile sauvegarde inéquations   }

Var
  Ineq    : StakIneq;                    { Pile pour affichage des inéq.   }
  PtrIneq : Integer;                     { Pointeur pile des inéq.         }

{----------------------------------------------------------------}
{ Retourne un pointeur vers l'accès du bloc-terme B.             }
{----------------------------------------------------------------}

Function AccessTerm( B : Integer ) : Integer;
Begin
  AccessTerm := Memory[B+BT_CONS]
End;

{----------------------------------------------------------------}
{ Retourne un pointeur vers le bloc-terme qui suit le bloc B.    }
{----------------------------------------------------------------}

Function NextTerm( B : Integer ) : Integer;
Begin
  CheckCondition(B <> NULL,'Cannot compute the next term of NULL');
  NextTerm := Memory[B+BT_NEXT];
End;

{----------------------------------------------------------------------------}
{ Initialise la pile des inéquations. Cette pile sert à stocker              }
{ toutes les inéquations rencontrées pour un affichage ultérieur.            }
{----------------------------------------------------------------------------}

Procedure InitIneq;
Begin
  PtrIneq := 0
End;

{----------------------------------------------------------------------------}
{ Ajoute à la pile des inéquations le pointeur E qui pointe vers             }
{ une inéquation (triplet <Tg,Td,Next>).                                     }
{----------------------------------------------------------------------------}

Procedure AddIneq( E : Integer );
Begin
  PtrIneq := PtrIneq + 1;
  CheckCondition(PtrIneq <= MaxIneq,'Maximum number of inequations reached');
  Ineq[PtrIneq] := E
End;

Procedure WriteTermBis( T : Integer; ArgList,Quotes : Boolean); Forward;

{----------------------------------------------------------------------------}
{ Ecrit une suite d'arguments séparés par des virgules.                      }
{ (Voir codage d'une suite d'arguments).                                     }
{----------------------------------------------------------------------------}

Procedure WriteArgument( F : Integer );
Begin
  WriteTermBis(Memory[F+TF_LTER],False,True);
  If Memory[F+TF_RTER] <> NULL Then
  Begin
    Write(',');
    WriteArgument(Memory[F+TF_RTER])
  End
End;

{----------------------------------------------------------------------------}
{ Ecrit le nom d'une variable, utilisateur ou temporaire.                    }
{----------------------------------------------------------------------------}

Procedure WriteVarName( V : Integer );
Begin
  Case Memory[V+TV_COPY] Of
  NO:
    Write(DictVar[Memory[V+TV_NVAR]].Name);
  YES:
    Write(DictVar[Memory[V+TV_NVAR]].Name,'_',V)
  End
End;

{----------------------------------------------------------------------------}
{ Ecrit le terme T tout en prenant en compte le fait que                     }
{ ce terme est ou n'est pas (booléen ArgList) un argument de prédicat        }
{ (nécessaire pour le parenthésage des listes imbriquées).                   }
{ Quote: when False and the term to display is equal to a string,            }
{ display it without the quotes.                                             }
{----------------------------------------------------------------------------}

Procedure WriteTermBis; (* ( T : Integer; ArgList,Quotes : Boolean ); *)
Var
  LeftT : Integer;
  ConsIdx : Integer;
  Cste : StrConst;
Begin
  Case TypeOfTerm(T) Of
  Constant :
    Begin
      Cste := DictConst[Memory[T+TC_CONS]];
      If (Not Quotes) And (Length(Cste) >= 2) Then
        If (Cste[1] = '"') And (Cste[Length(Cste)] = '"') Then
          Begin
            Delete(Cste,1,1);
            Delete(Cste,Length(Cste),1)
          End;
      Write(Cste);
    End;
  Variable  :
    Begin
      If Memory[T+TV_COPY] = NO Then
        WriteVarName(T)
      Else
        If Memory[T+TV_IRED] = NO Then
          WriteVarName(T)
        Else
          WriteTermBis(Memory[T+TV_TRED],False,Quotes);
      If Memory[T+TV_IWAT] = YES Then
        AddIneq(Memory[T+TV_FWAT])
    End;
  FuncSymbol  :
    Begin
      LeftT := Memory[T+TF_LTER];
      If (TypeOfTerm(LeftT) = Constant) Then
      Begin
        ConsIdx := Memory[LeftT+TC_CONS];
        If Not (DictConst[ConsIdx][1] In Digits) Then
        Begin
          Cste := DictConst[ConsIdx];
          If Cste = '.' Then
          Begin
            If ArgList Then Write('(');
            T := Memory[T+TF_RTER];
            WriteTermBis(Memory[T+TF_LTER],True,True);
            Write('.');
            T := Memory[T+TF_RTER];
            WriteTermBis(Memory[T+TF_LTER],False,True);
            If ArgList Then Write(')')
          End
          Else
          Begin
            Write(Cste);
            Write('(');
            WriteArgument(Memory[T+TF_RTER]);
            Write( ')')
          End
        End
      End
    Else
      Begin
        Write('<');
        WriteArgument(T);
        Write( '>')
      End
    End
  End
End;

Procedure WriteTerm( T : Integer ); Forward;

{----------------------------------------------------------------------------}
{ Restitution des équations ou inequations de la chaîne pointée par E.       }
{----------------------------------------------------------------------------}

Procedure WriteEquations( E : Integer; Var Before : Boolean );
Begin
  If E <> NULL Then
  Begin
    If Before Then Write(', ');
    Before := True;
    WriteTerm(Memory[E+EQ_LTER]);
    Case Memory[E+EQ_TYPE] Of
    REL_EQUA:
      Write(' = ');
    REL_INEQ:
      Write(' <> ');
    End;
    WriteTerm(Memory[E+EQ_RTER]);
    WriteEquations(Memory[E+EQ_NEXT],Before)
  End
End;

{----------------------------------------------------------------------------}
{ Restitution de toutes les inéquations de la pile Ineq.                     }
{----------------------------------------------------------------------------}

Procedure WriteInequations( Var Before : Boolean );
Var I,K : Integer;
Begin
  K := PtrIneq;
  For I := 1 To K Do WriteEquations(Ineq[I],Before)
End;

{----------------------------------------------------------------------------}
{ Ecriture d'un terme qui n'est pas un argument de prédicat.                 }
{----------------------------------------------------------------------------}

Procedure WriteTerm; (* ( T : Integer ); *)
Begin
  WriteTermBis(T,False,True)
End;

{----------------------------------------------------------------------------}
{ Restitue une partie intéressante du système réduit. La                     }
{ partie intéressante est définie comme la partie qui concerne les variables }
{ du dictionnaire comprises entre First et Last.                             }
{----------------------------------------------------------------------------}

Procedure WriteSystem( First,Last : Integer; Curl : Boolean );
Var
  I       : Integer;
  V       : Integer;
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
  If Curl Then CurlyBrace;
  InitIneq;
  Before  := False;
  For I := First To Last Do
  Begin
    V := DictVar[I].Ptr;
    If Memory[V+TV_IRED] = YES Then
    Begin
      CurlyBrace;
      If Before Then Write(', ');
      Write(DictVar[I].Name,' = ');
      Before := True;
      WriteTerm(Memory[V+TV_TRED])
    End;
    If Memory[V+TV_IWAT] = YES Then
    Begin
      CurlyBrace;
      AddIneq(Memory[V+TV_FWAT])
    End
  End;
  WriteInequations(Before);
  If Printed Then
    Write(' }')
End;

{----------------------------------------------------------------------------}
{ Restitution du terme d'un bloc-terme pointé par B.                         }
{----------------------------------------------------------------------------}

Procedure UnparseOneTerm( B : Integer );
Begin
  WriteTerm(Memory[B+BT_TERM])
End;

{----------------------------------------------------------------------------}
{ Restitution des termes d'une suite de bloc-termes à partir de B.           }
{----------------------------------------------------------------------------}

Procedure UnparseTerms( B : Integer; CR : Boolean);
Begin
  If B <> NULL Then
  Begin
    If CR Then
    Begin
      Writeln;
      Write('        ')
    End;
    UnparseOneTerm(B);
    If Not CR Then
      Write(' ');
    UnparseTerms(Memory[B+BT_NEXT],CR)
  End
End;

{----------------------------------------------------------------------------}
{ Restitution de la règle pointée par R.                                     }
{----------------------------------------------------------------------------}

Procedure UnparseOneRule( R : Integer );
Var B : Integer;
Begin
  InitIneq;
  B := Memory[R+RU_FBTR];
  UnparseOneTerm(B);
  Write(' -> ');
  UnparseTerms(Memory[B+BT_NEXT],True);
  WriteSystem(Memory[R+RU_FVAR],Memory[R+RU_LVAR],False);
  Writeln(';');
End;

{----------------------------------------------------------------------------}
{ Restitution d'une liste de règles pointée par R.                           }
{----------------------------------------------------------------------------}

Procedure UnparseRules(R : Integer);
Begin
  if R <> NULL Then
  Begin
    UnparseOneRule(R);
    UnparseRules(Memory[R+RU_NEXT])
  End
End;

{----------------------------------------------------------------------------}
{ Restitution des règles du contexte d'une règle Q.                          }
{----------------------------------------------------------------------------}

Procedure UnparseQuestionRules( Q : Integer; RuleType : Integer );
Var
  R : Integer;
  Stop : Boolean;
Begin
  R := Memory[Q+QU_FRUL];
  Stop := R = NULL;
  While Not Stop Do
  Begin
    If Memory[R+RU_TYPE] = RuleType Then
      UnparseOneRule(R);
    Stop := R = Memory[Q+QU_LRUL];
    R := Memory[R+RU_NEXT];
    Stop := Stop Or (R = NULL)
  End
End;

{----------------------------------------------------------------------------}
{ Restitution d'une question stockée à l'adresse Q.                          }
{----------------------------------------------------------------------------}

Procedure UnparseOneQuery(Q : Integer);
Begin
  Write('-> ');
  InitIneq;
  UnparseTerms(Memory[Q+QU_FBTR],False);
  WriteSystem(Memory[Q+QU_FVAR],Memory[Q+QU_LVAR],False);
  Writeln(';')
End;

{----------------------------------------------------------------------------}
{ Restitution d'une liste de questions pointée par Q.                        }
{----------------------------------------------------------------------------}

Procedure UnparseQueries(Q : Integer);
Begin
  if Q <> NULL Then
  Begin
    UnparseOneQuery(Q);
    UnparseQueries(Memory[Q+QU_NEXT])
  End
End;

{----------------------------------------------------------------------------}
{ Restitution des questions du programme P.                                  }
{----------------------------------------------------------------------------}

Procedure UnparseProgramQueries( P : Integer );
Begin
  UnparseQueries(Memory[P+PP_FQRY])
End;


{----------------------------------------------------------------------------}
{ Restitution des règles du programme P.                                     }
{----------------------------------------------------------------------------}

Procedure UnparseProgramRules( P : Integer );
Begin
  UnparseRules(Memory[P+PP_FRUL])
End;
