{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Unparse.pas                                                }
{   Author      : Christophe Bisière                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                              U N P A R S I N G                             }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Const MaxIneq = 1000;

Type StakIneq = Array[1..MaxIneq] Of EqPtr; { Pile sauvegarde inéquations   }

Var
  Ineq    : StakIneq;                    { Pile pour affichage des inéq.   }
  PtrIneq : Integer;                     { Pointeur pile des inéq.         }



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

Procedure AddIneq( E : EqPtr );
Begin
  PtrIneq := PtrIneq + 1;
  CheckCondition(PtrIneq <= MaxIneq,'Maximum number of inequations reached');
  Ineq[PtrIneq] := E
End;

Procedure WriteTermBis( T : TermPtr; ArgList,Quotes : Boolean); Forward;

{----------------------------------------------------------------------------}
{ write a comma-separated list of arguments, coded as F(arg1,F(arg2,Nil))    }
{----------------------------------------------------------------------------}

Procedure WriteArgument( F : FuncPtr );
Begin
  WriteTermBis(F^.TF_LTER,False,True);
  If F^.TF_RTER <> Nil Then
  Begin
    Write(',');
    CheckCondition((F^.TF_RTER=Nil) Or (TypeOfTerm(F^.TF_RTER)=FuncSymbol),
      'broken argument list');
    WriteArgument(FuncPtr(F^.TF_RTER))
  End
End;

{----------------------------------------------------------------------------}
{ Retourne une constante, avec ou sans quotes.                               }
{----------------------------------------------------------------------------}

Function GetConstAsString( C : ConstPtr; Quotes : Boolean ) : AnyStr;
Var Cste : AnyStr;
Begin
  Cste := DictConst[C^.TC_CONS];
  If (Not Quotes) And (Length(Cste) >= 2) Then
    If (Cste[1] = '"') And (Cste[Length(Cste)] = '"') Then
    Begin
      Delete(Cste,1,1);
      Delete(Cste,Length(Cste),1)
    End;
  GetConstAsString := Cste
End;

{----------------------------------------------------------------------------}
{ Retourne le nom d'une variable, utilisateur ou temporaire.                 }
{----------------------------------------------------------------------------}

Function GetVarNameAsString( V : VarPtr ) : AnyStr;
Var 
  s : AnyStr;
  k : Integer;
Begin
  CheckCondition(TypeOfTerm(TermPtr(V))=Variable,
    'GetVarNameAsString(V): V is not a variable');
  k := PObjectCopyNumber(TPObjPtr(V));
  If k = 0 Then
    s := DictVar[V^.TV_NVAR].Name
  Else
  Begin
    Str(k,s);
    s := DictVar[V^.TV_NVAR].Name + '_' + s { FIXME: should be an invalid variable name }
  End;
  GetVarNameAsString := s // + '_' + PtrToName(TPObjPtr(V))
End;

{----------------------------------------------------------------------------}
{ Ecrit le nom d'une variable, utilisateur ou temporaire.                    }
{----------------------------------------------------------------------------}

Procedure WriteVarName( V : VarPtr );
Begin
  Write(GetVarNameAsString(V))
End;

{----------------------------------------------------------------------------}
{ Return the constant the term T is equal to, of Nil if T is not equal to a  }
{ constant.                                                                  }
{----------------------------------------------------------------------------}

Function EvaluateToConstant( T : TermPtr ) : ConstPtr;
Var C : ConstPtr;
Begin
  C := Nil;
  Case TypeOfTerm(T) Of
  Constant :
    C := ConstPtr(T);
  Variable :
    If VarPtr(T)^.TV_TRED <> Nil Then
      C := EvaluateToConstant(VarPtr(T)^.TV_TRED)
  End;
  EvaluateToConstant := C
End;

{----------------------------------------------------------------------------}
{ Ecrit le terme T tout en prenant en compte le fait que                     }
{ ce terme est ou n'est pas (booléen ArgList) un argument de prédicat        }
{ (nécessaire pour le parenthésage des listes imbriquées).                   }
{ Quote: when False and the term to display is equal to a string,            }
{ display it without the quotes.                                             }
{----------------------------------------------------------------------------}

Procedure WriteTuple( F : FuncPtr );
Begin
  Write('<');
  WriteArgument(F);
  Write('>')
End;

Procedure WriteTermBis; (* ( T : TermPtr; ArgList,Quotes : Boolean ); *)
Var
  V : VarPtr;
  F,F1,F2 : FuncPtr;
  C : ConstPtr;
  LeftT : TermPtr;
  ConsIdx : Integer;
  Cste : StrConst;
Begin
  Case TypeOfTerm(T) Of
  Constant :
    Begin
      C := ConstPtr(T);
      Write(GetConstAsString(C,Quotes))
    End;
  Variable  :
    Begin
      V := VarPtr(T);
      If PObjectCopyNumber(TPObjPtr(V)) = 0 Then
      Begin
        WriteVarName(V)
      End
      Else
        If V^.TV_TRED = Nil Then
          WriteVarName(V)
        Else
        Begin
          WriteTermBis(V^.TV_TRED,False,Quotes)
        End;
      If V^.TV_FWAT <> Nil Then
        AddIneq(V^.TV_FWAT)
    End;
  FuncSymbol  :
    Begin
      F := FuncPtr(T);
      LeftT := F^.TF_LTER;
      If (TypeOfTerm(LeftT) = Constant) Then
      Begin
        ConsIdx := ConstPtr(LeftT)^.TC_CONS;
        Cste := DictConst[ConsIdx];
        If (Cste[1] In Digits) Then { should only happen when printing subtrees during debugging}
        Begin
          WriteTuple(F)
        End
        Else If Cste = '.' Then { F(.,F(a,F(b,Nil))) => a.b }
        Begin
          If ArgList Then Write('(');
          F1 := FuncPtr(F^.TF_RTER);
          WriteTermBis(F1^.TF_LTER,True,True);
          Write('.');
          F2 := FuncPtr(F1^.TF_RTER); { TODO: check type }
          WriteTermBis(F2^.TF_LTER,False,True);
          If ArgList Then Write(')')
        End
        Else
        Begin 
          Write(Cste);
          If F^.TF_RTER<>Nil Then { F(name,F(a,F(b,Nil) => name(a,b) }
          Begin
            Write('(');
            WriteArgument(FuncPtr(F^.TF_RTER)); { TODO: check type }
            Write( ')')
          End
        End
      End
      Else { F(a,F(b,Nil) => <a,b> where a not a constant }
      Begin
        WriteTuple(F)
      End
    End
  End
End;

Procedure WriteTerm( T : TermPtr ); Forward;

Procedure WriteOneEquation( E : EqPtr );
Begin
  WriteTerm(E^.EQ_LTER);
  Case E^.EQ_TYPE Of
  REL_EQUA:
    Write(' = ');
  REL_INEQ:
    Write(' <> ');
  End;
  WriteTerm(E^.EQ_RTER)
End;

{----------------------------------------------------------------------------}
{ write equations in the list E, starting with a comma if Comma is True     }
{----------------------------------------------------------------------------}

Procedure WriteEquationsBis( E : EqPtr; Var Comma : Boolean );
Begin
  If E <> Nil Then
  Begin
    If Comma Then Write(', ');
    Comma := True;
    WriteOneEquation(E);
    WriteEquationsBis(E^.EQ_NEXT, Comma)
  End
End;

{ write }
Procedure WriteEquations( E : EqPtr );
Var Comma : Boolean;
Begin
  Comma := False;
  WriteEquationsBis(E,Comma)
End;

Procedure WriteSys( S : SysPtr );
Begin
  Write('{ ');
  WriteEquations(S^.SY_EQUA);
  If (S^.SY_EQUA<>Nil) And (S^.SY_INEQ<>Nil) Then
    Write(', ');
  WriteEquations(S^.SY_INEQ);
  Write(' }')
End;

{----------------------------------------------------------------------------}
{ Restitution de toutes les inéquations de la pile Ineq.                     }
{----------------------------------------------------------------------------}

Procedure WriteInequations( Var Before : Boolean );
Var I,K : Integer;
Begin
  K := PtrIneq;
  For I := 1 To K Do WriteEquationsBis(Ineq[I],Before)
End;

{----------------------------------------------------------------------------}
{ Ecriture d'un terme qui n'est pas un argument de prédicat.                 }
{----------------------------------------------------------------------------}

Procedure WriteTerm; (* ( T : TermPtr ); *)
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
  V       : VarPtr;
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
    If V^.TV_TRED <> Nil Then
    Begin
      CurlyBrace;
      If Before Then Write(', ');
      Write(GetVarNameAsString(V),' = ');
      Before := True;
      WriteTerm(V^.TV_TRED)
    End;
    If V^.TV_FWAT <> Nil Then
    Begin
      CurlyBrace;
      AddIneq(V^.TV_FWAT)
    End
  End;
  WriteInequations(Before);
  If Printed Then
    Write(' }')
End;

{----------------------------------------------------------------------------}
{ Restitution du terme d'un bloc-terme pointé par B.                         }
{----------------------------------------------------------------------------}

Procedure UnparseOneTerm( B : BTermPtr );
Begin
  WriteTerm(B^.BT_TERM)
End;

{----------------------------------------------------------------------------}
{ Restitution des termes d'une suite de bloc-termes à partir de B.           }
{----------------------------------------------------------------------------}

Procedure UnparseTerms( B : BTermPtr; CR : Boolean);
Begin
  If B <> Nil Then
  Begin
    If CR Then
    Begin
      WriteLn;
      Write('        ')
    End;
    UnparseOneTerm(B);
    If Not CR Then
      Write(' ');
    UnparseTerms(B^.BT_NEXT,CR)
  End
End;

{----------------------------------------------------------------------------}
{ Restitution de la règle pointée par R.                                     }
{----------------------------------------------------------------------------}

Procedure UnparseOneRule( R : RulePtr );
Var B : BTermPtr;
Begin
  InitIneq;
  B := R^.RU_FBTR;
  UnparseOneTerm(B);
  Write(' -> ');
  UnparseTerms(B^.BT_NEXT,True);
  WriteSystem(R^.RU_FVAR,R^.RU_LVAR,False);
  WriteLn(';')
End;

{----------------------------------------------------------------------------}
{ Restitution d'une liste de règles pointée par R.                           }
{----------------------------------------------------------------------------}

Procedure UnparseRules(R : RulePtr);
Begin
  if R <> Nil Then
  Begin
    UnparseOneRule(R);
    UnparseRules(NextRule(R))
  End
End;

{----------------------------------------------------------------------------}
{ Restitution des règles du contexte d'une règle Q.                          }
{----------------------------------------------------------------------------}

Procedure UnparseQuestionRules( Q : QueryPtr; RuleType : RuType );
Var
  R : RulePtr;
  Stop : Boolean;
Begin
  R := Q^.QU_FRUL;
  Stop := R = Nil;
  While Not Stop Do
  Begin
    If R^.RU_TYPE = RuleType Then
      UnparseOneRule(R);
    Stop := R = Q^.QU_LRUL;
    R := NextRule(R);
    Stop := Stop Or (R = Nil)
  End
End;

{----------------------------------------------------------------------------}
{ Restitution d'une question stockée à l'adresse Q.                          }
{----------------------------------------------------------------------------}

Procedure UnparseOneQuery(Q : QueryPtr);
Begin
  Write('-> ');
  InitIneq;
  UnparseTerms(Q^.QU_FBTR,False);
  WriteSystem(Q^.QU_FVAR,Q^.QU_LVAR,False);
  WriteLn(';')
End;

{----------------------------------------------------------------------------}
{ Restitution d'une liste de questions pointée par Q.                        }
{----------------------------------------------------------------------------}

Procedure UnparseQueries(Q : QueryPtr);
Begin
  if Q <> Nil Then
  Begin
    UnparseOneQuery(Q);
    UnparseQueries(Q^.QU_NEXT)
  End
End;

{----------------------------------------------------------------------------}
{ Restitution des questions du programme P.                                  }
{----------------------------------------------------------------------------}

Procedure UnparseProgramQueries( P : ProgPtr );
Begin
  UnparseQueries(P^.PP_FQRY)
End;


{----------------------------------------------------------------------------}
{ Restitution des règles du programme P.                                     }
{----------------------------------------------------------------------------}

Procedure UnparseProgramRules( P : ProgPtr );
Begin
  UnparseRules(P^.PP_FRUL)
End;
