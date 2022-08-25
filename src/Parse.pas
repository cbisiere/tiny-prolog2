{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Parse.pas                                                  }
{   Author      : Christophe Bisière                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                              P A R S I N G                                 }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Const Letters   : CharSet = ['a'..'z','A'..'Z'];   { Ensemble des lettres    }
      Digits  : CharSet = ['0'..'9'];            { Ensemble des chiffres   }

Var TopVar : Integer;      { Début de recherche (localité des Vars)     }

Function ReduceSystem( RightStopper : Integer;
                       Backtrackable : Boolean ) : Boolean; Forward;

{-----------------------------------------------------------------------}
{                                                                       }
{  UN TERME T :                                                         }
{                                                                       }
{               |-------|                                               }
{               |       |                                               }
{         T+0   | TYPE  |---> Type de terme : TERM_C, TERM_V, TERME_F.  }
{               |       |                                               }
{               |-------|                                               }
{                                                                       }
{  Suivent les données spécifiques au type.                             }
{                                                                       }
{-----------------------------------------------------------------------}

Const
  TT_TYPE = 0;

Type TTerm = (Variable,Constant,FuncSymbol,Dummy); { Types de terme }

{-----------------------------------------------------------------------}
{ Retourne le type du terme pointé par T.                               }
{-----------------------------------------------------------------------}

Function TypeOfTerm( T : Integer ) : TTerm;
Begin
  TypeOfTerm := Dummy;
  If T <> NULL Then
    Case Memory[T+TT_TYPE] Of
    TERM_C :
      TypeOfTerm := Constant;
    TERM_V :
      TypeOfTerm := Variable;
    TERM_F :
      TypeOfTerm := FuncSymbol
    End
End;

{-----------------------------------------------------------------------}
{                                                                       }
{  UNE CONSTANTE C :                                                    }
{                                                                       }
{               |-------|                                               }
{               |       |                                               }
{         C+0   | TYPE  |---> Code for 'constante' : TERM_C.            }
{               |       |                                               }
{               |-------|                                               }
{               |       |                                               }
{         C+1   | NumC  |---> Numéro de la constante dans le diction-   }
{               |       |     naire des constantes.                     }
{               |-------|                                               }
{                                                                       }
{      Pour chaque rencontre d'une constante, une allocation est        }
{   réalisée. Cela implique que la comparaison de deux constantes ne    }
{   se réduit pas à la comparaison de deux pointeurs.                   }
{   Cette solution a été adoptée pour simplifier la procédure de reco-  }
{   pie des règles.                                                     }
{                                                                       }
{-----------------------------------------------------------------------}

Const
  TC_length = 2;
  TC_CONS = 1;

{-----------------------------------------------------------------------}
{ Réalise une allocation mémoire pour la constante Ch.                  }
{ Retourne l'adresse où a été installée cette constante.                }
{-----------------------------------------------------------------------}

Function InstallConst( Ch : StrIdent ) : Integer;
Var C : Integer;
Begin
  C := Alloc(TC_length);
  Memory[C+TT_TYPE] := TERM_C;          { Code 'constante'          }
  Memory[C+TC_CONS] := IndexConst(Ch);  { Pointeur dans DictConst   }
  InstallConst := C
End;

{-----------------------------------------------------------------------}
{                                                                       }
{  UN SYMBOLE FONCTIONNEL (BINAIRE) F :                                 }
{                                                                       }
{               |-------|                                               }
{               |       |                                               }
{         F     | TYPE  |---> Code 'symbole fonctionnel' : TERM_F       }
{               |       |                                               }
{               |-------|                                               }
{               |       |                                               }
{         F+1   | TRED  |---> Pointe vers le membre droit auquel est    }
{               |       |     associé ce symbole fonctionnel dans le    }
{               |-------|     système réduit (0 sinon).                 }
{               |       |                                               }
{         F+2   | LTER  |---> Pointe vers le fils gauche.               }
{               |       |                                               }
{               |-------|                                               }
{               |       |                                               }
{         F+3   | RTER  |---> Pointe vers le fils droit.                }
{               |       |                                               }
{               |-------|                                               }
{                                                                       }
{   L'unique symbole fonctionnel du système sert à coder les prédicats  }
{   Prolog de la manière suivante :                                     }
{                                                                       }
{           F                                                           }
{          / \               (1) Dans le cas d'un N-uplet, le nom du    }
{       nom   F                  prédicat est le premier argument.      }
{            / \                                                        }
{         Arg1  F            (2) Il est bien entendu que Arg1 .. ArgN   }
{              / \               peuvent aussi être des prédicats.      }
{           Arg2   ...                                                  }
{                   \        (3) Le '.' (liste) est considéré comme     }
{                    F           un prédicat quelconque et stocké comme }
{                  /  \          tel.                                   }
{               ArgN    0                                               }
{                                                                       }
{-----------------------------------------------------------------------}

Const
  TF_length = 4;
  TF_TRED = 1;
  TF_LTER = 2;
  TF_RTER = 3;

{-----------------------------------------------------------------------}
{ Réalise une allocation mémoire pour un symbole                        }
{ fonctionnel binaire dont le fils gauche est T1 et le fils droit       }
{ est T2. Retourne un pointeur vers l'allocation.                       }
{-----------------------------------------------------------------------}

Function InstallSymbol( T1,T2 : Integer ) : Integer;
Var F : Integer;
Begin
  F := Alloc(TF_length);
  Memory[F+TT_TYPE] := TERM_F;     { Code 'Symbole Fonct.'     }
  Memory[F+TF_TRED] := NULL;       { Pointeur futur mbre droit }
  Memory[F+TF_LTER] := T1;         { Pointeur premier terme    }
  Memory[F+TF_RTER] := T2;         { Pointeur deuxième terme   }
  InstallSymbol := F
End;

{-----------------------------------------------------------------------}
{                                                                       }
{  UNE VARIABLE V :                                                     }
{                                                                       }
{               |-------|                                               }
{               |       |                                               }
{         V+0   | TYPE  |---> Code for 'Variable' : TERM_V.             }
{               |       |                                               }
{               |-------|                                               }
{               |       |                                               }
{         V+1   | COPY  |---> True si cette variable a été créée via    }
{               |       |     une copie.                                }
{               |-------|                                               }
{               |       |                                               }
{         V+2   | NVAR  |---> Numéro de la variable dans le diction-    }
{               |       |     naire des variables.                      }
{               |-------|                                               }
{               |       |                                               }
{         V+3   | IRED  |---> Booléen qui indique si cette variable est }
{               |       |     actuellement le membre gauche d'une équa- }
{               |-------|     tion du système réduit.                   }
{               |       |                                               }
{         V+4   | IWAT  |---> Booléen qui indique si cette variable     }
{               |       |     surveille actuellement une ou des inéqua- }
{               |-------|     tions.                                    }
{               |       |                                               }
{         V+5   | TRED  |---> Pointe vers le membre droit du système    }
{               |       |     réduit. N'est significatif que si IRED =  }
{               |-------|     True.                                     }
{               |       |                                               }
{         V+6   | FWAT  |---> Pointe vers la première inéquation que    }
{               |       |     cette variable surveille. N'est signifi-  }
{               |-------|     catif que si IWAT = True.                 }
{                                                                       }
{   Remarques :                                                         }
{                                                                       }
{     (1) Dans l'algorithme original de réduction d'un système d'équa-  }
{         tions et d'inéquations, la présence simultanée des deux in-   }
{         dicateurs Equ et Sur est indispensable parce que l'étape 2 de }
{         la résolution d'inéquations se fait après l'étape 1. Il nous  }
{         faut savoir à l'étape 2 si une variable est à la fois membre  }
{         gauche du système réduit et gardienne d'une inéquation.       }
{         Dans l'interpreteur Prolog, l'étape 2 de la résolution d'iné- }
{         quations a été intégrée dans la résolution d'équations. La    }
{         présence simultanée des deux indicateurs a été conservée tout }
{         de même pour des raisons de clarté.                           }
{                                                                       }
{     (2) Une même variable n'est allouée qu'une seule fois, sauf si    }
{         elle se trouve dans deux règles différentes (localité des     }
{         variables par rapport aux règles).                            }
{                                                                       }
{                                                                       }
{-----------------------------------------------------------------------}

Const
  TV_length = 7;
  TV_COPY = 1;
  TV_NVAR = 2;
  TV_IRED = 3;
  TV_IWAT = 4;
  TV_TRED = 5;
  TV_FWAT = 6;

{-----------------------------------------------------------------------}
{ Réalise une allocation mémoire pour la variable                       }
{ Ch, si celle-ci n'a pas déjà été allouée pour la règle courante. Dans }
{ tous les cas elle retourne un pointeur vers son allocation.           }
{-----------------------------------------------------------------------}

Function InstallVariable( Ch : StrIdent ) : Integer;
Var
  V : Integer;
  PosInDictVar : Integer;
Begin
  PosInDictVar := Position(TopVar,Ch);
  If PosInDictVar = 0 Then
  Begin
    V := Alloc(TV_length);
    PosInDictVar := NewVar(Ch,V);
    Memory[V+TT_TYPE] := TERM_V;       { Code 'Variable'           }
    Memory[V+TV_COPY] := NO;           { Not a copy                }
    Memory[V+TV_NVAR] := PosInDictVar; { Pointeur dans Dictvar     }
    Memory[V+TV_IRED] := NO;           { Pas d'équation            }
    Memory[V+TV_IWAT] := NO;           { Pas d'inéquation          }
    Memory[V+TV_TRED] := NULL;         { partie droite équation    }
    Memory[V+TV_FWAT] := NULL;         { Première inéquation       }
    InstallVariable := V
  End
  Else
    InstallVariable := GetVarPtr(PosInDictVar) { adresse d'implantation }
End;

{-----------------------------------------------------------------------}
{                                                                       }
{  UNE EQUATION OU INEQUATION, DANS LE SYSTEME REDUIT OU NON :          }
{                                                                       }
{               |-------|                                               }
{               |       |                                               }
{         E+0   | TYPE  |---> Code REL_EQUA pour une équation,          }
{               |       |     Code REL_INEQ pour une inéquation.        }
{               |-------|                                               }
{               |       |                                               }
{         E+1   | LTER  |---> Pointe vers le membre gauche.             }
{               |       |                                               }
{               |-------|                                               }
{               |       |                                               }
{         E+2   | RTER  |---> Pointe vers le membre droit.              }
{               |       |                                               }
{               |-------|                                               }
{               |       |                                               }
{         E+3   | NEXT  |---> Pointe vers l'inéquation suivante (0 si   }
{               |       |     cette inéquation est la dernière de la    }
{               |-------|     chaîne).                                  }
{                                                                       }
{                                                                       }
{-----------------------------------------------------------------------}

Const
  EQ_length = 4;
  EQ_TYPE = 0;
  EQ_LTER = 1;
  EQ_RTER = 2;
  EQ_NEXT = 3;

{----------------------------------------------------------------------------}
{ Crée une nouvelle équation ou inéquation.                                  }
{----------------------------------------------------------------------------}

Function PushEquation( Code : Integer; T1,T2 : Integer ) : Integer;
Var E : Integer;
Begin
  CheckCondition((Code=REL_EQUA) Or (Code=REL_INEQ), 'Unknown relation');
  E := Alloc(EQ_length);
  Memory[E+EQ_TYPE] := Code;
  Memory[E+EQ_LTER] := T1;
  Memory[E+EQ_RTER] := T2;
  Memory[E+EQ_NEXT] := NULL;
  PushEquation := E
End;

{-----------------------------------------------------------------------}
{ Lit et code les arguments d'un prédicat (EndChar=')')                 }
{ ou d'un N-Uplet (EndChar='>'). Retourne un pointeur vers la           }
{ structure qui représente ces arguments.                               }
{-----------------------------------------------------------------------}

Function ReadOneTerm : Integer; Forward;

Function GetArgument( EndChar : Char ) : Integer;
Var
  T : Integer;
  c : Char;
Begin
  T := ReadOneTerm;
  c := GetCharNb(c);
  If c =  ',' Then
    GetArgument := InstallSymbol(T,GetArgument(EndChar))
  Else
  If c = EndChar  Then
    GetArgument := InstallSymbol(T,NULL)
  Else
  Begin
    RaiseError('"' + EndChar + '" expected');
    GetArgument := NULL
  End
End;

{-----------------------------------------------------------------------}
{ Lit un terme en entrée, le code en mémoire. Retourne                  }
{ l'adresse où il a été implanté.                                       }
{-----------------------------------------------------------------------}

Function ReadOneTerm; (* : Integer *)
Var
  T : Integer;
  Ch  : StrIdent;
  c,c2 : Char;
Begin
  Ch := '';
  Spaces;
  Get(Ch,Letters);
  Case Length(Ch) Of
  0 :
    Begin
      c := NextChar(c);
      If c In Digits Then     { un entier }
      Begin
        Get(Ch,Digits);
        T := InstallConst(Ch);
      End
      Else
      If c ='(' Then          { une forme ( <terme> ) }
      Begin
        c := GetChar(c);
        T := ReadOneTerm;
        Verify(')')
      End
      Else
      If c = '<' Then        { un N-uplet }
      Begin
        c := GetChar(c);
        T := GetArgument('>')
      End
      Else
        RaiseError('digit, ")" or "<" expected')
    End;
  1 :
    Begin                      { une variable }
      Get(Ch,Digits);
      Get(Ch,['''']);
      T := InstallVariable(Ch);
    End;
  Else { an identifier }
    Begin
      Get(Ch,Letters);
      While (NextChar(c)='-') And (NextNextChar(c2) In Letters) Do
      Begin
        Ch := Ch + GetChar(c);
        Get(Ch,Letters)
      End;
      Get(Ch,Digits);
      If NextChar(c) = '(' Then                { => un prédicat    }
      Begin
        c := GetChar(c);
        T := InstallSymbol(InstallConst(Ch),GetArgument(')'));
      End
      Else
        T := InstallConst(Ch)       { => une constante  }
    End
  End;
  c := NextChar(c);
  If c = '.' Then    { un élément de liste }
  Begin
    c := GetChar(c);
    T := InstallSymbol( InstallConst('.'),
        InstallSymbol(T,InstallSymbol(ReadOneTerm,NULL)) )
  End;
  ReadOneTerm := T    { Retourne adresse d'implantation de ce terme }
End;

{-----------------------------------------------------------------------}
{ Retourne l'accès du terme T (pour pré-unification). Si le terme       }
{ T n'a pas une constante pour accès, la fonction retourne NULL.        }
{-----------------------------------------------------------------------}

Function Access( T : Integer ) : Integer;
Var LeftT : Integer;
Begin
  Access := NULL;
  Case TypeOfTerm(T) Of
  Constant :
    Access := T;
  FuncSymbol  :
    Begin
      LeftT := Memory[T+TF_LTER];
      Case TypeOfTerm(LeftT) Of
      Constant :
        Access := LeftT;
      FuncSymbol  :
        Begin
        End
      End
    End
  End
End;

{-----------------------------------------------------------------------}
{                                                                       }
{  UNE EQUATION OU UNE INEQUATION (PILE DROITE) E :                     }
{                                                                       }
{               |-------|                                               }
{               |       |                                               }
{         Z-0   | TYPE  |---> Code REL_EQUA pour une équation,          }
{               |       |     Code REL_INEQ pour une inéquation.        }
{               |-------|                                               }
{               |       |                                               }
{         Z-1   | LTER  |---> Pointe vers le membre gauche.             }
{               |       |                                               }
{               |-------|                                               }
{               |       |                                               }
{         Z-2   | RTER  |---> Pointe vers le membre droit.              }
{               |       |                                               }
{               |-------|                                               }
{                                                                       }
{                                                                       }
{-----------------------------------------------------------------------}

Const
  ZZ_length = 3;
  ZZ_TYPE = -0;
  ZZ_LTER = -1;
  ZZ_RTER = -2;

{----------------------------------------------------------------------------}
{ Crée une nouvelle équation (Code=REL_EQUA) ou inéquation (Code=REL_INEQ    }
{ de forme T1 = T2 dans la pile droite de la mémoire principale.             }
{----------------------------------------------------------------------------}

Procedure PushOneEquationToSolve( Code : Integer; T1,T2 : Integer );
Var Z : Integer;
Begin
  CheckCondition((T1<>NULL) And (T2<>NULL),'Equation with NULL term');
  Z := AllocRight(ZZ_length);
  Memory[Z+ZZ_TYPE] := Code;
  Memory[Z+ZZ_LTER] := T1;
  Memory[Z+ZZ_RTER] := T2;
End;

{----------------------------------------------------------------------------}
{ Copie la liste d'équations dans la pile d'équation du système à réduiire.  }
{----------------------------------------------------------------------------}

Procedure PushEquationsToSolve( E : Integer );
Begin
  Repeat
    PushOneEquationToSolve(Memory[E+EQ_TYPE],Memory[E+EQ_LTER],Memory[E+EQ_RTER]);
    E := Memory[E+EQ_NEXT]
  Until E = NULL
End;

{----------------------------------------------------------------------------}
{ Retourne un pointeur vers l'équation au sommet de la pile droite.          }
{----------------------------------------------------------------------------}

Function TopEquationToSolve : Integer;
Var Z : Integer;
Begin
  Z := PtrRight + ZZ_length - 1;
  CheckCondition(Z <= SizeMem,'Top relation is out of stack');
  TopEquationToSolve := Z
End;

{----------------------------------------------------------------------------}
{ Retourne True si Z est un pointeur vers l'équation au sommet de la pile    }
{ droite.                                                                    }
{----------------------------------------------------------------------------}

Function IsTopEquationToSolve( Z : Integer ) : Boolean;
Begin
  IsTopEquationToSolve := (Z = TopEquationToSolve)
End;

{----------------------------------------------------------------------------}
{ Retourne l'équation suivante ou 0 s'il n'y a plus d'équation.              }
{----------------------------------------------------------------------------}

Function NextEquationToSolve( Z : Integer ) : Integer;
Begin
  Z := Z - ZZ_length;
  CheckCondition(Z - ZZ_length + 1 >= PtrRight,'Next relation is out of stack');
  NextEquationToSolve := Z
End;

{----------------------------------------------------------------------------}
{ Dépile une équatiion.                                                      }
{----------------------------------------------------------------------------}

Procedure PopEquationToSolve( Var Code : Integer; Var T1,T2 : Integer);
Var Z : Integer;
Begin
  Z := PtrRight + ZZ_length - 1;
  Code := Memory[Z+ZZ_TYPE];
  T1 := Memory[Z+ZZ_LTER];
  T2 := Memory[Z+ZZ_RTER];
  PtrRight := PtrRight + ZZ_length;
End;

{----------------------------------------------------------------------------}
{ Echange deux équatiions.                                                   }
{----------------------------------------------------------------------------}

Procedure SwapEquationToSolve( Z1, Z2 : Integer );
Begin
  SwapMem(Z1+ZZ_TYPE,Z2+ZZ_TYPE);
  SwapMem(Z1+ZZ_LTER,Z2+ZZ_LTER);
  SwapMem(Z1+ZZ_RTER,Z2+ZZ_RTER);
End;

{----------------------------------------------------------------------------}
{ Modifie les équations et inéquations du sommet de la pile                  }
{ droite (sans dépasser RightStopper) de telle sorte que les équations       }
{ se trouvent toutes avant les inéquations. Cette procédure est indispen-    }
{ sable parce que la procédure Reduce peut, au cours de son                  }
{ travail, ajouter des inéquations (remise en cause d'inéquations) à droite. }
{----------------------------------------------------------------------------}

Procedure SortEquationsToSolve( RightStopper : Integer );
Var Stop : Boolean;
    Z, FirstZ, NextZ : Integer;
Begin
  If (RightStopper > PtrRight) Then
  Begin
    Stop := False;
    FirstZ := RightStopper - 1;
    While Not (IsTopEquationToSolve(FirstZ) Or Stop) Do
    Begin
      Stop := True;
      Z := FirstZ;
      Repeat
        NextZ := NextEquationToSolve(Z);
        If (Memory[Z+ZZ_TYPE] = REL_EQUA)
          And (Memory[NextZ+ZZ_TYPE] = REL_INEQ) Then
        Begin
          SwapEquationToSolve(Z,NextZ);
          Stop := False
        End;
        Z := NextZ;
      Until IsTopEquationToSolve(NextZ) Or Not Stop;
      FirstZ := NextEquationToSolve(FirstZ)
    End
  End
End;

{-----------------------------------------------------------------------}
{ Lit une équation ou une inéquation.                                   }
{-----------------------------------------------------------------------}

Function ReadEquation : Integer;
Var
  E : Integer;
  T1, T2 : Integer;
  Code : Integer;
  c : Char;
Begin
  E := NULL;
  T1 := ReadOneTerm;
  If Not Error Then
  Begin
    c := GetCharNb(c);
    Case c Of
    '=' :
      Code := REL_EQUA;   { code 'Equation'   }
    '<' :
      Begin
        Verify('>');
        Code := REL_INEQ  { code 'Inéquation' }
      End;
    Else
      RaiseError('= or <> expected')
    End
  End;
  If Not Error Then
    T2 := ReadOneTerm;  { terme droit }
  If Not Error Then
    E := PushEquation(Code,T1,T2);
  ReadEquation := E
End;

{-----------------------------------------------------------------------}
{                                                                       }
{  UN SYSTEME :                                                         }
{                                                                       }
{    Avant réduction, le système est codé sous forme d'une séquence     }
{  d'équations ou d'inéquations dans la pile droite.                    }
{                                                                       }
{    Après réduction, les inéquations du système réduit sont stockées   }
{  dans la pile gauche, sous forme de listes chainées d'inéquations.    }
{-----------------------------------------------------------------------}

{-----------------------------------------------------------------------}
{ Lit un système d'équations et d'inéquations et la stocke              }
{ dans la pile de la mémoire principale.                                }
{-----------------------------------------------------------------------}

Function ReadSystem : Integer;
Var
  E, FirstE, PrevE : Integer;
  First : Boolean;
  c     : Char;
Begin
  FirstE := NULL;
  PrevE := NULL;
  Verify('{');
  If Not Error Then
  Begin
    First := True;
    Repeat
      E := ReadEquation;
      If First Then
      Begin
        FirstE := E;
        First := False
      End
      Else
        Memory[PrevE+EQ_NEXT] := E;
      PrevE := E
    Until (Error) Or (GetCharNb(c) <> ',');
    If (Not Error) And (c <> '}') Then RaiseError('Missing }')
  End;
  ReadSystem := FirstE
End;

{-----------------------------------------------------------------------}
{ Compile un système d'équations et d'inéquations et le réduit.         }
{ Les équations du système réduit sont intégrées dans le codage des     }
{ variables. Les inéquations du système réduit sont codées comme des    }
{ listes chaînées, stockées au sommet de la pile gauche.                }
{-----------------------------------------------------------------------}

Function CompileSystem : Integer;
Var
  S : Integer;
  Stopper : Integer;
Begin
  S := ReadSystem;
  If Not Error Then
  Begin
    Stopper := PtrRight;
    PushEquationsToSolve(S);
    If Not ReduceSystem(Stopper,False) Then
      RaiseError('Constraints cannot be satisfied');
  End;
  CompileSystem := S
End;

{-----------------------------------------------------------------------}
{                                                                       }
{  UN BLOC-TERME B :                                                    }
{                                                                       }
{               |-------|                                               }
{               |       |                                               }
{         B     | TERM  |---> Pointe vers le terme T associé au bloc.   }
{               |       |                                               }
{               |-------|                                               }
{               |       |                                               }
{         B+1   | NEXT  |---> Pointe vers le terme suivant, ou NULL si  }
{               |       |     le bloc B est le dernier de la chaîne.    }
{               |-------|                                               }
{               |       |                                               }
{         B+2   | CONS  |---> Pointe vers la constante d'accès utilisée }
{               |       |     par la pré-unification. Vaut 0 si ce      }
{               |-------|     terme n'a pas une constante pour accès.   }
{                                                                       }
{-----------------------------------------------------------------------}

Const
  BT_length = 3;
  BT_TERM = 0;
  BT_NEXT = 1;
  BT_CONS = 2;

{-----------------------------------------------------------------------}
{ Code un bloc-terme en mémoire.                                        }
{-----------------------------------------------------------------------}

Function CompileOneTerm : Integer;
Var B : Integer;
Begin
  B := Alloc(BT_length);
  Memory[B+BT_TERM] := ReadOneTerm;
  Memory[B+BT_NEXT] := NULL;
  Memory[B+BT_CONS] := Access(Memory[B+BT_TERM]);
  CompileOneTerm := B
End;

{-----------------------------------------------------------------------}
{ Code une suite de termes. Dès qu'un caractère                         }
{ contenu dans StopChars est rencontré, le processus s'arrête.          }
{-----------------------------------------------------------------------}

Function CompileTerms( StopChars : CharSet ) : Integer;
Var
  B : Integer;
  c : Char;
Begin
  c := NextCharNb(c);
  If (Not (c In StopChars)) And (Not Error) Then
  Begin
    B := Alloc(BT_length);
    Memory[B+BT_TERM] := ReadOneTerm;
    Memory[B+BT_CONS] := Access(Memory[B+BT_TERM]);
    Memory[B+BT_NEXT] := CompileTerms(StopChars)
  End
  Else
    B := NULL;
  CompileTerms := B
End;

{-----------------------------------------------------------------------}
{                                                                       }
{  UNE REGLE R :                                                        }
{                                                                       }
{               |-------|                                               }
{               |       |                                               }
{         R+0   | NEXT  |---> Pointe vers la règle suivante, ou NULL si }
{               |       |     la règle R est la dernière du programme.  }
{               |-------|                                               }
{               |       |                                               }
{         R+1   | SIZE  |---> Taille occupée par le code de l'ensemble  }
{               |       |     des termes qui composent cette règle.     }
{               |-------|                                               }
{               |       |                                               }
{         R+2   | FVAR  |---> Adresse dans le dictionnaire de la        }
{               |       |     premiere variable de cette règle.         }
{               |-------|                                               }
{               |       |                                               }
{         R+3   | LVAR  |---> Adresse dans le dictionnaire de la        }
{               |       |     dernière variable de cette règle.         }
{               |-------|                                               }
{               |       |                                               }
{         R+4   | SYST  |---> Adresse de la première équation ou        }
{               |       |     inéquation spécifiée dans cette règle,    }
{               |-------|     ou 0 si pas de système.                   }
{                                                                       }
{    Le premier bloc-terme de cette règle (tête) commence en R+4.       }
{                                                                       }
{-----------------------------------------------------------------------}

Const
  RU_length = 5;
  RU_NEXT = 0;
  RU_SIZE = 1;
  RU_FVAR = 2;
  RU_LVAR = 3;
  RU_SYST = 4;
  RU_FBTR = 5; { Do not move! }

{-----------------------------------------------------------------------}
{ Code une règle en mémoire.                                            }
{-----------------------------------------------------------------------}

Procedure CompileOneRule( R : Integer );
Var B : Integer;
    c : Char;
Begin
  TopVar := NbVar + 1; { Première variable dans DictVar  }
  Spaces;
  Memory[R+RU_FVAR] := NbVar + 1;
  B := CompileOneTerm;        { head }
  Verify('->');
  If Not Error Then
  Begin
    Memory[B+BT_NEXT] := CompileTerms(['{',';']);
    c := NextCharNb(c);
    Memory[R+RU_SYST] := NULL;
    If c = '{' Then
      Memory[R+RU_SYST] := CompileSystem; { WARNING: MAY ADD THINGS }
    Verify(';');
    Memory[R+RU_SIZE] := PtrLeft-R+1;
    Memory[R+RU_LVAR] := NbVar;
    TopVar := NbVar + 1             { Var locales à une règle        }
  End
End;

{-----------------------------------------------------------------------}
{ Code une suite de règles. Dès qu'un caractère                         }
{ contenu dans StopChars est rencontré, le processus s'arrête.          }
{-----------------------------------------------------------------------}

Function CompileRules( StopChars : CharSet ) : Integer;
Var
  R : Integer;
  c : Char;
Begin
  c := NextCharNb(c);
  If (Not (c In StopChars)) And (Not Error) Then
  Begin
    R := Alloc(RU_length);
    CompileOneRule(R);
    Memory[R+RU_NEXT] := CompileRules(StopChars)
  End
  Else
    R := NULL;
  CompileRules := R
End;

{-----------------------------------------------------------------------}
{                                                                       }
{  UNE QUESTION Q :                                                     }
{                                                                       }
{               |-------|                                               }
{               |       |                                               }
{         Q+0   | NEXT  |---> Adresse de la question suivante           }
{               |       |     ou NULL si dernière question.             }
{               |-------|                                               }
{               |       |                                               }
{         Q+1   | FRUL  |---> Contexte : première régle.                }
{               |       |                                               }
{               |-------|                                               }
{               |       |                                               }
{         Q+2   | LRUL  |---> Contexte : dernière régle.                }
{               |       |                                               }
{               |-------|                                               }
{               |       |                                               }
{         Q+3   | FVAR  |---> Adresse dans le dictionnaire de la        }
{               |       |     première variable de cette question.      }
{               |-------|                                               }
{               |       |                                               }
{         Q+4   | LVAR  |---> Adresse dans le dictionnaire de la        }
{               |       |     dernière variable de cette question.      }
{               |-------|                                               }
{               |       |                                               }
{         Q+5   | FBTR  |---> Pointe vers le premier bloc terme de la   }
{               |       |     question (NULL si pas de terme).          }
{               |-------|                                               }
{               |       |                                               }
{         Q+6   | FCON  |---> Pointe dans le dictionnaire de la         }
{               |       |     première constante de cette question.     }
{               |-------|                                               }
{               |       |                                               }
{         Q+7   | SYST  |---> Adresse de la première équation ou        }
{               |       |     inéquation spécifiée dans cette question, }
{               |-------|     ou 0 si pas de système.                   }
{                                                                       }
{                                                                       }
{-----------------------------------------------------------------------}

Const
  QU_length = 8;
  QU_NEXT = 0;
  QU_FRUL = 1;
  QU_LRUL = 2;
  QU_FVAR = 3;
  QU_LVAR = 4;
  QU_FBTR = 5;
  QU_FCON = 6;
  QU_SYST = 7;

{-----------------------------------------------------------------------}
{ Code une question.                                                    }
{ Si un système est présent, on tente de le réduire.                    }
{-----------------------------------------------------------------------}

Procedure CompileOneQuery( Q : Integer );
Var c : Char;
Begin
  Spaces;
  TopVar := NbVar + 1; { Première variable dans DictVar  }
  Memory[Q+QU_FRUL] := NULL;
  Memory[Q+QU_LRUL] := NULL;
  Memory[Q+QU_FVAR] := NbVar + 1;
  Memory[Q+QU_FCON] := NbConst + 1;
  Memory[Q+QU_FBTR] := CompileTerms(['{',';']);
  Memory[Q+QU_SYST] := NULL;
  c := NextCharNb(c);
  If c = '{' Then
    Memory[Q+QU_SYST] := CompileSystem;
  Verify(';');
  Memory[Q+QU_LVAR] := NbVar      { Dernière variable dans DictVar }
End;

{-----------------------------------------------------------------------}
{                                                                       }
{  UN PROGRAMME P :                                                     }
{                                                                       }
{               |-------|                                               }
{               |       |                                               }
{         P+0   | FRUL  |---> Pointe vers la première règle du          }
{               |       |     programme, NULL si pas de règles.         }
{               |-------|                                               }
{               |       |                                               }
{         P+1   | LRUL  |---> Pointe vers la dernière règle du          }
{               |       |     programme, NULL si pas de règles.         }
{               |-------|                                               }
{               |       |                                               }
{         P+2   | FQRY  |---> Pointe vers la première question du       }
{               |       |     programme, NULL si pas de question.       }
{               |-------|                                               }
{               |       |                                               }
{         P+3   | LQRY  |---> Pointe vers la dernière question du       }
{               |       |     programme, NULL si pas de question.       }
{               |-------|                                               }
{               |       |                                               }
{         P+4   | STAC  |---> Sommet pile principale après lecture      }
{               |       |     du programme.                             }
{               |-------|                                               }
{               |       |                                               }
{         P+5   | LVAR  |---> Dernière variable du programme.           }
{               |       |                                               }
{               |-------|                                               }
{               |       |                                               }
{         P+6   | LCON  |---> Dernière constante du programme.          }
{               |       |                                               }
{               |-------|                                               }
{                                                                       }
{-----------------------------------------------------------------------}

Const
  PP_length = 7;
  PP_FRUL = 0;
  PP_LRUL = 1;
  PP_FQRY = 2;
  PP_LQRY = 3;
  PP_STAC = 4;
  PP_LVAR = 5;
  PP_LCON = 6;

Function CreateEmptyProgram : Integer;
Var P : Integer;
Begin
  P := Alloc(PP_length);
  Memory[P+PP_FRUL] := NULL;
  Memory[P+PP_LRUL] := NULL;
  Memory[P+PP_FQRY] := NULL;
  Memory[P+PP_LQRY] := NULL;
  Memory[P+PP_LVAR] := NbVar;
  Memory[P+PP_LCON] := NbConst;
  Memory[P+PP_STAC] := PtrLeft;
  CreateEmptyProgram := P
End;

{-----------------------------------------------------------------------}
{ Code une suite de questions. Si ContChar est non vide, un caractère de}
{ cet ensemble doit être présent au ddébut de chaque question. Dès qu'un}
{ caractère contenu dans StopChars est rencontré, le processus s'arrête.}
{-----------------------------------------------------------------------}

Function CompileQueries( P : Integer; WithArrow : Boolean;
  ContChar, StopChars : CharSet ) : Integer;
Var
  Q : Integer;
  c : Char;
Begin
  Q := NULL;
  c := NextCharNb(c);
  If ((ContChar=[]) Or (c In ContChar))
    And (Not (c In StopChars)) And (Not Error) Then
  Begin
    If WithArrow Then
      Verify('->');
    Q := Alloc(QU_length);
    CompileOneQuery(Q);
    Memory[Q+QU_FRUL] := Memory[P+PP_FRUL];
    Memory[Q+QU_LRUL] := Memory[P+PP_LRUL];
    Memory[Q+QU_NEXT] := CompileQueries(P,WithArrow,ContChar,StopChars)
  End;
  CompileQueries := Q
End;

{----------------------------------------------------------------------------}
{ Enlève les questions entrées en ligne de commande.                         }
{----------------------------------------------------------------------------}

Procedure RemoveCommandLineQueries( P : Integer );
Begin
  NbVar   := Memory[P+PP_LVAR];
  NbConst := Memory[P+PP_LCON];
  PtrLeft := Memory[P+PP_STAC]
End;

{----------------------------------------------------------------------------}
{ Code les questions entrées en ligne de commande.                           }
{----------------------------------------------------------------------------}

Function CompileCommandLineQueries( P : Integer ) : Integer;
Var
  Q : Integer;
  c : Char;
Begin
  Q := CompileQueries(P,False,[],[';',EndOfInput]);
  c := NextCharNb(c);
  if c <> EndOfInput Then
    RaiseError('unexpected characters after the last query: "' + c + '"');
  CompileCommandLineQueries := Q
End;

{----------------------------------------------------------------------------}
{ Code les règles et les questions d'un programme Prolog.                    }
{----------------------------------------------------------------------------}

Procedure CompileRulesAndQueries( P : Integer );
Var
  HeadQ, Q : Integer;
  HeadR, R : Integer;
  c : Char;
  Stop : Boolean;
Begin
  Stop := False;
  Error := False;
  HeadQ := NULL;
  HeadR := NULL;
  Repeat
    c := NextCharNb(c);
    If (c=EndOfInput) Or (c=';') Then
      Stop := True
    Else If c='-' Then
    Begin
      Q := CompileQueries(P,True,['-'],[EndOfInput,';']);
      { Set program's first query if not set yet. }
      If Memory[P+PP_FQRY] = NULL Then
        Memory[P+PP_FQRY] := Q;
      { Attach this list to the previous one, if any }
      If (HeadQ <> NULL) Then
        Memory[HeadQ+QU_NEXT] := Q;
      { The new head is the last query in this list }
      While (Memory[Q+QU_NEXT] <> NULL) Do
        Q := Memory[Q+QU_NEXT];
      { Set program's last query. }
      If Q <> NULL Then
        Memory[P+PP_LQRY] := Q;
      HeadQ := Q
    End
    Else
    Begin
      R := CompileRules([EndOfInput,';','-']);
      { Set program's first rule if not set yet. }
      If Memory[P+PP_FRUL] = NULL Then
        Memory[P+PP_FRUL] := R;
      { Attach this list to the previous one, if any }
      If (HeadR <> NULL) Then
        Memory[HeadR+RU_NEXT] := R;
      { The new head is the last query in this list }
      While (Memory[R+RU_NEXT] <> NULL) Do
        R := Memory[R+RU_NEXT];
      { Set program's last rule. }
      If R <> NULL Then
        Memory[P+PP_LRUL] := R;
      HeadR := R
    End
  Until Stop Or Error;
  { Machine state }
  Memory[P+PP_LVAR] := NbVar;
  Memory[P+PP_LCON] := NbConst;
  Memory[P+PP_STAC] := PtrLeft
End;

{----------------------------------------------------------------------------}
{ Charge un programme à partir d'un fichier.                                 }
{----------------------------------------------------------------------------}

Procedure LoadProgram( P : Integer; FileName : AnyStr );
Begin
  If SetFileForInput(FileName) Then
    CompileRulesAndQueries(P)
  Else
    RaiseError('Cannot open file ' + FileName)
End;
