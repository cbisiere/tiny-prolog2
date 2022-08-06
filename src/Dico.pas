{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   Fichier     : Dico.pas                                                    }
{   Auteur      : Christophe BISIERE                                         }
{   Date        : 07/01/88                                                   }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{            G E S T I O N   D E S   D I C T I O N N A I R E S               }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{ F. NumConst (C : StrIdent) : Integer;                   -> DicoConst       }
{                                                                            }
{ F. Position (Top : Integer; Elt : StrIdent) : Integer;  -> DicoVar         }
{ F. InstalIn (Top : Integer; Elt : StrIdent; P : Integer;                   }
{     Var Value : Integer): Boolean;                      -> DicoVar         }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Directive de Compilation : Verifier les Indices de Tableaux.      }
{$V-} { Directive de Compilation : Ne pas verifier la taille des Chaines. }


Const MaxConst     = 100;                     { Nombre max. de Cstes         }
      MaxVar       = 100;                     { Nombre max. de Vars          }
      MaxSizeIdent = 40;                      { Taille max. d'un ident       }

Type StrIdent   = String[MaxSizeIdent];           { Type chaine Ident.       }
     TDicoConst = Array[1..MaxConst] Of StrIdent; { Type Dico. des Cstes     }
     TDicoVar   = Array[1..MaxVar]   Of           { Type Dico. des Vars      }
                    Record
                      Nom : StrIdent;         { Nom de la variable           }
                      Ptr : Integer           { Pointe vers son allocation   }
                    End;

Var DicoVar   : TDicoVar;                    { Dictionnaire des variables    }
    DicoConst : TDicoConst;                  { Dictionnaire des constantes   }
    NbVar     : Integer;                     { Nombre de variables stockees  }
    NbConst   : Integer;                     { Nombre de constantes stockees }


{----------------------------------------------------------------------------}
{ Function NumConst (C : StrIdent) : Integer;                                }
{----------------------------------------------------------------------------}
{ La fonction NumConst recoit un identificateur de constante (identificateur }
{ ou entier) C. Elle recherche si C est deja stocké dans le dictionnaire des }
{ constantes DicoConst. Si oui, elle retourne l'indice dans le dictionnaire  }
{ ou elle a trouvé cet identificateur. Sinon elle ajoute C au sommet du      }
{ dictionnaire et retourne l'indice correspondant.                           }
{----------------------------------------------------------------------------}

Function NumConst( C : StrIdent ) : Integer;
Var I,Po    : Integer;
    Trouve  : Boolean;
Begin
  Trouve := False;
  I      := 1;
  While (I<=NbConst) And Not Trouve Do
    Begin
      If DicoConst[I] = C Then
        Begin
          Trouve := True;
          Po     := I;
        End
      Else I := I + 1
    End;
  If Not Trouve Then
    Begin
      NbConst := NbConst + 1;
      DicoConst[NbConst] := C;
      Po := NbConst
    End;
  NumConst := Po
End;


{----------------------------------------------------------------------------}
{ Function Position (Top : Integer; Elt : StrIdent) : Integer;               }
{----------------------------------------------------------------------------}
{ La fonction Position retourne l'indice dans le dictionnaire des variables  }
{ ou est stocké l'identificateur Elt. La recherche ne se fait que dans une   }
{ partie du dictionnaire : de Top à NbVar. Si la recherche echoue, la        }
{ fonction retourne 0.                                                       }
{----------------------------------------------------------------------------}

Function Position( Top      : Integer;       { Debut de la recherche         }
                   Elt      : StrIdent       { Element a chercher            }
                  ) : Integer;               { Position si trouve, 0 sinon   }
Var I,Po    : Integer;
    Trouve  : Boolean;
Begin
  Po       := 0;
  Trouve   := False;
  I        := Top;
  While (I<=NbVar) And Not Trouve Do
    Begin
      If DicoVar[I].Nom = Elt Then
        Begin
          Trouve := True;
          Po     := I;
        End
      Else I := I + 1
    End;
  Position := Po
End;


{----------------------------------------------------------------------------}
{ Function InstalIn(Top:Integer; Elt:StrIdent; P:Integer; Var Value:Integer) }
{      : Boolean;                                                            }
{----------------------------------------------------------------------------}
{ InstalIn tente de mettre en place un identificateur de variable dans le    }
{ dictionnaire des variables DicoVar (sachant que la recherche ne se fera    }
{ que de Top à NbVar). Deux cas peuvent se presenter :                       }
{                                                                            }
{     (1) Cet element est deja dans le Dico :                                }
{             * Value retourne le pointeur stocke                            }
{             * La fonction retourne False                                   }
{     (2) Cet element n'est pas dans le Dico :                               }
{             * Le nouvel element (Elt,P) est instalé                        }
{             * Value retourne la position dans le Dico                      }
{             * La fonction retourne True                                    }
{                                                                            }
{----------------------------------------------------------------------------}

Function InstalIn(     Top      : Integer;          { Debut de la recherche  }
                       Elt      : StrIdent;         { Element a installer    }
                       P        : Integer;          { Pointeur a installer   }
                   Var Value    : Integer           { Parametre              }
                  ) : Boolean;    { True si Elt est un nouvel element        }
Var Po : Integer;
Begin
  Po := Position(Top,Elt);
  If Po <> 0 Then
    Value := DicoVar[Po].Ptr
  Else
    Begin
      NbVar := NbVar + 1;
      DicoVar[NbVar].Nom := Elt;
      DicoVar[NbVar].Ptr := P;
      Value           := NbVar
    End;
  InstalIn := Po = 0
End;


