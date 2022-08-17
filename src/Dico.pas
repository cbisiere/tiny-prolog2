{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   Fichier     : Dico.pas                                                   }
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
{ F. NewVar( Nom : StrIdent; Adr : Integer ) : Integer;   -> DicoVar         }
{ F. GetVarPtr( Pos : Integer ) : Integer;                -> DicoVar         }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Directive de compilation : Vérifier les indices des tableaux.     }
{$V-} { Directive de compilation : Ne pas vérifier la taille des chaînes. }


Const MaxConst     = 100;                     { Nombre max. de Cstes         }
      MaxVar       = 100;                     { Nombre max. de Vars          }
      MaxSizeIdent = 40;                      { Taille max. d'un ident       }

Type StrIdent   = String[MaxSizeIdent];           { Type chaîne Ident.       }
     TDicoConst = Array[1..MaxConst] Of StrIdent; { Type Dico. des Cstes     }
     TDicoVar   = Array[1..MaxVar]   Of           { Type Dico. des Vars      }
                    Record
                      Nom : StrIdent;         { Nom de la variable           }
                      Ptr : Integer           { Pointe vers son allocation   }
                    End;

Var DicoVar   : TDicoVar;                    { Dictionnaire des variables    }
    DicoConst : TDicoConst;                  { Dictionnaire des constantes   }
    NbVar     : Integer;                     { Nombre de variables stockées  }
    NbConst   : Integer;                     { Nombre de constantes stockées }


{----------------------------------------------------------------------------}
{ Function NumConst (C : StrIdent) : Integer;                                }
{----------------------------------------------------------------------------}
{ La fonction NumConst reçoit un identificateur de constante (identificateur }
{ ou entier) C. Elle recherche si C est déjà stocké dans le dictionnaire des }
{ constantes DicoConst. Si oui, elle retourne l'indice dans le dictionnaire  }
{ où elle a trouvé cet identificateur. Sinon elle ajoute C au sommet du      }
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
{ où est stocké l'identificateur Elt. La recherche ne se fait que dans une   }
{ partie du dictionnaire : de Top à NbVar. Si la recherche échoue, la        }
{ fonction retourne 0.                                                       }
{----------------------------------------------------------------------------}

Function Position( Top      : Integer;       { Début de la recherche         }
                   Elt      : StrIdent       { Elément à chercher            }
                  ) : Integer;               { Position si trouvé, 0 sinon   }
Var I,Pos   : Integer;
    Trouve  : Boolean;
Begin
  Pos      := 0;
  Trouve   := False;
  I        := Top;
  While (I<=NbVar) And Not Trouve Do
    Begin
      If DicoVar[I].Nom = Elt Then
        Begin
          Trouve := True;
          Pos    := I;
        End
      Else I := I + 1
    End;
  Position := Pos
End;


{----------------------------------------------------------------------------}
{ Function NewVar( Nom : StrIdent; Adr : Integer ) : Integer;                }
{----------------------------------------------------------------------------}
{ Ajoute un identificateur de variable dans le dictionnaire des variables    }
{ DicoVar.                                                                   }
{----------------------------------------------------------------------------}

Function NewVar( Nom : StrIdent; Adr : Integer ) : Integer;
Begin
  NbVar := NbVar + 1;
  DicoVar[NbVar].Nom := Nom;
  DicoVar[NbVar].Ptr := Adr;
  NewVar := NbVar
End;

{----------------------------------------------------------------------------}
{ Function GetVarPtr( Pos : Integer ) : Integer;                             }
{----------------------------------------------------------------------------}
{ Retourne le pointeur de la variable stockée en position Pos dans DicoVar.  }
{----------------------------------------------------------------------------}

Function GetVarPtr( Pos : Integer ) : Integer;
Begin
  GetVarPtr := DicoVar[Pos].Ptr
End;
