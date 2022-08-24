{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Dict.pas                                                   }
{   Author      : Christophe Bisière                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                         D i C T I O N A R I E S                            }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Const
  MaxConst     = 100;                     { Nombre max. de Cstes         }
  MaxVar       = 100;                     { Nombre max. de Vars          }
  MaxSizeIdent = 40;                      { Taille max. d'un ident       }

Type
  StrIdent   = String[MaxSizeIdent];           { Type chaîne Ident.       }
  TDictConst = Array[1..MaxConst] Of StrIdent; { Type Dict. des Cstes     }
  TDictVar   = Array[1..MaxVar]   Of           { Type Dict. des Vars      }
    Record
      Name : StrIdent;        { Nom de la variable           }
      Ptr : Integer           { Pointe vers son allocation   }
    End;

Var
  DictVar   : TDictVar;                    { Dictionnaire des variables    }
  DictConst : TDictConst;                  { Dictionnaire des constantes   }
  NbVar     : Integer;                     { Nombre de variables stockées  }
  NbConst   : Integer;                     { Nombre de constantes stockées }

{----------------------------------------------------------------------------}
{ La fonction reçoit un identificateur de constante (identificateur          }
{ ou entier) C. Elle recherche si C est déjà stocké dans le dictionnaire des }
{ constantes DictConst. Si oui, elle retourne l'indice dans le dictionnaire  }
{ où elle a trouvé cet identificateur. Sinon elle ajoute C au sommet du      }
{ dictionnaire et retourne l'indice correspondant.                           }
{----------------------------------------------------------------------------}

Function IndexConst( C : StrIdent ) : Integer;
Var
  I,Po   : Integer;
  Found  : Boolean;
Begin
  Found := False;
  I      := 1;
  While (I<=NbConst) And Not Found Do
  Begin
    If DictConst[I] = C Then
    Begin
      Found := True;
      Po     := I;
    End
    Else
      I := I + 1
  End;
  If Not Found Then
  Begin
    NbConst := NbConst + 1;
    CheckCondition(NbConst <= MaxConst,'Maximum number of constants reached');
    DictConst[NbConst] := C;
    Po := NbConst
  End;
  IndexConst := Po
End;

{----------------------------------------------------------------------------}
{ La fonction Position retourne l'indice dans le dictionnaire des variables  }
{ où est stocké l'identificateur Elt. La recherche ne se fait que dans une   }
{ partie du dictionnaire : de Top à NbVar. Si la recherche échoue, la        }
{ fonction retourne 0.                                                       }
{----------------------------------------------------------------------------}

Function Position( Top      : Integer;       { Début de la recherche         }
                   Elt      : StrIdent       { Elément à chercher            }
                  ) : Integer;               { Position si trouvé, 0 sinon   }
Var
  I,Pos   : Integer;
  Found  : Boolean;
Begin
  Pos      := 0;
  Found   := False;
  I        := Top;
  While (I<=NbVar) And Not Found Do
  Begin
    If DictVar[I].Name = Elt Then
    Begin
      Found := True;
      Pos    := I;
    End
    Else
      I := I + 1
  End;
  Position := Pos
End;

{----------------------------------------------------------------------------}
{ Ajoute un identificateur de variable dans le dictionnaire des variables    }
{ DictVar.                                                                   }
{----------------------------------------------------------------------------}

Function NewVar( Nom : StrIdent; Adr : Integer ) : Integer;
Begin
  NbVar := NbVar + 1;
  CheckCondition(NbVar <= MaxVar,'Maximum number of variables reached');
  DictVar[NbVar].Name := Nom;
  DictVar[NbVar].Ptr := Adr;
  NewVar := NbVar
End;

{----------------------------------------------------------------------------}
{ Retourne le pointeur de la variable stockée en position Pos dans DictVar.  }
{----------------------------------------------------------------------------}

Function GetVarPtr( Pos : Integer ) : Integer;
Begin
  GetVarPtr := DictVar[Pos].Ptr
End;
