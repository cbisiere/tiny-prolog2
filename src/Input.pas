{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   Fichier     : Input.pas                                                  }
{   Auteur      : Christophe BISIERE                                         }
{   Date        : 07/01/88                                                   }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{            L E C T U R E   D U   F L O T   D ' E N T R E E                 }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{     P. Erreur (S : AnyStr);                  Constate une erreur           }
{     P. OpenFic;                              Ouverture Fichier Utilisateur }
{     F. GetC (Var c : Char) : Char;           Lecture Non Bufferisee        }
{     F. GetChar (Var c : Char) : Char;        Lecture Bufferisee            }
{     P. UnGetChar (c : Char);                 Remet Caractere dans Buffer   }
{     F. GetCharNb (Var c : Char) : Char;      Lit Caractere non blanc       }
{     P. Get (Var Ch : AnyStr; E : CharSet);   Lit Suite de Caracteres       }
{     P. Verifier( Ch : AnyStr);               Verification Syntaxique       }
{                                                                            }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Directive de compilation : Vérifier les indices des tableaux.     }
{$V-} { Directive de compilation : Ne pas vérifier la taille des chaînes. }


Type  AnyStr    = String[80];                { Type chaîne de travail        }
      CharSet   = Set Of Char;               { Type ensemble de caractères   }

Const SizeBufIn = 10;                        { Taille Buffer d'entree        }
      FinEntree = #$FF;                      { Code caractère 'fin entrée'   }

Var   Entree  : AnyStr;                      { Chaîne lue                    }
      PtrInp  : Byte;                        { Pointeur dans cette chaîne    }
      Calu    : Char;                        { Caractère courant             }
      BufIn   : Array[1..SizeBufIn] Of Char; { Buffer d'entrée               }
      PtrIn   : Byte;                        { Pointeur dans ce buffer       }
      Fic     : Text;                        { Fichier utilisateur           }
      FicOpen : Boolean;                     { Fichier ouvert ?              }
      Error   : Boolean;                     { Erreur ?                      }



{----------------------------------------------------------------------------}
{ Procedure Erreur( S : AnyStr );                                            }
{----------------------------------------------------------------------------}
{ La procédure Erreur constate une erreur de syntaxe. Elle affiche le        }
{ message S et positionne le booléen Error à True.                           }
{----------------------------------------------------------------------------}

Procedure Erreur( S : AnyStr );
Begin
  Writeln;
  Writeln('Erreur : ',S);
  Writeln;
  Error := True
End;


{----------------------------------------------------------------------------}
{ Procedure OpenFic;                                                         }
{----------------------------------------------------------------------------}
{ Ouverture du Fichier Programme : cette procédure demande le nom du fichier }
{ contenant le programme Prolog. Si un nom est entré, elle vérifie que ce    }
{ fichier existe bien, sinon le programme est arrêté (primitive Halt;).      }
{----------------------------------------------------------------------------}

Procedure OpenFic;
Var NomFic : AnyStr;
Begin
  Repeat
    Write('Prolog file to execute: ');
    Readln(NomFic);
    Writeln;
    If NomFic <> '' Then
      Begin
        Assign(Fic,NomFic);
        {$I-}
        Reset(Fic);
        {$I+}
      End
  Until (IOResult=0) Or (NomFic='');
  If NomFic='' Then Halt;
  FicOpen := True
End;

{----------------------------------------------------------------------------}
{ Function GetC (Var c : Char) : Char;                                       }
{----------------------------------------------------------------------------}
{ Lecture d'un caractère : cette fonction teste d'abord si le Fichier        }
{ Utilisateur a bien été ouvert. Si non elle demande l'ouverture. Puis, si   }
{ c'est possible, elle retourne le caractère pointé par PtrInp dans la ligne }
{ Entree. Cette operation peut être impossible pour deux raisons :           }
{ (1) Il n'y a plus de caractère dans Entree. GetC provoque alors la lecture }
{     d'une nouvelle ligne et retourne le caractère Blanc (une fin de ligne  }
{     est considérée comme un séparateur);                                   }
{ (2) C'est le fin du fichier Fic. GetC retourne alors le caractère          }
{     FinEntree.                                                             }
{----------------------------------------------------------------------------}

Function GetC( Var c : Char ) : Char;
Begin
  If Not FicOpen Then
    Begin
      OpenFic;
      PtrInp := 255;
      Entree := '';
    End;
  If (Length(Entree)<>0) And (PtrInp <= Length(Entree)) Then
    Begin
      c := Entree[PtrInp];   { Lit un caractère      }
      PtrInp := PtrInp + 1   { Pointe sur le suivant }
    End
  Else
    Begin
      If Eof(Fic) Then
        Begin
          c := FinEntree;
          If FicOpen Then Close(Fic);
          FicOpen := False
        End
      Else
        Begin
          Readln(Fic,Entree);
          PtrInp := 1;
          c := ' '
        End
    End;
  GetC := c
End;

{----------------------------------------------------------------------------}
{ Function GetChar (Var c : Char) : Char;                                    }
{----------------------------------------------------------------------------}
{ Lecture bufferisée d'un caractère : si le buffer d'entrée BufIn est vide,  }
{ la fonction GetChar retourne le caractère renvoyé par GetC. Dans le cas    }
{ contraire, c'est qu'un caractère au moins a été remis dans le buffer grâce }
{ à la procédure UnGetChar. GetChar retourne alors le dernier caractère qui  }
{ a été remis dans ce Buffer.                                                }
{----------------------------------------------------------------------------}

Function GetChar( Var c : Char ) : Char;
Var Gc : Char;
Begin
  If PtrIn = 0 Then Gc := GetC( c )
  Else
    Begin
      Gc      := BufIn[PtrIn];
      PtrIn   := PtrIn - 1
    End;
  GetChar := Gc;
End;

{----------------------------------------------------------------------------}
{ Procedure UnGetChar (c : Char);                                            }
{----------------------------------------------------------------------------}
{ Remise d'un caractère dans le buffer : cette procédure tente d'ajouter au  }
{ buffer le caractère c, pour qu'il soit relu au prochain appel de GetChar.  }
{ Si cette operation est impossible (buffer plein), le programme est stoppé. }
{----------------------------------------------------------------------------}

Procedure UnGetChar( c : Char );
Begin
  If PtrIn = SizeBufIn Then
    Begin
      Write('Error in UnGetChar: Buffer is full.');
      Halt
    End
  Else
    Begin
      PtrIn        := PtrIn + 1;
      BufIn[PtrIn] := c
    End;
End;

{----------------------------------------------------------------------------}
{ Function GetCharNb (Var c : Char) : Char;                                  }
{----------------------------------------------------------------------------}
{ Lecture d'un caractère non blanc : cette fonction retourne le prochain     }
{ caractère non blanc en entrée, par appel à GetChar.                        }
{----------------------------------------------------------------------------}

Function GetCharNb( Var c : Char ) : Char;
Begin
  Repeat Until GetChar(c) <> ' ';
  GetCharNb := c
End;

{----------------------------------------------------------------------------}
{ Procedure Get (Var Ch : AnyStr; E : CharSet);                              }
{----------------------------------------------------------------------------}
{ Ajoute à une chaîne une suite de caractères d'un certain type : cette      }
{ procédure concatène à la chaîne Ch un caractère en entrée, tant que celui- }
{ ci appartient à l'ensemble E.                                              }
{----------------------------------------------------------------------------}

Procedure Get( Var Ch : AnyStr; E : CharSet );
Begin
  While (GetChar(Calu) In E) Do Ch := Ch + Calu;
  UnGetChar(Calu)
End;


{----------------------------------------------------------------------------}
{ Procedure Verifier( Ch : AnyStr);                                          }
{----------------------------------------------------------------------------}
{ Vérifie la présence de la chaîne Ch en entrée. Appel à la procédure        }
{ Erreur si cette chaîne n'est pas trouvée.                                  }
{----------------------------------------------------------------------------}

Procedure Verifier( Ch : AnyStr );
Var Ok : Boolean;
    I  : Integer;
Begin
  Ok := True;
  I  := 1;
  UnGetChar(GetCharNb(Calu));
  While ( Ok ) And ( I <= Length(Ch) ) Do
    Begin
      Ok := GetChar(Calu) = Ch[I];
      I  := I + 1
    End;
  If Not Ok Then Erreur(Ch+' expected');
  UnGetChar(GetCharNb(Calu))
End;

