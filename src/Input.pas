{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Input.pas                                                  }
{   Author      : Christophe Bisière                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                                 R E A D                                    }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Type
  CharSet   = Set Of Char;               { Type ensemble de caractères   }
  TInput    = (InputFile, Repl);         { Type of input                 }

Const
  SizeBufIn = 10;                          { Taille Buffer d'entree      }
  EndOfInput = #$FF;                       { Code 'fin entrée'           }
  EndOfLine = #10;                         { Code 'fin de ligne'         }
  BlankSet : CharSet = [' ',#9,EndOfLine]; { Caractères d'espacement     }

Var
  CurrentLine : AnyStr;                  { Chaîne lue                    }
  PtrInp  : Byte;                        { Prochain car ds CurrentLine   }
  BufIn   : Array[1..SizeBufIn] Of Char; { Buffer d'entrée               }
  PtrIn   : Byte;                        { Pointeur dans ce buffer       }
  CurrentFile : Text;                    { Fichier utilisateur           }
  FileIsOpen : Boolean;                  { Fichier ouvert ?              }
  LineNum : Integer;
  Error   : Boolean;                     { En erreur ?                   }
  Source  : TInput;                      { Where do we get input?        }

{----------------------------------------------------------------------------}
{ Constate une erreur. Affiche un message.                                   }
{----------------------------------------------------------------------------}

Procedure RaiseError( S : AnyStr );
Var K : Integer;
Begin
  If Not Error Then
  Begin
    Case Source Of
    InputFile:
      Writeln('Error line ',LineNum,': ',S);
    Repl:
      Writeln('Error: ',S)
    End;
    Writeln(CurrentLine);
    For K := 1 to PtrInp-1 Do
      Write(' ');
    Writeln('^')
  End;
  Error := True
End;

{----------------------------------------------------------------------------}
{ Génère une erreur fatale si une condition n'est pas satisfaite.            }
{----------------------------------------------------------------------------}

Procedure CheckCondition; (* ( Cond : Boolean; Message : AnyStr) *)
Begin
  If Not Cond Then
  Begin
    RaiseError('Internal error: ' + Message);
    Halt
  End
End;

{----------------------------------------------------------------------------}
{ Vide la chaîne de lecture.                                                 }
{----------------------------------------------------------------------------}

Procedure InitCurrentLine;
Begin
  CurrentLine := '';
  PtrInp := Length(CurrentLine) + 1
End;

{----------------------------------------------------------------------------}
{ Initialise la lecture.                                                     }
{----------------------------------------------------------------------------}

Procedure InitInput;
Begin
  InitCurrentLine;
  PtrIn := 0
End;

{----------------------------------------------------------------------------}
{ Ouverture d'un fichier pour lecture d'un programme Prolog.                 }
{----------------------------------------------------------------------------}

Function SetFileForInput( FileName : AnyStr ) : Boolean;
Begin
  Assign(CurrentFile,FileName);
  {$I-}
  Reset(CurrentFile);
  {$I+}
  FileIsOpen := IOResult = 0;
  If FileIsOpen Then
  Begin
    InitInput;
    Source := InputFile;
    LineNum := 0
  End;
  SetFileForInput := FileIsOpen
End;

{----------------------------------------------------------------------------}
{ Lecture d'une ligne au clavier.                                            }
{----------------------------------------------------------------------------}

Procedure LireCommande;
Begin
  InitInput;
  Source := Repl;
  Readln(CurrentLine)
End;

{----------------------------------------------------------------------------}
{ Lecture d'un caractère.                                                    }
{ Retourne le caractère pointé par PtrInp dans la ligne CurrentLine.              }
{ Cette operation peut être impossible pour deux raisons :                   }
{ (1) Il n'y a plus de caractère dans CurrentLine. GetC provoque alors la lecture }
{     d'une nouvelle ligne et retourne le caractère EndOfLine;               }
{ (2) C'est le fin du fichier Fic. GetC retourne alors le caractère          }
{     EndOfInput.                                                            }
{----------------------------------------------------------------------------}

Function GetC( Var c : Char ) : Char;
Begin
  If Source = InputFile Then
  Begin
    If (Length(CurrentLine) = 0) Or (PtrInp > Length(CurrentLine)) Then
    Begin
      If Not Eof(CurrentFile) Then
      Begin
        InitCurrentLine;
        Readln(CurrentFile,CurrentLine);
        LineNum := LineNum + 1
      End
      Else
      Begin
        If FileIsOpen Then Close(CurrentFile);
        FileIsOpen := False;
        GetC := EndOfInput;
        Exit
      End
    End
  End;
  If (Length(CurrentLine) > 0) And (PtrInp <= Length(CurrentLine)) Then
  Begin
    c := CurrentLine[PtrInp];
    PtrInp := PtrInp + 1
  End
  Else
  Begin
    InitCurrentLine;
    Case Source Of
    InputFile:
      c := EndOfLine;
    Repl:
      c := EndOfInput
    End
  End;
  GetC := c
End;

{----------------------------------------------------------------------------}
{ Lecture bufferisée d'un caractère : si le buffer d'entrée BufIn est vide,  }
{ la fonction GetChar retourne le caractère renvoyé par GetC. Dans le cas    }
{ contraire, c'est qu'un caractère au moins a été remis dans le buffer grâce }
{ à la procédure UnGetChar. GetChar retourne alors le dernier caractère qui  }
{ a été remis dans ce Buffer.                                                }
{----------------------------------------------------------------------------}

Function GetChar( Var c : Char ) : Char;
Begin
  If PtrIn = 0 Then
    c := GetC( c )
  Else
  Begin
    c  := BufIn[PtrIn];
    PtrIn := PtrIn - 1
  End;
  GetChar := c
End;

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
    PtrIn := PtrIn + 1;
    BufIn[PtrIn] := c
  End
End;

{----------------------------------------------------------------------------}
{ Lecture avancée d'un caractère.                                            }
{----------------------------------------------------------------------------}

Function NextChar( Var c : Char ) : Char;
Begin
  UnGetChar(GetChar(c));
  NextChar := c
End;

{----------------------------------------------------------------------------}
{ Double lecture avancée d'un caractère.                                     }
{----------------------------------------------------------------------------}

Function NextNextChar( Var c : Char ) : Char;
Var c1 : Char;
Begin
  c1 := GetChar(c1);
  c := NextChar(c);
  UnGetChar(c1);
  NextNextChar := c
End;

{----------------------------------------------------------------------------}
{ Lecture d'un caractère non blanc : cette fonction retourne le prochain     }
{ caractère non blanc en entrée, par appel à GetChar.                        }
{----------------------------------------------------------------------------}

Function GetCharNb( Var c : Char ) : Char;
Begin
  Repeat Until Not (GetChar(c) In BlankSet);
  GetCharNb := c
End;

{----------------------------------------------------------------------------}
{ Lecture avancée d'un caractère non blanc.                                  }
{----------------------------------------------------------------------------}

Function NextCharNb( Var c : Char ) : Char;
Begin
  UnGetChar(GetCharNb(c));
  NextCharNb := c
End;

{----------------------------------------------------------------------------}
{ Lecture d'éventuels espaces.                                               }
{----------------------------------------------------------------------------}

Procedure Spaces;
Var c : Char;
Begin
  c := NextCharNb(c)
End;

{----------------------------------------------------------------------------}
{ Ajoute à une chaîne une suite de caractères d'un certain type : cette      }
{ procédure concatène à la chaîne Ch un caractère en entrée, tant que celui- }
{ ci appartient à l'ensemble E.                                              }
{----------------------------------------------------------------------------}

Procedure Get( Var Ch : AnyStr; E : CharSet );
Var c : Char;
Begin
  While (GetChar(c) In E) Do Ch := Ch + c;
  UnGetChar(c)
End;

{----------------------------------------------------------------------------}
{ Vérifie la présence de la chaîne Ch en entrée, éventuellement après une    }
{ suite de blancs. Génère une erreur si cette chaîne n'est pas trouvée.      }
{----------------------------------------------------------------------------}

Procedure Verify( Ch : AnyStr );
Var
  Ok : Boolean;
  I  : Integer;
  c  : Char;
Begin
  Ok := True;
  I  := 1;
  c := NextCharNb(c);
  While ( Ok ) And ( I <= Length(Ch) ) Do
  Begin
    Ok := GetChar(c) = Ch[I];
    I  := I + 1
  End;
  If Not Ok Then
    RaiseError('"' + Ch + '" expected')
End;
