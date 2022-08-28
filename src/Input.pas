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
  EndOfInput = #$FF;                       { Code 'fin entrée'           }
  EndOfLine = #10;                         { Code 'fin de ligne'         }
  Letters : CharSet = ['a'..'z','A'..'Z'];   { ASCII letters      }
  Digits  : CharSet = ['0'..'9'];            { digits       }
  BlankSet : CharSet = [' ',#9,EndOfLine];   { space chars  }
  SizeBufIn = 10;                          { Taille Buffer d'entree      }

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

  Function HaveChars : Boolean;
  Begin
    HaveChars := (Length(CurrentLine) > 0) And (PtrInp <= Length(CurrentLine))
  End;

Begin
  If Not HaveChars Then
    Case Source Of
    InputFile :
      If Not Eof(CurrentFile) Then
      Begin
        InitCurrentLine;
        Readln(CurrentFile,CurrentLine);
        LineNum := LineNum + 1;
        If LineNum > 1 Then { actually, we just finished reading a line }
        Begin
          GetC := EndOfLine;
          Exit
        End
      End
      Else
      Begin
        If FileIsOpen Then Close(CurrentFile);
        FileIsOpen := False;
        GetC := EndOfInput;
        Exit
      End;
    Repl:
      Begin
        GetC := EndOfInput;
        Exit
      End
    End;
  If HaveChars Then
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

Procedure GetCharWhile( Var Ch : AnyStr; E : CharSet );
Var c : Char;
Begin
  While (GetChar(c) In E) Do Ch := Ch + c;
  UnGetChar(c)
End;

{----------------------------------------------------------------------------}
{ Returns True if c is a ISO-8859-1 letter or the first byte                 }
{ of a 2-byte UTF-8 letter.                                                  }
{----------------------------------------------------------------------------}

Function IsLetter( c : Char ) : Boolean;
Begin
  { reject ascii non-letters, and ISO-8859-1 non-letters,               }
  { see https://fr.wikipedia.org/wiki/ISO/CEI_8859-1                    }
  { this rejects UTF8 identifiers or variable names containing a 2-byte }
  { UTF8 letters starting with a byte sets in the second condition      }
  IsLetter := Not (c In ([#$00..#$7F] - Letters))
    And Not (c In [#$A0..#$BF,#$D7,#$F7])
End;

{----------------------------------------------------------------------------}
{ Append to Ch any letter in the input stream. It is assumed that the input  }
{ stream is either ISO-8859-1 or UTF-8 encoded. We rely on heuristics.       }
{ The function returns the number of characters added to Ch.                 }
{----------------------------------------------------------------------------}

Function GrabLetters( Var Ch : AnyStr ) : Integer;
Var
  c,c2 : Char;
  S : AnyStr;
  n : Byte;
  Stop : Boolean;
Begin
  n := 0;
  Repeat
    Stop := False;
    { get next run of ASCII letters }
    S := '';
    GetCharWhile(S,Letters);
    n := n + length(S);
    Ch := Ch + S;
    { examine the char on which we stopped }
    c := NextChar(c);
    Stop := Not IsLetter(c);
    If Not Stop Then
    Begin
      Ch := Ch + GetChar(c); { glob it }
      n := n + 1;
      { could be a 2-byte UTF8 character? }
      If c In [#$C0..#$DF] Then
        { Heuristic 2: no identifiers or variable names in a UTF8 program }
        {  contain a 2-byte UTF8 letter made of these two codes }
        If (c = #$C3) and (NextChar(c2) in [#$80..#$BF]) Then
          Ch := Ch + GetChar(c2) { glob it without counting it }
    End
  Until Stop;
  GrabLetters := n
End;

{----------------------------------------------------------------------------}
{ Append chars to Ch until a char in E is read.                              }
{----------------------------------------------------------------------------}

Procedure GetCharUntil( Var Ch : AnyStr; E : CharSet );
Var c : Char;
Begin
  While Not (GetChar(c) In E) Do Ch := Ch + c;
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
