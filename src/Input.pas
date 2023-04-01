{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Input.pas                                                  }
{   Author      : Christophe Bisière                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                                 R E A D                                    }
{                                                                            }
{----------------------------------------------------------------------------}

Procedure CoreDump( P : ProgPtr; Message : AnyStr; Trace : Boolean ); Forward;

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Type
  CharSet   = Set Of Char;
  TInput    = (InputFile, Repl);          { input type                 }

Const
  EndOfInput = #$1A;                       { Code for 'end of input': Ctrl-Z   }
  EndOfLine = #10;                         { Code for 'end of line'         }
  Letters : CharSet = ['a'..'z','A'..'Z','_'];  { ASCII letters + underscore }
  Digits  : CharSet = ['0'..'9'];            { digits       }
  BlankSet : CharSet = [' ',#9,EndOfLine];   { space chars  }
  SizeBufIn = 10;                          { input buffer size      }

Var
  CurrentLine : AnyStr;                  { string read                   }
  PtrInp  : Byte;                        { next char in CurrentLine      }
  BufIn   : Array[1..SizeBufIn] Of Char; { input buffer                  }
  PtrIn   : Byte;                        { pointer to this buffer        }
  CurrentFile : Text;                    { use file                      }
  FileIsOpen : Boolean;                  { is a file open?               }
  LineNum : Integer;
  Error   : Boolean;                     { an error?                     }
  Source  : TInput;                      { where do we get input?        }
  CurrentProgram : ProgPtr;              { current Prolog program        }

{ an error occurred; display a message }
Procedure RaiseError( S : AnyStr );
Var K : Integer;
Begin
  If Not Error Then
  Begin
    Case Source Of
    InputFile:
      WriteLn('Error line ',LineNum,': ',S);
    Repl:
      WriteLn('Error: ',S)
    End;
    WriteLn(CurrentLine);
    For K := 1 to PtrInp-1 Do
      Write(' ');
    WriteLn('^')
  End;
  Error := True
End;

{ assert }
Procedure CheckCondition; (* ( Cond : Boolean; Message : AnyStr) *)
Begin
  If Not Cond Then
  Begin
    RaiseError('Internal error: ' + Message);
    Writeln('CORE DUMP:');
    PrintMemoryStats;
    DumpRegisteredObject;
    If CurrentProgram <> Nil Then
      CoreDump(CurrentProgram,'Runtime error',True);
    Halt(1)
  End
End;

{ empty the input string }
Procedure InitCurrentLine;
Begin
  CurrentLine := '';
  PtrInp := Length(CurrentLine) + 1
End;

{ initialize the input system }
Procedure InitInput;
Begin
  InitCurrentLine;
  PtrIn := 0
End;

{ open a file }
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

{ read a line from the keyboard }
Procedure ReadCommand;
Begin
  InitInput;
  Source := Repl;
  ReadLnKbd(CurrentLine)
End;

{ read a char; if there is no more characters to read, read a new line in 
  CurrentLine and return EndOfLine; if the end of the file has been reached
  return EndOfInput }
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
        ReadLn(CurrentFile,CurrentLine);
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

{ buffered read  }
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

{ put a char back in the buffer }
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

{ return the next char without consuming it }
Function NextChar( Var c : Char ) : Char;
Begin
  UnGetChar(GetChar(c));
  NextChar := c
End;

{ ditto but two chars in advance }
Function NextNextChar( Var c : Char ) : Char;
Var c1 : Char;
Begin
  c1 := GetChar(c1);
  c := NextChar(c);
  UnGetChar(c1);
  NextNextChar := c
End;

{ read the next non-blank char }
Function GetCharNb( Var c : Char ) : Char;
Begin
  Repeat Until Not (GetChar(c) In BlankSet);
  GetCharNb := c
End;

{ return the next non-blank char without consuming it }
Function NextCharNb( Var c : Char ) : Char;
Begin
  UnGetChar(GetCharNb(c));
  NextCharNb := c
End;

{ consume spaces if any }
Procedure Spaces;
Var c : Char;
Begin
  c := NextCharNb(c)
End;

{ append to a string chars while they belong to a certain set }
Procedure GetCharWhile( Var Ch : AnyStr; E : CharSet );
Var c : Char;
Begin
  While (GetChar(c) In E) Do Ch := Ch + c;
  UnGetChar(c)
End;

{ returns True if c is a ISO-8859-1 letter or the first byte 
  of a 2-byte UTF-8 letter }
Function IsLetter( c : Char ) : Boolean;
Begin
  { reject ascii non-letters, and ISO-8859-1 non-letters,               }
  { see https://fr.wikipedia.org/wiki/ISO/CEI_8859-1                    }
  { this rejects UTF8 identifiers or variable names containing a 2-byte }
  { UTF8 letters starting with a byte sets in the second condition      }
  IsLetter := Not (c In ([#$00..#$7F] - Letters))
    And Not (c In [#$A0..#$BF,#$D7,#$F7])
End;

{ append to a string any letter in the input stream; it is assumed that 
  the input stream is either ISO-8859-1 or UTF-8 encoded; we rely on 
  heuristics; the function returns the number of characters added to the
  string }
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

{ append chars to Ch until a char in E is read }
Procedure GetCharUntil( Var Ch : AnyStr; E : CharSet );
Var c : Char;
Begin
  While Not (GetChar(c) In E) Do Ch := Ch + c;
  UnGetChar(c)
End;

{ verify that a certain string is in input, possibly after a run of spaces }
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
