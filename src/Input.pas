{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Input.pas                                                  }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                                 R E A D                                    }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Const
  EndOfInput = #$1A;                       { Code for 'end of input': Ctrl-Z   }
  Letters : CharSet = ['a'..'z','A'..'Z']; { 7-bit ASCII letters }
  Alpha : CharSet = ['a'..'z','A'..'Z','_','0'..'9']; { 7-bit ASCII alpha }
  Digits  : CharSet = ['0'..'9'];            { digits       }
  BlankSet : CharSet = [' ',#9,EndOfLine];   { space chars  }
  SizeBufIn = 10;                            { input buffer size      }

{ input stack }

Const
  MaxNbIFiles = 10;

Type
  TIFileStackPtr = 0..MaxNbIFiles;
  TBufInIndex = 1..SizeBufIn;

  TIFileState = Record
    FName : AnyStr;
    CurrentFile : TIFile;                 { file handler                  }
    FileIsOpen : Boolean;                 { is this file open?            }
    DeviceType  : TIODeviceType;          { where do we get input from?   }
    CurrentLine : AnyStr;                 { string read                   }
    HaveChars : Boolean;                  { are chars still available?    }
    PtrInp  : 0..AnyStrMaxSize;           { index of last char read       }
    WasEOL : Boolean;                     { last char was end of line     }
    LineNum : Integer;                    { current line number           }
    BufIn : Array[TBufInIndex] Of Char;   { input buffer                  }
    PtrIn : 0..SizeBufIn;                 { pointer to this buffer        }
  End;
  TIFileStack = Record
    Top : TIFileStackPtr;
    Stack : Array[1..MaxNbIFiles] Of TIFileState
  End;

Var
  IFileStack : TIFileStack;

{ debug: return a string containing the available chars }
Function AvailableChars : AnyStr;
Var 
  str : AnyStr;
  i : 0..SizeBufIn;
Begin
  str := '';
  With IFileStack.Stack[IFileStack.Top] Do
  Begin
    For i := 1 to PtrIn Do
      str := str + BufIn[i];
    If PtrInp < Length(CurrentLine) Then
      str := str + Copy(CurrentLine,PtrInp+1,AnyStrMaxSize)
  End;
  AvailableChars := str
End;

{ debug: display the codes of available chars }
Procedure DumpAvailableChars;
Begin
  CWriteStrCharCodes(AvailableChars)
End;

{ write an error message, pointing to the error }
Procedure DisplayInputErrorMessage; (* ( msg : AnyStr ); *)
Var K : Integer;
Begin
  With IFileStack.Stack[IFileStack.Top] Do
  Begin
    Case DeviceType Of
    TFile:
      Begin
        CWrite('Error line ' + IntToStr(LineNum) + ': ' + msg);
        CWriteLn
      End;
    TTerminal:
      Begin
        CWrite('Error: ' + msg);
        CWriteLn
      End
    End;
    CWrite(CurrentLine);
    CWriteLn;
    For K := 1 to PtrInp-1 Do
      CWrite(' ');
    CWrite('^');
    CWriteLn
  End
End;

{ ignores characters remaining unread in the current input line }
Procedure ClearInput;
Begin
  With IFileStack.Stack[IFileStack.Top] Do
  Begin
    CurrentLine := '';
    PtrInp := 0;
    PtrIn := 0; { reset "undo" buffer}
    HaveChars := False;
    WasEOL := True { fake }
  End
End;

{ initialize the state of the current input }
Procedure InitInput;
Begin
  ClearInput;
  IFileStack.Stack[IFileStack.Top].LineNum := 0
End;

{ return the name of the current input file }
Function InputIs : AnyStr;
Begin
  InputIs := IFileStack.Stack[IFileStack.Top].FName
End;

{ append a file to the input stack; return zero if the file cannot
  be opened }
Function PushIFile( FileName : AnyStr ) : TIFileStackPtr;
Var K : TIFileStackPtr;
Begin
  CheckCondition(IFileStack.Top < MaxNbIFiles,'Input Stack is full');
  IFileStack.Top := IFileStack.Top + 1;
  With IFileStack.Stack[IFileStack.Top] Do
  Begin
    FName := FileName;
    If FName = CONSOLE_NAME Then
    Begin
      DeviceType := TTerminal;
      FileIsOpen := True
    End
    Else
    Begin
      DeviceType := TFile;
      FileIsOpen := OpenForRead(FName,CurrentFile)
    End;
    If FileIsOpen Then 
    Begin
      InitInput;
      K := IFileStack.Top
    End
    Else
    Begin
      IFileStack.Top := IFileStack.Top - 1;
      K := 0
    End
  End;
  PushIFile := K
End;

{ initialize the input system, setting up the console as the default 
  input device }
Procedure InitIFileStack;
Var K : TIFileStackPtr;
Begin
  IFileStack.Top := 0;
  K := PushIFile(CONSOLE_NAME);
  CheckCondition(K>0,'cannot open default input console')
End;

{ lookup; return zero if the entry is not in the stack }
Function IFileIndex( FileName : AnyStr ) : TIFileStackPtr;
Var
  Found : Boolean;
  K : TIFileStackPtr;
Begin
  Found := False;
  K := IFileStack.Top;
  While Not Found And (K > 0) Do
  Begin
    Found := IFileStack.Stack[K].FName = FileName;
    If Not Found Then
      K := K - 1
  End;
  IFileIndex := K
End;

{ set a file as the current input file }
Function SetFileForInput( FileName : AnyStr ) : Boolean;
Var
  K,I : TIFileStackPtr;
  tmp : TIFileState;
Begin
  K := IFileIndex(FileName);
  If K = 0 Then
    K := PushIFile(FileName)
  Else If K < IFileStack.Top Then { not on top: move to top }
  Begin
    tmp := IFileStack.Stack[K];
    For I := K To IFileStack.Top - 1 Do
      IFileStack.Stack[I] := IFileStack.Stack[I+1];
    IFileStack.Stack[IFileStack.Top] := tmp
  End;
  SetFileForInput := K > 0
End;

{ close the input file at index K in the stack }
Procedure CloseIFileAtIndex( K : TIFileStackPtr );
Begin
  CheckCondition((K>0) And (K<=IFileStack.Top),
    'out of range input stack index');
  With IFileStack.Stack[K] Do
    If FileIsOpen And (DeviceType = TFile) Then
    Begin
      CloseIFile(FName,CurrentFile);
      FileIsOpen := False
    End
End;

{ close all the opened input files and reset the input stack }
Procedure ResetIFileStack;
Var
  I : TIFileStackPtr;
Begin
  For I := 1 To IFileStack.Top Do
    CloseIFileAtIndex(I);
  InitIFileStack
End;

{ close an input file; if it is the console, move it back to 
  position 1; TODO: what PII+ does in that case? }
Procedure CloseInput( FileName : AnyStr );
Var
  K,I : TIFileStackPtr;
  tmp : TIFileState;
Begin
  K := IFileIndex(FileName);
  If K > 0 Then { delete }
  Begin
    CloseIFileAtIndex(K);
    Case IFileStack.Stack[K].DeviceType Of
    TTerminal:
      If (K = IFileStack.Top) And (K > 1) Then
      Begin
        tmp := IFileStack.Stack[IFileStack.Top];
        For I := IFileStack.Top DownTo 2 Do
          IFileStack.Stack[I] := IFileStack.Stack[I-1];
        IFileStack.Stack[1] := tmp
      End;
    TFile:
      Begin
        If K < IFileStack.Top Then 
          For I := K To IFileStack.Top-1 Do
            IFileStack.Stack[I] := IFileStack.Stack[I+1];
        IFileStack.Top := IFileStack.Top - 1
      End
    End
  End
End;

{ close the current input file, if it is not the console }
Procedure CloseCurrentInput;
Begin
  CloseInput(IFileStack.Stack[IFileStack.Top].FName)
End;

{ return true if the current input is the terminal }
Function InputIsTerminal : Boolean;
Begin
  InputIsTerminal := IFileStack.Stack[IFileStack.Top].DeviceType = TTerminal
End;

{ read a line from the keyboard }
Procedure ReadFromConsole;
Begin
  With IFileStack.Stack[IFileStack.Top] Do
  Begin
    InitInput;
    ReadLnKbd(CurrentLine);
    HaveChars := Length(CurrentLine) > 0
  End
End;

{ read from the file, converting CR-LF, CR, and LF to EndOfLine}
Procedure ReadFromFile;
Var 
  c : Char;
  s : AnyStr; { current char, or two chars if CR is followed by a char that is not LF}
Begin
  With IFileStack.Stack[IFileStack.Top] Do
  Begin
    ClearInput;
    While Not Eof(CurrentFile) And (Length(CurrentLine)<AnyStrMaxSize-1) Do { leave one place for a char after CR }
    Begin
      Read(CurrentFile,c);
      If (Ord(c)=13) Or (Ord(c)=10) Then
        s := EndOfLine
      Else
        s := c;
      { handle possible CR-LF sequence }
      If Ord(c)=13 Then
      Begin
        If Not Eof(CurrentFile) Then
        Begin
          Read(CurrentFile,c);
          If Ord(c)<>10 Then { not LF: make sure we do not lose it }
            s := s + c
        End
      End;
      CurrentLine := CurrentLine + s
    End;
    HaveChars := (Length(CurrentLine) > 0) Or (PtrIn > 0)
  End
End;

{ grab a char }
Function OneChar : Char;
Var c : Char;
Begin
  With IFileStack.Stack[IFileStack.Top] Do
  Begin
    PtrInp := PtrInp + 1;
    c := CurrentLine[PtrInp];
    HaveChars := (PtrInp < Length(CurrentLine)) Or (PtrIn > 0);
    OneChar := c
  End
End;

{ read a char; if there is no more characters to read, read a new line in 
  CurrentLine and return EndOfLine; if the end of the file has been reached
  return EndOfInput }
Function GetC( Var c : Char ) : Char;
Begin
  With IFileStack.Stack[IFileStack.Top] Do
  Begin
    If WasEOL Then { first char after EndOfLine }
      LineNum := LineNum + 1;
    WasEOL := False;

    If Not HaveChars Then
    Begin
      Case DeviceType Of
      TFile :
        Begin
          If Eof(CurrentFile) Then
          Begin
            If FileIsOpen Then 
              CloseIFile(FName,CurrentFile);
            FileIsOpen := False;
            c := EndOfInput;
            GetC := c;
            Exit
          End;
          ReadFromFile
        End;
      TTerminal:
        Begin
          c := EndOfInput;
          GetC := c;
          Exit
        End
      End
    End;

    c := OneChar;
    If c = EndOfLine Then
      WasEOL := True;
    GetC := c
  End
End;

{ buffered read  }
Function GetChar( Var c : Char ) : Char;
Begin
  With IFileStack.Stack[IFileStack.Top] Do
  Begin
    If PtrIn = 0 Then
      c := GetC(c)
    Else
    Begin
      c  := BufIn[PtrIn];
      PtrIn := PtrIn - 1;
      HaveChars := (PtrInp < Length(CurrentLine)) Or (PtrIn > 0);
    End;
    GetChar := c
  End
End;

{ put a char back in the buffer }
Procedure UnGetChar( c : Char );
Begin
  With IFileStack.Stack[IFileStack.Top] Do
  Begin
    CheckCondition(PtrIn < SizeBufIn,'UnGetChar: buffer is full.');
    PtrIn := PtrIn + 1;
    BufIn[PtrIn] := c;
    HaveChars := True
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
Var
  c1 : Char;
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
  c := NextCharNb(c) { FIXME }
End;

{ append to a string chars while they belong to a certain set; return
  the number of chats appended }
Function GetCharWhile( Ch : StrPtr; E : CharSet ) : LongInt;
Var 
  c : Char;
  n : LongInt;
Begin
  n := 0;
  While (GetChar(c) In E) Do 
  Begin
    StrAppendChar(Ch,c);
    n := n + 1
  End;
  UnGetChar(c);
  GetCharWhile := n
End;

{ returns True if c is a ISO-8859-1 alpha or the first byte 
  of a 2-byte UTF-8 alpha, where alpha is defined as letters,
  digits and underscore }
Function IsAlpha( c : Char ) : Boolean;
Begin
  { reject ascii non-letters, and ISO-8859-1 non-letters,               }
  { see https://fr.wikipedia.org/wiki/ISO/CEI_8859-1                    }
  { this rejects UTF8 identifiers or variable names containing a 2-byte }
  { UTF8 letters starting with a byte sets in the second condition      }
  IsAlpha := Not (c In ([#$00..#$7F] - Alpha))
    And Not (c In [#$A0..#$BF,#$D7,#$F7])
End;

{ try to read one letter, as defined page R1-2 of the Prolog II+ manual:
  "A"|...|"Z"|"a"|...|"z"|"À" ... "ß" - "x" | "à" ... "ÿ" - "÷"
  where x is the multiplication sign;
  it is assumed that, if the input is ISO-8859-1, there is no "Ã" (C3)
  followed by a char that would make the 2-byte sequence an UTF8
  letter; hopefully 80-9F are undefined in ISO-8859-1; A0-BF is possible
  but none of those combinations (e.g. Ã¢) would be a valid part of an
  identifier, so either the input is UTF8 and there is a syntax error,
  or the the input is UTF8, which we therefore assume;
  "big letters" are A-Z only, see PrologII+ manual, R 1-23  }
Function GrabOneLetter( Var Ch : StrPtr; Var IsUpper : Boolean ) : Boolean;
Var
  c1,c2 : Char;
  Found : Boolean;
Begin
  IsUpper := False;
  Found := False;
  c1 := NextChar(c1);
  c2 := NextNextChar(c2);
  { 2-byte UTF8: https://www.utf8-chartable.de/ }
  If (c1 = #$C3) And (c2 In ([#$80..#$BF] - [#$97,#$B7])) Then
  Begin
    Found := True;
    StrAppendChar(Ch,GetChar(c1));
    StrAppendChar(Ch,GetChar(c2));
    IsUpper := False
  End
  Else
  { ISO-8859-1: https://fr.wikipedia.org/wiki/ISO/CEI_8859-1 }
  If c1 In (Letters + [#$C0..#$FF] - [#$D7,#$F7]) Then
  Begin
    Found := True;
    StrAppendChar(Ch,GetChar(c1));
    IsUpper := c1 In ['A'..'Z']
  End;
  GrabOneLetter := Found
End;

{ grab letters; return the number of letters appended to Ch }
Function GrabLetters( Var Ch : StrPtr ) : LongInt;
Var
  n : LongInt;
  IsUpper : Boolean;
Begin
  n := 0;
  While GrabOneLetter(Ch,IsUpper) Do
    n := n + 1;
  GrabLetters := n
End;

{ append to a string any alphanumeric characters (plus underscore)
  in the input stream; it is assumed that the input stream is either 
  ISO-8859-1 or UTF-8 encoded; the function returns the number of 
  characters added to the string }
Function GrabAlpha( Var Ch : StrPtr ) : LongInt;
Var
  n : LongInt;
  More : Boolean;
  IsUpper : Boolean;
Begin
  n := 0;
  More := True;
  While More Do
  Begin
    n := n + GetCharWhile(Ch,Alpha);
    More := GrabOneLetter(Ch,IsUpper);
    If More Then
      n := n + 1
  End;
  GrabAlpha := n
End;

{ append chars to string Ch until a char in E is read }
Procedure GetCharUntil( Var Ch : StrPtr; E : CharSet );
Var c : Char;
Begin
  While Not (GetChar(c) In E) Do 
    StrAppendChar(Ch,c);
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
  While Ok And (I <= Length(Ch)) Do
  Begin
    Ok := GetChar(c) = Ch[I];
    I  := I + 1
  End;
  RaiseErrorIf(Not Ok,'"' + Ch + '" expected')
End;

{ read a line (from the keyboard) if no more characters are available
  in the input line; optionally skip spaces beforehand; skipping 
  spaces is useful when reading a terms with in(t); this is the 
  opposite when reading a char (in_char) or line (inl); }
Procedure CheckConsoleInput( SkipSpaces : Boolean );
Var c : Char;
Begin
  With IFileStack.Stack[IFileStack.Top] Do
    If (DeviceType = TTerminal) Then
    Begin
      If SkipSpaces Then
        If GetCharNb(c) = EndOfInput Then
          InitInput;
      If Not HaveChars Then
        ReadFromConsole
    End
End;