{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : IStack.pas                                                 }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                           I N P U T   S T A C K                            }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ input stack }

{
  ASSUMPTIONS AND LIMITS:
  -----------------------
  1) IFilesMax: 5
    maximum number of stacked input files, including the predefined 
    input 'console'.
}

Unit IStack;

Interface

Uses
  Strings,
  Errs,
  Chars,
  IChar,
  Buffer,
  Files,
  IStream;

Const
  IFilesMax = 5;

Type
  TIFileStackIndex = 0..IFilesMax; { stack of input files; see PII+ doc R5-1}

Procedure DoClearInput;
Procedure DisplayInputErrorMessage( msg : TString );
Function InputIs : TString;
Function InputEncoding : TEncoding;
Procedure SetInputEncoding( Enc : TEncoding );
Procedure InitIFileStack;
Function SetFileForInput( FileName : TString ) : Boolean;
Procedure ResetIFileStack;
Procedure CloseInputByName( FileName : TString );
Procedure CloseCurrentInput;
Procedure ReadFromConsole;
Procedure GetIChar( Var e : TIChar );
Function GetChar( Var c : TChar ) : TChar;
Function GetCharNb( Var c : TChar ) : TChar;
Procedure UnGetChar;
Procedure UngetChars( line : TLineNum; col : TCharPos );
Procedure NextIChar( Var e : TIChar );
Function NextChar( Var c : TChar ) : TChar;
Function NextNextChar( Var c : TChar ) : TChar;
Procedure CheckConsoleInput( SkipSpaces : Boolean );
Procedure IStackDumpCurrentInput;


Implementation
{-----------------------------------------------------------------------------}

Type
  TIFileStack = Record
    Top : TIFileStackIndex;
    Stack : Array[1..IFilesMax] Of TIStream
  End;

Var
  IFileStack : TIFileStack;

{----------------------------------------------------------------------------}
{ accessors (top IStream)                                                    }
{----------------------------------------------------------------------------}

{ initialize the read buffers of the current input }
Procedure ResetInput;
Begin
  With IFileStack Do
    ResetIStream(Stack[Top])
End;

{ ignores characters remaining unread in the current input line }
Procedure DoClearInput;
Begin
  With IFileStack.Stack[IFileStack.Top] Do
    BufDiscardUnread(IBuf)
End;

{ write an error message, pointing to the error }
Procedure DisplayInputErrorMessage( msg : TString );
Begin
  With IFileStack Do
    DisplayIStreamErrorMessage(Stack[Top],msg)
End;

{ return the name of the current input file }
Function InputIs : TString;
Begin
  With IFileStack Do
    InputIs := Stack[Top].FName
End;

{ return true if the current input is the terminal }
Function InputIsConsole : Boolean;
Begin
  With IFileStack Do
    InputIsConsole := Stack[Top].DeviceType = TTerminal
End;

{ encoding of the current input }
Function InputEncoding : TEncoding;
Begin
  With IFileStack Do
    InputEncoding := GetIStreamEncoding(Stack[Top])
End;

{ set the encoding of the current input }
Procedure SetInputEncoding( Enc : TEncoding );
Begin
  With IFileStack Do
    SetIStreamEncoding(Stack[Top],Enc)
End;

{----------------------------------------------------------------------------}
{ stack management                                                           }
{----------------------------------------------------------------------------}

{ append a file to the input stack; return zero if the file cannot
  be opened }
Function PushIFile( FileName : TString ) : TIFileStackIndex;
Var 
  f : TIStream;
  K : TIFileStackIndex;
Begin
  With IFileStack Do
  Begin
    CheckCondition(Top < IFilesMax,'Input Stack is full');
    OpenIStream(FileName,f);
    If f.IsOpen Then
    Begin
      Top := Top + 1;
      Stack[Top] := f;
      K := Top
    End
    Else
      K := 0
  End;
  PushIFile := K
End;

{ initialize the input system, setting up the console as the default 
  input device }
Procedure InitIFileStack;
Var K : TIFileStackIndex;
Begin
  IFileStack.Top := 0;
  K := PushIFile(CONSOLE_NAME);
  CheckCondition(K>0,'cannot open default input console')
End;

{ lookup from the top; return zero if the entry is not in the stack }
Function IFileIndex( FileName : TString ) : TIFileStackIndex;
Var
  Found : Boolean;
  K : TIFileStackIndex;
Begin
  With IFileStack Do
  Begin
    Found := False;
    K := Top;
    While Not Found And (K > 0) Do
    Begin
      Found := Stack[K].FName = FileName;
      If Not Found Then
        K := K - 1
    End
  End;
  IFileIndex := K
End;

{ set a file as the current input file }
Function SetFileForInput( FileName : TString ) : Boolean;
Var
  K,I : TIFileStackIndex;
  tmp : TIStream;
Begin
  With IFileStack Do
  Begin
    K := IFileIndex(FileName);
    If K = 0 Then
      K := PushIFile(FileName)
    Else If K < Top Then { not on top: move to top }
    Begin
      tmp := Stack[K];
      For I := K To Top - 1 Do
        Stack[I] := Stack[I+1];
      Stack[Top] := tmp
    End
  End;
  SetFileForInput := K > 0
End;

{ close the input file at index K in the stack }
Procedure CloseIFileAtIndex( K : TIFileStackIndex );
Begin
  With IFileStack Do
  Begin
    CheckCondition((K > 0) And (K <= Top), 
        'CloseIFileAtIndex: out of range input stack index');
    CloseIStream(Stack[K])
  End
End;

{ close all opened input files }
Procedure CloseAllIFiles;
Var
  I : TIFileStackIndex;
Begin
  With IFileStack Do
    For I := Top DownTo 1 Do
      CloseIFileAtIndex(I)
End;

{ reset the input stack, leaving the console in its current state; console
 input buffer should *not* be reset before executing a user query typed at the
 prompt, otherwise in_char(c) would not work as intended }
Procedure ResetIFileStack;
Var
  console : TIStream;
  K : TIFileStackIndex;
Begin
  With IFileStack Do
  Begin
    { backup the console state, including the input buffer }
    K := IFileIndex(CONSOLE_NAME);
    CheckCondition(K > 0, 'ResetIFileStack: unable to locate the console');
    console := Stack[K];
    { make sure all the files are closed }
    CloseAllIFiles;
    { reinstall the console }
    Top := 1;
    Stack[Top] := console
  End
End;

{ close an input file; at index K in the stack; if it is the console, move it 
 back to position 1; TODO: what PII+ does in that case? }
Procedure CloseInputAtIndex( K : TIFileStackIndex );
Var
  I : TIFileStackIndex;
  tmp : TIStream;
Begin
  With IFileStack Do
  Begin
    Case Stack[K].DeviceType Of
    TTerminal:
      If (K = Top) And (K > 1) Then
      Begin
        tmp := Stack[Top];
        For I := Top DownTo 2 Do
          Stack[I] := Stack[I-1];
        Stack[1] := tmp
      End;
    TFile:
      Begin
        CloseIFileAtIndex(K);
        If K < Top Then 
          For I := K To Top-1 Do
            Stack[I] := Stack[I+1];
        Top := Top - 1
      End
    End
  End
End;

{ close an input file by name }
Procedure CloseInputByName( FileName : TString );
Var
  K : TIFileStackIndex;
Begin
  With IFileStack Do
  Begin
    K := IFileIndex(FileName);
    If K > 0 Then { delete }
      CloseInputAtIndex(K)
  End
End;

{ close the current input file, if it is not the console }
Procedure CloseCurrentInput;
Begin
  With IFileStack Do
    CloseInputAtIndex(Top)
End;

{----------------------------------------------------------------------------}
{ read on current input                                                      }
{----------------------------------------------------------------------------}

{ read a line from the keyboard: FIXME: why top? }
Procedure ReadFromConsole;
Begin
  With IFileStack Do
    ReadLineFromKeyboard(Stack[Top])
End;

{ read one codepoint with position }
Procedure GetIChar( Var e : TIChar );
Begin
  With IFileStack Do
    GetICharFromStream(Stack[Top],e)
End;

{ read one codepoint }
Function GetChar( Var c : TChar ) : TChar;
Begin
  With IFileStack Do
    GetChar := GetCharFromStream(Stack[Top],c)
End;

{ read the next non-blank codepoint }
Function GetCharNb( Var c : TChar ) : TChar;
Begin
  With IFileStack Do
    GetCharNb := GetCharNbFromStream(Stack[Top],c)
End;

{ unread the last read codepoint }
Procedure UnGetChar;
Begin
  With IFileStack Do
    UngetCharFromStream(Stack[Top])
End;

{ unread up to a certain line and column number }
Procedure UngetChars( line : TLineNum; col : TCharPos );
Begin
  With IFileStack Do
    UngetCharsFromStream(Stack[Top],line, col)
End;

{ return the next codepoint with position, without consuming it }
Procedure NextIChar( Var e : TIChar );
Begin
  With IFileStack Do
    NextICharFromStream(Stack[Top],e)
End;

{ return the next codepoint without consuming it }
Function NextChar( Var c : TChar ) : TChar;
Begin
  With IFileStack Do
    NextChar := NextCharFromStream(Stack[Top],c)
End;

{ ditto but two codepoints in advance }
Function NextNextChar( Var c : TChar ) : TChar;
Begin
  With IFileStack Do
    NextNextChar := NextNextCharFromStream(Stack[Top],c)
End;

{ if the current input is from a console, read a line from the keyboard when 
 there is no more chars available }
Procedure CheckConsoleInput( SkipSpaces : Boolean );
Begin
  With IFileStack Do
    CheckConsoleInputStream(Stack[Top],SkipSpaces)
End;


{----------------------------------------------------------------------------}
{ DEBUG                                                                      }
{----------------------------------------------------------------------------}

Procedure IStackDumpCurrentInput;
Begin
  With IFileStack Do
    IStreamDump(Stack[Top])
End;

End.