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
  ShortStr,
  Errs,
  Chars,
  IChar,
  Buffer,
  Files,
  Trace,
  IStream;

Const
  IFilesMax = 5;

Type
  TIFileStackIndex = 0..IFilesMax; { stack of input files; see PII+ doc R5-1}

Procedure DoClearInput;
Procedure DisplayInputErrorMessage( msg : TString );
Function InputIs : TAlias;
Function InputEncoding : TEncoding;
Procedure SetInputEncoding( Enc : TEncoding );
Procedure InitIFileStack;
Function SetFileForInput( Var FileDesc : TFileDescriptor; 
    FileAlias : TAlias; FileName : TPath; 
    IsDefault : Boolean ) : TIStreamPtr;
Procedure ResetIFileStack;
Function GetIStreamFromFileDescriptor( FileDesc : TFileDescriptor ): TIStreamPtr;
Function GetIStreamFromFileAlias( FileAlias : TAlias ) : TIStreamPtr;
Procedure CloseInputByFileDescriptor( FileDesc : TFileDescriptor );
Procedure CloseInputByFileAlias( FileAlias : TAlias );
Procedure CloseInputByFileName( FileName : TPath );
Procedure CloseCurrentInput;

Function GetInputConsole : TIStreamPtr;
Procedure ReadFromConsole;
Procedure GetIChar( Var e : TIChar );
Function GetChar( Var c : TChar ) : TChar;
Function GetCharNb( Var c : TChar ) : TChar;
Procedure UnGetChar;
Procedure UngetChars( line : TLineNum; col : TCharPos );
Procedure NextIChar( Var e : TIChar );
Function NextChar( Var c : TChar ) : TChar;
Function NextNextChar( Var c : TChar ) : TChar;

Procedure IStackDumpCurrentInput;
Procedure IStackDumpAll;


Implementation
{-----------------------------------------------------------------------------}

Type
  TIFileStack = Record
    Top : TIFileStackIndex;
    Stack : Array[1..IFilesMax] Of TIStreamPtr
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

{ return the current (that is, top) input stream }
Function CurrentInput : TIStreamPtr;
Begin
  CurrentInput := IFileStack.Stack[IFileStack.Top]
End;

{ ignore all characters remaining unread in the current input line }
Procedure DoClearInput;
Begin
  ClearInputFromIStream(CurrentInput)
End;

{ write an error message, pointing to the error }
Procedure DisplayInputErrorMessage( msg : TString );
Begin
  With IFileStack Do
    DisplayIStreamErrorMessage(Stack[Top],msg)
End;

{ return the name (alias) of the current input file }
Function InputIs : TAlias;
Begin
  With IFileStack Do
    InputIs := GetIStreamAlias(Stack[Top])
End;

{ return true if the current input is the terminal }
Function InputIsConsole : Boolean;
Begin
  With IFileStack Do
    InputIsConsole := GetIStreamDeviceType(Stack[Top]) = TTerminal
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
Function PushIFile( FileAlias : TAlias; FileName : TPath ) : TIFileStackIndex;
Var 
  K : TIFileStackIndex;
Begin
  K := 0;
  With IFileStack Do
  Begin
    CheckCondition(Top < IFilesMax,'Input Stack is full');
    OpenIStream(GetNewFileDescriptor,FileAlias,FileName,Stack[Top+1]);
    If IStreamIsOpen(Stack[Top+1]) Then
    Begin
      Top := Top + 1;
      K := Top
    End
  End;
  PushIFile := K
End;

{ allocate memory for input stream records in the stack }
Procedure AllocIStack;
Var 
  I : TIFileStackIndex;
Begin
  For I := 1 To IFilesMax Do
    GetMem(IFileStack.Stack[I],SizeOf(TIStream))
End;

{ initialize the input system, setting up the console as the default 
  input device }
Procedure InitIFileStack;
Var 
  K : TIFileStackIndex;
Begin
  With IFileStack Do
    Top := 0;
  K := PushIFile(CONSOLE_NAME,CONSOLE_NAME);
  CheckCondition(K>0,'cannot open default input console')
End;

{ return the console }
Function GetInputConsole : TIStreamPtr;
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
      Found := GetIStreamAlias(Stack[K]) = CONSOLE_NAME;
      If Not Found Then
        K := K - 1
    End;
    CheckCondition(Found,'cannot find the input console');
    GetInputConsole := Stack[K]
  End
End;

{ lookup from the top, combining three search criteria:
 - descriptor if Desc is not zero
 - alias if FileAlias is not empty
 - file name if FileName is not empty
 return zero if the entry is not in the stack }
Function IFileIndex( FileDesc : TFileDescriptor; 
    FileAlias : TAlias; FileName : TPath ) : TIFileStackIndex;
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
      Found := 
          ((FileDesc = 0) Or (GetIStreamDescriptor(Stack[K]) = FileDesc)) And
          ((FileAlias = '') Or (GetIStreamAlias(Stack[K]) = FileAlias)) And
          ((FileName = '') Or (GetIStreamFileName(Stack[K]) = FileName));
      If Not Found Then
        K := K - 1
    End
  End;
  IFileIndex := K
End;

{ set a file (FileAlias, FileName) as an input stream, if it does not exist
 yet; makes it the default if IsDefault is True; in cas of success, set the 
 file descriptor parameter and return the stream }
Function SetFileForInput( Var FileDesc : TFileDescriptor; 
    FileAlias : TAlias; FileName : TPath; 
    IsDefault : Boolean ) : TIStreamPtr;
Var
  K,I : TIFileStackIndex;
  tmp : TIStreamPtr;
Begin
  SetFileForInput := Nil;
  With IFileStack Do
  Begin
    { warn when the file is known under a different alias }
    K := IFileIndex(0,'',FileName);
    If K <> 0 Then 
      If GetIStreamAlias(Stack[K]) <> FileAlias Then
      Begin
        CWriteWarning('input file already aliased : ''');
        CWrite(FileName);
        CWrite('''');
        CWriteLn;
        Exit;
      End;
    K := IFileIndex(0,FileAlias,FileName);
    If K = 0 Then
      K := PushIFile(FileAlias,FileName)
    Else If IsDefault And (K < Top) Then { not on top: move to top }
    Begin
      tmp := Stack[K];
      For I := K To Top - 1 Do
        Stack[I] := Stack[I+1];
      Stack[Top] := tmp
    End;
    If K = 0 Then
      Exit;
    FileDesc := GetIStreamDescriptor(Stack[K]);
    SetFileForInput := Stack[K]
  End
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
  console : TIStreamPtr;
Begin
  With IFileStack Do
  Begin
    { backup the console pointer, as the input buffer must be preserved }
    console := GetInputConsole;
    CheckCondition(console <> Nil, 'ResetIFileStack: unable to locate the console');
    { make sure all the files are closed; this does not affect the console }
    CloseAllIFiles;
    { reinstall the console as the sole input stream }
    Top := 1;
    Stack[Top] := console
  End
End;

{ close an input file; at index K in the stack; if it is the console, move it 
 back to position 1; TODO: what PII+ does in that case? }
Procedure CloseInputAtIndex( K : TIFileStackIndex );
Var
  I : TIFileStackIndex;
  tmp : TIStreamPtr;
Begin
  With IFileStack Do
  Begin
    Case GetIStreamDeviceType(Stack[K]) Of
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

{ get the IStream having a given file descriptor, or Nil }
Function GetIStreamFromFileDescriptor( FileDesc : TFileDescriptor ): TIStreamPtr;
Var
  K : TIFileStackIndex;
Begin
  GetIStreamFromFileDescriptor := Nil;
  With IFileStack Do
  Begin
    K := IFileIndex(FileDesc,'','');
    If K = 0 Then
      Exit;
    GetIStreamFromFileDescriptor := Stack[K]
  End
End;

{ get the IStream having a given file descriptor }
Function GetIStreamFromFileAlias( FileAlias : TAlias ) : TIStreamPtr;
Var
  K : TIFileStackIndex;
Begin
  GetIStreamFromFileAlias := Nil;
  With IFileStack Do
  Begin
    K := IFileIndex(0,FileAlias,'');
    If K = 0 Then
      Exit;
    GetIStreamFromFileAlias := Stack[K]
  End
End;

{ close an input file by descriptor, alias or file name }

Procedure CloseInputBy( FileDesc : TFileDescriptor; 
    FileAlias : TAlias; FileName : TPath );
Var
  K : TIFileStackIndex;
Begin
  With IFileStack Do
  Begin
    K := IFileIndex(FileDesc,FileAlias,FileName);
    If K > 0 Then { delete }
      CloseInputAtIndex(K)
  End
End;

{ close an input file by file descriptor }
Procedure CloseInputByFileDescriptor( FileDesc : TFileDescriptor );
Begin
 CloseInputBy(FileDesc,'','')
End;

{ close an input file by file alias }
Procedure CloseInputByFileAlias( FileAlias : TAlias );
Begin
 CloseInputBy(0,FileAlias,'')
End;

{ close an input file by file name }
Procedure CloseInputByFileName( FileName : TPath );
Begin
 CloseInputBy(0,'',FileName)
End;

{ close the current input file, if it is not the console }
Procedure CloseCurrentInput;
Begin
  With IFileStack Do
    CloseInputAtIndex(Top)
End;

{----------------------------------------------------------------------------}
{ read from current input stream                                             }
{----------------------------------------------------------------------------}

{ read a line from the keyboard }
Procedure ReadFromConsole;
Begin
  ReadLineFromKeyboard(GetInputConsole)
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

{----------------------------------------------------------------------------}
{ DEBUG                                                                      }
{----------------------------------------------------------------------------}

Procedure IStackDumpCurrentInput;
Begin
  With IFileStack Do
    IStreamDump(Stack[Top])
End;

Procedure IStackDumpAll;
Var
  I : TIFileStackIndex;
Begin
  With IFileStack Do
    For I := Top DownTo 1 Do
      IStreamDump(Stack[I])
End;

{ initialize the unit }
Begin
  AllocIStack
End.