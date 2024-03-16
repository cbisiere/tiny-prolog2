{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : OStack.pas                                                 }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                          O U T P U T   S T A C K                           }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ output stack }

{
  ASSUMPTIONS AND LIMITS:
  -----------------------
  1) OFilesMax: 5
    maximum number of stacked output files, including the predefined 
    output 'console'.
}

Unit OStack;

Interface

Uses
  ShortStr,
  Errs,
  Files,
  Trace,
  OStream;

Const
  OFilesMax = 5;

Type
  TOFileStackIndex = 0..OFilesMax; { stack of output files; see PII+ doc R5-1}

Function OutputIs : TAlias;
Procedure InitOFileStack;
Function SetFileForOutput( Var FileDesc : TFileDescriptor; 
    FileAlias : TAlias; FileName : TPath;
    IsDefault : Boolean ) : TOStreamPtr;
Procedure ResetOFileStack;
Function GetOStreamFromFileDescriptor( FileDesc : TFileDescriptor ): TOStreamPtr;
Function GetOStreamFromFileAlias( FileAlias : TAlias ) : TOStreamPtr;
Procedure CloseOutputByFileDescriptor( FileDesc : TFileDescriptor );
Procedure CloseOutputByFileAlias( FileAlias : TAlias );
Procedure CloseOutputByFileName( FileName : TPath );

Function OutputIsTerminal : Boolean;
Procedure WriteToCurrentOutput( s : TString );
Procedure CloseCurrentOutput;
Procedure FlushCurrentOutput;

Procedure OStackDumpCurrentOutput;
Procedure OStackDumpAll;


Implementation
{-----------------------------------------------------------------------------}

Type
  TOFileStack = Record
    Top : TOFileStackIndex;
    Stack : Array[1..OFilesMax] Of TOStreamPtr
  End;

Var
  OFileStack : TOFileStack;

{----------------------------------------------------------------------------}
{ accessors (top OStream)                                                    }
{----------------------------------------------------------------------------}

{ return the current (that is, top) output stream }
Function CurrentOutput : TOStreamPtr;
Begin
  CurrentOutput := OFileStack.Stack[OFileStack.Top]
End;

{ return the name (alias) of the current output file }
Function OutputIs : TAlias;
Begin
  With OFileStack Do
    OutputIs := GetOStreamAlias(Stack[Top])
End;

{ return true if the current output is the terminal }
Function OutputIsConsole : Boolean;
Begin
  With OFileStack Do
    OutputIsConsole := GetOStreamDeviceType(Stack[Top]) = TTerminal
End;

{----------------------------------------------------------------------------}
{ stack management                                                           }
{----------------------------------------------------------------------------}

{ append a file to the output stack; return zero if the file cannot
  be opened }
Function PushOFile( FileAlias : TAlias; FileName : TPath ) : TOFileStackIndex;
Var 
  K : TOFileStackIndex;
Begin
  K := 0;
  With OFileStack Do
  Begin
    CheckCondition(Top < OFilesMax,'Output Stack is full');
    OpenOStream(GetNewFileDescriptor,FileAlias,FileName,Stack[Top+1]);
    If OStreamIsOpen(Stack[Top+1]) Then
    Begin
      Top := Top + 1;
      K := Top
    End
  End;
  PushOFile := K
End;

{ allocate memory for output stream records in the stack }
Procedure AllocOStack;
Var 
  I : TOFileStackIndex;
Begin
  For I := 1 To OFilesMax Do
    GetMem(OFileStack.Stack[I],SizeOf(TOStream))
End;

{ initialize the output system, setting up the console as the default 
  output device }
Procedure InitOFileStack;
Var 
  K : TOFileStackIndex;
Begin
  With OFileStack Do
    Top := 0;
  K := PushOFile(CONSOLE_NAME,CONSOLE_NAME);
  CheckCondition(K>0,'cannot open default output console')
End;

{ lookup from the top, combining three search criteria:
 - descriptor if Desc is not zero
 - alias if FileAlias is not empty
 - file name if FileName is not empty
 return zero if the entry is not in the stack }
Function OFileIndex( FileDesc : TFileDescriptor; 
    FileAlias : TAlias; FileName : TPath ) : TOFileStackIndex;
Var
  Found : Boolean;
  K : TOFileStackIndex;
Begin
  With OFileStack Do
  Begin
    Found := False;
    K := Top;
    While Not Found And (K > 0) Do
    Begin
      Found := 
          ((FileDesc = 0) Or (GetOStreamDescriptor(Stack[K]) = FileDesc)) And
          ((FileAlias = '') Or (GetOStreamAlias(Stack[K]) = FileAlias)) And
          ((FileName = '') Or (GetOStreamFileName(Stack[K]) = FileName));
      If Not Found Then
        K := K - 1
    End
  End;
  OFileIndex := K
End;

{ set a file (FileAlias, FileName) as an output stream, if it does not exist
 yet; makes it the default if IsDefault is True; in cas of success, set the 
 file descriptor parameter and return the stream }
Function SetFileForOutput( Var FileDesc : TFileDescriptor; 
    FileAlias : TAlias; FileName : TPath;
    IsDefault : Boolean ) : TOStreamPtr;
Var
  K,I : TOFileStackIndex;
  tmp : TOStreamPtr;
Begin
  SetFileForOutput := Nil;
  With OFileStack Do
  Begin
    { warn when the file is known under a different alias }
    K := OFileIndex(0,'',FileName);
    If K <> 0 Then 
      If GetOStreamAlias(Stack[K]) <> FileAlias Then
      Begin
        CWriteWarning('output file already aliased : ''');
        CWrite(FileName);
        CWrite('''');
        CWriteLn;
        Exit;
      End;
    K := OFileIndex(0,FileAlias,FileName);
    If K = 0 Then
      K := PushOFile(FileAlias,FileName)
    Else If IsDefault And (K < Top) Then { not on top: move to top }
    Begin
      tmp := Stack[K];
      For I := K To Top - 1 Do
        Stack[I] := Stack[I+1];
      Stack[Top] := tmp
    End;
    If K = 0 Then
      Exit;
    FileDesc := GetOStreamDescriptor(Stack[K]);
    SetFileForOutput := Stack[K]
  End
End;

{ close the output file at index K in the stack }
Procedure CloseOFileAtIndex( K : TOFileStackIndex );
Begin
  With OFileStack Do
  Begin
    CheckCondition((K > 0) And (K <= Top), 
        'CloseOFileAtIndex: out of range output stack index');
    CloseOStream(Stack[K])
  End
End;

{ close all opened output files }
Procedure CloseAllOFiles;
Var
  I : TOFileStackIndex;
Begin
  With OFileStack Do
    For I := Top DownTo 1 Do
      CloseOFileAtIndex(I)
End;

{ reset the output stack, leaving the console in its current state; console
 output buffer should *not* be reset before executing a user query typed at the
 prompt, otherwise in_char(c) would not work as intended }
Procedure ResetOFileStack;
Var
  console : TOStreamPtr;
  K : TOFileStackIndex;
Begin
  With OFileStack Do
  Begin
    { backup the console state, including the output buffer }
    K := OFileIndex(0,CONSOLE_NAME,'');
    CheckCondition(K > 0, 'ResetOFileStack: unable to locate the console');
    console := Stack[K];
    { make sure all the files are closed }
    CloseAllOFiles;
    { reinstall the console }
    Top := 1;
    Stack[Top] := console
  End
End;

{ close an output file; at index K in the stack; if it is the console, move it 
 back to position 1; TODO: what PII+ does in that case? }
Procedure CloseOutputAtIndex( K : TOFileStackIndex );
Var
  I : TOFileStackIndex;
  tmp : TOStreamPtr;
Begin
  With OFileStack Do
  Begin
    Case GetOStreamDeviceType(Stack[K]) Of
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
        CloseOFileAtIndex(K);
        If K < Top Then 
          For I := K To Top-1 Do
            Stack[I] := Stack[I+1];
        Top := Top - 1
      End
    End
  End
End;

{ get the OStream having a given file descriptor, or Nil }
Function GetOStreamFromFileDescriptor( FileDesc : TFileDescriptor ): TOStreamPtr;
Var
  K : TOFileStackIndex;
Begin
  GetOStreamFromFileDescriptor := Nil;
  With OFileStack Do
  Begin
    K := OFileIndex(FileDesc,'','');
    If K = 0 Then
      Exit;
    GetOStreamFromFileDescriptor := Stack[K]
  End
End;

{ get the OStream having a given file descriptor }
Function GetOStreamFromFileAlias( FileAlias : TAlias ) : TOStreamPtr;
Var
  K : TOFileStackIndex;
Begin
  GetOStreamFromFileAlias := Nil;
  With OFileStack Do
  Begin
    K := OFileIndex(0,FileAlias,'');
    If K = 0 Then
      Exit;
    GetOStreamFromFileAlias := Stack[K]
  End
End;

{ close an output file by descriptor, alias or file name }

Procedure CloseOutputBy( FileDesc : TFileDescriptor; 
    FileAlias : TAlias; FileName : TPath );
Var
  K : TOFileStackIndex;
Begin
  With OFileStack Do
  Begin
    K := OFileIndex(FileDesc,FileAlias,FileName);
    If K > 0 Then { delete }
      CloseOutputAtIndex(K)
  End
End;

{ close an output file by file descriptor }
Procedure CloseOutputByFileDescriptor( FileDesc : TFileDescriptor );
Begin
 CloseOutputBy(FileDesc,'','')
End;

{ close an output file by file alias }
Procedure CloseOutputByFileAlias( FileAlias : TAlias );
Begin
 CloseOutputBy(0,FileAlias,'')
End;

{ close an output file by file name }
Procedure CloseOutputByFileName( FileName : TPath );
Begin
 CloseOutputBy(0,'',FileName)
End;


{----------------------------------------------------------------------------}
{ Operations on current output                                               }
{----------------------------------------------------------------------------}

{ return true if the current output is the terminal }
Function OutputIsTerminal : Boolean;
Begin
  OutputIsTerminal := GetOStreamDeviceType(CurrentOutput) = TTerminal
End;

{ write a short string to the current output }
Procedure WriteToCurrentOutput( s : TString );
Begin
  WriteToOStream(CurrentOutput,s)
End;

{ close the current output file, if it is not the console }
Procedure CloseCurrentOutput;
Begin
  With OFileStack Do
    CloseOutputAtIndex(Top)
End;

{ flush the current output file }
Procedure FlushCurrentOutput;
Begin
  FlushOStream(CurrentOutput)
End;


{----------------------------------------------------------------------------}
{ DEBUG                                                                      }
{----------------------------------------------------------------------------}

Procedure OStackDumpCurrentOutput;
Begin
  With OFileStack Do
    OStreamDump(Stack[Top])
End;

Procedure OStackDumpAll;
Var
  I : TOFileStackIndex;
Begin
  With OFileStack Do
    For I := Top DownTo 1 Do
      OStreamDump(Stack[I])
End;

{ initialize the unit }
Begin
  AllocOStack
End.