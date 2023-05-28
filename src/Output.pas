{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Output.pas                                                 }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                               O U T P U T                                  }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ output stack }

Const
  MaxNbOFiles = 20;

Type
  TOFileStackPtr = 0..MaxNbOFiles;

  TOFileState = Record
    FName : AnyStr;
    CurrentFile : TOFile;
    FileIsOpen : Boolean;
    DeviceType  : TIODeviceType
  End;
  TOFileStack = Record
    Top : TOFileStackPtr;
    Stack : Array[1..MaxNbOFiles] Of TOFileState
  End;

Var
  OFileStack : TOFileStack;

{ append a file to the output stack }
Function PushOFile( FileName : AnyStr ) : TOFileStackPtr;
Var K : TOFileStackPtr;
Begin
  CheckCondition(OFileStack.Top < MaxNbOFiles,'Output Stack is full');
  OFileStack.Top := OFileStack.Top + 1;
  With OFileStack.Stack[OFileStack.Top] Do
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
      FileIsOpen := OpenForWrite(FName,CurrentFile)
    End;
    If FileIsOpen Then 
    Begin
      K := OFileStack.Top
    End
    Else
    Begin
      OFileStack.Top := OFileStack.Top - 1;
      K := 0
    End;
  End;
  PushOFile := K
End;

{ initialize the output system, setting up the console as the default 
  output device }
Procedure InitOFileStack;
Var K : TOFileStackPtr;
Begin
  OFileStack.Top := 0;
  K := PushOFile(CONSOLE_NAME);
  CheckCondition(K>0,'cannot open default output console')
End;

{ lookup; return zero if the entry is not in the stack }
Function OFileIndex( FileName : AnyStr ) : TOFileStackPtr;
Var
  Found : Boolean;
  K : TOFileStackPtr;
Begin
  Found := False;
  K := OFileStack.Top;
  While Not Found And (K > 0) Do
  Begin
    Found := OFileStack.Stack[K].FName = FileName;
    If Not Found Then
      K := K - 1
  End;
  OFileIndex := K
End;

{ set a file as the current output file }
Function SetFileForOutput( FileName : AnyStr ) : Boolean;
Var
  K,I : TOFileStackPtr;
  tmp : TOFileState;
Begin
  K := OFileIndex(FileName);
  If K = 0 Then
    K := PushOFile(FileName)
  Else If K < OFileStack.Top Then { not on top: move to top }
  Begin
    tmp := OFileStack.Stack[K];
    For I := K To OFileStack.Top - 1 Do
      OFileStack.Stack[I] := OFileStack.Stack[I+1];
    OFileStack.Stack[OFileStack.Top] := tmp
  End;
  SetFileForOutput := K > 0
End;

{ close the output file at index K in the stack }
Procedure CloseOFileAtIndex( K : TOFileStackPtr );
Begin
  CheckCondition((K>0) And (K<=OFileStack.Top),
    'out of range output stack index');
  With OFileStack.Stack[K] Do
    If FileIsOpen And (DeviceType = TFile) Then
    Begin
      CloseOFile(FName,CurrentFile);
      FileIsOpen := False
    End
End;

{ close all the opened files and reset the output stack }
Procedure ResetOFileStack;
Var
  I : TOFileStackPtr;
Begin
  For I := 1 To OFileStack.Top Do
    CloseOFileAtIndex(I);
  InitOFileStack
End;

{ close an output file; if it is the console, move it back to 
  position 1; TODO: what PII+ does in that case? }
Procedure CloseOutput( FileName : AnyStr );
Var
  K,I : TOFileStackPtr;
  tmp : TOFileState;
Begin
  K := OFileIndex(FileName);
  If K > 0 Then { delete }
  Begin
    CloseOFileAtIndex(K);
    Case OFileStack.Stack[K].DeviceType Of
    TTerminal:
      If (K = OFileStack.Top) And (K > 1) Then
      Begin
        tmp := OFileStack.Stack[OFileStack.Top];
        For I := OFileStack.Top DownTo 2 Do
          OFileStack.Stack[I] := OFileStack.Stack[I-1];
        OFileStack.Stack[1] := tmp
      End;
    TFile:
      Begin
        If K < OFileStack.Top Then 
          For I := K To OFileStack.Top-1 Do
            OFileStack.Stack[I] := OFileStack.Stack[I+1];
        OFileStack.Top := OFileStack.Top - 1
      End
    End
  End
End;

{ close the current output file }
Procedure CloseCurrentOutput;
Begin
  CloseOutput(OFileStack.Stack[OFileStack.Top].FName)
End;

{ close the current output file, if it is not the console }
Procedure FlushCurrentOutput;
Begin
  With OFileStack.Stack[OFileStack.Top] Do
    If FileIsOpen And (DeviceType = TFile) Then
      FlushFile(FName,CurrentFile)
End;

{ return the name of the current output file }
Function OutputIs : AnyStr;
Begin
  With OFileStack.Stack[OFileStack.Top] Do
    OutputIs := FName
End;

{ return true if the current output is the terminal }
Function OutputIsTerminal : Boolean;
Begin
  With OFileStack.Stack[OFileStack.Top] Do
    OutputIsTerminal := DeviceType = TTerminal
End;

{ write a Pascal string to the current output }
Procedure WriteToCurrentOutput; (* ( s : AnyStr ); *)
Begin
  With OFileStack.Stack[OFileStack.Top] Do
    Case DeviceType Of
      TTerminal:
        CWrite(s);
      TFile:
        WriteToFile(FName,CurrentFile,s)
    End
End;
