{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Echo.pas                                                   }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                              E C H O   F I L E                             }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ Write to echo file }

Unit Echo;

Interface

Uses
  ShortStr,
  Errs,
  Files;

Procedure TerminateEcho;
Procedure WriteToEchoFile( s : TString );
Procedure WritelnToEchoFile( s : TString );

Implementation
{-----------------------------------------------------------------------------}

Var
  InitOk : Boolean;
  EchoFile : TOFile;
  EchoIsOn : Boolean;

Const
  ECHO_FILE : TShortPath = 'echo.txt';

{ close the echo file }
Procedure TerminateEcho;
Begin
  CloseOFile(ECHO_FILE, EchoFile)
End;

{ write to the echo file if trace is on }
Procedure WriteToEchoFile( s : TString );
Begin
  If EchoIsOn Then
    WriteToFile(ECHO_FILE,EchoFile,s)
End;

{ writeln to the echo file if trace is on }
Procedure WritelnToEchoFile( s : TString );
Begin
  If EchoIsOn Then
    WritelnToFile(ECHO_FILE,EchoFile,s)
End;

{ initialize the echo system }
Begin
  InitOk := OpenForWrite(ECHO_FILE, EchoFile);
  CheckCondition(InitOk,'cannot open echo file');
  EchoIsOn := True
End.