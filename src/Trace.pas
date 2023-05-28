{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Trace.pas                                                  }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                                T R A C E                                   }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Var
  EchoFile : TOFile;
  Echo : Boolean;

Const
  ECHO_FILE = 'echo.txt';

{ initialize the trace system }
Procedure InitTrace;
Var 
  Ok : Boolean;
Begin
  Ok := OpenForWrite(ECHO_FILE, EchoFile);
  CheckCondition(Ok,'cannot open echo file');
  Echo := True
End;

{ close the trace file }
Procedure TerminateTrace;
Begin
  CloseOFile(ECHO_FILE, EchoFile)
End;

{ console write, possibly with echo to the echo file }
Procedure CWrite( s : AnyStr );
Begin
  If Echo Then
    WriteToFile(ECHO_FILE,EchoFile,s);
  Write(s)
End;

Procedure CWriteLn;
Begin
  If Echo Then
    WriteToFile(ECHO_FILE,EchoFile,CRLF);
  WriteLn
End;

{ write a byte or an integer }
Procedure CWriteInt( v : Integer );
Begin
  CWrite(IntToStr(v))
End;

{ write a byte or an integer }
Procedure CWriteBool( b : Boolean );
Begin
  CWrite(BoolToStr(b))
End;

{ display a string content as an array of char codes }
Procedure CWriteStrCharCodes( s : AnyStr );
Var 
  i : 1..AnyStrMaxSize;
  First : Boolean;
Begin
  First := True;
  CWrite('[');
  For i := 1 to Length(s) Do
  Begin
    If Not First Then
      CWrite(',');
    CWriteInt(Ord(s[i]));
    First := False
  End;
  CWrite(']')
End;
