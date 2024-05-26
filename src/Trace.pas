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

Unit Trace;

Interface

Uses
  Chars,
  ShortStr,
  Num,
  Errs,
  Files,
  Crt2;

Procedure TerminateTrace;
Procedure WriteToEchoFile( s : TString );
Procedure WritelnToEchoFile( s : TString );
Procedure CWrite( s : TString );
Procedure CWriteLn;
Procedure CWriteInt( v : Integer );
Procedure CWritePosInt( v : PosInt );
Procedure CWriteLongInt( v : LongInt );
Procedure CWriteBool( b : Boolean );
Procedure CWriteWarning( s : TString );
Procedure CWriteLnWarning( s : TString );

Implementation
{-----------------------------------------------------------------------------}

Var
  InitOk : Boolean;
  EchoFile : TOFile;
  Echo : Boolean;

Const
  ECHO_FILE : TPath = 'echo.txt';

{ close the trace file }
Procedure TerminateTrace;
Begin
  CloseOFile(ECHO_FILE, EchoFile)
End;

{ write to the echo file if trace is on }
Procedure WriteToEchoFile( s : TString );
Begin
  If Echo Then
    WriteToFile(ECHO_FILE,EchoFile,s)
End;

{ writeln to the echo file if trace is on }
Procedure WritelnToEchoFile( s : TString );
Begin
  If Echo Then
    WritelnToFile(ECHO_FILE,EchoFile,s)
End;

{ write a char to the terminal }
Procedure CWriteChar( cc : TChar );
Begin
  WriteToEchoFile(cc);
  CrtWriteChar(cc)
End;

{ write a string of 1-byte chars to the terminal }
Procedure CWrite( s : TString );
Begin
  WriteToEchoFile(s);
  CrtWriteShortString(s)
End;

{ write a new line to the terminal }
Procedure CWriteLn;
Begin
  WritelnToEchoFile('');
  CrtWriteLn
End;

{ write a byte or an integer }
Procedure CWriteInt( v : Integer );
Begin
  CWrite(IntToShortString(v))
End;

{ write a byte or a positive integer }
Procedure CWritePosInt( v : PosInt );
Begin
  CWrite(PosIntToShortString(v))
End;

{ write a long integer }
Procedure CWriteLongInt( v : LongInt );
Begin
  CWrite(LongIntToShortString(v))
End;

{ write a byte or an integer }
Procedure CWriteBool( b : Boolean );
Begin
  CWrite(BoolToShortString(b))
End;

{ display a string content as an array of char codes }
Procedure CWriteStrCharCodes( s : TString );
Var 
  i : 1..StringMaxSize;
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

{ write a warning }
Procedure CWriteWarning( s : TString );
Begin
  CWrite('***WARNING: ');
  CWrite(s);
End;

{ writeln a warning }
Procedure CWriteLnWarning( s : TString );
Begin
  CWriteWarning(s);
  CWriteLn
End;

{ initialize the trace system }
Begin
  InitOk := OpenForWrite(ECHO_FILE, EchoFile);
  CheckCondition(InitOk,'cannot open echo file');
  Echo := True
End.