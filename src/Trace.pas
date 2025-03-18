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
{                            T R A C E   F I L E                             }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }

{ Write to trace file for debugging purpose }

Unit Trace;

Interface

Uses
  ShortStr,
  Errs,
  Files;

Procedure TerminateTrace;
Procedure WriteToTraceFile( s : TString );
Procedure WritelnToTraceFile( s : TString );

Implementation
{-----------------------------------------------------------------------------}

Var
  InitOk : Boolean;
  TraceFile : TOFile;
  TraceIsOn : Boolean;

Const
  TRACE_FILE : TShortPath = 'trace.txt';

{ close the trace file }
Procedure TerminateTrace;
Begin
  CloseOFile(TRACE_FILE, TraceFile)
End;

{ write to the echo file if trace is on }
Procedure WriteToTraceFile( s : TString );
Begin
  If TraceIsOn Then
    WriteToFile(TRACE_FILE,TraceFile,s)
End;

{ writeln to the echo file if trace is on }
Procedure WritelnToTraceFile( s : TString );
Begin
  If TraceIsOn Then
    WritelnToFile(TRACE_FILE,TraceFile,s)
End;

{ initialize the trace system }
Begin
  InitOk := OpenForWrite(TRACE_FILE, TraceFile);
  CheckCondition(InitOk,'cannot open trace file');
  TraceIsOn := True
End.