{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Dump.pas                                                   }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022-2026                                                  }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                              D U M P   F I L E                             }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }

{ Write to the dump file }

Unit Dump;

Interface

Uses
  ShortStr,
  Errs,
  Files;

Procedure CloseDumpFile;

Procedure WriteToDumpFile( s : TString );
Procedure WritelnToDumpFile( s : TString );
Procedure WriteLineBreakToDumpFile;

Implementation
{-----------------------------------------------------------------------------}

Var
  DumpFile : TOFile;
  DumpFileIsOpen : Boolean;

Const
  DUMP_FILE : TShortPath = 'dump.log';

{-----------------------------------------------------------------------}
{ dump file                                                            }
{-----------------------------------------------------------------------}

{ close the dump file if it is open; to be called before shut down }
Procedure CloseDumpFile;
Begin
  If DumpFileIsOpen Then
  Begin
    CloseOFile(DUMP_FILE, DumpFile);
    DumpFileIsOpen := False
  End
End;

{ if not yet open, open a fresh, empty dump file }
Procedure LazyOpenDumpFile;
Begin
  If Not DumpFileIsOpen Then
  Begin
    DumpFileIsOpen := OpenForWrite(DUMP_FILE,DumpFile);
    If Not DumpFileIsOpen Then
      RuntimeError('cannot open dump file')
  End;
End;

{-----------------------------------------------------------------------}
{ write to the dump file                                                }
{-----------------------------------------------------------------------}

{ write to the dump file }
Procedure WriteToDumpFile( s : TString );
Begin
  LazyOpenDumpFile;
  WriteToFile(DUMP_FILE,DumpFile,s)
End;

{ writeln to the dump file }
Procedure WritelnToDumpFile( s : TString );
Begin
  LazyOpenDumpFile;
  WritelnToFile(DUMP_FILE,DumpFile,s)
End;

{ write a line break to the dump file }
Procedure WriteLineBreakToDumpFile;
Begin
  LazyOpenDumpFile;
  WritelnToFile(DUMP_FILE,DumpFile,'')
End;

{-----------------------------------------------------------------------}
{ initialize the unit                                                   }
{-----------------------------------------------------------------------}

Begin
  DumpFileIsOpen := False
End.