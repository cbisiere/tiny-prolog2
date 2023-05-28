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
{                                F I L E S                                   }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Type
  TIODeviceType    = (TFile, TTerminal);   { input/output device type         }
  TIFile = Text;
  TOFile = Text;

Const
  CONSOLE_NAME = 'console';

{ open a text file: read mode }
Function OpenForRead( Filename : AnyStr; Var TxtFile : TIFile ) : Boolean;
Begin
  Assign(TxtFile,Filename);
  {$I-}
  Reset(TxtFile);
  {$I+}
  OpenForRead := IOResult = 0
End;

{ open a text file: write mode }
Function OpenForWrite( Filename : AnyStr; Var TxtFile : TOFile ) : Boolean;
Begin
  Assign(TxtFile,Filename);
  {$I-}
  Rewrite(TxtFile);
  {$I+}
  OpenForWrite := IOResult = 0
End;

{ flush a text file }
Procedure FlushFile( Filename : AnyStr; Var TxtFile : TOFile );
Begin
  {$I-}
  Flush(TxtFile)
  {$I+}
End;

{ close an input file }
Procedure CloseIFile( Filename : AnyStr; Var TxtFile : TIFile );
Begin
  {$I-}
  Close(TxtFile)
  {$I+}
End;

{ close an output file }
Procedure CloseOFile( Filename : AnyStr; Var TxtFile : TOFile );
Begin
  {$I-}
  Close(TxtFile)
  {$I+}
End;

{ write a string to a file }
Procedure WriteToFile( Filename : AnyStr; Var TxtFile : TOFile; s : AnyStr );
Begin
  {$I-}
  Write(TxtFile,s);
  Flush(TxtFile) { FIXME: this should not be needed }
  {$I+}
End;

