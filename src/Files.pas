{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Files.pas                                                  }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                                F I L E S                                   }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ TODO: handle i/o errors }

{ NOTE: using Text as input file type (instead of File Of Char) triggers a 
 bug, where Read unexpectedly returns #00 with IOResult still equal to 0 }

Unit Files;

Interface

Uses
  ShortStr;

Type
  TIODeviceType    = (TFile, TTerminal);   { input/output device type }
  TIFile = File Of Char;
  TOFile = Text;

Const
  CONSOLE_NAME = 'console';

{ path handling }
Function ExtractPath( fn : TString ) : TString;
Function OSFilename( Filename : TString ) : TString;

{ input files: }
Function OpenForRead( Filename : TString; Var TxtFile : TIFile ) : Boolean;
Function OpenForWrite( Filename : TString; Var TxtFile : TOFile ) : Boolean;
Procedure FlushFile( Filename : TString; Var TxtFile : TOFile );
Procedure CloseIFile( Filename : TString; Var TxtFile : TIFile );

{ output files: }
Procedure CloseOFile( Filename : TString; Var TxtFile : TOFile );
Procedure WriteToFile( Filename : TString; Var TxtFile : TOFile; s : TString );
Function ReadFromFile( Filename : TString; Var TxtFile : TIFile; 
    Var c : Char ) : Boolean;

Implementation
{-----------------------------------------------------------------------------}
{ TP4/FPC compatibility code }
{$IFDEF FPC}

Uses
  Sysutils;

{$ENDIF}
{$IFDEF MSDOS}
{-----------------------------------------------------------------------------}

Const
  DirectorySeparator : Char = '\';

{ TODO: return the file path part of the filename fn including the final 
 directory separator (FPC emulation) }
Function ExtractFilePath( fn : TString ) : TString;
Begin
  ExtractFilePath := fn { TBD }
End;

{ get the current directory; when non empty, it includes a 
 trailing path separator; FIXME: return HOME env var if any? }
Function GetUserDir: TString;
Var
  s : String;
Begin
  GetDir(0,s);
  GetUserDir := s { FIXME: check length? }
End;

{$ENDIF}
{-----------------------------------------------------------------------------}
 
{ extract the file path part of filename fn including the final 
 directory separator }
Function ExtractPath( fn : TString ) : TString;
Begin
  ExtractPath := ExtractFilePath(fn)
End;

{ get the user directory, including a trailing separator }
Function GetUserDirectory: TString;
Begin
  GetUserDirectory := GetUserDir;
End;

{ get the directory separator }
Function GetDirectorySeparator : Char;
Begin
  GetDirectorySeparator := DirectorySeparator
End;

{ return a full pathname to a file, usable in Assign, by 1) expanding "~/" 
 to the home dir when present, and 2) replacing the internal representation 
 '/' of the directory separator with the os-dependant one }
Function OSFilename( Filename : TString ) : TString;
Var
  sep : Char;
  i : TStringSize;
Begin
  If Length(Filename) >= 2 Then
    If Copy(Filename,1,2) = '~/'  Then
    Begin
      Delete(Filename,1,2);
      { TODO: check that getting Unicode here is fine }
      { TODO: check no truncate, warn user }
      Insert(GetUserDir,Filename,1);
    End;
  sep := GetDirectorySeparator;
  For i := 1 to Length(Filename) Do
    if FileName[i] = '/' Then
      FileName[i] := sep;
  OSFilename := FileName
End;

{ open a text file: read mode }
Function OpenForRead( Filename : TString; Var TxtFile : TIFile ) : Boolean;
Begin
  Assign(TxtFile,OSFilename(Filename));
  {$I-}
  Reset(TxtFile);
  {$I+}
  OpenForRead := IOResult = 0
End;

{ open a text file: write mode }
Function OpenForWrite( Filename : TString; Var TxtFile : TOFile ) : Boolean;
Begin
  Assign(TxtFile,OSFilename(Filename));
  {$I-}
  Rewrite(TxtFile);
  {$I+}
  OpenForWrite := IOResult = 0
End;

{ flush a text file }
Procedure FlushFile( Filename : TString; Var TxtFile : TOFile );
Begin
  {$I-}
  Flush(TxtFile)
  {$I+}
End;

{ close an input file }
Procedure CloseIFile( Filename : TString; Var TxtFile : TIFile );
Begin
  {$I-}
  Close(TxtFile)
  {$I+}
End;

{ close an output file }
Procedure CloseOFile( Filename : TString; Var TxtFile : TOFile );
Begin
  {$I-}
  Close(TxtFile)
  {$I+}
End;

{ write a string to a file }
Procedure WriteToFile( Filename : TString; Var TxtFile : TOFile; s : TString );
Begin
  {$I-}
  Write(TxtFile,s);
  Flush(TxtFile) { FIXME: this should not be needed }
  {$I+}
End;

{ read a single char from a file }
Function ReadFromFile( Filename : TString; Var TxtFile : TIFile; 
    Var c : Char ) : Boolean;
Begin
  {$I-}
  Read(TxtFile,c);
  {$I+}
  ReadFromFile := IOResult = 0
End;

End.