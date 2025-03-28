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
{$I define.inc }

{ TODO: handle i/o errors }

{ NOTE: using Text as input file type (instead of File Of Char) triggers a 
 bug, where Read unexpectedly returns #00 with IOResult still equal to 0 }

Unit Files;

Interface

Uses
  ShortStr;

Type
  TIFile = File Of Char;
  TOFile = Text;
  TShortPath = TString; { file or directory full path }

{ path handling }
Function GetDirectorySeparator : Char;
Function ExtractPath( fn : TShortPath ) : TShortPath;
Function OSFilename( Filename : TShortPath ) : TShortPath;

{ file info }
Function FileExistsOnDisk( Filename : TShortPath ): Boolean;

{ input files: }
Function OpenForRead( Filename : TShortPath; Var TxtFile : TIFile ) : Boolean;
Function OpenForWrite( Filename : TShortPath; Var TxtFile : TOFile ) : Boolean;
Procedure FlushFile( Filename : TShortPath; Var TxtFile : TOFile );
Procedure CloseIFile( Filename : TShortPath; Var TxtFile : TIFile );

{ output files: }
Procedure CloseOFile( Filename : TShortPath; Var TxtFile : TOFile );
Procedure WriteToFile( Filename : TShortPath; Var TxtFile : TOFile; s : TString );
Procedure WritelnToFile( Filename : TShortPath; Var TxtFile : TOFile; s : TString );
Function ReadFromFile( Filename : TShortPath; Var TxtFile : TIFile; 
    Var c : Char ) : Boolean;

Implementation
{-----------------------------------------------------------------------------}
{ TP4/FPC compatibility code }
{$IFDEF FPC}
Uses
  Sysutils;
{$ELSE}
Const
  DirectorySeparator : Char = '\';

{ extract the file path part of filename fn including the final 
 directory separator; works with platform sep and '/'' }
Function ExtractFilePath( fn : TShortPath ) : TShortPath;
Var
  Done : Boolean;
Begin
  Repeat
    Done := Length(fn) = 0;
    If Not Done Then
      Done := fn[Length(fn)] In [DirectorySeparator,'/'];
    If Not Done Then
      Delete(fn,Length(fn),1)
  Until Done;
  ExtractFilePath := fn
End;

{ get the current directory; when non empty, it includes a 
 trailing path separator; FIXME: return HOME env var if any? }
Function GetUserDir: TShortPath;
Var
  s : String;
Begin
  GetDir(0,s);
  GetUserDir := s { FIXME: check length? }
End;

{ return True if a file exists; warning: if the file was already open, it 
 is reset }
Function FileExists( Filename : TShortPath ): Boolean;
Var
 f : File;
Begin 
  FileExists := False;
  If Filename = '' Then
    Exit;
  {$I-}
  Assign(f, Filename); 
  Reset(f);
  Close(f);
  {$I+}
  FileExists := (IOResult = 0) 
End;
{$ENDIF}
{-----------------------------------------------------------------------------}
 
{ get the directory separator }
Function GetDirectorySeparator : Char;
Begin
  GetDirectorySeparator := DirectorySeparator
End;

{ extract the file path part of filename fn including the final 
 directory separator; works with platform sep and '/'' }
Function ExtractPath( fn : TShortPath ) : TShortPath;
Begin
  ExtractPath := ExtractFilePath(fn)
End;

{ get the user directory, including a trailing separator }
Function GetUserDirectory: TShortPath;
Begin
  GetUserDirectory := GetUserDir;
End;

{ return a full pathname to a file, usable in Assign, by 1) expanding "~" 
 to the home dir when present, and 2) replacing the internal representation 
 '/' of the directory separator with the os-dependant one }
Function OSFilename( Filename : TShortPath ) : TShortPath;
Var
  sep : Char;
  i : TStringSize;
Begin
  If (Filename = '~') Or (Copy(Filename,1,2) = '~/')  Then
  Begin
    Delete(Filename,1,2);
    { TODO: check that getting Unicode here is fine }
    { TODO: check no truncate, warn user }
    Insert(GetUserDirectory,Filename,1);
  End;
  sep := GetDirectorySeparator;
  For i := 1 to Length(Filename) Do
    if Filename[i] = '/' Then
      Filename[i] := sep;
  OSFilename := Filename
End;

{ return True if a file exists }
Function FileExistsOnDisk( Filename : TShortPath ): Boolean;
Begin
  FileExistsOnDisk := FileExists(OSFilename(Filename))
End;

{ open a text file: read mode }
Function OpenForRead( Filename : TShortPath; Var TxtFile : TIFile ) : Boolean;
Begin
  Assign(TxtFile,OSFilename(Filename));
  {$I-}
  Reset(TxtFile);
  {$I+}
  OpenForRead := IOResult = 0
End;

{ open a text file: write mode }
Function OpenForWrite( Filename : TShortPath; Var TxtFile : TOFile ) : Boolean;
Begin
  Assign(TxtFile,OSFilename(Filename));
  {$I-}
  Rewrite(TxtFile);
  {$I+}
  OpenForWrite := IOResult = 0
End;

{ flush a text file }
Procedure FlushFile( Filename : TShortPath; Var TxtFile : TOFile );
Begin
  {$I-}
  Flush(TxtFile)
  {$I+}
End;

{ close an input file }
Procedure CloseIFile( Filename : TShortPath; Var TxtFile : TIFile );
Begin
  {$I-}
  Close(TxtFile)
  {$I+}
End;

{ close an output file }
Procedure CloseOFile( Filename : TShortPath; Var TxtFile : TOFile );
Begin
  {$I-}
  Close(TxtFile)
  {$I+}
End;

{ write a string to a file }
Procedure WriteToFile( Filename : TShortPath; Var TxtFile : TOFile; s : TString );
Begin
  {$I-}
  Write(TxtFile,s);
  Flush(TxtFile) { FIXME: this should not be needed }
  {$I+}
End;

{ writeln a string to a file }
Procedure WritelnToFile( Filename : TShortPath; Var TxtFile : TOFile; s : TString );
Begin
  {$I-}
  Writeln(TxtFile,s);
  Flush(TxtFile) { FIXME: this should not be needed }
  {$I+}
End;

{ read a single char from a file }
Function ReadFromFile( Filename : TShortPath; Var TxtFile : TIFile; 
    Var c : Char ) : Boolean;
Begin
  {$I-}
  Read(TxtFile,c);
  {$I+}
  ReadFromFile := IOResult = 0
End;

End.