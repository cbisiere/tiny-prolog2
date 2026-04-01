{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Paper.pas                                                   }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022-2026                                                  }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                            P A P E R   F I L E                             }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }

{ Write to the paper file

 Notes:
 - PII+ doc p.133: "The log file is only closed at the end of the session"; 
 - TODO: should the paper file be reset across runs of the Prolog 
   executable (a.k.a. sessions)? For now we reset it, but it could be tested 
   on PII+ }

Unit Paper;

Interface

Uses
  ShortStr,
  Errs,
  Files;

Procedure SetPaperFilename( Filename : TShortPath );
Procedure ClosePaperFile;

Function GetPaperState : Boolean;
Procedure SetPaperState( state : Boolean );

Procedure WriteToPaperFile( s : TString );
Procedure WritelnToPaperFile( s : TString );

Implementation
{-----------------------------------------------------------------------------}

Var
  PaperIsOn : Boolean; { global on/off state }
  PaperFileName : TShortPath;
  PaperFile : TOFile;
  PaperFileIsOpen : Boolean;
  FirstSwitchToOn : Boolean; { for lazy opening }

{-----------------------------------------------------------------------}
{ paper file                                                            }
{-----------------------------------------------------------------------}

{ overwrite the default paper filename }
Procedure SetPaperFilename( Filename : TShortPath );
Begin
  CheckCondition(Not PaperFileIsOpen,
      'SetPaperFilename: too late, paper file has already been opened');
  PaperFileName := Filename
End;

{ close the paper file if it is open; must only be called at the end of the
 session (upon dying) }
Procedure ClosePaperFile;
Begin
  If PaperFileIsOpen Then
  Begin
    CloseOFile(PaperFileName,PaperFile);
    PaperFileIsOpen := False
  End
End;

{ reset and open the paper file }
Procedure OpenPaperFile;
Begin
  PaperFileIsOpen := OpenForWrite(PaperFileName,PaperFile);
  If Not PaperFileIsOpen Then
    RuntimeError('cannot open paper file')
End;

{-----------------------------------------------------------------------}
{ get / set state                                                       }
{-----------------------------------------------------------------------}

{ is paper on? }
Function GetPaperState : Boolean;
Begin
  GetPaperState := PaperIsOn
End;

{ set the paper state, lazy opening the paper file on the first switch to on; 
 note that because PaperIsOn is unconditionally assigned to the target state, 
 paper/0 always succeeds on first call even if the paper file cannot be opened; 
 in that case, the consequence is a run time error, not a failure to clear the
 goal }
Procedure SetPaperState( state : Boolean );
Begin
  { lazy opening of the paper file on the first switch to on }
  FirstSwitchToOn := Not FirstSwitchToOn And state;
  If FirstSwitchToOn Then
    OpenPaperFile;
  PaperIsOn := State
End;

{-----------------------------------------------------------------------}
{ write to the paper file                                               }
{-----------------------------------------------------------------------}

{ write to the paper file if requested and possible }
Procedure WriteToPaperFile( s : TString );
Begin
  If PaperIsOn And PaperFileIsOpen Then
    WriteToFile(PaperFileName,PaperFile,s)
End;

{ writeln to the paper file if requested and possible }
Procedure WritelnToPaperFile( s : TString );
Begin
  If PaperIsOn And PaperFileIsOpen Then
    WritelnToFile(PaperFileName,PaperFile,s)
End;

{-----------------------------------------------------------------------}
{ initialize the unit                                                   }
{-----------------------------------------------------------------------}

Begin
  PaperFileIsOpen := False;
  { default paper file name }
  SetPaperFilename('prolog.log');
  { start with no paper }
  PaperIsOn := False;
  { first switch to On state did not happen yet }
  FirstSwitchToOn := False
End.