{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Mirror.pas                                                 }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                           M I R R O R   F I L E S                          }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ Write to the mirror files, that is, echo file and trace file }

Unit Mirror;

Interface

Uses
  ShortStr,
  Trace,
  Echo;


Procedure WriteToMirrorFiles( s : TString );
Procedure WritelnToMirrorFiles( s : TString );

Implementation
{-----------------------------------------------------------------------------}

{ write a char to the trace and echo systems }
Procedure WriteToMirrorFiles( s : TString );
Begin
  WriteToEchoFile(s);
  WriteToTraceFile(s)
End;

{ writeln a char to the trace and echo systems }
Procedure WritelnToMirrorFiles( s : TString );
Begin
  WritelnToEchoFile(s);
  WritelnToTraceFile(s)
End;

End.