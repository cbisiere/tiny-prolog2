{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : OStream.pas                                                }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023-2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                       O U T P U T   S T R E A M                            }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ output characters to console or files }

{
  ASSUMPTIONS AND LIMITS:
  -----------------------
  1) for correct display of strings contained in a Prolog program, terminal  
    encoding is the same as the file
}

Unit OStream;

Interface

Uses
  ShortStr,
  Num,
  Errs,
  Files,
  Trace;

{ output stream }
Type
  TOStream = Record
    FDesc : TFileDescriptor;        { 1,2,...                                 }
    Alias : TAlias;                 { name of the stream                      }
    FName : TPath;                  { file's full path                        }
    OFile : TOFile;                 { file handler (if not a console)         }
    IsOpen : Boolean;               { is the stream ready for output?         }
    DeviceType : TIODeviceType;     { type of device                          }
  End;
  TOStreamPtr = ^TOStream;

Function OStreamIsOpen( f : TOStreamPtr ) : Boolean;
Function GetOStreamDescriptor( f : TOStreamPtr ) : TFileDescriptor;
Function GetOStreamAlias( f : TOStreamPtr ) : TAlias;
Function GetOStreamFileName( f : TOStreamPtr ) : TPath;
Function GetOStreamDeviceType( f : TOStreamPtr ) : TIODeviceType;

Procedure OpenOStream( Desc : TFileDescriptor; FileAlias : TAlias; 
    FileName : TPath; f : TOStreamPtr );
Procedure CloseOStream( f : TOStreamPtr );
Procedure WriteToOStream( f : TOStreamPtr; s : TString );
Procedure FlushOStream( f : TOStreamPtr );

Procedure OStreamDump( f : TOStreamPtr );


Implementation
{-----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ getters / setters                                                          }
{----------------------------------------------------------------------------}

{ is an output stream open? }
Function OStreamIsOpen( f : TOStreamPtr ) : Boolean;
Begin
  With f^ Do
    OStreamIsOpen := IsOpen
End;

{ get file descriptor }
Function GetOStreamDescriptor( f : TOStreamPtr ) : TFileDescriptor;
Begin
  With f^ Do
    GetOStreamDescriptor := FDesc
End;

{ get file alias }
Function GetOStreamAlias( f : TOStreamPtr ) : TAlias;
Begin
  With f^ Do
    GetOStreamAlias := Alias
End;

{ get file path }
Function GetOStreamFileName( f : TOStreamPtr ) : TPath;
Begin
  With f^ Do
    GetOStreamFileName := FName
End;

{ get device type }
Function GetOStreamDeviceType( f : TOStreamPtr ) : TIODeviceType;
Begin
  With f^ Do
    GetOStreamDeviceType := DeviceType
End;

{----------------------------------------------------------------------------}
{ core methods                                                               }
{----------------------------------------------------------------------------}

{ open an output stream }
Procedure OpenOStream( Desc : TFileDescriptor; FileAlias : TAlias; 
    FileName : TPath; f : TOStreamPtr );
Begin
  With f^ Do
  Begin
    FDesc := Desc;
    Alias := FileAlias;
    FName := FileName;
    If Alias = CONSOLE_NAME Then
    Begin
      DeviceType := TTerminal;
      IsOpen := True
    End
    Else
    Begin
      DeviceType := TFile;
      IsOpen := OpenForWrite(FName,OFile)
    End
  End
End;

{ close an output stream; never close the console }
Procedure CloseOStream( f : TOStreamPtr );
Begin
  With f^ Do
  Begin
    If IsOpen And (DeviceType = TFile) Then
    Begin
      CloseOFile(FName,OFile);
      IsOpen := False
    End
  End
End;

{ write a short string to an output stream }
Procedure WriteToOStream( f : TOStreamPtr; s : TString );
Begin
  With f^ Do
    Case DeviceType Of
      TTerminal:
        CWrite(s);
      TFile:
        WriteToFile(FName,OFile,s)
    End
End;

{ flush an output stream }
Procedure FlushOStream( f : TOStreamPtr );
Begin
  With f^ Do
    If IsOpen And (DeviceType = TFile) Then
      FlushFile(FName,OFile)
End;

{----------------------------------------------------------------------------}
{ DEBUG                                                                      }
{----------------------------------------------------------------------------}

Procedure OStreamDump( f : TOStreamPtr );
Begin
  With f^ Do
  Begin
    WriteToEchoFile('State of stream #' + PosIntToStr(FDesc));
    WriteToEchoFile(CRLF);
    WriteToEchoFile(' Alias: ' + Alias);
    WriteToEchoFile(CRLF);
    WriteToEchoFile(' Path: ' + FName);
    WriteToEchoFile(CRLF)
  End
End;

End.