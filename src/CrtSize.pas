{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : CrtSize.pas                                                }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022-2025                                                  }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                             C R T   S I Z E                                }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ return Crt size; handle Crt resize when proper OS support is available; when 
 support is available, the reported screen size can be updated asynchronously
 or through an explicit call to a sync function; async is the default }

Unit CrtSize;

Interface

Uses
{$IFDEF MSWINDOWS}
  Windows, { to get initial terminal size on Windows }
{$ENDIF}
{$IFDEF UNIX}
  Crt, { to get initial terminal size: ScreenWidth, ScreenHeight }
  Common,
  TermIO,
  BaseUnix, { to handle dynamic resize }
{$ENDIF}
  ShortStr,
  Trace;

Const
  TRACE_CRT_SIZE = True;

{ how changes of size are reported: synchronous (through an explicit sync call) 
 or asynchronous (screen size updated in real time) }
Type
  TCrtSizeSyncMode = (CRT_SIZE_MODE_SYNC,CRT_SIZE_MODE_ASYNC);

Var
  CrtSizeScreenWidth : Word; { reported number of columns }
  CrtSizeScreenHeight : Word; { reported number of rows }
  CrtSizeChanged : Boolean; { must be reset to False by caller }

Procedure CrtSizeDump;
Procedure CrtSizeSetSyncMode( mode : TCrtSizeSyncMode );
Procedure CrtSizeSync;
Function CrtResizeIsPending : Boolean;


Implementation
{-----------------------------------------------------------------------------}

Var
  SignalCount : Word; { debug }
  CrtSizeSyncMode : TCrtSizeSyncMode; { current sync mode }
  CrtSizeCurrentWidth : Word; { number of columns (updated real time) }
  CrtSizeCurrentHeight : Word; { number of rows (updated real time) }


{----------------------------------------------------------------------------}
{ trace                                                                      }
{----------------------------------------------------------------------------}

{ trace crt op to trace file }
Procedure CrtSizeTrace;
Begin
  If TRACE_CRT_SIZE Then
    WritelnToTraceFile('CrtSize(' + IntToShortString(CrtSizeCurrentWidth) + ',' 
        + IntToShortString(CrtSizeCurrentHeight) + ')')
End;


{----------------------------------------------------------------------------}
{ sync mode                                                                  }
{----------------------------------------------------------------------------}

Procedure CrtSizeSetSyncMode( mode : TCrtSizeSyncMode );
Begin
  CrtSizeSyncMode := mode
End;

Procedure CrtSizeSync;
Begin
  CrtSizeScreenWidth := CrtSizeCurrentWidth;
  CrtSizeScreenHeight := CrtSizeCurrentHeight
End;


{----------------------------------------------------------------------------}
{ get initial screen size                                                    }
{----------------------------------------------------------------------------}
{ Turbo Pascal: initial screen size constants not available }
{$IFDEF TPC}
Procedure CrtSizeInit;
Begin
  CrtSizeCurrentWidth := 80;
  CrtSizeCurrentHeight := 25
End;
{$ELSE}
{ Fpc's Win32 and Win64: initial size provided by API }
{$IFDEF MSWINDOWS}
Procedure CrtSizeInit;
Var
  Info: TConsoleScreenBufferinfo;
  Width,Height : DWord;
Begin
  Width := 80;
  Height := 25;
  If GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE),Info) Then
  Begin
    Width := Info.dwSize.X;
    Height := Info.dwSize.Y
  End;
  CrtSizeCurrentWidth := Width;
  CrtSizeCurrentHeight := Height
End;
{$ELSE}
{ fpc's Unix: initial screen size constants are readily available }
Procedure CrtSizeInit;
Begin
  CrtSizeCurrentWidth := ScreenWidth;
  CrtSizeCurrentHeight := ScreenHeight
End;
{$ENDIF}
{$ENDIF}
{-----------------------------------------------------------------------------}


{----------------------------------------------------------------------------}
{ detect terminal resize                                                     }
{----------------------------------------------------------------------------}

{$IFDEF UNIX}
{ detection of terminal resize, adapted from Lazarus's watchconsolesize:
 https://sources.debian.org/src/lazarus/3.6%2Bdfsg1-4/debugger/test/watchconsolesize.pas/
 see also:
 https://palma.strom.sk/doc/fpc/rtl/oldlinux/winsize.html
}
Procedure CrtSizeUpdate;
Var
  WinSize: TWinSize;
Begin
  FillChar(WinSize,SizeOf(WinSize),0);
  If IsaTty(StdInputHandle) = 1 Then
    If FpIOCtl(StdInputHandle,TIOCGWINSZ,@WinSize) >= 0 Then
    Begin
      CrtSizeChanged := True;
      CrtSizeCurrentWidth := WinSize.ws_col;
      CrtSizeCurrentHeight := WinSize.ws_row;
      If CrtSizeSyncMode = CRT_SIZE_MODE_ASYNC Then
        CrtSizeSync;
      CrtSizeTrace
    End;
  SignalCount := SignalCount + 1
End;

Procedure CrtTermHandler( sig : LongInt; info : PSigInfo; context : PSigContext ); 
Cdecl;
Begin
  Case sig Of
    SIGWINCH: CrtSizeUpdate
  End
End;

Function CrtInstallTermHandler: Boolean;
Var
  action : SigActionRec;
Begin
  FillChar(action, SizeOf(action), 0);
  action.Sa_Handler := CrtTermHandler;
  action.Sa_Flags := SA_SIGINFO;
  CrtInstallTermHandler := FpSigAction(SIGWINCH,@action,Nil) = 0
End;
{$ENDIF}

Function CrtResizeIsPending : Boolean;
Begin
  CrtResizeIsPending := CrtSizeChanged
End;


{----------------------------------------------------------------------------}
{ debug                                                                      }
{----------------------------------------------------------------------------}

{ display debug data }
Procedure CrtSizeDump;
Begin
  WritelnToTraceFile('| CRT SIZE: ');
  WritelnToTraceFile(' SignalCount = ' + IntToShortString(SignalCount));
  WritelnToTraceFile(' CrtSizeScreenWidth = ' + IntToShortString(CrtSizeScreenWidth));
  WritelnToTraceFile(' CrtSizeScreenHeight = ' + IntToShortString(CrtSizeScreenHeight));
  WritelnToTraceFile(' CrtSizeCurrentWidth = ' + IntToShortString(CrtSizeCurrentWidth));
  WritelnToTraceFile(' CrtSizeCurrentHeight = ' + IntToShortString(CrtSizeCurrentHeight));
  WritelnToTraceFile(' CrtSizeChanged = ' + BoolToShortString(CrtSizeChanged));
  WritelnToTraceFile('')
End;


{----------------------------------------------------------------------------}
{ init                                                                       }
{----------------------------------------------------------------------------}

{ initialize the unit }
Begin
  SignalCount := 0;
  CrtSizeChanged := False;
  { detect initial screen size }
  CrtSizeInit;
  { sync reported size }
  CrtSizeSync;
  { default sync mode }
  CrtSizeSetSyncMode(CRT_SIZE_MODE_ASYNC);
{$IFDEF UNIX}
  { install handler for dynamic terminal resize detection }
  If Not CrtInstallTermHandler() Then
    Pass;
{$ENDIF}
  CrtSizeTrace
End.