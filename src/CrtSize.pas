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

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ return Crt size; handle Crt resize when proper OS support is available; when 
 support is available, the reported screen size can be updated asynchronously
 or through an explicit call to a sync function; async is the default }

Unit CrtSize;

Interface

Uses
{$IFDEF UNIX}
  Crt, { to get initial terminal size: ScreenWidth, ScreenHeight }
  Common,
  TermIO,
  BaseUnix,
{$ENDIF}
  ShortStr,
  Trace;

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


Implementation
{-----------------------------------------------------------------------------}

Var
  SignalCount : Word; { debug }
  CrtSizeSyncMode : TCrtSizeSyncMode; { current sync mode }
  CrtSizeCurrentWidth : Word; { number of columns (updated real time) }
  CrtSizeCurrentHeight : Word; { number of rows (updated real time) }

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
{ get size                                                                   }
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
        CrtSizeSync
      {//};CrtSizeDump;
    End;
  SignalCount := SignalCount + 1
End;

Procedure TermHandler( sig : LongInt; info : PSigInfo; context : PSigContext ); 
Cdecl;
Begin
  Case sig Of
    SIGWINCH: CrtSizeUpdate
  End
End;

Function InstallTermHandler: Boolean;
Var
  action : SigActionRec;
Begin
  FillChar(action, SizeOf(action), 0);
  action.Sa_Handler := TermHandler;
  action.Sa_Flags := SA_SIGINFO;
  InstallTermHandler := fpSigAction(SIGWINCH,@action,Nil) = 0
End;

{$ELSE}

Const
  ScreenWidth = 80;
  ScreenHeight = 25;

{$ENDIF}


{----------------------------------------------------------------------------}
{ debug                                                                      }
{----------------------------------------------------------------------------}

{ display debug data }
Procedure CrtSizeDump;
Begin
  WriteToTraceFile('| CRT SIZE: ');
  WriteToTraceFile(' Count=' + IntToShortString(SignalCount));
  WriteToTraceFile(' Width=' + IntToShortString(CrtSizeScreenWidth));
  WriteToTraceFile(' Height=' + IntToShortString(CrtSizeScreenHeight));
  WriteToTraceFile(' Current Width=' + IntToShortString(CrtSizeCurrentWidth));
  WriteToTraceFile(' Current Height=' + IntToShortString(CrtSizeCurrentHeight));
  WritelnToTraceFile('')
End;


{----------------------------------------------------------------------------}
{ init                                                                       }
{----------------------------------------------------------------------------}

{ initialize the unit }
Begin
  SignalCount := 0;
  CrtSizeChanged := False;
  { set current size }
  CrtSizeCurrentWidth := ScreenWidth;
  CrtSizeCurrentHeight := ScreenHeight;
  { set reported size }
  CrtSizeSync;
  { default sync mode }
  CrtSizeSetSyncMode(CRT_SIZE_MODE_ASYNC);
{$IFDEF UNIX}
  If Not InstallTermHandler() Then
    Pass
{$ENDIF}
{//};CrtSizeDump;
End.