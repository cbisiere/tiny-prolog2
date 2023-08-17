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
{                               E R R O R S                                  }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Procedure DisplayInputErrorMessage( msg : TString ); Forward;
Procedure CloseCurrentInput; Forward;
Procedure TerminateTrace; Forward;
Procedure CoreDump( Message : TString; Trace : Boolean ); Forward;

Var
  Error : Boolean; { an error occurred? }

{ an error occurred; display a message }
Procedure RaiseError( msg : TString );
Begin
  If Not Error Then
  Begin
    DisplayInputErrorMessage(msg);
    CloseCurrentInput;
    Error := True
  End
End;

{ a syntax error occurred; display a message }
Procedure SyntaxError( msg : TString );
Begin
  RaiseError('Syntax error: ' + msg)
End;

{ a runtime error occurred; display a message }
Procedure RuntimeError( msg : TString );
Begin
  RaiseError('Runtime error: ' + msg)
End;

{ terminate the programme with a return code }
Procedure Terminate( code : Integer );
Begin
  TerminateTrace;
  Halt(code)
End;

{ a bug occurred: display a message and terminate }
Procedure Bug( msg : TString; dump : Boolean );
Begin
  RaiseError('Internal error (sorry): ' + msg);
  If dump Then
    CoreDump('Runtime error',True);
  Terminate(1)
End;

{ assert }
Procedure CheckCondition( Condition : Boolean; msg : TString );
Begin
  If Not Condition Then
    Bug(msg,False)
End;
