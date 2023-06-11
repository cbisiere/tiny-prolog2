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

Procedure DisplayInputErrorMessage( msg : AnyStr ); Forward;
Procedure CloseCurrentInput; Forward;

Var
  Error : Boolean; { an error occurred? }

{ an error occurred; display a message }
Procedure RaiseError( msg : AnyStr );
Begin
  If Not Error Then
  Begin
    DisplayInputErrorMessage(msg);
    CloseCurrentInput;
    Error := True
  End
End;

{ raise an error if a condition is met }
Procedure RaiseErrorIf( cond : Boolean; msg : AnyStr );
Begin
  If Cond Then
    RaiseError(msg)
End;

{ terminate the programme with a return code }
Procedure Terminate( code : Integer );
Begin
  TerminateTrace;
  Halt(code)
End;