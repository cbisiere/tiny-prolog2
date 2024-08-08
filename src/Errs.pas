{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Errs.pas                                                   }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                         E R R O R S  /  Q U I T                            }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Unit Errs;

Interface

Uses
  ShortStr;

Type 
  TErrorState = (
    NO_ERROR,
    PARAMETER_ERROR,
    SYNTAX_ERROR,
    RUNTIME_ERROR,
    USER_INTERRUPT
  );

Procedure ResetError;

Function GetErrorMessage : TString;
Function Error : Boolean;
Function ErrorState : TErrorState;
Function FatalError : Boolean;
Function QuitRequested : Boolean;

Procedure HaltProgram;
Procedure SetQuitOn( n : Integer );

Procedure SyntaxError( msg : TString );
Procedure RuntimeError( msg : TString );
Procedure ParameterError( msg : TString );
Procedure UserInterrupt;
Procedure Bug( msg : TString );

Procedure CheckCondition( Condition : Boolean; msg : TString );

Implementation
{-----------------------------------------------------------------------------}

Var
  State : TErrorState;
  Quit : Boolean; { halt is required }
  Code : Integer; { termination code }
  Message : TString; { error message }

{ reset the unit to no error state }
Procedure ResetError;
Begin
  State := NO_ERROR;
  Quit := False;
  Code := 0;
  Message := ''
End;

{ return the current error message }
Function GetErrorMessage : TString;
Begin
  GetErrorMessage := Message
End;

{ is there an error? }
Function Error : Boolean;
Begin
  Error := State <> NO_ERROR
End;

{ error state }
Function ErrorState : TErrorState;
Begin
  ErrorState := State
End;

{ is there a fatal error? }
Function FatalError : Boolean;
Begin
  FatalError := Error And Quit
End;

{ is there an ongoing request to quit? }
Function QuitRequested : Boolean;
Begin
  QuitRequested := Quit
End;

{ an error occurred; display a message }
Procedure RaiseError( t : TErrorState; msg : TString );
Begin
  State := t;
  Message := msg
End;

{ quit }
Procedure HaltProgram;
Begin
  Halt(Code)
End;

{ ask for termination }
Procedure SetQuitOn( n : Integer );
Begin
  Quit := True;
  Code := n
End;

{ a syntax error occurred; display a message }
Procedure SyntaxError( msg : TString );
Begin
  RaiseError(SYNTAX_ERROR,'Syntax error: ' + msg)
End;

{ a runtime error occurred; display a message }
Procedure RuntimeError( msg : TString );
Begin
  RaiseError(RUNTIME_ERROR,'Runtime error: ' + msg)
End;

{ a parameter (command line) error occurred; display a message }
Procedure ParameterError( msg : TString );
Begin
  RaiseError(PARAMETER_ERROR,'Command line error: ' + msg)
End;

{ user interruption }
Procedure UserInterrupt;
Begin
  RaiseError(USER_INTERRUPT,'USER INTERRUPT')
End;

{ a bug occurred: display a message and terminate }
Procedure Bug( msg : TString );
Begin
  Writeln;
  Writeln('*** Internal error (sorry): ' + msg);
  Halt(1)
End;

{ assert }
Procedure CheckCondition( Condition : Boolean; msg : TString );
Begin
  If Not Condition Then
    Bug(msg)
End;

{ initialize the unit }
Begin
  ResetError
End.