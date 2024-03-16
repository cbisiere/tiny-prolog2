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

Procedure ResetError;

Function GetErrorMessage : TString;
Function Error : Boolean;
Function FatalError : Boolean;
Function QuitRequested : Boolean;

Procedure RaiseError( msg : TString );
Procedure HaltProgram;
Procedure SetQuitOn( n : Integer );

Procedure SyntaxError( msg : TString );
Procedure RuntimeError( msg : TString );
Procedure Bug( msg : TString );

Procedure CheckCondition( Condition : Boolean; msg : TString );

Implementation
{-----------------------------------------------------------------------------}

Var
  Err : Boolean; { an error occurred? }
  Quit : Boolean; { halt is required }
  Code : Integer; { termination code }
  Message : TString; { error message }

{ reset the unit to no error state }
Procedure ResetError;
Begin
  Err := False;
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
  Error := Err
End;

{ is there a fatal error? }
Function FatalError : Boolean;
Begin
  FatalError := Err And Quit
End;

{ is there an ongoing request to quit? }
Function QuitRequested : Boolean;
Begin
  QuitRequested := Quit
End;

{ an error occurred; display a message }
Procedure RaiseError( msg : TString );
Begin
  Err := True;
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
  RaiseError('Syntax error: ' + msg)
End;

{ a runtime error occurred; display a message }
Procedure RuntimeError( msg : TString );
Begin
  RaiseError('Runtime error: ' + msg)
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