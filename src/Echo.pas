{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Echo.pas                                                   }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022-2026                                                  }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                            E C H O   S T A T E                             }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }

{ global echo state (must be sync'ed with Prolog engine's internal state) }

Unit Echo;

Interface

Function GetEchoState : Boolean;
Procedure SetEchoState( state : Boolean );

Implementation
{-----------------------------------------------------------------------------}

Var
  EchoIsOn : Boolean;

{ is echo on? }
Function GetEchoState : Boolean;
Begin
  GetEchoState := EchoIsOn
End;

{ set the echo state }
Procedure SetEchoState( state : Boolean );
Begin
 EchoIsOn := state
End;

{ initialize the echo state }
Begin
  EchoIsOn := False
End.