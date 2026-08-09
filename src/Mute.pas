{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Mute.pas                                                   }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022-2026                                                  }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                            M U T E   S T A T E                             }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }

{ global mute state (must be sync'ed with Prolog engine's internal state) }

Unit Mute;

Interface

Function GetMuteState : Boolean;
Procedure SetMuteState( state : Boolean );

Implementation
{-----------------------------------------------------------------------------}

Var
  MuteIsOn : Boolean;

{ is mute on? }
Function GetMuteState : Boolean;
Begin
  GetMuteState := MuteIsOn
End;

{ set the mute state }
Procedure SetMuteState( state : Boolean );
Begin
 MuteIsOn := state
End;

{ initialize the mute state }
Begin
  MuteIsOn := False
End.