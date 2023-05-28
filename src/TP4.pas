{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : TP4.pas                                                    }
{   Author      : Christophe Bisiere                                         }
{   Date        : 2023-05-20                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                   T U R B O   P A S C A L  4   C O M P A T                 }
{                                                                            }
{----------------------------------------------------------------------------}

{ make Turbo Pascal 4 behaves as Turbo Pascal 3 }

{$R-}    {Range checking off}
{$B+}    {Boolean complete evaluation on}
{$S+}    {Stack checking on}
{$I+}    {I/O checking on}
{$N-}    {No numeric coprocessor}
{ $M 65500,16384,655360} {Turbo 3 default stack and heap}

Uses
  Crt,
  Turbo3;

{$I TP3.pas }
