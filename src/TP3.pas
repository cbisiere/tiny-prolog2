{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : TP3.pas                                                    }
{   Author      : Christophe Bisière                                         }
{   Date        : 2022-09-17                                                 }
{   Updated     : 2022                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                   T U R B O   P A S C A L  3  C O M P A T                  }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ ReadKey, as in Free Pascal Compiler's Crt module }
Function ReadKey : Char;
Var c : Char;
Begin
  Read(Kbd, c);
  If (c=#27) And KeyPressed Then
    c := #00;
  ReadKey := c
End;
