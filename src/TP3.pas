{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : TP3.pas                                                    }
{   Author      : Christophe Bisière                                         }
{   Date        : 2022-09-17                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                   T U R B O   P A S C A L  3   C O M P A T                 }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ simulate a LongInt }
Type LongInt = Real;

{ ReadKey, as in Free Pascal Compiler's Crt module }
Function ReadKey : Char;
Var c : Char;
Begin
  Read(Kbd, c);
  If (c=#27) And KeyPressed Then
    c := #00;
  ReadKey := c
End;

{ set out-of-memory detection }
Procedure InitMalloc;
Begin
End;

{ allocate memory; return Nil if there is not enough memory }
Function Malloc( size : Integer ) : Pointer;
Var 
  m : Real;
  p : Pointer;
Begin
  m := MaxAvail;
  If m<0 Then
    m := 16*(65536.0 + m); { FIXME: assuming 16-bit system; p126 }
  If MaxAvail < size Then
    p := Nil
  Else
    GetMem(p,size);
  Malloc := p
End;