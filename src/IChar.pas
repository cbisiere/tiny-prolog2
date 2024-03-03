{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : IChar.pas                                                  }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                             I N P U T   C H A R                            }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ multi-byte characters, with line number and character position tracing }

{
  ASSUMPTIONS AND LIMITS:
  -----------------------
  1) TCharPos: Integer
    maximum number of characters in an input line = MAXINT;
    However, everything should work with a larger type (e.g., LongInt). 
    Note that input files are read char by char so the length of each 
    input line is not limited to BufSize.
}

Unit IChar;

Interface

Uses
  Chars,
  Common;

Const
  { code for 'end of input' (ASCII SUB / Ctrl-Z); 
   this character is returned from various functions when input is exhausted  
   (Eof for files, Eol for keyboard input); it is also honored when present   
   in the input 
   see: https://en.wikipedia.org/wiki/Substitute_character }
  EndOfInput = #$1A;                       

{ basic types }
Type 
  TLineNum = Integer;  { input line number }
  TCharPos = Integer; { position of a char in an input line }

{ element in the buffet: input char }
Type 
  TIChar = Record
    Val : TChar; { the char  }
    Lnb : TLineNum; { line number }
    Pos : TCharPos { position in the current line }
  End;

Procedure SetIChar( Var e : TIChar; v : TChar; line : TLineNum; col : TCharPos );
Procedure NewICharFromPrev( Var e : TIChar; p : TIChar; v : TChar );

Function IsEol( e : TIChar ) : Boolean;

Implementation

{ set a character }
Procedure SetIChar( Var e : TIChar; v : TChar; line : TLineNum; 
    col : TCharPos );
Begin
  With e Do
  Begin
    Val := v;
    Lnb := line;
    Pos := col
  End
End;

{ set a character e with a given value v, computing its position 
 data from a previous character p }
Procedure NewICharFromPrev( Var e : TIChar; p : TIChar; v : TChar );
Begin
  e := p;
  With e Do
  Begin
    If (Val = EndOfInput) Or (v = EndOfInput) Then
      Pass
    Else If Val = NewLine Then { first char after a new line }
    Begin
      Lnb := Lnb + 1;
      Pos := 1
    End
    Else { new char in the same input line }
      Pos := Pos + 1;
    Val := v
  End
End;

{ true if character e is an end of line }
Function IsEol( e : TIChar ) : Boolean;
Begin
  IsEol := e.Val = NewLine
End;

{ true if character e is an end of input }
Function IsEndOfInput( e : TIChar ) : Boolean;
Begin
  IsEndOfInput := e.Val = EndOfInput
End;

{ set a character to Eol }
Procedure SetToEol( Var e : TIChar );
Begin
  e.Val := NewLine
End;

{ set a character to Eof }
Procedure SetToEndOfInput( Var e : TIChar );
Begin
  e.Val := EndOfInput
End;

End.