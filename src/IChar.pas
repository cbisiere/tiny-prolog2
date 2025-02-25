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

Function IsTab( e : TIChar ) : Boolean;
Function IsEol( e : TIChar ) : Boolean;
Function IsChar( e : TIChar; cc : TChar ) : Boolean;

Procedure SetICharVal( Var e : TIChar; v : TChar );
Procedure SetICharPos( Var e : TIChar; line : TLineNum; col : TCharPos );
Procedure SetIChar( Var e : TIChar; v : TChar; line : TLineNum; col : TCharPos );
Procedure SwapIChar( Var e1,e2 : TIChar );

Procedure SetICharPosFrom( Var e : TIChar; e1 : TIChar );
Procedure SetICharPosFromNext( Var e : TIChar; n : TIChar );
Procedure SetICharPosFromPrev( Var e : TIChar; p : TIChar );

Implementation


{----------------------------------------------------------------------------}
{ char test/set                                                              }
{----------------------------------------------------------------------------}

{ true if character e is a tab }
Function IsTab( e : TIChar ) : Boolean;
Begin
  IsTab := e.Val.Bytes = #09
End;

{ true if character e is an end of line }
Function IsEol( e : TIChar ) : Boolean;
Begin
  IsEol := e.Val.Bytes = NewLine
End;

{ true if character e is an end of input }
Function IsEndOfInput( e : TIChar ) : Boolean;
Begin
  IsEndOfInput := e.Val.Bytes = EndOfInput
End;

{ set a character to Eol }
Procedure SetToEol( Var e : TIChar );
Begin
  e.Val.Bytes := NewLine
End;

{ set a character to Eof }
Procedure SetToEndOfInput( Var e : TIChar );
Begin
  e.Val.Bytes := EndOfInput
End;

{ test the char part }
Function IsChar( e : TIChar; cc : TChar ) : Boolean;
Begin
  IsChar := e.Val.Bytes = cc.Bytes
End;

{----------------------------------------------------------------------------}
{ basic set functions                                                        }
{----------------------------------------------------------------------------}

{ set a character's value }
Procedure SetICharVal( Var e : TIChar; v : TChar );
Begin
  With e Do
    Val := v
End;

{ set a character's position }
Procedure SetICharPos( Var e : TIChar; line : TLineNum; col : TCharPos );
Begin
  With e Do
  Begin
    Lnb := Line;
    Pos := Col
  End
End;

{ set a character }
Procedure SetIChar( Var e : TIChar; v : TChar; line : TLineNum; 
    col : TCharPos );
Begin
  SetICharVal(e,v);
  SetICharPos(e,line,col)
End;

{ swap two characters }
Procedure SwapIChar( Var e1,e2 : TIChar );
Var
  tmp : TIChar;
Begin
  tmp := e1;
  e1 := e2;
  e2 := tmp
End;


{----------------------------------------------------------------------------}
{ position calculation                                                       }
{----------------------------------------------------------------------------}

{ set a character's position from another character }
Procedure SetICharPosFrom( Var e : TIChar; e1 : TIChar );
Begin
  SetICharPos(e,e1.Lnb,e1.Pos)
End;

{ set a character's position based on the previous character }
Procedure SetICharPosFromPrev( Var e : TIChar; p : TIChar );
Begin
  If IsEndOfInput(p) Or IsEndOfInput(e) Then
    SetICharPos(e,p.Lnb,p.Pos)
  Else If IsEol(p) Then { first char after a new line }
    SetICharPos(e,p.Lnb+1,1)
  Else { new char in the same input line }
    SetICharPos(e,p.Lnb,p.Pos+1)
End;

{ set a character's position based on the next character; this procedure is 
 meant to be called when prepending a char, otherwise, if that char is a 
 NewLine, its position will be  wrong (and set to 1) as we do not know the 
 length of the line this NewLine is the end of }
Procedure SetICharPosFromNext( Var e : TIChar; n : TIChar );
Begin
  If IsEndOfInput(n) Or IsEndOfInput(e) Then
    SetICharPos(e,n.Lnb,n.Pos)
  Else If IsEol(e) Then
    SetICharPos(e,n.Lnb-1,1) { Pos=1 may be wrong, but we do not have enough info }
  Else 
    SetICharPos(e,n.Lnb,n.Pos-1) { previous char in the same input line }
End;

End.