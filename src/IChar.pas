{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : IChar.pas                                                  }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022-2026                                                  }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                             I N P U T   C H A R                            }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }

{ multi-byte characters, with line number and character position tracking }

{
  ASSUMPTIONS AND LIMITS:
  -----------------------
  1) TCharPos: Integer
    maximum number of characters in an input line = MAXINT;
    However, everything should work with a larger type (e.g., PosInt). 
    Note that input files are read char by char so the length of each 
    input line is not limited to BufSize.
}

Unit IChar;

Interface

Uses
  Chars,
  Common,
  ShortStr,
  Num,
  Errs,
  Dump;

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

Function TICharGetEncoding( e : TIChar ) : TEncoding;
Function TICharGetByte( e : TIChar; i : TCharByteIndex ) : Char;
Function TICharGetLength( e : TIChar ) : TCharByteIndex;
Function TICharIs( e : TIChar; b : TCharBytes ) : Boolean;
Function TICharIsSoftMark( e : TIChar ) : Boolean;
Function TICharIsIn( e : TIChar; EE : CharSet ) : Boolean;
Function TICharIsTab( e : TIChar ) : Boolean;
Function TICharIsEndOfInput( e : TIChar ) : Boolean;
Function TICharIsEol( e : TIChar ) : Boolean;
Function TICharIsSpace( e : TIChar ) : Boolean;

Procedure TICharSetChar( Var e : TIChar; v : TChar );
Procedure TICharSetPos( Var e : TIChar; line : TLineNum; col : TCharPos );
Procedure TICharSet( Var e : TIChar; v : TChar; line : TLineNum; col : TCharPos );
Procedure TICharSwap( Var e1,e2 : TIChar );
Function TICharToCodePoint( e : TIChar; Var cp : TCodePoint ) : Boolean;

Procedure TICharSetPosFrom( Var e : TIChar; e1 : TIChar );
Procedure TICharSetPosFromNext( Var e : TIChar; n : TIChar );
Procedure TICharSetPosFromPrev( Var e : TIChar; p : TIChar );

Function TICharToDebugShortString( e : TIChar ) : TString;
Procedure TICharDump( e : TIChar );

Implementation


{----------------------------------------------------------------------------}
{ get/test                                                                   }
{----------------------------------------------------------------------------}

{ get encoding }
Function TICharGetEncoding( e : TIChar ) : TEncoding;
Begin
  TICharGetEncoding := TCharGetEncoding(e.Val)
End;

{ get byte number i }
Function TICharGetByte( e : TIChar; i : TCharByteIndex ) : Char;
Begin
  TICharGetByte := TCharGetByte(e.Val,i)
End;

{ number of bytes in a character }
Function TICharGetLength( e : TIChar ) : TCharByteIndex;
Begin
  TICharGetLength := TCharGetLength(e.Val)
End;

{ test the char part }
Function TICharIs( e : TIChar; b : TCharBytes ) : Boolean;
Begin
  TICharIs := TCharIs(e.Val,b)
End;

{ true if e is a soft mark }
Function TICharIsSoftMark( e : TIChar ) : Boolean;
Begin
  TICharIsSoftMark := TCharIsSoftMark(e.Val)
End;

{ true if character e in in char set EE }
Function TICharIsIn( e : TIChar; EE : CharSet ) : Boolean;
Begin
  TICharIsIn := TCharIsIn(e.Val,EE)
End;

{ true if character e is a tab }
Function TICharIsTab( e : TIChar ) : Boolean;
Begin
  TICharIsTab := TCharIs(e.Val,#09)
End;

{ true if character e is an end of input mark }
Function TICharIsEndOfInput( e : TIChar ) : Boolean;
Begin
  TICharIsEndOfInput := TCharIsEndOfInput(e.Val)
End;

{ true if character e is an end of line }
Function TICharIsEol( e : TIChar ) : Boolean;
Begin
  TICharIsEol := TCharIsEol(e.Val)
End;

{ true if character e is a space character }
Function TICharIsSpace( e : TIChar ) : Boolean;
Begin
  TICharIsSpace := TCharIsSpace(e.Val)
End;


{----------------------------------------------------------------------------}
{ set                                                                        }
{----------------------------------------------------------------------------}

{ set a character to Eol }
Procedure TICharSetToEol( Var e : TIChar );
Begin
  e.Val := CC_END_OF_LINE
End;

{ set a character to Eof }
Procedure TICharSetToEndOfInput( Var e : TIChar );
Begin
  e.Val := CC_END_OF_INPUT
End;

{ set a character's value }
Procedure TICharSetChar( Var e : TIChar; v : TChar );
Begin
  With e Do
    Val := v
End;

{ set a character's position }
Procedure TICharSetPos( Var e : TIChar; line : TLineNum; col : TCharPos );
Begin
  With e Do
  Begin
    Lnb := Line;
    Pos := Col
  End
End;

{ set a character }
Procedure TICharSet( Var e : TIChar; v : TChar; line : TLineNum; 
    col : TCharPos );
Begin
  TICharSetChar(e,v);
  TICharSetPos(e,line,col)
End;

{ swap two characters }
Procedure TICharSwap( Var e1,e2 : TIChar );
Var
  tmp : TIChar;
Begin
  tmp := e1;
  e1 := e2;
  e2 := tmp
End;

Function TICharToCodePoint( e : TIChar; Var cp : TCodePoint ) : Boolean;
Begin
  TICharToCodePoint := TCharToCodePoint(e.Val,cp)
End;

{----------------------------------------------------------------------------}
{ position calculation                                                       }
{----------------------------------------------------------------------------}

{ set a character's position from another character }
Procedure TICharSetPosFrom( Var e : TIChar; e1 : TIChar );
Begin
  TICharSetPos(e,e1.Lnb,e1.Pos)
End;

{ set a character's position based on the previous character; note that EOF is
 treated as a regular character, one position after the last character of the 
 stream }
Procedure TICharSetPosFromPrev( Var e : TIChar; p : TIChar );
Begin
  CheckCondition(Not TICharIsEndOfInput(p),
      'TICharSetPosFromPrev: prev is EOF');
  If TICharIsEol(p) Then { first char after a new line }
    TICharSetPos(e,p.Lnb+1,1)
  Else { new char in the same input line }
    TICharSetPos(e,p.Lnb,p.Pos+1)
End;

{ set a character's position based on the next character; this procedure is 
 meant to be called when prepending a char, otherwise, if that char is  
 EOL, its position will be  wrong (and set to 1) as we do not know the 
 length of the line this EOL is the end of; note that EOF is treated as a 
 regular character, one position after the last character of the stream }
Procedure TICharSetPosFromNext( Var e : TIChar; n : TIChar );
Begin
  CheckCondition(Not TICharIsEndOfInput(e),
      'TICharSetPosFromNext: char is EOF');
  If TICharIsEol(e) Then
    TICharSetPos(e,n.Lnb-1,1) { Pos=1 may be wrong, but we do not have enough info }
  Else 
    TICharSetPos(e,n.Lnb,n.Pos-1) { previous char in the same input line }
End;

{----------------------------------------------------------------------------}
{ dump                                                                       }
{----------------------------------------------------------------------------}

{ dump the content of char e }
Function TICharToDebugShortString( e : TIChar ) : TString;
Begin
  TICharToDebugShortString := PosIntToShortString(e.Lnb) + ':' + 
      PosIntToShortString(e.Pos) + ':' + TCharToDebugShortString(e.Val)
End;

{ dump the content of char e }
Procedure TICharDump( e : TIChar );
Begin
  WriteToDumpFile(TICharToDebugShortString(e))
End;

End.