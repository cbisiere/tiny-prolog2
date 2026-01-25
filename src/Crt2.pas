{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Crt2.pas                                                   }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022-2025                                                  }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                          E N H A N C E D   C R T                           }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }

{ enhanced CRT procedures w/ partial support for multi-byte codepoints }

{ Notes on using the CRT unit in a multi-byte character set environment:

 https://www.freepascal.org/docs-html/rtl/crt/index.html reads:

  "The CRT unit stems from the TP/Dos area. It is designed to work with 
   single-byte character sets, where 1 char = 1 byte. That means that 
   widestrings or UTF-8 encoded (ansi)strings will not work correctly."

 It appears that:

 - using Write to output a string containing UTF-8 multi-byte chars in a 
   UTF-8 terminal works, as all characters are displayed correctly
 - also, backspaces (#08) correctly clear multi-byte characters  
 - however, after writing a multi-byte character, the cursor position 
   (WhereX) is wrong, as each byte is counted as one char; so, basically,
   WhereX does not return the position on the screen, but the number of 
   bytes minus one on the left of the cursor; at this point, setting GotoXY 
   does not work properly anymore when passing x>1, (although relative 
   positioning still works: e.g. GotoXY(WhereX-1,WhereY) still move left the 
   visual cursor by one screen column, even in the presence of multi-byte 
   characters)
 - as a consequence, CRT will emit a line break when writing a string 
   containing, e.g., more than 40 2-byte UTF-8 chars in a 80-char wide 
   terminal; visually, the line breaks in the middle of the line, that is,
   after 80 bytes
 - as CRT displays a line break after 80 bytes, the break might occur
   in the middle of a UTF-8 sequence, displaying a U+FFFD replacement
   character at the end of the line, and at the beginning of the next 
   one as well

 How to mitigate these issues?

 - by taking over the management of line breaks, to avoid breaking 
   UTF-8 sequences; as a result, sequences of characters containing 
   multi-byte characters will not use the whole screen width, but 
   otherwise are ok
 - by offering a workaround to the sole usage of GotoXY (w/ x > 1) in 
   this application, which is to put the cursor right after the prompt
   in the REPL or right after a piece of text when clearing in_term; 
   we do this by memorizing the sequence of bytes of the last printed 
   line, which allows the readline unit to reprint that piece of text, 
   thus positioning the cursor at the correct location 
}

{
  ASSUMPTIONS AND LIMITS:
  -----------------------
  1) CrtScreenMaxWidth: 255
    maximum width of terminal screen (in number of chars)
}

Unit Crt2;

Interface

Uses
  Crt,
  Common,
  CrtSize,
  ShortStr,
  Chars,
  Num,
  Errs,
  Trace;

Const
  TRACE_CRT = True;

Const
  CrtScreenMaxWidth = 255;
  CrtScreenMaxHeight = 255;
  CrtTabSize = 8; { number of spaces per tab char; FIXME: terminal-dependent }

Type
  { screen coordinate }
  TCrtCoordX = 1..CrtScreenMaxWidth;
  TCrtCoordY = 1..CrtScreenMaxHeight;
  { counters }
  TCrtColumnCount = 0..CrtScreenMaxWidth;
  TCrtRowCount = 0..CrtScreenMaxHeight;

Function CrtGetScreenWidth : TCrtCoordX;
Function CrtGetScreenHeight : TCrtCoordY;

Function CrtWindMinX : PosInt;
Function CrtWindMinY : PosInt;
Function CrtWindMaxX : PosInt;
Function CrtWindMaxY : PosInt;
Procedure CrtFullScreen;

Function CrtIsBroken( y : TCrtCoordY ) : Boolean;

Procedure CrtGotoXY( x : TCrtCoordX; y : TCrtCoordY );
Procedure CrtGotoLeft;
Procedure CrtGotoRight;
Procedure CrtGotoFirstCol;
Procedure CrtGotoLine( y : TCrtCoordY );

Procedure CrtInsLine;
Procedure CrtDelLine;
Procedure CrtDelLines( n : TCrtRowCount );
Procedure CrtClrEol;
Procedure CrtClrLine( y : TCrtCoordY );
Procedure CrtClrLines( y : TCrtCoordY; n : TCrtRowCount );
Procedure CrtClrSrc;

Procedure CrtBeep;
Procedure CrtBackspace;
Procedure CrtWriteln;
Procedure CrtWrite( cc : TChar );

Function CrtCharWrapSize( cc : TChar ) : Byte;
Function CrtCharWidthOnScreen( cc : TChar ) : Byte;

Function CrtWraps( bytes : PosInt; cc : TChar ) : Boolean;

Procedure CrtWriteChar( cc : TChar );
Procedure CrtWriteShortString( s : TString );

Procedure CrtDump;

Implementation
{-----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ window size                                                                }
{----------------------------------------------------------------------------}

{ X coordinate of the left edge }
Function CrtWindMinX : PosInt;
Begin
{$IFDEF FPC}
  CrtWindMinX := WindMinX
{$ELSE}
  CrtWindMinX := Lo(WindMin)
{$ENDIF}
End;

{ Y coordinate of the top edge }
Function CrtWindMinY : PosInt;
Begin
{$IFDEF FPC}
  CrtWindMinY := WindMinY
{$ELSE}
  CrtWindMinY := Hi(WindMin)
{$ENDIF}
End;

{ X coordinate of the right edge }
Function CrtWindMaxX : PosInt;
Begin
{$IFDEF FPC}
  CrtWindMaxX := WindMaxX
{$ELSE}
  CrtWindMaxX := Lo(WindMax)
{$ENDIF}
End;

{ Y coordinate of the bottom edge }
Function CrtWindMaxY : PosInt;
Begin
{$IFDEF FPC}
  CrtWindMaxY := WindMaxY
{$ELSE}
  CrtWindMaxY := Hi(WindMax)
{$ENDIF}
End;


{----------------------------------------------------------------------------}
{ screen size                                                                }
{----------------------------------------------------------------------------}

{ screen width in number of 1-byte characters }
Function CrtGetScreenWidth : TCrtCoordX;
Begin
  CrtGetScreenWidth := Min(CrtWindMaxX,CrtScreenMaxWidth)
End;

{ screen height in number of rows }
Function CrtGetScreenHeight : TCrtCoordY;
Begin
  CrtGetScreenHeight := Min(CrtWindMaxY,CrtScreenMaxHeight)
End;

Procedure CrtFullScreen;
Begin
  Window(1,1,CrtSizeScreenWidth,CrtSizeScreenHeight)
End;

{----------------------------------------------------------------------------}
{ tracking of broken screen rows due to multibyte chars                      }
{----------------------------------------------------------------------------}

{ track rows where GotoXY with x>1 cannot be used due to multibyte chars }
Type 
  TCrtBroken = Array[TCrtCoordY] Of Boolean;
Var 
  CrtBroken : TCrtBroken;

{ get row y's broken state }
Function CrtIsBroken( y : TCrtCoordY ) : Boolean;
Begin
  CrtIsBroken := CrtBroken[y]
End;

{ set row y's broken state }
Procedure CrtSetBroken( y : TCrtCoordY; State : Boolean );
Begin
  CrtBroken[y] := State
End;

{ update broken state after an InsLine at line y }
Procedure CrtBrokenInsLine( y : TCrtCoordY );
Var
  i : TCrtCoordY;
Begin
  For i := CrtGetScreenHeight DownTo y+1 Do
    CrtBroken[i] := CrtBroken[i-1];
  CrtBroken[y] := False
End;

{ update broken state after a DelLine at line y }
Procedure CrtBrokenDelLine( y : TCrtCoordY );
Var
  i : TCrtCoordY;
Begin
  For i := y To CrtGetScreenHeight-1 Do
    CrtBroken[i] := CrtBroken[i+1];
  CrtBroken[CrtGetScreenHeight] := False
End;

{ no line is broken }
Procedure CrtResetBroken;
Var
  y : TCrtCoordY;
Begin
  For y := 1 to CrtGetScreenHeight Do
    CrtBroken[y] := False
End;


{----------------------------------------------------------------------------}
{ Crt primitives: calls to Crt procedures must go through this               }
{----------------------------------------------------------------------------}

{ trace crt op to trace file }
Procedure CrtTrace( s : TString );
Begin
  If TRACE_CRT Then
    WritelnToTraceFile('Crt(' + IntToShortString(WhereX) + ',' 
        + IntToShortString(WhereY) + '): '+ s)
End;

{ move cursor at the beginning of line i }
Procedure CrtGotoXY( x : TCrtCoordX; y : TCrtCoordY );
Begin
  CrtTrace('GotoXY(' + IntToShortString(x) + ',' + IntToShortString(y) + ')');
  GotoXY(x,y)
End;

{ move cursor left by one char; CHECK: works when used more than once? }
Procedure CrtGotoLeft;
Begin
  CrtGotoXY(WhereX-1,WhereY)
End;

{ move cursor right by one char }
Procedure CrtGotoRight;
Begin
  CrtGotoXY(WhereX+1,WhereY)
End;

{ move cursor to the beginning of the current line }
Procedure CrtGotoFirstCol;
Begin
  CrtGotoXY(1,WhereY)
End;

{ move cursor at the beginning of line i }
Procedure CrtGotoLine( y : TCrtCoordY );
Begin
  CrtGotoXY(1,y)
End;

{ insert a blank line, moving the lines below down by one line; the last screen 
 line disappears cursor does not move }
Procedure CrtInsLine;
Begin
  CrtTrace('InsLine');
  InsLine;
  CrtBrokenInsLine(WhereY)
End;

{ delete the current line; lines below, if any, moves up, showing a blank line
 at the bottom; cursor does not move }
Procedure CrtDelLine;
Begin
  CrtTrace('DelLine');
  DelLine;
  CrtBrokenDelLine(WhereY)
End;

{ delete n lines; cursor does not move }
Procedure CrtDelLines( n : TCrtRowCount );
Var
  i : TCrtRowCount;
Begin
  For i := 1 To n Do
    CrtDelLine
End;

{ clear the current line from cursor position; cursor does not move }
Procedure CrtClrEol;
Begin
  CrtTrace('ClrEol');
  ClrEol;
  If WhereX = 1 Then
    CrtSetBroken(WhereY,False)
End;

{ clear row y; move the cursor to the beginning }
Procedure CrtClrLine( y : TCrtCoordY );
Begin
  CrtGotoLine(y);
  CrtClrEol
End;

{ clear n lines from y and move the cursor at the beginning of line y; allows 
 part or all of the target lines to be out of the screen }
Procedure CrtClrLines( y : TCrtCoordY; n : TCrtRowCount );
Var
  i : TCrtCoordY;
Begin
  If y <= CrtGetScreenHeight Then
  Begin
    For i := Min(y + n - 1,CrtGetScreenHeight) DownTo y Do
      CrtClrLine(i)
  End
End;

{ clear the whole screen; cursor moves to top-left corner }
Procedure CrtClrSrc;
Begin
  CrtTrace('ClrScr');
  ClrScr;
  CrtResetBroken
End;

{ emit a beep sound }
Procedure CrtBeep;
Begin
  CrtTrace('Beep');
  Write(#07) { bell }
End;

{ emit a backspace char }
Procedure CrtBackspace;
Begin
  CrtTrace('Backspace');
  Write(#08)
End;

{ write a blank line }
Procedure CrtWriteln;
Begin
  CrtTrace('Writeln');
  If WhereY = CrtGetScreenHeight Then
    CrtBrokenDelLine(1)
  Else
    CrtSetBroken(WhereY + 1,False);
  Writeln
End;

{ write a char; this breaks WhereX if the char is multibyte }
Procedure CrtWrite( cc : TChar );
Begin
  CrtTrace('Write(''' + cc.Bytes + ''')');
  Write(cc.Bytes);
  CrtTrace('');
  CrtSetBroken(WhereY,CrtIsBroken(WhereY) Or IsMultibyte(cc))
End;


{----------------------------------------------------------------------------}
{ wrap calculations                                                          }
{----------------------------------------------------------------------------}

{ number of bytes that a char takes when trying to fit in the screen row 
 without breaking the line; a n-byte UTF-8 char "consumes" n bytes wrt
 wrapping because the unit Crt does not know anything about multi-byte
 chars, even if the terminal is able to display the UTF-8 char correctly,
 taking a single screen "column" }
Function CrtCharWrapSize( cc : TChar ) : Byte;
Begin
  If cc.Bytes = #09 Then
    CrtCharWrapSize := CrtTabSize + 1 { weird, but this is what I observe }
  Else
    CrtCharWrapSize := Length(cc.Bytes)
End;

{ number of columns the char takes on the screen }
Function CrtCharWidthOnScreen( cc : TChar ) : Byte;
Begin
  If cc.Bytes = #09 Then
    CrtCharWidthOnScreen := CrtTabSize + 1 { weird, see above }
  Else
    CrtCharWidthOnScreen := 1
End;

{ return true if displaying cc triggers Crt to display a line break, when 
 the line already contains bytes bytes; note that the '>='' comparison (instead 
 of the more natural '>') implies that the last column is never used; however,
 using this last column would be tricky as spurious line breaks would be
 generated); the 120-byte limit has been observed on macOs's iTerm2 }
Function CrtWraps( bytes : PosInt; cc : TChar ) : Boolean;
Begin
  CrtWraps := bytes + CrtCharWrapSize(cc) >= Min(120,CrtGetScreenWidth)
End;


{----------------------------------------------------------------------------}
{ writes that prevent breaking multibyte chars                               }
{----------------------------------------------------------------------------}

{ these simple procedures are meant to be use by the output subsystem; the 
 command line editor requires more complex display procedures }
{ these procedures assume that, even when broken, WhereX is still the total 
 number of bytes (minus 1) of all the chars left of cursor }

{ write a char on screen, breaking the line when the screen is full or when
 the char itself if NewLine; remember that the input subsystem replaces CR, LF, 
 and CRLF with (NewLine) (LF), so CR should only appear when we add it 
 ourselves, which we avoid doing }
Procedure CrtWriteChar( cc : TChar );
Begin
  If cc.Bytes = NewLine Then
    CrtWriteln
  Else
  Begin
    If CrtWraps(WhereX-1,cc) Then { avoid breaking multibyte chars }
      CrtWriteln;
    CrtWrite(cc)
  End
End;

{ write a string of 1-byte characters on screen }
Procedure CrtWriteShortString( s : TString );
Var
  i : TStringSize;
  cc : TChar;
Begin
  For i := 1 to Length(s) Do
  Begin
    ASCIIChar(cc,s[i]);
    CrtWriteChar(cc)
  End
End;


{----------------------------------------------------------------------------}
{ debug                                                                      }
{----------------------------------------------------------------------------}

{ display debug data }
Procedure CrtDump;
Var
  y : TCrtCoordY;
Begin
  WritelnToTraceFile('| CRT: ');
  WritelnToTraceFile('  WhereX = ' + IntToShortString(WhereX));
  WritelnToTraceFile('  WhereY = ' + IntToShortString(WhereY));
  WritelnToTraceFile('  CrtWindMaxX = ' + IntToShortString(CrtWindMaxX));
  WritelnToTraceFile('  CrtWindMaxY = ' + IntToShortString(CrtWindMaxY));
  WriteToTraceFile('  ');
  For y := 1 To CrtGetScreenHeight Do
  Begin
    WriteToTraceFile(' ' + IntToShortString(Ord(CrtBroken[y])))
  End;
  WritelnToTraceFile('')
End;


{----------------------------------------------------------------------------}
{ init                                                                       }
{----------------------------------------------------------------------------}

{ initialize the unit }
Begin
  CrtResetBroken
End.