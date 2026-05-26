{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Crt2.pas                                                   }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022-2026                                                  }
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
 - by keeping track of the current line, allowing the console input system 
   (REPL or in*/1 primitives) to recover what will be considered as a prompt 
   (as, sometimes, this prompt needs to be redrawn) 
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
  Dump;

Const
  TRACE_CRT = False;

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

  { state of the current row, up to the left of the visual cursor }
  TCrtRow = Record
    Valid : Boolean; { false if not reliable due to a GotoXY jump with X <> 1 }
    Size : TCrtColumnCount; { number of chars left of the cursor }
    Chars : Array[TCrtCoordX] Of TChar
  End;


Function CrtGetScreenWidth : TCrtCoordX;
Function CrtGetScreenHeight : TCrtCoordY;

Function CrtWindMinX : PosInt;
Function CrtWindMinY : PosInt;
Function CrtWindMaxX : PosInt;
Function CrtWindMaxY : PosInt;
Procedure CrtFullScreen;

Function CrtIsBroken( y : TCrtCoordY ) : Boolean;

Procedure CrtCurrentRowCopyTo( Var r : TCrtRow );
Procedure CrtRowWrite( r : TCrtRow );
Function CrtRowCountBytes( r : TCrtRow) : PosInt; 

Procedure CrtGotoXY( x : TCrtCoordX; y : TCrtCoordY );
Function CrtIsCursorOnFirstCol : Boolean;
Procedure CrtGotoFirstCol;
Procedure CrtGotoLine( y : TCrtCoordY );

Procedure CrtInsLine;
Procedure CrtDelLine;
Procedure CrtClrEol;
Procedure CrtClrLine( y : TCrtCoordY );
Procedure CrtClrLines( y : TCrtCoordY; n : TCrtRowCount );
Procedure CrtClrSrc;

Procedure CrtBeep;
Procedure CrtBackspace;
Procedure CrtWriteLn;
Procedure CrtWriteBytesOf( cc : TChar );

Function CrtCharWrapSize( cc : TChar ) : Byte;
Function CrtCharWidthOnScreen( cc : TChar ) : Byte;

Function CrtWraps( bytes : PosInt; cc : TChar ) : Boolean;

Procedure CrtWriteRegularChar( cc : TChar );
Procedure CrtWriteShortString( s : TString );

Procedure CrtDump;
Procedure CrtDumpCurrentRow;
Procedure CrtDumpRow( tag : TString; r : TCrtRow );

Implementation
{-----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ window size                                                                }
{----------------------------------------------------------------------------}

 { cf. TP4 doc p.303 }
 
{ X coordinate of the left edge }
Function CrtWindMinX : PosInt;
Begin
{$IFDEF FPC}
  CrtWindMinX := WindMinX
{$ELSE}
  CrtWindMinX := Lo(WindMin) + 1
{$ENDIF}
End;

{ Y coordinate of the top edge }
Function CrtWindMinY : PosInt;
Begin
{$IFDEF FPC}
  CrtWindMinY := WindMinY
{$ELSE}
  CrtWindMinY := Hi(WindMin) + 1
{$ENDIF}
End;

{ X coordinate of the right edge }
Function CrtWindMaxX : PosInt;
Begin
{$IFDEF FPC}
  CrtWindMaxX := WindMaxX
{$ELSE}
  CrtWindMaxX := Lo(WindMax) + 1
{$ENDIF}
End;

{ Y coordinate of the bottom edge }
Function CrtWindMaxY : PosInt;
Begin
{$IFDEF FPC}
  CrtWindMaxY := WindMaxY
{$ELSE}
  CrtWindMaxY := Hi(WindMax) + 1
{$ENDIF}
End;


{----------------------------------------------------------------------------}
{ screen size                                                                }
{----------------------------------------------------------------------------}

{ screen width in number of 1-byte characters }
Function CrtGetScreenWidth : TCrtCoordX;
Begin
  CrtGetScreenWidth := Min(CrtWindMaxX,CrtScreenMaxWidth) { FIXME: why not CrtSizeScreenWidth? }
End;

{ screen height in number of rows }
Function CrtGetScreenHeight : TCrtCoordY;
Begin
  CrtGetScreenHeight := Min(CrtWindMaxY,CrtScreenMaxHeight) { FIXME: why not CrtSizeScreenHeight? }
End;

Procedure CrtFullScreen;
Begin
  Window(CrtWindMinX,CrtWindMinY,CrtSizeScreenWidth,CrtSizeScreenHeight)
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
{ tracking of the current row                                                }
{----------------------------------------------------------------------------}

{ the current row is the screen row on which the visual cursor currently is;
 we try to keep a copy of this row, which is doable until the user does a 
 GotoXY jump to a different row Y and a column X > 1; in that case, we note
 that our copy is no more valid and fill in the unknown chars up to X-1 with 
 blank spaces }

Var
  CrtRow : TCrtRow;

{ set validity state }
Procedure CrtCurrentRowSetValidity( b : Boolean );
Begin
  CrtRow.Valid := b
End;

{ set size }
Procedure CrtCurrentRowSetSize( n : TCrtColumnCount );
Begin
  CrtRow.Size := n
End;

{ fill in the current row with blank spaces, without changing its size }
Procedure CrtCurrentRowFillWithBlankSpaces;
Var
  i : TCrtColumnCount;
Begin
  With CrtRow Do
  Begin
    For i := 1 to Size Do
      Chars[i] := CC_BLANK_SPACE
  End
End;

{ set the current row as invalid, without changing its size; as we do not know 
 what are the characters on the left of the visual cursor, we set them to blank 
 spaces in an attempt to minimize potential visual glitches }
Procedure CrtCurrentRowSetAsInvalid;
Begin
  CrtCurrentRowSetValidity(False);
  CrtCurrentRowFillWithBlankSpaces
End;


{ cursor is on the first column of the screen; must be called after the cursor
 is moved to beginning of a row }
Procedure CrtResetCurrentRow;
Begin
  CheckCondition(CrtIsCursorOnFirstCol,'Crt2: cursor not on first column');
  CrtCurrentRowSetValidity(True);
  CrtCurrentRowSetSize(0)
End;

{ insert a char }
Procedure CrtCurrentRowAppendOneChar( cc : TChar );
Begin
  CheckCondition(CrtRow.Size < CrtScreenMaxWidth,
      'CrtCurrentRowAppendOneChar: overflow');
  With CrtRow Do
  Begin
    Size := Size + 1;
    Chars[Size] := cc
  End
End;

{ delete a char }
Procedure CrtCurrentRowDeleteOneChar;
Begin
  CheckCondition(CrtRow.Size > 0,
      'CrtCurrentRowDeleteOneChar: underflow');
  With CrtRow Do
    Size := Size - 1
End;

{ procedures working on another row (must *not* be the local variable CrtRow) }

{ copy the current row }
Procedure CrtCurrentRowCopyTo( Var r : TCrtRow );
Begin
  r := CrtRow
End;

{ write a row (that must not wrap and must not contain any special characters); 
 this is a low-level procedure, which does not use any mirror files, 
 and which is meant to write to the screen a prompt, that might be redrawn 
 several times during keyboard input before been pushed to the mirror files }
Procedure CrtRowWrite( r : TCrtRow );
Var
  i : TCrtColumnCount;
Begin
  With r Do
    For i := 1 to Size Do
      CrtWriteBytesOf(Chars[i])
End;

{ compute the total number of bytes in a row }
Function CrtRowCountBytes( r : TCrtRow) : PosInt; 
Var
  i : TCrtColumnCount;
  n : PosInt;
Begin
  n := 0;
  With r Do
    For i := 1 to Size Do
      n := n + TCharGetLength(Chars[i]);
  CrtRowCountBytes := n
End;


{----------------------------------------------------------------------------}
{ Crt primitives: calls to Crt procedures must go through this               }
{----------------------------------------------------------------------------}

{ trace crt op to trace file }
Procedure CrtTrace( s : TString );
Begin
  If TRACE_CRT Then
  Begin
    WritelnToDumpFile('Crt(' + IntToShortString(WhereX) + ',' 
        + IntToShortString(WhereY) + '): '+ s)
  End
End;

{ is cursor on the first column? }
Function CrtIsCursorOnFirstCol : Boolean;
Begin
  CrtIsCursorOnFirstCol := WhereX = CrtWindMinX
End;

{ is cursor on the last row? }
Function CrtIsCursorOnLastRow : Boolean;
Begin
  CrtIsCursorOnLastRow := WhereY = CrtSizeScreenHeight
End;

{ move cursor at position x,y }
Procedure CrtGotoXY( x : TCrtCoordX; y : TCrtCoordY );
Begin
  CrtTrace('GotoXY(' + IntToShortString(x) + ',' + IntToShortString(y) + ')');
  GotoXY(x,y);
  { update the state of the current row: for now, we assume that the state 
   stays valid only if we jump to the first column; TODO: moving the cursor
   on a different column of the same row, when that row is not broken, may 
   still be ok }
  If CrtIsCursorOnFirstCol Then
    CrtResetCurrentRow
  Else
  Begin
    CrtCurrentRowSetSize(x-1);
    CrtCurrentRowSetAsInvalid
  End
End;

{ move cursor to the beginning of the current line }
Procedure CrtGotoFirstCol;
Begin
  CrtGotoXY(CrtWindMinX,WhereY)
End;

{ move cursor at the beginning of line y }
Procedure CrtGotoLine( y : TCrtCoordY );
Begin
  CrtGotoXY(CrtWindMinX,y)
End;

{ insert a blank line at the cursor position, moving the lines below down by 
 one line; the last screen line disappears cursor does not move }
Procedure CrtInsLine;
Begin
  CrtTrace('InsLine');
  InsLine;
  CrtBrokenInsLine(WhereY);
  CrtCurrentRowFillWithBlankSpaces { blank and still valid }
End;

{ delete the current line; lines below, if any, moves up, showing a blank line
 at the bottom; cursor does not move }
Procedure CrtDelLine;
Begin
  CrtTrace('DelLine');
  DelLine;
  CrtBrokenDelLine(WhereY);
  { current row stays valid only if the deleted line is the last on the screen }
  If CrtIsCursorOnLastRow Then
    CrtCurrentRowFillWithBlankSpaces { blank and still valid }
  Else
    CrtCurrentRowSetAsInvalid
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
  CrtResetBroken;
  CrtResetCurrentRow
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
  Write(#08);
  CrtCurrentRowDeleteOneChar
End;

{ write a blank line }
Procedure CrtWriteLn;
Begin
  CrtTrace('Writeln');
  If WhereY = CrtGetScreenHeight Then
    CrtBrokenDelLine(1)
  Else
    CrtSetBroken(WhereY + 1,False);
  Writeln;
  CrtResetCurrentRow
End;

{ write a char; this breaks WhereX if the char is multibyte }
Procedure CrtWriteBytesOf( cc : TChar );
Begin
  CrtTrace('Write ' + TCharToDebugShortString(cc));
  Write(TCharGetBytes(cc));
  CrtTrace('');
  CrtSetBroken(WhereY,CrtIsBroken(WhereY) Or TCharIsMultibyte(cc));
  CrtCurrentRowAppendOneChar(cc)
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
  If TCharIs(cc,#09) Then
    CrtCharWrapSize := CrtTabSize + 1 { weird, but this is what I observe }
  Else
    CrtCharWrapSize := TCharGetLength(cc)
End;

{ number of columns the char takes on the screen }
Function CrtCharWidthOnScreen( cc : TChar ) : Byte;
Begin
  If TCharIs(cc,#09) Then
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
 the char itself if EOL; remember that the input subsystem replaces CR, LF, 
 and CRLF with EOL (that is, LF), so CR should only appear when we add it 
 ourselves, which we avoid doing }
Procedure CrtWriteRegularChar( cc : TChar );
Begin
  CheckCondition(Not TCharIsSoftMark(cc),'CrtWriteRegularChar: soft mark');
  If CrtWraps(WhereX-1,cc) Then { avoid breaking multibyte chars }
    CrtWriteLn;
  CrtWriteBytesOf(cc)
End;

{ write a string of 1-byte characters on screen }
Procedure CrtWriteShortString( s : TString );
Var
  i : TStringSize;
  cc : TChar;
Begin
  For i := 1 to Length(s) Do
  Begin
    TCharSetFromAscii(cc,s[i]);
    CrtWriteRegularChar(cc)
  End
End;


{----------------------------------------------------------------------------}
{ dump                                                                       }
{----------------------------------------------------------------------------}

{ dump Crt state }
Procedure CrtDump;
Var
  y : TCrtCoordY;
Begin
  WritelnToDumpFile('| CRT: ');
  WritelnToDumpFile('  WhereX = ' + IntToShortString(WhereX));
  WritelnToDumpFile('  WhereY = ' + IntToShortString(WhereY));
  WritelnToDumpFile('  CrtWindMinX = ' + IntToShortString(CrtWindMinX));
  WritelnToDumpFile('  CrtWindMinY = ' + IntToShortString(CrtWindMinY));
  WritelnToDumpFile('  CrtWindMaxX = ' + IntToShortString(CrtWindMaxX));
  WritelnToDumpFile('  CrtWindMaxY = ' + IntToShortString(CrtWindMaxY));
  { tracking of broken rows }
  WriteToDumpFile('  ');
  For y := 1 To CrtGetScreenHeight Do
  Begin
    WriteToDumpFile(' ' + IntToShortString(Ord(CrtBroken[y])))
  End;
  WriteLineBreakToDumpFile
End;

{ dump a Crt row }
Procedure CrtDumpRow( tag : TString; r : TCrtRow );
Var
  i : TCrtColumnCount;
Begin
  WritelnToDumpFile('  ' + tag + '.Valid = ' + BoolToShortString(r.Valid));
  WritelnToDumpFile('  ' + tag + '.Size = ' + PosIntToShortString(r.Size));
  WriteToDumpFile('  ' + tag + '.Chars = ');
  For i := 1 to r.Size Do
    TCharDump(r.Chars[i]);
  WriteLineBreakToDumpFile
End;

{ dump Crt current row }
Procedure CrtDumpCurrentRow;
Begin
  CrtDumpRow('CrtRow',CrtRow)
End;


{----------------------------------------------------------------------------}
{ init                                                                       }
{----------------------------------------------------------------------------}

{ initialize the unit }
Begin
  CrtResetCurrentRow;
  CrtResetBroken
End.