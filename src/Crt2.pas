{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Crt2.pas                                                   }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                          E N H A N C E D   C R T                           }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

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
   bytes minus one on the left of the cursor, and GotoXY does not work
   properly when x > 1
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
  ShortStr,
  Chars,
  Num,
  Errs;

Const 
  CrtScreenMaxWidth = 255;
  CrtTabSize = 8; { number of spaces per tab char; FIXME: terminal-dependent }

{ current row }
Type
  TCrtRowData = Array[1..CrtScreenMaxWidth] Of TChar;
  TCrtRow = Record
    Len : Byte; { number of chars in the row }
    Wrap : Integer; { Crt will wrap when greater than CrtScreenMaxWidth }
    Row : TCrtRowData
  End;

Type
  TCrtCoord = 1..255;

Var
  CrtRow : TCrtRow;
  CrtScreenWidth : TCrtCoord;
  CrtScreenHeight : TCrtCoord;


Function CrtCharWidthOnScreen( cc : TChar ) : Byte;
Function CrtFits( cc : TChar ) : Boolean;
Procedure CrtChar( R : TCrtRowData; i : TCrtCoord; Var cc : TChar);

Procedure CrtWriteLn;
Procedure CrtWriteCharThatFits( cc : TChar );
Procedure CrtWriteChar( cc : TChar );
Procedure CrtWriteShortString( s : TString );

Procedure CrtBackspace;
Procedure CrtClrLines( y : TCrtCoord; n : Byte );
Procedure CrtClrSrc;
Procedure CrtBeep;

Procedure CrtReplay( R : TCrtRow; y : TCrtCoord );


Implementation
{-----------------------------------------------------------------------------}
{ TP4/FPC compatibility code }
{$IFDEF MSDOS}

Const
  ScreenWidth = 80;
  ScreenHeight = 25;

{$ENDIF}
{-----------------------------------------------------------------------------}


{ screen width in number of 1-byte characters }
Function GetScreenWidth : TCrtCoord;
Begin
  GetScreenWidth := ScreenWidth
End;

{ screen height in number of rows }
Function GetScreenHeight : TCrtCoord;
Begin
  GetScreenHeight := ScreenHeight
End;
{----------------------------------------------------------------------------}
{ initializations                                                            }
{----------------------------------------------------------------------------}

{ reset the memorized line }
Procedure CrtResetLine;
Begin
  With CrtRow Do
  Begin
    Len := 0;
    Wrap := 0
  End;
End;

{----------------------------------------------------------------------------}
{ get / set                                                                  }
{----------------------------------------------------------------------------}

{ number of bytes that a char takes when trying to fit in the screen row 
 without breaking the line; a n-byte UTF-8 char "consumes" n bytes wrt
 wrapping because the unit Crt does not know anything about multi-byte
 chars, even if the terminal is able to display the UTF-8 char correctly,
 taking a single "column" }
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

{ return true if cc fits into the current screen row; we do not use 
 the last column, as displaying a character here will trigger a line
 break }
Function CrtFits( cc : TChar ) : Boolean;
Begin
  CrtFits := CrtRow.Wrap + CrtCharWrapSize(cc) < CrtScreenWidth
End;

{ get char i in row data R }
Procedure CrtChar( R : TCrtRowData; i : TCrtCoord; Var cc : TChar);
Begin;
  cc := R[i]
End;

{ set to cc char i in row data R }
Procedure CrtSetChar( Var R : TCrtRowData; i : TCrtCoord; cc : TChar);
Begin
  R[i] := cc
End;

{----------------------------------------------------------------------------}
{ main procedures                                                            }
{----------------------------------------------------------------------------}

{ all the procedures below keep CrtRow in sync with what is displayed on 
 screen; using other Crt procedures, such as GotoXY, may break this and 
 therefore should be avoided }

{ new line }
Procedure CrtWriteLn;
Begin
  WriteLn;
  CrtResetLine
End;

{ write a char on screen, breaking the line when the screen is full }
Procedure CrtWriteCharThatFits( cc : TChar );
Begin
  Write(cc.Bytes);
  { we must exit in case of error, as the error module uses Crt to display 
   messages about bugs; if the bug is about Crt, this could create an 
   infinite loop }
  If Error Then Exit;
  CheckCondition(CrtFits(cc),'Character does not fit into the screen');
  With CrtRow Do
  Begin
    Len := Len + 1;
    CrtSetChar(Row,Len,cc);
    Wrap := Wrap + CrtCharWrapSize(cc)
  End
End;

{ write a char on screen, breaking the line when the screen is full or when
 the char itself if NewLine; remember that the input subsystem replaces CR, LF, 
 and CRLF with (NewLine) (LF), so CR should only appear when we add it 
 ourselves, which we avoid doing
 FIXME: StrPtr must use TChar instead of 1-byte chars }
Procedure CrtWriteChar( cc : TChar );
Begin
  If cc.Bytes = NewLine Then
    CrtWriteLn
  Else
  Begin
    If Not CrtFits(cc) Then
      CrtWriteLn;
    CrtWriteCharThatFits(cc)
  End
End;

{ write a string of 1-byte characters on screen; the string char is assumed 
 to fit into the screen row }
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

{ backspace one char at the cursor }
Procedure CrtBackspace;
Var
  cc : TChar;
Begin
  CheckCondition(CrtRow.Len > 1,'CrtBackspace: no chars to delete');
  Write(#08); { backspace }
  ClrEol;
  With CrtRow Do
  Begin
    CrtChar(Row,Len,cc);
    Wrap := Wrap - CrtCharWrapSize(cc);
    Len := Len - 1
  End
End;

{ clear the current line }
Procedure CrtClrLine;
Begin
  GotoXY(1,WhereY);
  ClrEol;
  CrtResetLine
End;

{ clear n lines from y and move the cursor at the beginning of line y }
Procedure CrtClrLines( y : TCrtCoord; n : Byte );
Var
  i : TCrtCoord;
Begin
  CheckCondition(y <= CrtScreenHeight,'CrtClrLines: target is out of screen');
  For i := Min(y + n - 1,CrtScreenHeight) DownTo y Do
  Begin
    GotoXY(1,i);
    ClrEol
  End;
  CrtClrLine
End;

{ delete the current line, move the cursor up and clear that line }
Procedure CrtDelLine;
Begin
  CheckCondition(WhereY > 1,'CrtDelLine: cannot delete line 1');
  DelLine;
  GotoXY(1,WhereY-1);
  CrtClrLine
End;

{ clear the screen }
Procedure CrtClrSrc;
Begin
  ClrScr;
  CrtResetLine
End;

{ emit a beep sound }
Procedure CrtBeep;
Begin
  Write(#07) { bell }
End;

{ replay (display) the row R at row number Y }
Procedure CrtReplay( R : TCrtRow; y : TCrtCoord );
Var
  i : TCrtCoord;
  cc : TChar;
Begin
  GotoXY(1,y);
  CrtClrLine;
  With R Do
    For i := 1 to Len Do
    Begin
      CrtChar(Row,i,cc);
      CrtWriteCharThatFits(cc)
    End
End;


{ initialize the unit }
Begin
  CrtScreenWidth := GetScreenWidth;
  CrtScreenHeight := GetScreenHeight;
  CrtResetLine
End.