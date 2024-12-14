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
    LenBytes : Word; { total number of bytes in the row }
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
Procedure CrtChar( R : TCrtRow; i : TCrtCoord; Var cc : TChar);

Procedure CrtWriteLn;
Procedure CrtWriteCharThatFits( cc : TChar );
Procedure CrtWriteChar( cc : TChar );
Procedure CrtWriteShortString( s : TString );

Procedure CrtMoveLeft;
Procedure CrtMoveRight;
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
{ wrap info                                                                  }
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

{----------------------------------------------------------------------------}
{ editing an array of chars R                                                }
{----------------------------------------------------------------------------}

{ procedures below work on a row R, w/o any change to what is displayed on 
 screen }

{ get char i in row data R }
Procedure CrtChar( R : TCrtRow; i : TCrtCoord; Var cc : TChar);
Begin;
  cc := R.Row[i]
End;

{ set to cc char i in row data R }
Procedure CrtSetChar( Var R : TCrtRow; i : TCrtCoord; cc : TChar);
Begin
  R.Row[i] := cc
End;

{ assign char i with char j of row R }
Procedure CrtAssignChar( Var R : TCrtRow; i,j : TCrtCoord );
Begin
  R.Row[i] := R.Row[j]
End;

{ insert char cc before position i on row R }
Procedure CrtInsertChar( Var R : TCrtRow; i : TCrtCoord; cc : TChar );
Var
  j : Byte; { loop index to move chars in the row }
Begin
  With R Do
  Begin
    { update length }
    Len := Len + 1;
    LenBytes := LenBytes + CrtCharWrapSize(cc);
    { move right all chars from char i, if any, making room for the new char }
    For j := Len - 1 DownTo i Do
      CrtAssignChar(R,j + 1,j);
    { save the new char at insertion point }
    CrtSetChar(R,i,cc)
  End
End;

{ delete char cc at position i on row R }
Procedure CrtDeleteChar( Var R : TCrtRow; i : TCrtCoord );
Var
  cc : TChar; { char being deleted }
  j : Byte; { loop index to move chars in the row }
Begin
  With R Do
  Begin
    CrtChar(R,i,cc);
    { move left all chars from char i, overwriting it }
    For j := Len - 1  DownTo i Do
      CrtAssignChar(R,j,j + 1);
    { update length }
    Len := Len - 1;
    LenBytes := LenBytes - CrtCharWrapSize(cc)
  End
End;

{----------------------------------------------------------------------------}
{ displaying an array of chars R                                             }
{----------------------------------------------------------------------------}

{ procedures below work on a row R, without changing it, but may change the 
 position of the current pointer (CP) on screen; it is assumed that none of
 there procedures trigger a wrap (change of line) }

{ display char cc; increase WhereX by one }
Procedure CrtDisplayChar( cc : TChar );
Begin
  Write(cc.Bytes)
End;

{ replay (display) row R, from char i, at current screen row; restore WhereX if
 RestoreX is True }
Procedure CrtDisplayChars( R : TCrtRow; i : TCrtCoord; RestoreX : Boolean );
Var
  X : TCrtCoord; { WhereX backup }
  j : TCrtCoord;
  cc : TChar;
Begin
  X := WhereX;
  ClrEol;
  For j := i to R.Len Do
  Begin
    CrtChar(R,j,cc);
    CrtDisplayChar(cc)
  End;
  If RestoreX Then
    GotoXY(X,WhereY)
End;

{ replay (display) a full row R at screen row number Y; changes WhereX and 
 WhereY }
Procedure CrtReplay( R : TCrtRow; y : TCrtCoord );
Begin
  GotoXY(1,y);
  CrtDisplayChars(R,1,False) { CP at the end }
End;

{----------------------------------------------------------------------------}
{                                                                            }
{  Main procedures                                                           }
{                                                                            }
{  Procedures below work on CrtRow, maintaining data in synch with what is   }
{  displayed on screen                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{----------------------------------------------------------------------------}
{ initializations                                                            }
{----------------------------------------------------------------------------}

{ reset the memorized line }
Procedure CrtResetLine;
Begin
  With CrtRow Do
  Begin
    Len := 0;
    LenBytes := 0
  End;
End;

{----------------------------------------------------------------------------}
{ wrap?                                                                      }
{----------------------------------------------------------------------------}

{ return true if cc fits into the current screen row; we do not use 
 the last column, as displaying a character here will trigger a line
 break }
Function CrtFits( cc : TChar ) : Boolean;
Begin
  CrtFits := CrtRow.LenBytes + CrtCharWrapSize(cc) < CrtScreenWidth
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

{ delete one char at the cursor }
Procedure CrtBackspace;
Begin
  CheckCondition(WhereX > 1,'CrtBackspace: no chars to delete');
  { update data structure }
  CrtDeleteChar(CrtRow,WhereX);
  { update display, w/o changing WhereX  }
  Write(#08); { backspace }
  CrtDisplayChars(CrtRow,WhereX + 1,True)
End;

{ insert a char on screen, at the insertion point WhereX, assuming there is 
 enough room for the new character to be displayed without Crt creating a line 
 break }
Procedure CrtWriteCharThatFits( cc : TChar );
Begin
  { display the char; full display is completed below }
  CrtDisplayChar(cc);
  { we must exit in case of error, as the error module uses Crt to display 
   messages about bugs; if the bug is about Crt, this could create an 
   infinite loop }
  If Error Then Exit;
  CheckCondition(CrtFits(cc),'Character does not fit into the screen');
  { update data }
  CrtInsertChar(CrtRow,WhereX,cc);
  { complete screen update, w/o changing WhereX }
  CrtDisplayChars(CrtRow,WhereX,True)
End;

{ write a char on screen, breaking the line when the screen is full or when
 the char itself if NewLine; remember that the input subsystem replaces CR, LF, 
 and CRLF with (NewLine) (LF), so CR should only appear when we add it 
 ourselves, which we avoid doing }
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

{ move cursor left by one char }
Procedure CrtMoveLeft;
Begin
  If WhereX > 1 Then
    GotoXY(WhereX - 1,WhereY)
End;

{ move cursor right by one char }
Procedure CrtMoveRight;
Begin
  If WhereX < CrtScreenWidth Then
    GotoXY(WhereX + 1,WhereY)
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

{ initialize the unit }
Begin
  CrtScreenWidth := GetScreenWidth;
  CrtScreenHeight := GetScreenHeight;
  CrtResetLine
End.