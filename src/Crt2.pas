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

Const 
  CrtScreenMaxWidth = 255;

{ current row }
Type
  TCrtRowData = Array[1..CrtScreenMaxWidth] Of TChar;
  TCrtRow = Record
    Len : Byte; { number of chars in the row }
    Bytes : Integer; { total number of bytes in the current screen row }
    Row : TCrtRowData
  End;

Var
  CrtRow : TCrtRow;
  CrtScreenWidth : TCrtCoord;
  CrtScreenHeight : TCrtCoord;

{----------------------------------------------------------------------------}
{ initializations                                                            }
{----------------------------------------------------------------------------}

{ reset the memorized line }
Procedure CrtResetLine;
Begin
  With CrtRow Do
  Begin
    Len := 0;
    Bytes := 0
  End;
End;

{ initialize the unit }
Procedure InitCrt;
Begin
  CrtScreenWidth := GetScreenWidth;
  CrtScreenHeight := GetScreenHeight;
  CrtResetLine
End;

{----------------------------------------------------------------------------}
{ get / set                                                                  }
{----------------------------------------------------------------------------}

{ return true if cc fits into the current screen row; we do not use 
 the last column, as displaying a character here will trigger a line
 break }
Function CrtFits( cc : TChar ) : Boolean;
Begin
  CrtFits := CrtRow.Bytes + Length(cc) < CrtScreenWidth
End;

{ get char i in row data R }
Function CrtChar( R : TCrtRowData; i : TCrtCoord ) : TChar;
Begin
  CrtChar := R[i]
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
  Write(cc);
  { we must exit in case of error, as the error module uses Crt to display 
   messages about bugs; if the bug is about Crt, this could create an 
   infinite loop }
  If Error Then Exit;
  CheckCondition(CrtFits(cc),'Character does not fit into the screen');
  With CrtRow Do
  Begin
    Len := Len + 1;
    CrtSetChar(Row,Len,cc);
    Bytes := Bytes + Length(cc)
  End
End;

{ write a char on screen, breaking the line when the screen is full or when
 the char itself if CRLF; 
 FIXME: StrPtr must use TChar instead of 1-byte chars }
Procedure CrtWriteChar( cc : TChar );
Begin
  If cc = #10 Then { LF part of a CRLF sequence: do nothing }
    Exit;
  If (cc = #13) Or (Not CrtFits(cc)) Then
    CrtWriteLn;
  CrtWriteCharThatFits(cc)
End;

{ write a string of 1-byte characters on screen; the string char is assumed 
 to fit into the screen row }
Procedure CrtWriteString( s : TString );
Var
  i : TStringSize;
Begin
  For i := 1 to Length(s) Do
    CrtWriteChar(s[i])
End;

{ backspace one char at the cursor }
Procedure CrtBackspace;
Begin
  CheckCondition(CrtRow.Len > 1,'CrtBackspace: no chars to delete');
  Write(#08); { backspace }
  ClrEol;
  With CrtRow Do
  Begin
    Bytes := Bytes - Length(CrtChar(Row,Len));
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
Begin
  GotoXY(1,y);
  CrtClrLine;
  With R Do
    For i := 1 to Len Do
      CrtWriteCharThatFits(CrtChar(Row,i))
End;
