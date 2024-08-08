{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Readline.pas                                               }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{          K E Y B O A R D   I N P U T   W I T H   H I S T O R Y             }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ Command line editing, with history:
 - edit starts at the X position, after an optional prompt;
   since editing can start at any position X, the string before X must be
   memorized, to be displayed again when the prompt line disappears on top
   of the screen (when a huge line is edited) and then reappears (when
   all the soft lines are deleted with backspaces)   
 - pasting text with several new lines triggers several returns: first
   line is returned from the current call to ReadLnKbd, second line on
   the second call, etc. This requires maintaining a keyboard buffer 
   across calls to ReadLnKbd, as each call might return before all 
   the input chars are processed 
 - #00 is used as soft break mark (an arbitrary choice, 
   provided the mark is not displayable and unlikely to be present in 
   user input)
 }

{
  ASSUMPTIONS AND LIMITS:
  -----------------------
  1) ASCII control characters (#00..#31,#127) are also control 
    characters in the input encoding 
}

Unit Readline;

Interface

Uses
  Num,
  Errs,
  Chars,
  Crt,
  Crt2,
  IChar,
  Buffer;

Procedure ReadLnKbd( Var B : TBuf; Var Encoding : TEncoding );
Function CtrlC : Boolean;

Implementation
{-----------------------------------------------------------------------------}

Const
  SPACES_PER_TAB = 4; { tabs are converted into spaces }
  MaxHist = 10;
  MaxHistPlusOne = 11; { for TP3 compatibility }
  SOFT_BREAK = #00; { value of the soft break mark }
  CTRL_C = #03; { ASCII encoding of Ctrl-C}

Type
  THIndex = 1..MaxHist;
  THCount = 0..MaxHist;
  THCursor = 0..MaxHistPlusOne;
  THistory = Record
    Cur : THCursor; { cursor when the user hit up/down arrow keys }
    Len : THCount; { nb of entries in use }
    Str : Array[THIndex] of TBuf { most recent on top }
  End;

{ pre-defined characters}
Var 
  CC_SOFT_BREAK,CC_BLANK_SPACE,CC_NEW_LINE : TChar;

{----------------------------------------------------------------------------}
{ base keyboard function                                                     }
{----------------------------------------------------------------------------}

{ read a single byte }
Function ReadOneKey : Char;
Var 
  c : Char;
Begin
  c := ReadKey;
  ReadOneKey := c
End;

{ return True if Ctrl-C has been hit; warning: if not a Ctrl-C, the character is
 definitively lost }
Function CtrlC : Boolean;
Begin
  CtrlC := KeyPressed And (ReadOneKey = CTRL_C)
End;

{ read a series of TChars from the keyboard; it is assumed that a  
 TChar made of several bytes is sent quickly, at least faster than a  
 human can type; as a result, no multi-byte characters will be split between 
 different calls to that function; note that when the user paste text at the 
 prompt, this function might return several chars;
 replace CR, LF, and CRLF with NewLine }
Procedure ReadRunOfTChars( Var B : TBuf; Var Encoding : TEncoding );
Var
  s : TCharBytes; { small read buffer }
  cc : TChar; { recognized TChar }
  Done : Boolean;
Begin
  BufInit(B);
  s := '';
  Done := False;
  While Not Done And Not Error Do
  Begin
    { replenish the small buffer with keys typed or pasted }
    While KeyPressed And (Length(s) < MaxBytesPerChar) Do
      s := s + ReadOneKey;
    Done := Length(s) = 0;
    If Not Done Then
      If BufNbFree(B) > 0 Then
        { get one TChar }
        If GetOneTCharNL(s,cc,Encoding) Then
          BufAppendTChar(B,cc)
        Else
          SyntaxError('character not recognized')
      Else
        RuntimeError('Too many characters')
  End
End;


{----------------------------------------------------------------------------}
{ command line history                                                       }
{----------------------------------------------------------------------------}

{ initialize command-line history H }
Procedure ResetHistory( Var H : THistory );
Begin
  H.Len := 0;
  H.Cur := 0
End;

{ insert B as the most recent buffer, deleting buffer with index k }
Procedure InsertAsTopHistory( Var H : THistory; k : THIndex; B : TBuf );
Var 
  j : THIndex;
Begin
  For j := k DownTo 2 Do
    H.Str[j] := H.Str[j-1];
  H.Str[1] := B;
  H.Cur := 0;
End;

{ register a new entry in the history H }
Procedure PushToHistory( Var H : THistory; B : TBuf );
Var 
  Edit : Boolean; { actual editing occurred? }
Begin
  Edit := True;
  If H.Cur > 0 Then 
    Edit := BufDiff(H.Str[H.Cur],B); { history line has been edited? }

  If Edit Then
  Begin
    { insert the edited buffer on top }
    If H.Len < MaxHist Then
      H.Len := H.Len + 1;
    InsertAsTopHistory(H,H.Len,B)
  End
  Else { no edit: move the non edited buffer on top }
    InsertAsTopHistory(H,H.Cur,B)
End;


{----------------------------------------------------------------------------}
{ ReadLn with history                                                        }
{----------------------------------------------------------------------------}

{ history }
Var
  Hist : THistory;
  PromptRow : TCrtRow; { backup of the screen row when readline is called }
  KbdBuf : TBuf; { keyboard buffer to support paste with chars after new line }

{ read one line from the keyboard, that is, a sequence of keys ending
  with an Enter key
 highlights:
 - return on Enter key, even if there are still keys in the keyboard 
   buffer; remaining keys will be processed on further calls; this
   allows for sequential treatment of copy-paste text containing
   several Enter keys;
 - detect encoding if Encoding is still unknown, impose it otherwise;
 - use soft marks to indicate line breaks such that the command
   line nicely fits into the screen width without breaking UTF-8 sequences; 
 assertions: 
 - in the buffer, soft marks are separated with at least one
   genuine char; 
 - the buffer never ends with a soft mark 
 }
Procedure ReadLnKbd( Var B : TBuf; Var Encoding : TEncoding );
Var
  Stop : Boolean;
  Ya : Integer; { current Y-coordinate of the prompt line (can be <= 0) }

  { line feed, maintaining the position Y trackers }
  Procedure DisplaySoftBreak;
  Begin
    If WhereY = CrtScreenHeight Then { last screen line }
      Ya := Ya - 1; { screen is full and will scroll one line up }
    CrtWriteLn
  End;

  { display the buffer from index i, breaking lines at soft marks }
  Procedure DisplayBufferFrom( i : TBufIndex );
  Begin
    While i <> 0 Do
    Begin
      If B.Buf[i].Val.Bytes = SOFT_BREAK Then
        DisplaySoftBreak
      Else
        CrtWriteCharThatFits(B.Buf[i].Val);
      i := NextIdx(B,i)
    End
  End;

  { display the whole buffer  }
  Procedure DisplayBuffer;
  Begin
    DisplayBufferFrom(FirstIdx(B))
  End;

  { clear the command line and display the prompt }
  Procedure ResetCommandLine;
  Begin
    BufInit(B);
    { make sur the prompt will be visible }
    Ya := Max(Ya,1);
    { clear all the visible edited rows }
    CrtClrLines(Ya,WhereY);
    { redraw the prompt }
    CrtReplay(PromptRow,Ya)
  End;

  { display at the cursor position the last (possibly soft) line  
   in the buffer; do not modify the content of the buffer itself }
  Procedure DisplayLastLine;
  Var 
    i : TBufIndex;
    Found : Boolean; { soft mark found }
  Begin
    i := LastIdx(B);

    { empty buffer: nothing to display }
    If i = 0 Then
      Exit;

    { locate the beginning of the last line in the buffer }
    Found := False;
    While Not Found And (i <> 0) Do
    Begin
      If B.Buf[i].Val.Bytes = SOFT_BREAK Then
        Found := True
      Else
        i := PrevIdx(B,i)
    End;
    If Found Then
      i := NextIdx(B,i)
    Else
      i := FirstIdx(B);

    { display all the chars up to the end of the buffer; assertions: 
     all the chars are displayable; the total number of bytes
     written is not larger than CrtScreenWidth }
    DisplayBufferFrom(i)
  End;

  { backspace action }
  Procedure Backspace;
  Var
    e1,e2 : TIChar;
  Begin

    { empty command line: emit a beep }
    If BufLen(B) = 0 Then
    Begin
      CrtBeep;
      Exit
    End;

    { remove the last char in the buffer }
    BufPop(e1,B);
    CheckCondition(e1.Val.Bytes <> SOFT_BREAK,'Backspace: popping a SOFT_BREAK');

    { deleted char was the only char left in the buffer }
    If BufLen(B) = 0 Then
    Begin
      CrtBackspace;
      Exit
    End;

    { get the char just before the one we just deleted }
    BufGetLast(e2,B);

    { simple case: more than one char on the current line }
    If e2.Val.Bytes <> SOFT_BREAK Then
    Begin
      CrtBackspace;
      Exit
    End;

    { backspacing on a single char on a soft line: }

    { delete the soft mark }
    BufPop(e2,B);
    { scroll down one line if the previous line is hidden }
    If WhereY = 1 Then
      Ya := Ya + 1;
    { clear the current line and (when possible) the line just above }
    CrtClrLines(Max(WhereY-1,1),Min(WhereY,2));

    { redraw the new edit line }
    If WhereY = Ya Then 
      CrtReplay(PromptRow,Ya);
    DisplayLastLine
  End;

  { append a char to the buffer, making sure at least n characters 
   are left free in the buffer; return True if the char has been
   appended; characters (user input) that do not fit into the buffer 
   are silently discarded }
  Function AppendChToBuf( cc : TChar; n : TBufIndex ) : Boolean;
  Var
    Can : Boolean;
  Begin
    Can := BufNbFree(B) > n;
    If Can Then
      BufAppendTChar(B,cc);
    AppendChToBuf := Can
  End;

  { append a char to the buffer, inserting extra soft breaks to avoid 
   breaking multi-byte sequences; make sure the buffer has one spot
   left for the extra NewLine appended by Enter }
  Procedure AppendChar( cc : TChar );
  Begin
    If Not CrtFits(cc) Then
    Begin
      If AppendChToBuf(CC_SOFT_BREAK,2) Then
        DisplaySoftBreak
      Else
        Exit
    End;
    If AppendChToBuf(cc,1) Then
      CrtWriteCharThatFits(cc)
  End;

  { convert tab into spaces }
  Procedure Tab;
  Var
    t : 1..SPACES_PER_TAB;
  Begin
    For t := 1 to SPACES_PER_TAB Do
      AppendChar(CC_BLANK_SPACE)
  End;

  { recall an older line if any; do nothing otherwise }
  Procedure UpArrow;
  Begin
    If Hist.Cur < Hist.Len Then
    Begin
      ResetCommandLine;
      Hist.Cur := Hist.Cur + 1;
      B := Hist.Str[Hist.Cur];
      DisplayBuffer
    End
  End;

  { recall a more recent line if any; reset the prompt otherwise }
  Procedure DownArrow;
  Begin
    ResetCommandLine;
    If Hist.Cur > 0 Then
      Hist.Cur := Hist.Cur - 1;
    If Hist.Cur > 0 Then
    Begin
      B := Hist.Str[Hist.Cur];
      DisplayBuffer
    End
  End;

  { Enter key has been hit (or came in through a paste operation) }
  Procedure Enter;
  Var
    Ok : Boolean;
  Begin
    If BufLen(B) > 0 Then
      PushToHistory(Hist, B);
    { visual feedback }
    DisplaySoftBreak;
    { remove all the soft marks }
    BufFilterOut(B,CC_SOFT_BREAK);
    { NewLine must be part of the input, so that:
      "> in_char(c);<NewLine>" sets c to NewLine; see PII+ R 5-4 }
    Ok := AppendChToBuf(CC_NEW_LINE,0);
    CheckCondition(Ok,'Enter: Cannot add new line');
    { since the user validated the input, we output the buffer to the echo 
     file }
    BufToEchoFile(B)
  End;

  { process the input buffer R, executing actions }
  Procedure ProcessInputBuffer( Var R : TBuf; Var Stop : Boolean );
  Var
    cc,cc2 : TChar;
    e,e2 : TIChar;
  Begin
    While (BufNbUnread(R) > 0) And Not Stop And Not Error Do
    Begin
      BufRead(e,R);
      cc := e.Val;
      If cc.Bytes = Newline Then { Enter }
      Begin
        Enter;
        Stop := True
      End
      Else If cc.Bytes = CTRL_C Then { Ctrl-C }
      Begin
        Stop := True;
        UserInterrupt
      End
      Else If cc.Bytes = #08 Then { Backspace }
        Backspace
      Else If cc.Bytes = #09 Then { tab }
        Tab
      Else If (cc.Bytes =  #00) 
          And (BufNbUnread(R) > 0) Then { extended or func. key }
      Begin { command history }
        BufGetRead(e2,R,1);
        cc2 := e2.Val;
        If cc2.Bytes = #72 Then
        Begin
          BufRead(e2,R);
          UpArrow
        End
        Else If cc2.Bytes = #80 Then
        Begin
          BufRead(e2,R);
          DownArrow
        End
      End;
      { filter out ASCII control chars }
      If Not IsIn(cc,[#00..#31,#127]) Then
        AppendChar(cc)
    End
  End;

Begin
  PromptRow := CrtRow; { backup the current line, which serves as a prompt }
  BufInit(B);
  Ya := WhereY;
  Stop := False;
  { process any user input left from the previous call }
  If BufNbUnread(KbdBuf) > 0 Then
    ProcessInputBuffer(KbdBuf,Stop);
  While Not Stop And Not Error Do
  Begin
    If KeyPressed Then
    Begin
      BufInit(KbdBuf);
      ReadRunOfTChars(KbdBuf,Encoding);
      ProcessInputBuffer(KbdBuf,Stop)
    End
  End
End;

{ initialize the command-line history }
Begin
  ASCIIChar(CC_SOFT_BREAK,SOFT_BREAK);
  ASCIIChar(CC_BLANK_SPACE,' ');
  ASCIIChar(CC_NEW_LINE,NewLine);
  BufInit(KbdBuf);
  ResetHistory(Hist)
End.