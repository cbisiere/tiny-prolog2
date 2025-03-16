{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : CLI.pas                                                    }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022-2025                                                  }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                C O M M A N D   L I N E   I N T E R F A C E                 }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }

{ Command line editing, with command history:
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
 }

{
  ASSUMPTIONS AND LIMITS:
  -----------------------
  1) ASCII control characters (#00..#31,#127) are also control 
    characters in the input encoding 
}

Unit CLI;

Interface

Uses
  Common,
  ShortStr,
  Num,
  Errs,
  Chars,
  Trace,
  Crt,
  Crt2,
  IChar,
  Buffer,
  CEdit;

Procedure CLISetPrompt( Prompt : TPrompt );
Procedure ReadLnKbd( Var B : TBuf; Var Encoding : TEncoding );
Function CtrlC : Boolean;

Implementation
{-----------------------------------------------------------------------------}

Const
  SPACES_PER_TAB = 4; { tabs are converted into spaces }
  MaxHist = 10;
  MaxHistPlusOne = 11; { for TP3 compatibility }
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

Var 
  CC_BLANK_SPACE,CC_NEW_LINE : TChar; { pre-defined composite characters}
  CLIPrompt : TPrompt; { prompt, set by main }


{----------------------------------------------------------------------------}
{ prompt                                                                     }
{----------------------------------------------------------------------------}

Procedure CLISetPrompt( Prompt : TPrompt );
Begin
  CLIPrompt := Prompt
End;

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
          BufPushChar(B,cc)
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

{ trace history }
Procedure HistoryDump( H : THistory );
Var 
  i : THIndex;
Begin
  WritelnToTraceFile('| HIST: ');
  With H Do
  Begin
    If Len > 0 Then
      For i := 1 To Len Do
      Begin
        WriteToTraceFile('| HIST '+IntToShortString(i)+': ');
        BufDump(H.Str[i])
      End
  End
End;

{----------------------------------------------------------------------------}
{ ReadLn with history                                                        }
{----------------------------------------------------------------------------}

{ history }
Var
  Hist : THistory;
  KbdBuf : TBuf; { keyboard buffer to support paste with chars after new line }

{ read one line from the keyboard, that is, a sequence of keys ending
  with an Enter key }

{ 
  NOTES:
  ------
 - edition starts at screen row WhereY, which is expected to be an empty ine 
 - return on Enter key, even if there are still keys in the keyboard; 
   buffer; remaining keys will be processed on further calls; this
   allows for sequential treatment of copy-paste text containing
   several Enter keys;
 - detect encoding if Encoding is still unknown, impose it otherwise;
 - B must not contain any NewLine
 }
Procedure ReadLnKbd( Var B : TBuf; Var Encoding : TEncoding );
Var
  Ed : TEditor; { the CL editor used to edit buffer B }
  Stop : Boolean;

  { set the command line buffer to B and show it }
  Procedure SetCommandLine;
  Begin
    CEditSet(Ed,B)
  End;

  { recall an older line if any; do nothing otherwise }
  Procedure UpArrow;
  Begin
    If Hist.Cur < Hist.Len Then
    Begin
      Hist.Cur := Hist.Cur + 1;
      B := Hist.Str[Hist.Cur];
      SetCommandLine
    End;
  End;

  { recall a more recent line if any; reset the prompt otherwise }
  Procedure DownArrow;
  Begin
    If Hist.Cur > 0 Then
      Hist.Cur := Hist.Cur - 1;
    If Hist.Cur > 0 Then
      B := Hist.Str[Hist.Cur]
    Else
      BufInit(B);
    SetCommandLine
  End;

  { Enter key has been hit (or came in through a paste operation); even if the
   edit cursor is not at the end of the command line, the whole command line
   is submitted for execution, with a <NewLine> appended at the end;
   TODO: check a paste (including a NL) in the middle of the edited command 
   line }
  Procedure EnterKey;
  Begin
    If BufLen(Ed.Buf) > 0 Then
      PushToHistory(Hist,Ed.Buf);
    { prepare the command line to submit: NewLine must be part of the input, 
     so that: "> in_char(c);<NewLine>" sets c to NewLine; see PII+ R 5-4 }
    B := Ed.Buf;
    BufPushChar(B,CC_NEW_LINE);
    { since the command line is submitted, we can now output the whole  
     command line to the mirror files }
    CEditWritePromptToMirrorFiles(Ed);
    BufToMirrorFiles(B);
    { visual feedback: new line }
    CrtWriteln
  End;

  { convert tab into spaces }
  Procedure Tab;
  Var
    t : 1..SPACES_PER_TAB;
  Begin
    For t := 1 to SPACES_PER_TAB Do
      CEditInsert(Ed,CC_BLANK_SPACE)
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
      { command line buffer full: force a submit }
      If BufNbFree(B) = 1 Then { the free char is used for the inserted NL }
        cc := CC_NEW_LINE;
      { process the char}
      If cc.Bytes = Newline Then { Enter }
      Begin
        EnterKey;
        Stop := True
      End
      Else If cc.Bytes = CTRL_C Then { Ctrl-C }
      Begin
        Stop := True;
        UserInterrupt
      End
      Else If cc.Bytes = #08 Then { Backspace }
        CEditDelete(Ed)
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
        Else If cc2.Bytes = #75 Then { left arrow }
        Begin
          BufRead(e2,R);
          CEditBackward(Ed)
        End
        Else If cc2.Bytes = #77 Then { right arrow }
        Begin
          BufRead(e2,R);
          CEditForward(Ed)
        End
      End;
      { insert if there are at least 2 free places (recomputing soft breaks may
       consume an extra place) and not a control char }
      If (BufNbFree(B) > 1) And (Not IsIn(cc,[#00..#31,#127])) Then
        CEditInsert(Ed,cc)
    End
  End;

Begin
  { setup and start the editor }
  CEditInit(Ed,CLIPrompt,WhereY);
  SetCommandLine;
  { edit, starting with leftovers from previous paste, if any }
  Stop := False;
  { process any user input left from the previous call }
  If BufNbUnread(KbdBuf) > 0 Then
    ProcessInputBuffer(KbdBuf,Stop);
  { command line editing }
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

{ initialize }
Begin
  ASCIIChar(CC_BLANK_SPACE,' ');
  ASCIIChar(CC_NEW_LINE,NewLine);
  BufInit(KbdBuf);
  ResetHistory(Hist);
  CLISetPrompt('')
End.