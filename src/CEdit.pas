{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : CEdit.pas                                                  }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022-2025                                                  }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                  C O M M A N D   L I N E   E D I T O R                     }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ Edit a single command }

{ 
  FEATURES:
  ---------
 - handle an uneditable prompt part at the beginning of the line, e.g. "> "
 - the command may span several lines, possibly even more than the screen can
   display
 - top or bottom lines may be hidden to ensure that the visual cursor is 
   always visible
 - handle multibyte chars; when a screen line y contains such chars, GotoXY on
   that line becomes unusable except for GotoXY(x,1); in that case, to 
   position the cursor on column x, the beginning of the line is rewritten up
   to char number x
 }

{
  ASSUMPTIONS AND LIMITS:
  -----------------------
 - prompt contains a maximum of 3, simple graphical ASCII chars 
}

Unit CEdit;

Interface

Uses
  Common,
  ShortStr,
  Num,
  Errs,
  Chars,
  Trace,
  Mirror,
  Crt,
  Crt2,
  IChar,
  Buffer;

Const
  TRACE_CEDIT = False;

{ prompt: short 1-byte char string }
Const
  MaxPromptLength = 3;
Type
  TPrompt = String[MaxPromptLength];

{ layout of a CL on screen; may takes several lines due to wrapping; NBlines = 0
 means the layout is undefined }
Type
  TVirtCoordY = Integer; { virtual Y coordinate (can be <= 0) }
  TLayout = Record
    ScreenY : TVirtCoordY; { Y-coordinate of the first line }
    NbLines : PosInt { number of screen line it would take to display it fully }
  End;
  TVirtCoordYDelta = Integer; { change in virtual screen coordinate }

{ state of the command line editor }
Type
  TEditor = Record
    { current screen layout; must be kept in sync with buffer Buf }
    Layout : TLayout;
    { previous layout; used to compute and clear dirty screen areas }
    PrevLayout : TLayout;
    { active line }
    IdxA : TBufIndex; { active line starts after this char; 0 if first line }
    NbBytes : PosInt; { number of bytes in the active line, up to char IdxW } 
    { buffer }
    NbMulti : PosInt; { number of multibyte chars in the buffer; UNUSED }
    Buf : TBuf; { the buffer }
    { prompt }
    NbBytesInPrompt : 0..MaxPromptLength;
    Prompt : TPrompt;
  End;

Procedure CEditInit( Var Ed : TEditor; s : TPrompt; y : TVirtCoordY );
Procedure CEditSetBuffer( Var Ed : TEditor; B : TBuf );

Procedure CEditWritePromptToMirrorFiles( Ed : TEditor );

Procedure CEditSet( Var Ed : TEditor; B : TBuf  );

Procedure CEditForward( Var Ed : TEditor );
Procedure CEditBackward( Var Ed : TEditor );
Procedure CEditInsert( Var Ed : TEditor; cc : TChar );
Procedure CEditDelete( Var Ed : TEditor );

Procedure CEditDump( Ed : TEditor );

Implementation
{-----------------------------------------------------------------------------}

{ trace an edit op }
Procedure CEditTrace( Ed : TEditor; s : TString );
Begin
  If TRACE_CEDIT Then
  Begin
    WritelnToTraceFile('CEdit: ' + s);
    CEditDump(Ed);
    CrtDump
  End
End;

{----------------------------------------------------------------------------}
{ prompt related procedures                                                  }
{----------------------------------------------------------------------------}

{ set the active line to the prompt line, counting the prompt size in the byte 
 count }
Procedure CEditResetActive( Var Ed : TEditor );
Begin
  With Ed Do
  Begin
    IdxA := 0;
    NbBytes := NbBytesInPrompt
  End
End;

{ true if the active line is the prompt line }
Function CEditActiveIsPrompt( Ed : TEditor ) : Boolean;
Begin
  CEditActiveIsPrompt := Ed.IdxA = 0
End;

{ write the prompt on screen, starting at visual cursor }
Procedure CEditWritePrompt( Ed : TEditor );
Var
  i : 1..MaxPromptLength;
  cc : TChar;
Begin
  With Ed Do
    For i := 1 to Length(Prompt) Do
    Begin
      ASCIIChar(cc,Prompt[i]);
      CrtWriteChar(cc)
    End
End;

{ write the prompt to the mirror (trace, echo) files }
Procedure CEditWritePromptToMirrorFiles( Ed : TEditor );
Begin
  WriteToMirrorFiles(Ed.Prompt)
End;


{----------------------------------------------------------------------------}
{ init                                                                       }
{----------------------------------------------------------------------------}

{ initialize a command line editor, with prompt s, located at screen row y }
Procedure CEditInit( Var Ed : TEditor; s : TPrompt; y : TVirtCoordY );
Begin
  With Ed Do
  Begin
    With PrevLayout Do { previous layout is undefined }
    Begin
      NbLines := 0
    End;
    With Layout Do
    Begin
      ScreenY := y;
      NbLines := 1
    End;
    Prompt := s;
    NbBytesInPrompt := Length(Prompt);
    NbMulti := 0; { assert: no multibyte char in prompt }
    BufInit(Buf)
  End;
  CEditResetActive(Ed)
End;


{----------------------------------------------------------------------------}
{ layout calculations                                                        }
{----------------------------------------------------------------------------}

{ are two layouts identical? }
Function SameLayout( l1,l2 : TLayout ) : Boolean;
Begin
  SameLayout := (l1.ScreenY = l2.ScreenY) And (l1.NbLines = l2.NbLines)
End;

{ return the last line of layout l }
Function LayoutLastLine( l : TLayout ) : TVirtCoordY;
Begin
  With l Do
    LayoutLastLine := ScreenY + NbLines - 1
End;

{ clip a layout to the screen }
Procedure ClipLayoutToScreen( Var l : TLayout );
Begin
  With l Do
  Begin
    NbLines := NbLines 
        - Max(0,1-ScreenY) 
        - Max(0,LayoutLastLine(l)-CrtScreenHeight);
    ScreenY := Max(1,ScreenY)
  End
End;

{ calculate the number of lines (virtually) displayed on screen }
Procedure CEditComputeNumberOfLines( Var Ed : TEditor );
Var 
  n : PosInt;
  b : PosInt;
  i : TBufIndex;
  cc : TChar;
Begin
  With Ed Do
  Begin
    n := 1;
    b := NbBytesInPrompt;
    i := 0;
    While (i <> Buf.IdxE) Do
    Begin
      i := NextIdx(Buf,i);
      BufGetCharAt(cc,Buf,i);
      { wrap before this char? }
      If CrtWraps(b,cc) Then
      Begin
        n := n + 1;
        b := 0
      End;
      { count bytes }
      b := b + CrtCharWrapSize(cc)
    End;
    Layout.NbLines := n
  End
End;


{----------------------------------------------------------------------------}
{ CEditState: primitives to update the editor's state (no display)           }
{----------------------------------------------------------------------------}

{ procedures below update: the buffer, IdxW, and NbBytes }

{ recompute IdxA and NbBytes, based on the buffer's write index IdxW;
 IdxA must point to the char before the first char of the active line (that is,
 the line containing IdxW), or must be 0 if the active line is the first one);
 calculate n, the line number of the active line (e.g. n=1 means the first line 
 of the command line is the active one) }
Procedure CEditStateComputeActiveLine( Var Ed : TEditor; Var n : PosInt );
Var
  i : TBufIndex;
  cc : TChar;
Begin
  n := 1;
  CEditResetActive(Ed);
  With Ed Do
  Begin
    { loop from start to IdxW }
    i := 0;
    While (i <> Buf.IdxW) Do
    Begin
      i := NextIdx(Buf,i);
      { count bytes of char i }
      BufGetCharAt(cc,Buf,i);
      NbBytes := NbBytes + CrtCharWrapSize(cc);
      { if we are not done yet, detect wrap by looking at the next char }
      If i <> Buf.IdxW Then
      Begin
        BufGetChar(cc,Buf,i,1);
        If CrtWraps(NbBytes,cc) Then
        Begin
          n := n + 1;
          IdxA := i; { wrap is after char i }
          NbBytes := 0
        End
      End
    End
  End
End;

{ Calculate and return the end of the active line }
Function CEditStateFindEndOfActiveLine( Ed : TEditor ) : TBufIndex;
Var 
  b : PosInt;
  i : TBufIndex;
  cc : TChar;
  Wrap : Boolean;
Begin
  With Ed Do
  Begin
    { start at IdxW, since we already know the number of bytes up to it }
    b := NbBytes;
    i := Buf.IdxW;
    { detect the next wrap }
    Wrap := False;
    While (i <> Buf.IdxE) And Not Wrap Do
    Begin
      { look ahead one char to test for wrapping after char i }
      BufGetChar(cc,Buf,i,1);
      Wrap := CrtWraps(b,cc);
      If Not Wrap Then
      Begin
        b := b + CrtCharWrapSize(cc);
        i := NextIdx(Buf,i)
      End
    End;
    CEditStateFindEndOfActiveLine := i
  End
End;

{ update the active line when the write cursor has just moved forward; 
 set Updated to true if the active line is updated (that is, moved downward) }
Procedure CEditStateUpdateActiveAfterForward( Var Ed : TEditor; 
    Var Updated : Boolean );
Var
  cc : TChar;
Begin
  With Ed Do
  Begin
    BufGetCharAt(cc,Buf,Buf.IdxW);
    { in case of wrap, meaning the char at IdxW belongs to the next line,
     the active line becomes the next line }
    Updated := CrtWraps(NbBytes,cc);
    If Updated Then
    Begin
      IdxA := PrevIdx(Buf,Buf.IdxW);
      NbBytes := 0
    End;
    { update number of bytes in the active line, from start to IdxW }
    NbBytes := NbBytes + CrtCharWrapSize(cc)
  End
End;

{ update the active line after the write cursor moved backward by one char cc; 
 set Updated to true if the active line is updated (that is, moved upward); 
 this updates IdxA and NBytes, but do not update IdxW (the caller must do it) }
Procedure CEditStateUpdateActiveAfterBackward( Var Ed : TEditor; cc : TChar;
    Var Updated : Boolean );
Var
  n : PosInt;
Begin
  With Ed Do
  Begin
    { update the number of bytes in the active line }
    NbBytes := NbBytes - CrtCharWrapSize(cc);
    { move up? }
    Updated := NbBytes = 0;
    If Updated Then
      CEditStateComputeActiveLine(Ed,n)
  End
End;

{ insert char cc after write index; set ActiveDown to true if this moves the 
 active line to the next line; this happens when cc does not fit into the 
 active line }
Procedure CEditStateInsert( Var Ed : TEditor; cc : TChar; 
    Var ActiveDown : Boolean );
Begin
  { update the multibyte char counter }
  If IsMultibyte(cc) Then
    Ed.NbMulti := Ed.NbMulti + 1;
  { insert the char, updating the write index }
  BufInsert(Ed.Buf,cc);
  { update the active line }
  CEditStateUpdateActiveAfterForward(Ed,ActiveDown);
  { recompute the number of lines; we optimizing for append op, that is, 
   large paste at the prompt that would otherwise be way too slow }
  If BufWriteCursorIsAtEnd(Ed.Buf) Then
  Begin 
    If ActiveDown Then
      With Ed.Layout Do 
        NbLines := NbLines + 1
  End
  Else
    CEditComputeNumberOfLines(Ed)
End;

{ delete char at write index; set ActiveUp to true if this moves the active line 
 to the previous line; this happens when the deleted char is the first char on 
 the active line; this procedure assumes that the char at write index can be 
 deleted, that is, it is not zero }
Procedure CEditStateDelete( Var Ed : TEditor; Var ActiveUp : Boolean );
Var 
  cc : TChar;
Begin
  With Ed Do
  Begin
    { save the char up for deletion }
    BufGetCharAt(cc,Buf,Buf.IdxW);
    { update the multibyte char counter }
    If IsMultibyte(cc) Then
      NbMulti := NbMulti - 1;
    { delete the char, updating the write index }
    BufDelete(Buf);
    { update the active line }
    CEditStateUpdateActiveAfterBackward(Ed,cc,ActiveUp)
  End;
  { recompute the number of lines; we optimizing for delete op at the prompt }
  If BufWriteCursorIsAtEnd(Ed.Buf) Then
  Begin 
    If ActiveUp Then
      With Ed.Layout Do 
        NbLines := NbLines - 1
  End
  Else
    CEditComputeNumberOfLines(Ed)
End;

{ backup the layout; to be called before updating the buffer (replace, insert,
 delete) }
Procedure CEditBackupLayout( Var Ed : TEditor );
Begin
  Ed.PrevLayout := Ed.Layout
End;

{ set the buffer of a command line editor, updating its state: the active 
 line (IdxA), the number of multibyte chars (NbMulti), and its screen 
 position (ScreenY) to show all the lines of the command line while ensuring 
 the active line is located inside the screen }
Procedure CEditSetBuffer( Var Ed : TEditor; B : TBuf );
Var
  n : PosInt;
Begin
  { set buffer }
  Ed.Buf := B;
  { sync state }
  Ed.NbMulti := BufCountMultibyteChars(Ed.Buf);
  CEditStateComputeActiveLine(Ed,n);
  CEditComputeNumberOfLines(Ed);
  { compute new Y position; do not scroll unless necessary, that is, show more
    lines and show the active line }
  With Ed.Layout Do
  Begin
    { compute screen position so the active line is on screen }
    { 1. move down, prompt at top }
    ScreenY := Max(1,ScreenY); 
    { 2. move up, last line at bottom }
    ScreenY := ScreenY - Max(0,LayoutLastLine(Ed.Layout)-CrtScreenHeight);
    { 3. move down, ensuring that the active line is visible }
    ScreenY := ScreenY - Max(0,1-(ScreenY+n-1))
  End
End;

{----------------------------------------------------------------------------}
{ CEditRefresh: primitives for displaying or refreshing the command line     }
{----------------------------------------------------------------------------}

{ is row y, which may be negative or nul, within screen range? }
Function RowIsVisible( y : TVirtCoordY ) : Boolean;
Begin
  RowIsVisible := (y >= 1) And (y <= CrtScreenHeight)
End;

{ scroll down by n lines, that is, move the screen contents up by n lines,
 leaving n blank lines at the bottom; 
 - does not use DelLine, which does not work well in a terminal
 - upon returning, the virtual cursor will be at the beginning of the last 
   screen line; caller must take care of moving it to the right location }
Procedure CEditScrollDown( n : PosInt );
Var
  i : PosInt;
Begin
  If WhereY <> CrtScreenHeight Then { minimize cursor movements }
    CrtGotoLine(CrtScreenHeight);
  For i := 1 to n Do
    CrtWriteln
End;

{ erase lines that must be cleared following a change in layout; we only 
 erase the part of the previous command left after the new command, because
 we scroll up in a single case, in which ScreenY is set to 1 }
Procedure CEditErase( Ed : TEditor );
Var
  l : TLayout; { zone to erase }
Begin
  { compute virtual layout to erase }
  With Ed Do
  Begin
    { same layout: nothing to clear}
    If SameLayout(Layout,PrevLayout) Then 
      Exit;
    { no intersection: clear whole previous layout}
    If (Layout.ScreenY > LayoutLastLine(PrevLayout)) Or 
        (PrevLayout.ScreenY > LayoutLastLine(Layout)) Then
      l := PrevLayout
    Else { layouts intersect but are different }
    Begin
      l.ScreenY := Layout.ScreenY + Layout.NbLines;
      l.NbLines := Max(0,LayoutLastLine(PrevLayout) - l.ScreenY + 1)
    End
  End;
  { then clip to screen and erase }
  ClipLayoutToScreen(l);
  With l Do
    If NbLines > 0 Then
      CrtClrLines(ScreenY,NbLines)
End;


{ Where to start displaying? }
Type 
  TStart = (FromActive,FromWrite);

{ display part of the command line, starting on the active line (supposed to
 be in WhereY), after a char defined by Start: either IdxA (meaning from
 the start of the active line) or the write index IdxW (meaning from the visual 
 cursor); from that char, the procedure display all the chars up to the end of 
 the command line, wrapping long lines, clipping to the bottom of the screen; 
 do not call this procedure with FromActive when the active line is the first
 line, as in that case the whole command must be displayed, including the 
 prompt; this procedure assumes that the visual cursor is properly positioned,
 that is, at the beginning of the active line (FromActive), or right after the 
 col of char IdxW (FromWrite) }
Procedure CEditDisplayFrom( Ed : TEditor; Start : TStart );
Var 
  b : PosInt;
  i : TBufIndex;
  cc : TChar;
Begin
  With Ed Do
  Begin
    { initialize; set i to the index of the char before the first to display }
    Case Start Of
    FromActive:
      Begin
        b := 0;
        i := IdxA
      End;
    FromWrite:
      Begin
        b := NbBytes;
        i := Buf.IdxW
      End
    End;
    { display }
    While (i <> Buf.IdxE) Do
    Begin
      i := NextIdx(Buf,i);
      BufGetCharAt(cc,Buf,i);
      If CrtWraps(b,cc) Then { must wrap before displaying cc }
      Begin
        CrtClrEol; { remove spurious chars at the end of the current line }
        If WhereY = CrtScreenHeight Then { clip }
          Exit;
        CrtWriteln;
        b := 0
      End;
      { write the char, keeping track of the number of bytes on screen line }
      CrtWrite(cc);
      b := b + CrtCharWrapSize(cc)
    End;
    CrtClrEol; { needed when deleting a char from a line above the last one }
    CEditErase(Ed)
  End
End;

{ display chars from char _after_ i to char j, from current XY cursor position, 
 assuming this does not trigger a screen wrap }
Procedure CEditDisplayRange( Ed : TEditor; i,j : TBufIndex );
Var
  cc : TChar;
Begin
  With Ed Do
    While (i <> j) Do
    Begin
      i := NextIdx(Buf,i);
      BufGetCharAt(cc,Buf,i);
      CrtWrite(cc)
    End
End;

{ display the active line up to character i, at screen row y }
Procedure CEditDisplayActiveUpTo( Ed : TEditor; i : TBufIndex; y : TCrtCoordY );
Begin
  CrtGotoLine(y);
  If CEditActiveIsPrompt(Ed) Then
    CEditWritePrompt(Ed);
  If BufLen(Ed.Buf) = 0 Then
    Exit;
  CEditDisplayRange(Ed,Ed.IdxA,i)
End;

{ display the entire active line at screen row y }
Procedure CEditDisplayActiveLine( Ed : TEditor; y : TCrtCoordY );
Begin
  CEditDisplayActiveUpTo(Ed,CEditStateFindEndOfActiveLine(Ed),y);
  CrtClrEol
End;

{ set the visual cursor to reflect the value of the write cursor in the active
 row, which is located at screen row y; if we cannot use GotoXY, we redraw
 the beginning of the active line up to the character at the write index }
Procedure CEditSyncCursor( Ed : TEditor; y : TCrtCoordY );
Var
  x : TCrtCoordX;
Begin
  If Not CrtIsBroken(y) Then
  Begin
    x := Ed.NbBytes+1;
    If Not ((y = WhereY) And (x = WhereX)) Then { avoid useless flickering }
      CrtGotoXY(x,y)
  End
  Else
    CEditDisplayActiveUpTo(Ed,Ed.Buf.IdxW,y)
End;


{----------------------------------------------------------------------------}
{ CEditRefresh: display CL and sync the visual cursor                        }
{----------------------------------------------------------------------------}

{ redisplay the command line from its write index, leaving the visual cursor 
 unchanged }
Procedure CEditRefreshFromWrite( Ed : TEditor );
Var
  y : TCrtCoordY;
Begin
  y := WhereY; { backup initial cursor position, as the next call may move it }
  CEditDisplayFrom(Ed,FromWrite);
  CEditSyncCursor(Ed,y)
End;

{ display the whole command line, including the prompt; clip all the lines to 
 the screen }
Procedure CEditRefresh( Ed : TEditor );
Var 
  n : PosInt; { row index of the active line, n=1 means first line in CL }
  y : TVirtCoordY; { screen row of the line currently processed; can be < 1 }
  vis : Boolean; { is this line visible? }
  b : PosInt; { byte counter on current line }
  i : TBufIndex;
  cc : TChar;
Begin
  With Ed Do
  Begin
    { assume the active line is the first line of the CL, for now }
    n := 1;
    { we start at the prompt line, which may be out of screen }
    y := Layout.ScreenY;
    vis := RowIsVisible(y);
    { display the prompt if the prompt line is not out of screen }
    If vis Then
    Begin
      CrtGotoLine(y);
      CEditWritePrompt(Ed)
    End;
    b := NbBytesInPrompt;
    i := 0;
    { for each char in the buffer }
    While (i <> Buf.IdxE) Do
    Begin
      i := NextIdx(Buf,i);
      BufGetCharAt(cc,Buf,i);
      If CrtWraps(b,cc) Then { must wrap before displaying cc }
      Begin
        If vis Then
          CrtClrEol; { remove spurious chars at the end of the current line }
        If y = CrtScreenHeight Then { clip }
          Exit;
        { cc (virtually) displayed on next screen line }
        y := y + 1; 
        vis := RowIsVisible(y);
        { go to the beginning of this screen line }
        If vis Then
        Begin
          CrtGotoLine(y);
        End;
        b := 0
      End;
      { bookkeeping }
      If i = IdxA Then
        n := y - Layout.ScreenY + 2; { line after IdxA's; assert: IdxA <> IdxE }
      b := b + CrtCharWrapSize(cc);
      { finally, write the char, if visible }
      If vis Then
        CrtWrite(cc)
    End;
    CrtClrEol;
    CheckCondition(RowIsVisible(Layout.ScreenY+n-1),
        'CEditRefresh: cursor row is outside of the screen');
    { sync visual cursor with write index }
    CEditSyncCursor(Ed,Layout.ScreenY+n-1)
  End
End;

{----------------------------------------------------------------------------}
{ CEditMove: change of active line                                           }
{----------------------------------------------------------------------------}

{ move the cursor at the beginning of the next line (as IdxW points there), 
 scrolling when necessary }
Procedure CEditMoveDown( Var Ed : TEditor );
Var 
  y : TCrtCoordY;
Begin
  y := WhereY;
  If y < CrtScreenHeight Then
    CEditSyncCursor(Ed,y + 1)
  Else
  Begin
    With Ed.Layout Do
      ScreenY := ScreenY - 1;
    CrtGotoLine(1);
    CrtDelLine;
    CEditDisplayActiveLine(Ed,y)
  End
End;

{ move the cursor at the end of the previous line (as IdxW points there), 
 scrolling when necessary; if Refresh then all the lines after the active
 line are refreshed; the visual cursor is assumed to be at the beginning 
 of the line from which we go up }
Procedure CEditMoveUp( Var Ed : TEditor; Refresh : Boolean );
Var 
  y : TCrtCoordY;
Begin
  y := WhereY;
  If y > 1 Then
  Begin
    { refresh the current line, which is just below the new active one }
    If Refresh Then
    Begin
      CEditSyncCursor(Ed,y - 1); { as cursor must be sync'ed before }
      CEditDisplayFrom(Ed,FromWrite)
    End;
    { sync cursor: at the end of the active line }
    CEditSyncCursor(Ed,y - 1)
  End
  Else
  Begin
    With Ed.Layout Do
      ScreenY := ScreenY + 1;
    { insert a blank row on top, the location of the new active line }
    CrtGotoLine(1);
    CrtInsLine;
    { draw from here, all the way down or just that line }
    If Refresh Then
      If CEditActiveIsPrompt(Ed) Then { redraw from the prompt }
        CEditRefresh(Ed)
      Else
      Begin
        CEditDisplayFrom(Ed,FromActive);
        CEditSyncCursor(Ed,y)
      End
    Else
      CEditDisplayActiveLine(Ed,y)
  End
End;

{----------------------------------------------------------------------------}
{ CEdit: helpers to keep state and display in sync                           }
{----------------------------------------------------------------------------}

{ go to the next line while editing the CL, keeping layout data in sync }
Procedure CEditWriteln( Var Ed : TEditor );
Begin
  If WhereY = CrtScreenHeight Then
    With Ed.Layout Do
      ScreenY := ScreenY - 1;
  CrtClrEol; { in case we are overwriting when refreshing part of the CL }
  CrtWriteln
End;

{ scroll down when necessary, preparing the display of the CL  } 
Procedure CEditScroll( Var Ed : TEditor );
Var 
  m : TVirtCoordYDelta; { number of lines to scroll }
Begin
  With Ed Do
  Begin
    m := PrevLayout.ScreenY-Layout.ScreenY; { >0 means new CL is above }
    CheckCondition((m >= 0) Or (Layout.ScreenY <= 1),
        'Wrong CL layout: invalid reverse scroll');
    { prepare display: scroll down and clear }
    If Layout.ScreenY <= 1 Then { no upper part to save }
      CrtClrSrc
    Else
    Begin
      If m > 0 Then { the whole screen needs to move up }
      Begin
        CEditScrollDown(m);
        PrevLayout.ScreenY := PrevLayout.ScreenY - m
      End;
      { erase rows, taking into account the updated previous layout }
      CEditErase(Ed)
    End
  End
End;


{----------------------------------------------------------------------------}
{ CEdit: high-level procedures that keep state and display in sync           }
{----------------------------------------------------------------------------}

{ each procedure in this section must first backup the current layout, such
 that zones to erase will be correctly computed }

{ set and display the CL, erasing the previous CL, and scrolling when
 necessary }
Procedure CEditSet( Var Ed : TEditor; B : TBuf );
Begin
  CEditBackupLayout(Ed);
  CEditSetBuffer(Ed,B);
  CEditScroll(Ed);
  CEditRefresh(Ed);
  CEditTrace(Ed,'Set')
End;

{ insert char cc after write index; cc is not a NewLine }
Procedure CEditInsert( Var Ed : TEditor; cc : TChar );
Var 
  ActiveDown : Boolean; { active line moved down? }
Begin
  CEditBackupLayout(Ed);
  { insert the char at write index }
  CEditStateInsert(Ed,cc,ActiveDown);
  { update display, minimizing cursor movements }
  If ActiveDown Then
    CEditWriteln(Ed);
  CrtWrite(cc);
  { not an append op: we must redraw from IdxW }
  If Not BufWriteCursorIsAtEnd(Ed.Buf) Then
    CEditRefreshFromWrite(Ed);
  CEditTrace(Ed,'Insert')
End;

{ move the cursor forward }
Procedure CEditForward( Var Ed : TEditor );
Var 
  ActiveDown : Boolean; { active line moved down? }
Begin
  CEditBackupLayout(Ed);
  With Ed Do
  Begin
    { can we even move right? }
    If BufWriteCursorIsAtEnd(Buf) Then
      Exit;
    { move right the write index }
    BufWriteCursorMoveToNext(Buf);
    { update active line }
    CEditStateUpdateActiveAfterForward(Ed,ActiveDown);
    { update the screen }
    If ActiveDown Then
      CEditMoveDown(Ed)
    Else
      CEditSyncCursor(Ed,WhereY)
  End;
  CEditTrace(Ed,'Forward')
End;

{ delete char at write index; beep if not possible }
Procedure CEditDelete( Var Ed : TEditor );
Var 
  ActiveUp : Boolean; { active line moved up? }
Begin
  { beep and exit when no char can be deleted }
  If BufWriteCursorIsAtStart(Ed.Buf) Then
  Begin
    CrtBeep;
    Exit
  End;
  CEditBackupLayout(Ed);
  { delete the char at write index }
  CEditStateDelete(Ed,ActiveUp);
  { update display }
  If ActiveUp Then
    CEditMoveUp(Ed,True)
  Else { simple case: delete a char that do not change the active line }
  Begin
    CEditSyncCursor(Ed,WhereY);
    CrtClrEol;
    If Not BufWriteCursorIsAtEnd(Ed.Buf) Then { not an append op: redraw }
      CEditRefreshFromWrite(Ed)
  End;
  CEditTrace(Ed,'Delete')
End;

{ move the cursor backward }
Procedure CEditBackward( Var Ed : TEditor );
Var 
  cc : TChar;
  ActiveUp : Boolean; { active line moved up? }
Begin
  CEditBackupLayout(Ed);
  With Ed Do
  Begin
    { can we even move left? }
    If BufWriteCursorIsAtStart(Buf) Then
      Exit;
    { save char before moving the write index }
    BufGetCharAt(cc,Buf,Buf.IdxW);
    { move left the write index }
    BufWriteCursorMoveToPrev(Buf);
    { update active line }
    CEditStateUpdateActiveAfterBackward(Ed,cc,ActiveUp);
    { update the screen }
    If ActiveUp Then
      CEditMoveUp(Ed,False)
    Else
      CEditSyncCursor(Ed,WhereY)
  End;
  CEditTrace(Ed,'Backward')
End;


{----------------------------------------------------------------------------}
{ CEditDump: debug                                                           }
{----------------------------------------------------------------------------}

{ dump the state of editor Ed }
Procedure CEditDump( Ed : TEditor );
Begin
  WriteToTraceFile('| CEDIT: ');
  With Ed Do
  Begin
    WriteToTraceFile(' ScreenY=' + IntToShortString(Layout.ScreenY));
    WriteToTraceFile(' NbLines=' + IntToShortString(Layout.NbLines));
    WriteToTraceFile(' old.ScreenY=' + IntToShortString(PrevLayout.ScreenY));
    WriteToTraceFile(' old.NbLines=' + IntToShortString(PrevLayout.NbLines));
    WritelnToTraceFile('');
    WriteToTraceFile('|        ');
    WriteToTraceFile(' IdxA=' + IntToShortString(IdxA));
    WriteToTraceFile(' NbBytes=' + PosIntToShortString(NbBytes));
    WriteToTraceFile(' NbMulti=' + PosIntToShortString(NbMulti));
    WriteToTraceFile(' NbBytesInPrompt=' + PosIntToShortString(NbBytesInPrompt));
    WriteToTraceFile(' Prompt=''' + Prompt + '''');
    WritelnToTraceFile('');
    BufDump(Buf)
  End
End;

End.