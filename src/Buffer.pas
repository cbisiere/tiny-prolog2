{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Buffer.pas                                                 }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022-2025                                                  }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                     C I R C U L A R   B U F F E R                          }
{                                                                            }
{----------------------------------------------------------------------------}
{I define.inc }

{ circular buffer with read and write cursors }

{
  FEATURES:
  ---------
 - use the IdxW index of the TBuf to manage editing: IdxW = 0 means the cursor
   is located on the first char of the current input, meaning you cannot
   backspace from here, or that insertion are done at the beginning of the 
   buffer; note that inserted characters increase IdxW; when IdxW points to the
   last character, inserting means appending
}

{
  ASSUMPTIONS AND LIMITS:
  -----------------------
  1) BufSize: 255 (TP3) or 1024 (FPC)
    - maximum number of chars in a line typed after the prompt; could be
    increased at a cost of memory, as one buffer is allocated for each
    input stream in the stack, be it a console or a file
   - maximum number of spaces after a term read by a 'in(t)' goal, as those
    spaces have to be unread and thus must still be present in the buffer
}

Unit Buffer;

Interface

Uses
  ShortStr,
  Num,
  Chars,
  Trace,
  Crt2,
  Errs,
  IChar,
  Mirror,
  CWrites;

Const
  { input buffer size }
{$IFDEF MM_TINY}
  BufSize = 255;
{$ELSE}
  BufSize = 4096; { so, more than two 80x25 screens }
{$ENDIF}

Type 
  TBufItem = TIChar; { items in the buffer (opaque to (most of) this module) }

Type 
  TBufIndex = 0..BufSize; { index in (or size of) the buffer }
  TBuf = Record
    { elements in the buffer: [IdxB,IdxE] }
    Len : TBufIndex; { number of elements }
    IdxB : TBufIndex; { first element or zero if empty }
    IdxE : TBufIndex; { last element or zero if empty }
    { elements that have been read: [IdxB,Idx]  }
    LenR : TBufIndex; { number of elements read }
    IdxR : TBufIndex; { last element read, or zero if none yet }
    { current pointer (editing) }
    IdxW : TBufIndex; { insertion after IdxW; prepend if 0; delete at IdxW }
    { storage structure }
    Buf : Array[1..BufSize] Of TBufItem
  End;

Function FirstIdx( B : TBuf ) : TBufIndex;
Function LastIdx( B : TBuf ) : TBufIndex;
Function NextIdx( B : TBuf; i : TBufIndex ) : TBufIndex;
Function PrevIdx( B : TBuf; i : TBufIndex ) : TBufIndex;
Function BufLen( B : TBuf ) : TBufIndex;
Function BufNbFree( B : TBuf ) : TBufIndex;
Function BufNbUnread( B : TBuf ) : TBufIndex;
Function BufNbRead( B : TBuf ) : TBufIndex;
Procedure BufGetAt( Var e : TBufItem; B : TBuf; i : TBufIndex );
Procedure BufGetLast( Var e : TBufItem; B : TBuf );
Procedure BufGetRead( Var e : TBufItem; B : TBuf; n : TBufIndex );
Procedure BufGetLastRead( Var e : TBufItem; B : TBuf );

Procedure BufInit( Var B : TBuf );
Procedure BufDiscard( Var B : TBuf; n : TBufIndex );
Procedure BufGetChar( Var cc : TChar; B : TBuf; i,n : TBufIndex );
Procedure BufGetCharAt( Var cc : TChar; B : TBuf; i : TBufIndex );
Procedure BufPushChar( Var B : TBuf; cc : TChar );
Function BufCountMultibyteChars( B : TBuf ) : PosInt;
Function BufIsChar( B : TBuf; i : TBufIndex; cc : TChar ) : Boolean;
Procedure BufDiscardUnread( Var B : TBuf );
Procedure BufSetAllRead( Var B : TBuf );
Procedure BufRead( Var e : TBufItem; Var B : TBuf );
Procedure BufUnread( Var B : TBuf );
Function BufDiff( B1,B2 : TBuf ) : Boolean;

Function BufWriteCursorIsAtStart( B : TBuf ) : Boolean;
Function BufWriteCursorIsAtEnd( B : TBuf ) : Boolean;
Procedure BufSetWriteCursorAtEnd( Var B : TBuf );
Procedure BufWriteCursorMoveToNext( Var B : TBuf );
Procedure BufWriteCursorMoveToPrev( Var B : TBuf );
Procedure BufGetItemAtWriteCursor( Var e : TBufItem; B : TBuf );
Procedure BufDelete( Var B : TBuf );
Procedure BufInsert( Var B : TBuf; cc : TChar );

Function BufDisplayLine( B : TBuf; max : TBufIndex ) : TBufIndex;
Procedure BufToMirrorFiles( B : TBuf );

Procedure CharDump( cc : TChar );
Procedure BufDump( B : TBuf );

Implementation
{-----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ index calculations for circular buffer                                     }
{----------------------------------------------------------------------------}

{ index i incremented by n; IncIdx(0,1) gives 1 }
Function IncIdx( i,n : TBufIndex ) : TBufIndex;
Begin
  IncIdx := ((i - 1 + n) mod BufSize) + 1
End;

{ index i decremented by n; DecIdx(1,1) gives 0 }
Function DecIdx( i,n : TBufIndex ) : TBufIndex;
Begin
  If i - n < 1 Then
    DecIdx := BufSize + i - n
  Else
    DecIdx := i - n
End;

{ test whether index i falls inside interval [i1,i2] }
Function InBetween( i,i1,i2 : TBufIndex ) : Boolean;
Begin
  CheckCondition((i > 0) And (i1 > 0) And (i2 > 0),
      'InBetween: at least one index is nul');
  If i1 <= i2 Then
    InBetween := (i >= i1) And (i <= i2)
  Else
    InBetween := (i >= i1) Or (i <= i2)
End;

{ number of elements between index i1 and index i2 }
Function NbBetween( i1,i2 : TBufIndex ) : TBufIndex;
Begin
  CheckCondition((i1 > 0) And (i2 > 0),
      'NbBetween: a bound is nul');
  NbBetween := i2 - i1 + 1 + BufSize*Ord(i2 < i1)
End;


{----------------------------------------------------------------------------}
{ Buffer: get/set                                                            }
{----------------------------------------------------------------------------}

{ first index; 0 if buffer is empty }
Function FirstIdx( B : TBuf ) : TBufIndex;
Begin
  FirstIdx := B.IdxB
End;

{ last index; 0 if buffer is empty }
Function LastIdx( B : TBuf ) : TBufIndex;
Begin
  LastIdx := B.IdxE
End;

{ next index after i; 0 if past the end of the buffer; next of 0 is first }
Function NextIdx( B : TBuf; i : TBufIndex ) : TBufIndex;
Begin
  If i = 0 Then
    NextIdx := FirstIdx(B)
  Else If i = LastIdx(B) Then
    NextIdx := 0
  Else
    NextIdx := IncIdx(i,1)
End;

{ previous index before i; 0 if past the beginning of the buffer; prev of 0
 is last }
Function PrevIdx( B : TBuf; i : TBufIndex ) : TBufIndex;
Begin
  If i = 0 Then
    PrevIdx := LastIdx(B)
  Else If i = FirstIdx(B) Then
    PrevIdx := 0
  Else
    PrevIdx := DecIdx(i,1)
End;

{ number of elements in a buffer }
Function BufLen( B : TBuf ) : TBufIndex;
Begin
  BufLen := B.Len
End;

{ number of space left (in number of elements) in a buffer }
Function BufNbFree( B : TBuf ) : TBufIndex;
Begin
  BufNbFree := BufSize - B.Len
End;

{ number of elements still not consumed }
Function BufNbUnread( B : TBuf ) : TBufIndex;
Begin
  BufNbUnread := B.Len - B.LenR
End;

{ number of elements already read in a buffer }
Function BufNbRead( B : TBuf ) : TBufIndex;
Begin
  BufNbRead := B.LenR
End;

{ check whether element with index i is in a buffer }
Procedure BufCheck( B : TBuf; i : TBufIndex );
Begin
  CheckCondition(BufLen(B) > 0,'Buf check: empty buffer');
  CheckCondition(InBetween(i,FirstIdx(B),LastIdx(B)),'Buf check: not in buffer')
End;

{ element of index i in a buffer, with offset n }
Procedure BufGet( Var e : TBufItem; B : TBuf; i,n : TBufIndex );
Begin
  i := IncIdx(i,n);
  BufCheck(B,i);
  e := B.Buf[i]
End;

{ element of index i in a buffer }
Procedure BufGetAt( Var e : TBufItem; B : TBuf; i : TBufIndex );
Begin
  BufGet(e,B,i,0)
End;

{ element of index i in a buffer, with negative offset n }
Procedure BufGetPrev( Var e : TBufItem; B : TBuf; i,n : TBufIndex );
Begin
  i := DecIdx(i,n);
  BufCheck(B,i);
  e := B.Buf[i]
End;

{ first element in a buffer }
Procedure BufGetFirst( Var e : TBufItem; B : TBuf );
Begin
  BufGet(e,B,FirstIdx(B),0)
End;

{ last element in a buffer }
Procedure BufGetLast( Var e : TBufItem; B : TBuf );
Begin
  BufGet(e,B,LastIdx(B),0)
End;

{ last element read from a buffer, with an offset }
Procedure BufGetRead( Var e : TBufItem; B : TBuf; n : TBufIndex );
Begin
  BufGet(e,B,B.IdxR,n)
End;

{ last element read from a buffer }
Procedure BufGetLastRead( Var e : TBufItem; B : TBuf );
Begin
  BufGetRead(e,B,0)
End;

{ set element of index i, with offset n, in a buffer }
Procedure BufSet( Var B : TBuf; i,n : TBufIndex; e : TBufItem );
Begin
  i := IncIdx(i,n);
  BufCheck(B,i);
  B.Buf[i] := e
End;

{ set element of index i, with negative offset n, in a buffer }
Procedure BufSetPrev( Var B : TBuf; i,n : TBufIndex; e : TBufItem );
Begin
  i := DecIdx(i,n);
  BufCheck(B,i);
  B.Buf[i] := e
End;

{----------------------------------------------------------------------------}
{ IdxW: get/set                                                              }
{----------------------------------------------------------------------------}

{ The write cursor IdxW ranges from 0 to N, the number of chars in the buffer;
 - in terms of display, the blinking cursor is on char IdxW+1, that is, from 
   the first char to the char right after the last char 
 - insertions are made before char IdxW+1
 - deletion by backspace are made at char IdxW; no deletion is possible when 
   IdxW is zero 
 }

{ Does IdxW has a sane value? }
Function BufWriteCursorIsSane( B : TBuf ) : Boolean;
Begin
  With B Do 
    BufWriteCursorIsSane := (IdxW = 0) Or InBetween(IdxW,IdxB,IdxE)
End;

{ Is IdxW at its starting position? meaning inserting is prepending, and delete
 by backspace is not possible }
Function BufWriteCursorIsAtStart( B : TBuf ) : Boolean;
Begin
  With B Do 
    BufWriteCursorIsAtStart := IdxW = 0
End;

{ Set IdxW such that next insert will be a prepend }
Procedure BufSetWriteCursorAtStart( Var B : TBuf );
Begin
  With B Do 
    IdxW := 0
End;

{ Is IdxW at its ending position? meaning inserting is appending, and delete by
 backspace deletes the last char; note that if the buffer is empty, this 
 function returns True }
Function BufWriteCursorIsAtEnd( B : TBuf ) : Boolean;
Begin
  With B Do 
    BufWriteCursorIsAtEnd := IdxW = IdxE
End;

{ Set IdxW at the end of buffer B; if B is empty, IdxW will be 0, interpreted 
 as "prepend" }
Procedure BufSetWriteCursorAtEnd( Var B : TBuf );
Begin
  With B Do 
    IdxW := LastIdx(B)
End;

{ advance IdxW by one item }
Procedure BufWriteCursorMoveToNext( Var B : TBuf );
Begin
  CheckCondition(Not BufWriteCursorIsAtEnd(B),'Buf: IdxW already at end');
  With B Do 
    IdxW := NextIdx(B,IdxW)
End;

{ move backward IdxW by one char; IdxW = 0 means further insertions will be 
 done at the beginning }
Procedure BufWriteCursorMoveToPrev( Var B : TBuf );
Begin
  CheckCondition(Not BufWriteCursorIsAtStart(B),'Buf: IdxW already at start');
  With B Do 
    IdxW := PrevIdx(B,IdxW)
End;

{ get the char at position IdxW }
Procedure BufGetItemAtWriteCursor( Var e : TBufItem; B : TBuf );
Begin
  CheckCondition(Not BufWriteCursorIsAtStart(B),'Buf: IdxW is at start');
  With B Do 
    BufGet(e,B,IdxW,0)
End;

{ clip IdxW to a buffer, setting it to zero if it points to a nonexisting char }
Procedure BufClipWriteCursor( Var B : TBuf );
Begin
  If Not BufWriteCursorIsSane(B) Then
    BufSetWriteCursorAtStart(B)
End;


{----------------------------------------------------------------------------}
{ init                                                                       }
{----------------------------------------------------------------------------}

{ initialize a buffer }
Procedure BufInit( Var B : TBuf );
Begin
  With B Do
  Begin
    Len := 0;
    IdxB := 0;
    IdxE := 0;
    LenR := 0;
    IdxR := 0;
    IdxW := 0
  End
End;

{ store a first element into a buffer }
Procedure BufStoreFirst( Var B : TBuf; e : TBufItem );
Const
  Loc : TBufIndex = 1; { index where to store this element }
Begin
  BufInit(B);
  With B Do
  Begin
    Len := 1;
    IdxB := Loc;
    IdxE := Loc;
  End;
  BufSet(B,Loc,0,e)
End;


{----------------------------------------------------------------------------}
{ push/pop item                                                              }
{----------------------------------------------------------------------------}

{ push an item at the end of a buffer }
Procedure BufPushItem( Var B : TBuf; e : TBufItem );
Begin
  CheckCondition(BufNbFree(B) > 0,'BufPushItem: Buf is full');
  With B Do 
  Begin
    If Len = 0 Then
      BufStoreFirst(B,e)
    Else
    Begin
      IdxE := IncIdx(IdxE,1);
      Len := Len + 1;
      BufSet(B,IdxE,0,e)
    End
  End
End;

{ pop one item from a buffer, assuming the write index is not pointing at the
 last item }
Procedure BufPopItem( Var B : TBuf; Var e : TBufItem );
Begin
  CheckCondition(BufLen(B) > 0,'BufPopItem: Buf is empty');
  CheckCondition(Not BufWriteCursorIsAtEnd(B),'BufPopItem: Cannot pop at IdxW');
  With B Do
  Begin
    { char to return }
    BufGetLast(e,B);
    { delete the last char }
    If BufLen(B) = 1 Then
      BufInit(B)
    Else
    Begin { at least two characters are in the buffer }
      { the last char was already read: rewind the read index by one }
      If IdxR = IdxE Then
      Begin
        IdxR := DecIdx(IdxR,1);
        LenR := LenR - 1
      End;
      { now, delete the char from the buffer }
      IdxE := DecIdx(IdxE,1);
      Len := Len - 1
    End
  End
End;


{----------------------------------------------------------------------------}
{ methods aware of items being of type TIChar                                }
{----------------------------------------------------------------------------}

{ char of index i in a buffer, with offset n }
Procedure BufGetChar( Var cc : TChar; B : TBuf; i,n : TBufIndex );
Var
  e : TBufItem;
Begin
  BufGet(e,B,i,n);
  cc := e.Val
End;

{ get the char at position i }
Procedure BufGetCharAt( Var cc : TChar; B : TBuf; i : TBufIndex );
Begin
  BufGetChar(cc,B,i,0)
End;

{ IChar of index i in a buffer is the char cc }
Function BufIsChar( B : TBuf; i : TBufIndex; cc : TChar ) : Boolean;
Var
  e : TBufItem;
Begin
  BufGet(e,B,i,0);
  BufIsChar := IsChar(e,cc)
End;

{ set i-th char's position data from its previous char}
Procedure BufSetPosFromPrev( Var B : TBuf; i : TBufIndex );
Var
  k : TBufIndex; { previous index }
Begin
  k := PrevIdx(B,i);
  With B Do
    SetICharPosFromPrev(Buf[i],Buf[k])
End;

{ recalculate all character positions, starting from i-th character }
Procedure BufSetPositions( Var B : TBuf; i : TBufIndex; 
    line : TLineNum; col : TCharPos );
Begin
  { set position data of the first character }
  SetICharPos(B.Buf[i],line,col);
  { char next to i is the first to recalculate }
  i := NextIdx(B,i);
  While i <> 0 Do
  Begin
    BufSetPosFromPrev(B,i);
    i := NextIdx(B,i)
  End
End;

{ append a composite char to a buffer, computing its position from
 the previous element }
Procedure BufPushChar( Var B : TBuf; cc : TChar );
Var
  p : TIChar; { previous char: from which to compute the new position }
  e : TIChar; { new char }
Begin
  SetIChar(e,cc,1,1); { default: line 1, char 1 }
  If BufLen(B) > 0 Then
  Begin
    BufGetLast(p,B);
    SetICharPosFromPrev(e,p)
  End;
  BufPushItem(B,e)
End;

{ return the number of multibyte chars in B }
Function BufCountMultibyteChars( B : TBuf ) : PosInt;
Var
  n : PosInt;
  i : TBufIndex;
  cc : TChar;
Begin
  n := 0;
  i := FirstIdx(B);
  While (i <> 0) Do
  Begin
    BufGetCharAt(cc,B,i);
    If IsMultibyte(cc) Then
      n := n + 1;
    i := NextIdx(B,i)
  End;
  BufCountMultibyteChars := n
End;

{----------------------------------------------------------------------------}
{ edit at IdxW                                                               }
{----------------------------------------------------------------------------}

{ delete the item at write index IdxW and move IdxW left }
Procedure BufDelete( Var B : TBuf );
Var
  i : TBufIndex;
  e,e1 : TBufItem;
Begin
  CheckCondition(Not BufWriteCursorIsAtStart(B),
      'Buf: cannot delete, IdxW is at start');
  { char just after IdxW, if any }
  i := NextIdx(B,B.IdxW);
  { move left all items from IdxW+1, by one char, overwriting the item at IdxW }
  If i <> 0 Then
  Begin
    { save position data of item at IdxW }
    BufGetItemAtWriteCursor(e1,B);
    { move items }
    While i <> 0 Do
    Begin
      BufGet(e,B,i,0); { char to move left by one spot }
      BufSetPrev(B,i,1,e); { Buf[i-1] <- Buf[i] }
      i := NextIdx(B,i)
    End;
    { update all position data from IdxW }
    BufSetPositions(B,B.IdxW,e1.Lnb,e1.Pos)
  End;
  { move IdxW left; this has to be done before popping the item to enforce 
   assumptions for BufPopItem regarding IdxW }
  BufWriteCursorMoveToPrev(B);
  { shorten the buffer by one char }
  BufPopItem(B,e)
End;

{ insert one char after IdxW; thus, IdxW = O means prepend; IdxW = IdxE means 
 append; move IdxW right }
Procedure BufInsert( Var B : TBuf; cc : TChar );
Var
  k,i : TBufIndex;
  e,e1,e2 : TBufItem;
Begin
  CheckCondition(BufNbFree(B) > 0,'Buf: cannot insert, buffer is full');
  If BufWriteCursorIsAtEnd(B) Then
    BufPushChar(B,cc)
  Else
  Begin
    { e1: compute what will be the position data at new IdxW }
    If BufWriteCursorIsAtStart(B) Then { prepend op; pos data left of first char }
    Begin
      BufGetFirst(e,B);
      SetICharPosFromNext(e1,e)
    End
    Else { insert or append; pos data right from IdxW }
    Begin
      BufGetItemAtWriteCursor(e,B);
      SetICharPosFromPrev(e1,e)
    End;
    { e2: item to swap from/to }
    SetIChar(e2,cc,0,0);
    { location of the new char }
    k := NextIdx(B,B.IdxW);
    { make some room for it }
    i := k;
    While i <> 0 Do
    Begin
      SwapIChar(B.Buf[i],e2);
      i := NextIdx(B,i)
    End;
    { append currently saved char; is cc when just appending }
    BufPushItem(B,e2);
    { update all position data from the inserted char }
    BufSetPositions(B,k,e1.Lnb,e1.Pos)
  End;
  { move right write index }
  BufWriteCursorMoveToNext(B)
End;


{----------------------------------------------------------------------------}
{ bulk discard                                                               }
{----------------------------------------------------------------------------}

{ discard n element at the beginning of a buffer; these elements must 
 have been read already }
Procedure BufDiscard( Var B : TBuf; n : TBufIndex );
Begin
  With B Do
  Begin
    BufCheck(B,IdxB);
    CheckCondition(Len >= n,'Buf: cannot discard that many elements');
    CheckCondition(LenR >= n,'Buf: cannot discard unread elements');
    If Len = n Then
      BufInit(B)
    Else
    Begin
      IdxB := IncIdx(IdxB,n);
      Len := Len - n;
      LenR := LenR - n;
      { IdxW pointed to a discarded char: reset it to "prepend" }
      BufClipWriteCursor(B)
    End
  End
End;


{----------------------------------------------------------------------------}
{ read/unread                                                                }
{----------------------------------------------------------------------------}

{ set all chars as read }
Procedure BufSetAllRead( Var B : TBuf );
Begin
  With B Do
  Begin
    IdxR := IdxE;
    LenR := Len
  End
End;

{ discard all unread elements in a buffer }
Procedure BufDiscardUnread( Var B : TBuf );
Begin
  With B Do 
  Begin
    If LenR = 0 Then
      BufInit(B)
    Else
    Begin
      IdxE := IdxR;
      Len := NbBetween(IdxB,IdxE);
      BufClipWriteCursor(B)
    End
  End
End;

{ retrieve an element from a buffer, advancing the read index }
Procedure BufRead( Var e : TBufItem; Var B : TBuf );
Begin
  CheckCondition(BufNbUnread(B) > 0,'Buf: nothing to read');
  With B Do
  Begin
    If LenR = 0 Then
    Begin
      IdxR := IdxB;
      LenR := 1
    End
    Else
    Begin
      IdxR := IncIdx(IdxR,1);
      LenR := LenR + 1
    End
  End;
  BufGetRead(e,B,0)
End;

{ move the read index backward }
Procedure BufUnread( Var B : TBuf );
Begin
  CheckCondition(BufNbRead(B) > 0,'Buf: cannot unread');
  With B Do
  Begin
    If LenR = 1 Then
    Begin
      IdxR := 0;
      LenR := 0
    End
    Else
    Begin
      IdxR := DecIdx(IdxR,1);
      LenR := LenR - 1
    End
  End
End;

{ have two buffers different content? }
Function BufDiff( B1,B2 : TBuf ) : Boolean;
Var 
  i : TBufIndex;
  Diff : Boolean;
Begin
  Diff := B1.Len <> B2.Len;
  i := FirstIdx(B1);
  While (i <> 0) And Not Diff Do 
  Begin
    Diff := B1.Buf[i].Val.Bytes <> B2.Buf[i].Val.Bytes;
    i := NextIdx(B1,i)
  End;
  BufDiff := Diff
End;


{----------------------------------------------------------------------------}
{ Display                                                                    }
{----------------------------------------------------------------------------}

{ display last characters read, belonging to the same line, and taking up to a 
 maximum of max columns; return the number of columns actually
 displayed }
Function BufDisplayLine( B : TBuf; max : TBufIndex ) : TBufIndex;
Var
  n,m : TBufIndex;
  i,j : TBufIndex;
  cols : Byte;
Begin
  n := 0;
  With B Do
  Begin
    If BufNbRead(B) > 0 Then
    Begin
      { find the index i from which to start displaying chars }
      m := 1;
      i := IdxR;
      j := PrevIdx(B,i);
      While (m < max) And (j <> 0) Do
      Begin
        cols := CrtCharWidthOnScreen(Buf[j].Val);
        If IsEol(Buf[j]) Or ((m + cols) > max) Then
          j := 0
        Else
        Begin
          i := j;
          m := m + cols;
          j := PrevIdx(B,j)
        End
      End;
      { display from char i to read index }
      n := 0;
      While i <> 0 Do
      Begin
        If IsEol(Buf[i]) Then { last char is Eol }
          CWrite(' ')
        Else
          CWriteChar(Buf[i].Val);
        n := n + CrtCharWidthOnScreen(Buf[i].Val);
        If i = IdxR Then
          i := 0
        Else
          i := NextIdx(B,i)
      End
    End
  End;
  If n > 0 Then
    CWriteLn;
  BufDisplayLine := n
End;

{ output to echo and trace files the content of buffer B }
Procedure BufToMirrorFiles( B : TBuf );
Var 
  i : TBufIndex;
Begin
  i := FirstIdx(B);
  While i <> 0 Do
  Begin
    WriteToMirrorFiles(B.Buf[i].Val.Bytes);
    i := NextIdx(B,i)
  End
End;


{----------------------------------------------------------------------------}
{ Debug                                                                      }
{----------------------------------------------------------------------------}

{ dump the content of char cc }
Procedure CharDump( cc : TChar );
Var
  i : 0..MaxBytesPerChar;
  s : TString;
Begin
  s := '(';
  For i := 1 to Length(cc.Bytes) Do
  Begin
    s := s + IntToShortString(Ord(cc.Bytes[i]));
    If i < Length(cc.Bytes) Then
      s := s + ','
  End;
  s := s + ')';
  WriteToTraceFile(s)
End;

{ dump the content of buffer B }
Procedure BufDump( B : TBuf );
Var 
  i : TBufIndex;
Begin
  WriteToTraceFile('| BUF: ');
  i := FirstIdx(B);
  While i <> 0 Do
  Begin
    WriteToTraceFile(IntToShortString(i) + ':');
    CharDump(B.Buf[i].Val);
    WriteToTraceFile(' ');
    i := NextIdx(B,i)
  End;
  WritelnToTraceFile('');
  WriteToTraceFile('| BUF: ');
  WriteToTraceFile(' Len=' + IntToShortString(B.Len));
  WriteToTraceFile(' IdxB=' + IntToShortString(B.IdxB));
  WriteToTraceFile(' IdxE=' + IntToShortString(B.IdxE));
  WriteToTraceFile(' LenR=' + IntToShortString(B.LenR));
  WriteToTraceFile(' IdxR=' + IntToShortString(B.IdxR));
  WriteToTraceFile(' IdxW=' + IntToShortString(B.IdxW));
  WritelnToTraceFile('')
End;

End.