{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Buffer.pas                                                 }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                     C I R C U L A R   B U F F E R                          }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ circular buffer with read cursor }

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
  Crt2,
  Errs,
  IChar,
  Trace;

Const
  { input buffer size }
{$IFDEF MSDOS}
  BufSize = 255;
{$ELSE}
  BufSize = 1024;
{$ENDIF}

Type 
  TBufItem = TIChar; { items in the buffer (opaque to this module) }

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
Procedure BufGetLast( Var e : TBufItem; B : TBuf );
Procedure BufGetRead( Var e : TBufItem; B : TBuf; n : TBufIndex );
Procedure BufGetLastRead( Var e : TBufItem; B : TBuf );

Procedure BufInit( Var B : TBuf );
Procedure BufPop( Var e : TIChar; Var B : TBuf );
Procedure BufDiscard( Var B : TBuf; n : TBufIndex );
Procedure BufAppendTChar( Var B : TBuf; cc : TChar );
Procedure BufDiscardUnread( Var B : TBuf );

Procedure BufRead( Var e : TBufItem; Var B : TBuf );
Procedure BufUnread( Var B : TBuf );

Function BufDisplayLine( B : TBuf; max : TBufIndex ) : TBufIndex;
Procedure BufToEchoFile( B : TBuf );
Procedure BufFilterOut( Var B : TBuf; cc : TChar );
Function BufDiff( B1,B2 : TBuf ) : Boolean;

Procedure CharDump( cc : TChar );
Procedure BufDump( B : TBuf );

Implementation
{-----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ index calculations                                                         }
{----------------------------------------------------------------------------}

{ index i incremented by n }
Function IncIdx( i,n : TBufIndex ) : TBufIndex;
Begin
  IncIdx := ((i - 1 + n) mod BufSize) + 1
End;

{ index i decremented by n }
Function DecIdx( i,n : TBufIndex ) : TBufIndex;
Begin
  If i - n < 1 Then
    DecIdx := BufSize + i - n
  Else
    DecIdx := i - n
End;

{----------------------------------------------------------------------------}
{ buffer getters and setters                                                 }
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

{ next index after i; 0 if past the end of the buffer }
Function NextIdx( B : TBuf; i : TBufIndex ) : TBufIndex;
Begin
  With B Do
    If i = IdxE Then
      NextIdx := 0
    Else
      NextIdx := IncIdx(i,1)
End;

{ previous index before i; 0 if past the beginning of the buffer }
Function PrevIdx( B : TBuf; i : TBufIndex ) : TBufIndex;
Begin
  With B Do
    If i = IdxB Then
      PrevIdx := 0
    Else
      PrevIdx := DecIdx(i,1)
End;

{ test whether index i falls inside interval [i1,i2] }
Function InBetween( i,i1,i2 : TBufIndex ) : Boolean;
Begin
  CheckCondition((i > 0) And (i1 > 0) And (i2 > 0),'InBetween: at least one index is nul');
  If i1 <= i2 Then
    InBetween := (i >= i1) And (i <= i2)
  Else
    InBetween := (i >= i1) Or (i <= i2)
End;

{ number of elements between index i1 and index i2 }
Function NbBetween( B : TBuf; i1,i2 : TBufIndex ) : TBufIndex;
Begin
  CheckCondition((i1 > 0) And (i2 > 0),'NbBetween: at least one index is nul');
  With B Do 
    NbBetween := i2 - i1 + 1 + BufSize*Ord(i2 < i1)
End;

{ number of elements in a buffer }
Function BufLen( B : TBuf ) : TBufIndex;
Begin
  With B Do 
    BufLen := Len
End;
{ number of space left (in number of elements) in a buffer }
Function BufNbFree( B : TBuf ) : TBufIndex;
Begin
  With B Do 
    BufNbFree := BufSize - Len
End;

{ number of elements still not consumed }
Function BufNbUnread( B : TBuf ) : TBufIndex;
Begin
  With B Do 
    BufNbUnread := Len - LenR
End;

{ number of elements already read in a buffer }
Function BufNbRead( B : TBuf ) : TBufIndex;
Begin
  With B Do 
    BufNbRead := LenR
End;

{ check whether element with index i is in a buffer }
Procedure BufCheck( B : TBuf; i : TBufIndex );
Begin
  CheckCondition(i <> 0,'Buf check: index zero');
  With B Do 
  Begin
    CheckCondition(Len > 0,'Buf check: empty buffer');
    CheckCondition(InBetween(i,IdxB,IdxE),'Buf check: not in buffer')
  End
End;

{ element of index i in a buffer, with offset n }
Procedure BufGet( Var e : TBufItem; B : TBuf; i,n : TBufIndex );
Begin
  i := IncIdx(i,n);
  BufCheck(B,i);
  With B Do 
    e := Buf[i]
End;

{ first element in a buffer }
Procedure BufGetFirst( Var e : TBufItem; B : TBuf );
Begin
  With B Do 
    BufGet(e,B,IdxB,0)
End;

{ last element in a buffer }
Procedure BufGetLast( Var e : TBufItem; B : TBuf );
Begin
  With B Do 
    BufGet(e,B,IdxE,0)
End;

{ last element read from a buffer, with an offset }
Procedure BufGetRead( Var e : TBufItem; B : TBuf; n : TBufIndex );
Begin
  With B Do 
    BufGet(e,B,IdxR,n)
End;

{ last element read from a buffer }
Procedure BufGetLastRead( Var e : TBufItem; B : TBuf );
Begin
  With B Do 
    BufGetRead(e,B,0)
End;

{ set element of index i in a buffer }
Procedure BufSet( Var B : TBuf; i : TBufIndex; e : TBufItem );
Begin
  BufCheck(B,i);
  With B Do 
    Buf[i] := e
End;

{----------------------------------------------------------------------------}
{ methods                                                                    }
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
    IdxR := 0
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
  BufSet(B,Loc,e)
End;

{ pop one element in buffer B }
Procedure BufPop( Var e : TIChar; Var B : TBuf );
Begin
  With B Do
  Begin
    CheckCondition(Len > 0,'BufPop: empty');
    BufGetLast(e,B);
    If BufLen(B) = 1 Then
      BufInit(B)
    Else
    Begin
      If IdxR = IdxE Then { last char was already read }
      Begin
        IdxR := DecIdx(IdxR,1);
        LenR := LenR - 1
      End;
      IdxE := DecIdx(IdxE,1);
      Len := Len - 1
    End
  End
End;

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
      LenR := LenR - n
    End
  End
End;

{ store an element into a buffer }
Procedure BufStore( Var B : TBuf; e : TBufItem );
Begin
  CheckCondition(BufNbFree(B) > 0,'Buf is full');
  With B Do 
  Begin
    If Len = 0 Then
      BufStoreFirst(B,e)
    Else
    Begin
      IdxE := IncIdx(IdxE,1);
      Len := Len + 1;
      BufSet(B,IdxE,e)
    End
  End
End;

{ append a composite char to a buffer, computing its position from
 the previous element }
Procedure BufAppendTChar( Var B : TBuf; cc : TChar );
Var
  p : TIChar; { char from which to compute the new position }
  e : TIChar; { new char }
  cc2 : TChar;
Begin
  If BufLen(B) > 0 Then
    BufGetLast(p,B)
  Else
  Begin
    ASCIIChar(cc2,NewLine);
    SetIChar(p,cc2,0,0)
  End;
  NewICharFromPrev(e,p,cc);
  BufStore(B,e)
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
      Len := NbBetween(B,IdxB,IdxE)
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

{ display to screen the content of buffer B }
Procedure BufToScreen( B : TBuf );
Var 
  i : TBufIndex;
Begin
  i := FirstIdx(B);
  While i <> 0 Do
  Begin
    CWrite(B.Buf[i].Val.Bytes);
    i := NextIdx(B,i)
  End
End;

{ output to echo file the content of buffer B }
Procedure BufToEchoFile( B : TBuf );
Var 
  i : TBufIndex;
Begin
  i := FirstIdx(B);
  While i <> 0 Do
  Begin
    WriteToEchoFile(B.Buf[i].Val.Bytes);
    i := NextIdx(B,i)
  End
End;

{ delete from buffer B all characters equal to cc; reset the read 
 cursor }
Procedure BufFilterOut( Var B : TBuf; cc : TChar );
Var 
  R : TBuf;
  i : TBufIndex;
Begin
  BufInit(R);
  i := FirstIdx(B);
  While i <> 0 Do
  Begin
    With B.Buf[i] Do
      If Val.Bytes <> cc.Bytes Then
        BufAppendTChar(R,Val);
    i := NextIdx(B,i)
  End;
  B := R
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
{ Debug                                                                      }
{----------------------------------------------------------------------------}

{ dump the content of char cc }
Procedure CharDump( cc : TChar );
Var
  i : 0..MaxBytesPerChar;
  s : TString;
Begin
  s := '<';
  For i := 1 to Length(cc.Bytes) Do
  Begin
    s := s + IntToShortString(Ord(cc.Bytes[i]));
    If i < Length(cc.Bytes) Then
      s := s + ','
  End;
  s := s + '>';
  WriteToEchoFile(s)
End;

{ dump the content of buffer B }
Procedure BufDump( B : TBuf );
Var 
  i : TBufIndex;
Begin
  i := FirstIdx(B);
  While i <> 0 Do
  Begin
    WriteToEchoFile(IntToShortString(i) + ':');
    CharDump(B.Buf[i].Val);
    WriteToEchoFile(' ');
    i := NextIdx(B,i)
  End;
  WriteToEchoFile(' IdxR=' + IntToShortString(B.IdxR))
End;

{ dump Crt state }
Procedure CrtDump;
Var
  i : TCrtCoord;
  cc : TChar;
Begin
  With CrtRow Do
  Begin
    WriteToEchoFile(IntToShortString(Len) + ' ' + IntToShortString(Wrap) + ' ');
    For i := 1 to Len Do
    Begin
      CrtChar(Row,i,cc);
      CharDump(cc)
    End
  End
End;

End.