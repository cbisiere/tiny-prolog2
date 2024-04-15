{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObjIO.pas                                                 }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                I N P U T   /   O U T P U T   S T R E A M S                 }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ double-linked list (stack) of i/o entities, including regular files, buffers,
 and console; read or write characters from single-byte or UTF-8 stream }

{
  ASSUMPTIONS AND LIMITS:
  -----------------------
  1) input / output streams are single-byte or UTF8 encoded
  2) for correct display of strings contained in a Prolog program, terminal  
    encoding is the same as the Prolog file
  3) single-byte files do not start with a sequence of chars equal to the 
    UTF-8 BOM sequence EF BB BF
  4) single-byte files do not contain any sequence of bytes that are
    valid UTF-8 code point 
}

Unit PObjIO;

Interface

Uses
  ShortStr,
  Num,
  Errs,
  Chars,
  Crt2,
  Files,
  Readline,
  IChar,
  Buffer,
  Trace,
  Common,
  Memory,
  PObj;

Type
  TFileDescriptor = PosInt; { file descriptor }
  TAlias = TString; { file name (a.k.a. "alias") }
  TIODeviceType = (DEV_FILE,DEV_BUFFER,DEV_TERMINAL,DEV_ANY);
  TStreamMode = (MODE_READ,MODE_WRITE,MODE_NONE,MODE_ANY);

Const
  CONSOLE_ALIAS : TAlias = 'console';
  BUFFER_ALIAS : TAlias = 'buffer';

{ stack of input /output streams }
Type
  StreamPtr = ^TObjStream;
  TObjStream = Record
    PO_META : TObjMeta;
    { not deep copied: }
    FI_PREV : StreamPtr;             { previous file (upper in the stack)      }
    FI_NEXT : StreamPtr;             { next file (lower in the stack)          }
    { extra data: }
    FI_DESC : TFileDescriptor;     { unique number: 1,2,...                  }
    FI_ALIA : TAlias;                { name of the stream                      }
    FI_PATH : TPath;                 { file's full path                        }
    FI_TYPE : TIODeviceType;         { file, buffer, or console?               }
    FI_OPEN : Boolean;               { is the stream ready for i/o?            }
    FI_LOCK : Boolean;               { user is not allowed to close atm        }
    FI_CHAR : LongInt;               { number of bytes read/written so far     }
    FI_ENCO : TEncoding;             { character encoding of the stream        }
    Case FI_MODE : TStreamMode Of    { opening mode: read / write              }
      MODE_READ: (
        FI_IFIL : TIFile;            { file handler (if not a console)         }
        FI_CBUF : TChar;             { 1 multi-byte char input buffer          }
        FI_IBUF : TBuf);             { input buffer                            }
      MODE_WRITE : (
        FI_OFIL : TOFile);           { file handler (if not a console)         }
      MODE_NONE : ();
  End;


Function NewStream( Alias : TAlias; Path : TPath; Dev : TIODeviceType;
    Mode : TStreamMode; Locked, WithDesc : Boolean ) : StreamPtr;
Function NewBuffer : StreamPtr;
Function NewConsole( Mode : TStreamMode ) : StreamPtr;

{ stream: get / set }
Function StreamAlias( f : StreamPtr ) : TAlias;
Function StreamDeviceType( f : StreamPtr ) : TIODeviceType;
Function StreamIsConsole( f : StreamPtr ) : Boolean;
Function StreamIsLocked( f : StreamPtr ) : Boolean;
Function StreamIsOpen( f : StreamPtr ) : Boolean;
Function StreamMode( f : StreamPtr ) : TStreamMode;
Procedure StreamSetMode( f : StreamPtr; Mode : TStreamMode );
Function StreamDescriptor( f : StreamPtr ) : TFileDescriptor;
Function StreamEncoding( f : StreamPtr ) : TEncoding;
Procedure StreamSetEncoding( f : StreamPtr; Enc : TEncoding );

{ stream: methods }
Procedure StreamClose( f : StreamPtr );

{ stack: lookup }
Function StreamStackLookup( top : StreamPtr; Desc : TFileDescriptor; 
    Alias : TAlias; Path : TPath; Mode : TStreamMode; 
    Dev : TIODeviceType ) : StreamPtr;
Function StreamStackInputConsole( top : StreamPtr ) : StreamPtr;
Function StreamStackLookupByPath( top : StreamPtr; Path : TPath ) : StreamPtr;
Function StreamStackLookupByMode( top : StreamPtr; 
    Mode : TStreamMode ) : StreamPtr;
Function StreamStackLookupByDevice( top : StreamPtr; 
    Dev : TIODeviceType ) : StreamPtr;
Function StreamStackLookupByDescriptor( top : StreamPtr; 
    Desc : TFileDescriptor ) : StreamPtr;
Function StreamStackLookupByAlias( top : StreamPtr; Alias : TAlias ) : StreamPtr;

{ stack: top }
Function StreamStackCurrentInput( top : StreamPtr ) : StreamPtr;
Function StreamStackCurrentOutput( top : StreamPtr ) : StreamPtr;

{ stack: close }
Procedure StreamStackCloseAll( top : StreamPtr );

{ stack: edit }
Procedure StreamChain( f, g : StreamPtr );
Procedure StreamStackUnchain( Var top : StreamPtr; f : StreamPtr );
Procedure StreamStackPush( Var top : StreamPtr; f : StreamPtr );
Procedure StreamStackMoveToTop( Var top : StreamPtr; f : StreamPtr );

{ stream: write }
Procedure StreamDisplayErrorMessage( f : StreamPtr; msg : TString );
Procedure StreamFlush( f : StreamPtr );
Procedure StreamWriteShortString( f : StreamPtr; s : TString );

{ stream: read }
Procedure ClearInputFromStream( f : StreamPtr );
Procedure ReadLineFromKeyboard( f : StreamPtr );
Procedure CheckConsoleInputStream( f : StreamPtr; SkipSpaces : Boolean );
Function GetCharFromStream( f : StreamPtr; Var c : TChar ) : TChar;
Function GetCharNbFromStream( f : StreamPtr; Var c : TChar ) : TChar;
Procedure UngetCharsFromStream( f : StreamPtr; line : TLineNum; 
    col : TCharPos );
Procedure UngetCharFromStream( f : StreamPtr );
Function NextCharFromStream( f : StreamPtr; Var c : TChar ) : TChar;
Function NextNextCharFromStream( f : StreamPtr; Var c : TChar ) : TChar;
Procedure GetICharFromStream( f : StreamPtr; Var e : TIChar );
Procedure NextICharFromStream( f : StreamPtr; Var e : TIChar );

Implementation
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------}
{ file descriptor, buffer count                                         }
{-----------------------------------------------------------------------}

Var
  FreeDesc : TFileDescriptor; { next file descriptor to assign }
  BufferCount : PosInt;

Function GetNewStreamDescriptor : TFileDescriptor;
Begin
  GetNewStreamDescriptor := FreeDesc;
  FreeDesc := FreeDesc + 1
End;

{----------------------------------------------------------------------------}
{ mow-level helpers                                                          }
{----------------------------------------------------------------------------}

{ reset the read buffers of an input stream; must be done once before
 reading from a file stream, or before reading a line from the keyboard }
Procedure ResetIStream( f : StreamPtr );
Begin
  With f^ Do
  Begin
    FI_CHAR := 0;
    FI_CBUF := '';
    BufInit(FI_IBUF)
  End
End;

{ set the mode of stream f }
Procedure StreamSetMode( f : StreamPtr; Mode : TStreamMode );
Begin
  With f^ Do
  Begin
    FI_MODE := Mode;
    Case FI_MODE Of 
    MODE_READ:
      Begin
        FI_OPEN := OpenForRead(FI_PATH,FI_IFIL);
        ResetIStream(f)
      End;
    MODE_WRITE:
      Begin
        FI_OPEN := OpenForWrite(FI_PATH,FI_OFIL)
      End;
    MODE_NONE:
      Begin
        FI_OPEN := False
      End;
    End
  End
End;

{-----------------------------------------------------------------------}
{ constructor                                                           }
{-----------------------------------------------------------------------}

{ new stream }
Function NewStream( Alias : TAlias; Path : TPath; Dev : TIODeviceType;
    Mode : TStreamMode; Locked, WithDesc : Boolean ) : StreamPtr;
Var 
  f : StreamPtr;
  ptr : TObjectPtr Absolute f;
Begin
  ptr := NewRegisteredPObject(FI,SizeOf(TObjStream),2,False,0);
  With f^ Do
  Begin
    FI_PREV := Nil;
    FI_NEXT := Nil;
    If WithDesc Then
      FI_DESC := GetNewStreamDescriptor
    Else
      FI_DESC := 0;
    FI_ALIA := Alias;
    FI_PATH := Path;
    FI_TYPE := Dev;
    FI_OPEN := FI_TYPE = DEV_TERMINAL; { a terminal is always open }
    FI_LOCK := Locked;
    FI_CHAR := 0;
    FI_ENCO := UNDECIDED;
  End;
  StreamSetMode(f,Mode);
  NewStream := f
End;

{ create a new buffer, w/o a mode, which thus will not be returned as the 
 current input or output stream; in the stack, several buffer with the same
 name can coexist; for now, we simulate buffers with files, so we have to
 generate a unique buffer file name for each new buffer; WARNING: those fake
 file names can no more be used as user file names }
Function NewBuffer : StreamPtr;
Var
  Path : TPath;
Begin
  BufferCount := BufferCount + 1;
  Path := BUFFER_ALIAS + '-' + PosIntToStr(BufferCount) + '.txt';
  NewBuffer := NewStream(BUFFER_ALIAS,Path,DEV_BUFFER,MODE_NONE,False,False)
End;

{ create a new console in mode Mode }
Function NewConsole( Mode : TStreamMode ) : StreamPtr;
Begin
  NewConsole := NewStream(CONSOLE_ALIAS,CONSOLE_ALIAS,DEV_TERMINAL,Mode,False,
      False)
End;

{-----------------------------------------------------------------------}
{ stack management                                                      }
{-----------------------------------------------------------------------}

{ previous stream, upper in the stack }
Function StreamPrev( f : StreamPtr ) : StreamPtr;
Begin
  With f^ Do
    StreamPrev := FI_PREV
End;

{ set g to be the previous stream of f }
Procedure StreamSetPrev( f,g : StreamPtr );
Begin
  With f^ Do
    FI_PREV := g
End;

{ next stream, lower in the stack }
Function StreamNext( f : StreamPtr ) : StreamPtr;
Begin
  With f^ Do
    StreamNext := FI_NEXT
End;

{ set g to be the next stream of f }
Procedure StreamSetNext( f,g : StreamPtr );
Begin
  With f^ Do
    FI_NEXT := g
End;

{ chain stream f to stream g, so we have f <--> g where f is higher in the 
 stack }
Procedure StreamChain( f, g : StreamPtr );
Begin
  StreamSetPrev(g,f);
  StreamSetNext(f,g)
End;

{ take f off chain }
Procedure StreamUnlink( f : StreamPtr );
Begin
  StreamSetPrev(f,Nil);
  StreamSetNext(f,Nil)
End;

{ unchain stream f, so we go from g <--> f <--> h to g <--> h; take f off 
 chain top; update the top of the list when necessary, that is, when f was 
 on top }
Procedure StreamStackUnchain( Var top : StreamPtr; f : StreamPtr );
Begin
  If f = top Then { Case 1: f was on top }
  Begin
    top := StreamNext(f);
    If top <> Nil Then
      StreamSetPrev(top,Nil)
  End
  Else If StreamNext(f) = Nil Then { Case 2: f was at the bottom }
    StreamSetNext(StreamPrev(f),Nil)
  Else
  Begin { Case 3: f was between the top and bottom of the chain }
    StreamSetNext(StreamPrev(f),StreamNext(f));
    StreamSetPrev(StreamNext(f),StreamPrev(f))
  End;
  StreamUnlink(f)
End;

{ push a stream to new top; update top }
Procedure StreamStackPush( Var top : StreamPtr; f : StreamPtr );
Begin
  StreamChain(f,top);
  top := f
End;

{ set a stream (already part of the list) to be the new top; update top }
Procedure StreamStackMoveToTop( Var top : StreamPtr; f : StreamPtr );
Begin
  If f = top Then { f already on top }
    Exit;
  StreamStackUnchain(top,f);
  StreamStackPush(top,f)
End;

{-----------------------------------------------------------------------}
{ stream: get / set                                                     }
{-----------------------------------------------------------------------}

{ is a stream open? }
Function StreamIsOpen( f : StreamPtr ) : Boolean;
Begin
  With f^ Do
    StreamIsOpen := FI_OPEN
End;

{ set open state }
Procedure StreamSetOpen( f : StreamPtr; IsOpen : Boolean );
Begin
  With f^ Do
    FI_OPEN := IsOpen
End;

{ is an input stream protected? When a stream is protected, the user cannot
 do anything on it (open, close, etc.) Prolog programs read through 'insert' 
 must be protected }
Function StreamIsLocked( f : StreamPtr ) : Boolean;
Begin
  With f^ Do
    StreamIsLocked := FI_LOCK
End;

{ get file mode }
Function StreamMode( f : StreamPtr ) : TStreamMode;
Begin
  With f^ Do
    StreamMode := FI_MODE
End;

{ get file descriptor }
Function StreamDescriptor( f : StreamPtr ) : TFileDescriptor;
Begin
  With f^ Do
    StreamDescriptor := FI_DESC
End;

{ get file alias }
Function StreamAlias( f : StreamPtr ) : TAlias;
Begin
  With f^ Do
    StreamAlias := FI_ALIA
End;

{ get file path }
Function StreamPath( f : StreamPtr ) : TPath;
Begin
  With f^ Do
    StreamPath := FI_PATH
End;

{ get device type }
Function StreamDeviceType( f : StreamPtr ) : TIODeviceType;
Begin
  With f^ Do
    StreamDeviceType := FI_TYPE
End;

{ true if f is a terminal }
Function StreamIsConsole( f : StreamPtr ) : Boolean;
Begin
  StreamIsConsole := StreamDeviceType(f) = DEV_TERMINAL
End;

{ get encoding }
Function StreamEncoding( f : StreamPtr ) : TEncoding;
Begin
  With f^ Do
    StreamEncoding := FI_ENCO
End;

{ set encoding }
Procedure StreamSetEncoding( f : StreamPtr; Enc : TEncoding );
Begin
  With f^ Do
    FI_ENCO := Enc
End;

{ return true if there is no more chars available for reading in f's buffer
 system }
Function StreamIsDry( f : StreamPtr ) : Boolean;
Begin
  With f^ Do
  Begin
    CheckCondition(FI_OPEN,'StreamIsDry: file is closed');
    StreamIsDry := (BufNbUnread(FI_IBUF) = 0) And (Length(FI_CBUF) = 0)
  End
End;


{----------------------------------------------------------------------------}
{ close                                                                      }
{----------------------------------------------------------------------------}

{ close an stream; never close a console }
Procedure StreamClose( f : StreamPtr );
Begin
  If StreamIsOpen(f) And Not StreamIsConsole(f) Then
  Begin
    Case StreamMode(f) Of
    MODE_READ:
      CloseIFile(StreamPath(f),f^.FI_IFIL);
    MODE_WRITE:
      CloseOFile(StreamPath(f),f^.FI_OFIL);
    MODE_NONE:
      Bug('StreamClose: open buffer with no mode ');
    End;
    StreamSetOpen(f,False)
  End
End;

{----------------------------------------------------------------------------}
{ error message                                                              }
{----------------------------------------------------------------------------}

{ write an error message on Crt, pointing a cursor to the error }
Procedure StreamDisplayErrorMessage( f : StreamPtr; msg : TString );
Var 
  HasRead : Boolean; { buffer contains read chars }
  e : TIChar; { last char read }
  i : TBufIndex;
  n : TBufIndex;
Begin
  With f^ Do
  Begin
    HasRead := BufNbRead(FI_IBUF) > 0;
    If Not HasRead Then
    Begin
      CWrite('Error: ' + msg);
      CWriteLn
    End
    Else
    Begin
      BufGetLastRead(e,FI_IBUF);
      Case FI_TYPE Of
      DEV_FILE:
        CWrite('Error line ' + IntToStr(e.Lnb) + ', position ' + IntToStr(e.Pos) + ': ' + msg);
      DEV_TERMINAL:
        CWrite('Error at position ' + IntToStr(e.Pos) + ': ' + msg)
      End;
      CWriteLn;
      n := BufDisplayLine(FI_IBUF,(CrtScreenWidth div 3) * 2);
      If n > 0 Then
      Begin
        For i := 1 to n-1 Do
          CWrite(' ');
        CWrite('^');
        CWriteLn
      End
    End
  End
End;

{-----------------------------------------------------------------------}
{ stack: reset                                                          }
{-----------------------------------------------------------------------}

{ close all opened, non-terminal files; note that buffers will be closed,
 which results in an invalid PII program state; a reset of the stack should
follow any call to this }
Procedure StreamStackCloseAll( top : StreamPtr );
Var
  f : StreamPtr;
Begin
  f := top;
  While f <> Nil Do
  Begin
    If StreamIsOpen(f) And Not StreamIsConsole(f) Then
      StreamClose(f);
    f := StreamNext(f)
  End
End;

{-----------------------------------------------------------------------}
{ stack: lookup                                                         }
{-----------------------------------------------------------------------}

{ lookup from stream top, combining five search criteria:
 - descriptor if Desc is not zero
 - alias if Alias is not empty
 - file path if Path is not empty
 - mode if Mode is not MODE_ANY
 - dev device type if Dev is not DEV_ANY
 return Nil if the entry is not in the stack }
Function StreamStackLookup( top : StreamPtr; Desc : TFileDescriptor; 
    Alias : TAlias; Path : TPath; Mode : TStreamMode; 
    Dev : TIODeviceType ) : StreamPtr;
Var
  f : StreamPtr;
Begin
  f := top;
  While f <> Nil Do
  Begin
    If ((Desc = 0) Or (StreamDescriptor(f) = Desc)) And
      ((Alias = '') Or (StreamAlias(f) = Alias)) And
      ((Path = '') Or (StreamPath(f) = Path)) And
      ((Mode = MODE_ANY) Or (StreamMode(f) = Mode)) And
      ((Dev = DEV_ANY) Or (StreamDeviceType(f) = Dev)) Then
    Begin
      StreamStackLookup := f;
      Exit
    End;
    f := StreamNext(f)
  End;
  StreamStackLookup := f
End;

{ return the input console }
Function StreamStackInputConsole( top : StreamPtr ) : StreamPtr;
Begin
  StreamStackInputConsole := StreamStackLookup(top,0,'','',MODE_READ,DEV_TERMINAL)
End;

{ return the first stream having a given path }
Function StreamStackLookupByPath( top : StreamPtr; Path : TPath ) : StreamPtr;
Begin
  StreamStackLookupByPath := StreamStackLookup(top,0,'',Path,MODE_ANY,DEV_ANY)
End;

{ return the first stream having a given mode }
Function StreamStackLookupByMode( top : StreamPtr; 
    Mode : TStreamMode ) : StreamPtr;
Begin
  StreamStackLookupByMode := StreamStackLookup(top,0,'','',Mode,DEV_ANY)
End;

{ return the first stream having a device type }
Function StreamStackLookupByDevice( top : StreamPtr; 
    Dev : TIODeviceType ) : StreamPtr;
Begin
  StreamStackLookupByDevice := StreamStackLookup(top,0,'','',MODE_ANY,Dev)
End;

{ return the stream having descriptor Desc in the list top, or Nil }
Function StreamStackLookupByDescriptor( top : StreamPtr; 
    Desc : TFileDescriptor ) : StreamPtr;
Begin
  StreamStackLookupByDescriptor := StreamStackLookup(top,Desc,'','',MODE_ANY,DEV_ANY)
End;

{ return the stream having descriptor Desc in the list top, or Nil }
Function StreamStackLookupByAlias( top : StreamPtr; Alias : TAlias ) : StreamPtr;
Begin
  StreamStackLookupByAlias := StreamStackLookup(top,0,Alias,'',MODE_ANY,DEV_ANY)
End;

{----------------------------------------------------------------------------}
{ current i/o                                                                }
{----------------------------------------------------------------------------}

{ return the current input stream }
Function StreamStackCurrentInput( top : StreamPtr ) : StreamPtr;
Var
  f : StreamPtr;
Begin
  f := StreamStackLookupByMode(top,MODE_READ);
  CheckCondition(f <> Nil,'StreamStackCurrentInput: no input stream');
  StreamStackCurrentInput := f
End;

{ return the current output stream }
Function StreamStackCurrentOutput( top : StreamPtr ) : StreamPtr;
Var
  f : StreamPtr;
Begin
  f := StreamStackLookupByMode(top,MODE_WRITE);
  CheckCondition(f <> Nil,'StreamStackCurrentOutput: no output stream');
  StreamStackCurrentOutput := f
End;

{----------------------------------------------------------------------------}
{ get: buffered access                                                       }
{----------------------------------------------------------------------------}

{ replenish the small char buffer FI_CBUF as much as possible }
Procedure GetCharsFromStream( f : StreamPtr );
Var
  c : Char;
Begin
  With f^ Do
  Begin
    While Not Error And FI_OPEN And (Length(FI_CBUF) < MaxBytesPerChar) Do
    Begin
      If Eof(FI_IFIL) Then
        StreamClose(f)
      Else If ReadFromFile(FI_PATH,FI_IFIL,c) Then
      Begin
        FI_CBUF := FI_CBUF + c;
        FI_CHAR := FI_CHAR + 1
      End
      Else
        RuntimeError('i/o error when reading from "' + FI_PATH + '"')
    End
  End
End;

{----------------------------------------------------------------------------}
{ read using circular buffer                                                 }
{----------------------------------------------------------------------------}

{ clear the input buffer of an input stream }
Procedure ClearInputFromStream( f : StreamPtr );
Begin
  With f^ Do
    BufDiscardUnread(FI_IBUF)
End;

{ input one line from the keyboard and store it into the char buffer; note 
 that the buffer is not *reset* }
Procedure ReadLineFromKeyboard( f : StreamPtr );
Begin
  ResetIStream(f);
  With f^ Do
    ReadLnKbd(FI_IBUF,FI_ENCO)
End;

{ move the read cursor backward by one }
Procedure UngetCharFromStream( f : StreamPtr );
Begin
  With f^ Do
    BufUnread(FI_IBUF)
End;

{ move the read cursor backward up to (and including) a certain line and 
 column number }
Procedure UngetCharsFromStream( f : StreamPtr; line : TLineNum; 
    col : TCharPos );
Var 
  e : TIChar;
  Found : Boolean;
Begin
  With f^ Do
  Begin
    Found := False;
    While Not Found And Not Error Do
    Begin
      If BufNbRead(FI_IBUF) = 0 Then
        RuntimeError('parsing limit reached: lookup too ahead to handle');
      If Error Then Exit;
      BufGetRead(e,FI_IBUF,0);
      CheckCondition((e.Lnb > line) Or (e.Lnb = line) And (E.Pos >= col), 
          'UngetCharsFromStream: target char disappeared!');
      Found := (e.Lnb = line) And (E.Pos = col);
      BufUnread(FI_IBUF)
    End
  End
End;

{ read a codepoint; if the end of the file has been reached or the codepoint 
 cannot be set, return EndOfInput; to test for the later case, the caller must 
 check Error; replace CR, LF, and CRLF with NewLine; return EndOfInput when
 the stream cannot return additional codepoints; Note to self: make sure all
 the code paths end with a call to BufRead, otherwise a subsequent call to 
 BufUnread with create a bug, as it will unread a different codepoint }
Procedure ReadCodepointFromStream( f : StreamPtr; Var e : TIChar );
Var
  cc : TChar;
Begin
  With f^ Do
  Begin
    { no more unread codepoints available: try to replenish the buffer }
    If BufNbUnread(FI_IBUF) = 0 Then
    Begin
      { make room for one additional codepoint }
      If BufNbFree(FI_IBUF) = 0 Then
        BufDiscard(FI_IBUF,1);
      Case FI_TYPE Of
      DEV_FILE :
        Begin
          GetCharsFromStream(f); { fill up FI_CBUF }
          cc := EndOfInput;
          If Length(FI_CBUF) > 0 Then
            If CodePointWithNewLine(FI_CBUF,cc,FI_ENCO) Then
              Pass;
          BufAppendTChar(FI_IBUF,cc)
        End;
      DEV_TERMINAL:
        BufAppendTChar(FI_IBUF,EndOfInput);
      End
    End;

    { read the next codepoint }
    BufRead(e,FI_IBUF)
  End
End;

{ read one codepoint with position }
Procedure GetICharFromStream( f : StreamPtr; Var e : TIChar );
Begin
  SetIChar(e,'',0,0);
  ReadCodepointFromStream(f,e)
End;

{ read one codepoint }
Function GetCharFromStream( f : StreamPtr; Var c : TChar ) : TChar;
Var
  e : TIChar;
Begin
  GetCharFromStream := '';
  c := '';
  GetICharFromStream(f,e);
  If Error Then Exit;
  c := e.Val;
  GetCharFromStream := c
End;

{ read the next non-blank codepoint }
Function GetCharNbFromStream( f : StreamPtr; Var c : TChar ) : TChar;
Begin
  GetCharNbFromStream := '';
  Repeat 
    c := GetCharFromStream(f,c);
    If Error Then Exit
  Until Not (c[1] In [' ',#9,#13,NewLine]) Or Error;
  If Error Then Exit;
  GetCharNbFromStream := c
End;

{ return the next codepoint with position, without consuming it }
Procedure NextICharFromStream( f : StreamPtr; Var e : TIChar );
Begin
  GetICharFromStream(f,e);
  If Error Then Exit;
  UngetCharFromStream(f)
End;

{ return the next codepoint without consuming it }
Function NextCharFromStream( f : StreamPtr; Var c : TChar ) : TChar;
Var
  e : TIChar;
Begin
  NextCharFromStream := '';
  c := '';
  NextICharFromStream(f,e);
  If Error Then Exit;
  c := e.Val;
  NextCharFromStream := c
End;

{ ditto but two codepoints in advance }
Function NextNextCharFromStream( f : StreamPtr; Var c : TChar ) : TChar;
Begin
  NextNextCharFromStream := '';
  c := GetCharFromStream(f,c);
  If Error Then Exit;
  c := GetCharFromStream(f,c);
  If Error Then Exit;
  UngetCharFromStream(f);
  UngetCharFromStream(f);
  NextNextCharFromStream := c
End;

{ if f is a console, then read a line (from the keyboard) if no more characters 
 are available in the input line; optionally skip spaces beforehand; skipping 
 spaces is useful when reading a terms with in(t); this is the opposite when 
 reading a char (in_char) or line (inl); }
Procedure CheckConsoleInputStream( f : StreamPtr; SkipSpaces : Boolean );
Var 
  c : TChar;
Begin
  If StreamIsConsole(f) Then
  Begin
    If SkipSpaces Then
    Begin
      c := GetCharNbFromStream(f,c);
      If Error Then Exit;
      If c = EndOfInput Then
        ResetIStream(f)
    End;
    If StreamIsDry(f) Then
      ReadLineFromKeyboard(f)
  End
End;

{----------------------------------------------------------------------------}
{ write                                                                      }
{----------------------------------------------------------------------------}

{ flush an output stream }
Procedure StreamFlush( f : StreamPtr );
Begin
  With f^ Do
    If FI_OPEN And (FI_TYPE = DEV_FILE) Then
      FlushFile(FI_PATH,FI_OFIL)
End;

{ write a short string to an output stream }
Procedure StreamWriteShortString( f : StreamPtr; s : TString );
Begin
  With f^ Do
    Case FI_TYPE Of
      DEV_TERMINAL:
        CWrite(s);
      DEV_FILE,DEV_BUFFER:
        WriteToFile(FI_PATH,FI_OFIL,s)
    End
End;

{----------------------------------------------------------------------------}
{ DEBUG                                                                      }
{----------------------------------------------------------------------------}

Procedure StreamDump( f : StreamPtr );
Begin
  With f^ Do
  Begin
    WriteToEchoFile('State of stream #' + PosIntToStr(FI_DESC));
    WriteToEchoFile(CRLF);
    WriteToEchoFile(' FI_ALIA: ' + FI_ALIA);
    WriteToEchoFile(CRLF);
    WriteToEchoFile(' Path: ' + FI_PATH);
    WriteToEchoFile(CRLF);
    WriteToEchoFile(' FI_IBUF: ');
    BufDump(FI_IBUF);
    WriteToEchoFile(CRLF);
    WriteToEchoFile(' FI_CBUF: ');
    CharDump(FI_CBUF);
    WriteToEchoFile(CRLF)
  End
End;

Begin
  FreeDesc := 1;
  BufferCount := 0
End.