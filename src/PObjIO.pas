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
{$I define.inc }

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
  Files,
  Trace,
  Crt2,
  CLI,
  IChar,
  Buffer,
  CWrites,
  Common,
  Memory,
  PObj,
  PObjStr;

Type
  TFileDescriptor = PosInt; { file descriptor }
  TAlias = StrPtr; { file name (a.k.a. "alias") }
  TPath = StrPtr; { full path }
  TIODeviceType = (DEV_FILE,DEV_BUFFER,DEV_TERMINAL,DEV_ANY);
  TStreamMode = (MODE_READ,MODE_WRITE,MODE_NONE,MODE_ANY);

{ stack of input /output streams }
Type
  StreamPtr = ^TObjStream;
  TObjStream = Record
    PO_META : TObjMeta;
    { not deep copied: }
    FI_PREV : StreamPtr;             { previous file (upper in the stack)      }
    FI_NEXT : StreamPtr;             { next file (lower in the stack)          }
    FI_ALIA : TAlias;                { name of the stream                      }
    FI_PATH : TPath;                 { file's full path                        }
    { extra data: }
    FI_DESC : TFileDescriptor;       { unique number: 1,2,...                  }
    FI_TYPE : TIODeviceType;         { file, buffer, or console?               }
    FI_OPEN : Boolean;               { is the stream ready for i/o?            }
    FI_LOCK : Boolean;               { user is not allowed to close atm        }
    FI_CHAR : LongInt;               { number of bytes read/written so far     }
    FI_ENCO : TEncoding;             { character encoding of the stream        }
    Case FI_MODE : TStreamMode Of    { opening mode: read / write              }
      MODE_READ: (
        FI_IFIL : TIFile;            { file handler (if not a console)         }
        FI_CBUF : TCharBytes;        { 1 multi-byte char input buffer          }
        FI_IBUF : TBuf);             { input buffer                            }
      MODE_WRITE : (
        FI_OFIL : TOFile);           { file handler (if not a console)         }
      MODE_NONE : ();
  End;

{ path }
Function Path_IsAbsolute( Filename : TPath ) : Boolean;
Function Path_ExtractPath( Filename : TPath ) : TPath;

{ constructors }
Function Stream_New( Alias : TAlias; Path : TPath; Dev : TIODeviceType;
    Mode : TStreamMode; Locked, WithDesc : Boolean ) : StreamPtr;
Function Stream_NewBuffer( Alias : TAlias ) : StreamPtr;
Function Stream_NewConsole( Alias : TAlias; Mode : TStreamMode ) : StreamPtr;

{ stream: get / set }
Function Stream_GetPath( f : StreamPtr ) : TPath;
Function Stream_GetShortPath( f : StreamPtr ) : TShortPath;
Function Stream_GetAlias( f : StreamPtr ) : TAlias;
Function Stream_GetDeviceType( f : StreamPtr ) : TIODeviceType;
Function Stream_IsConsole( f : StreamPtr ) : Boolean;
Function Stream_IsLocked( f : StreamPtr ) : Boolean;
Function Stream_IsOpen( f : StreamPtr ) : Boolean;
Function Stream_GetMode( f : StreamPtr ) : TStreamMode;
Procedure Stream_SetMode( f : StreamPtr; Mode : TStreamMode );
Function Stream_GetDescriptor( f : StreamPtr ) : TFileDescriptor;
Function Stream_GetEncoding( f : StreamPtr ) : TEncoding;
Procedure Stream_SetEncoding( f : StreamPtr; Enc : TEncoding );

{ stream: methods }
Procedure Stream_Close( f : StreamPtr );

{ stream: write }
Procedure Stream_DisplayErrorMessage( f : StreamPtr; msg : TString );
Procedure Stream_Flush( f : StreamPtr );
Procedure Stream_WriteShortString( f : StreamPtr; s : TString );
Procedure Stream_WritelnShortString( f : StreamPtr; s : TString );
Procedure Stream_Writeln( f : StreamPtr );

{ stream: read }
Procedure Stream_ClearInput( f : StreamPtr );
Procedure Stream_ReadLineFromKeyboard( f : StreamPtr );
Procedure Stream_CheckConsoleInput( f : StreamPtr; SkipSpaces : Boolean );
Procedure Stream_GetChar( f : StreamPtr; Var c : TChar );
Procedure Stream_GetCharNb( f : StreamPtr; Var c : TChar );
Procedure Stream_UngetChars( f : StreamPtr; line : TLineNum; 
    col : TCharPos );
Procedure Stream_UngetChar( f : StreamPtr );
Procedure Stream_NextChar( f : StreamPtr; Var c : TChar );
Procedure Stream_NextNextChar( f : StreamPtr; Var c : TChar );
Procedure Stream_GetIChar( f : StreamPtr; Var e : TIChar );
Procedure Stream_NextIChar( f : StreamPtr; Var e : TIChar );
Function Stream_NewStr( f : StreamPtr ) : StrPtr;

{ write }
Procedure Stream_WriteStr( f : StreamPtr; s : StrPtr) ;
Procedure Stream_WriteLnStr( f : StreamPtr; s : StrPtr) ;

{ stack: lookup }
Function Streams_Lookup( top : StreamPtr; Desc : TFileDescriptor; 
    Alias : TAlias; Path : TPath; Mode : TStreamMode; 
    Dev : TIODeviceType ) : StreamPtr;
Function Streams_InputConsole( top : StreamPtr ) : StreamPtr;
Function Streams_OutputConsole( top : StreamPtr ) : StreamPtr;
Function Streams_LookupByPath( top : StreamPtr; Path : TPath ) : StreamPtr;
Function Streams_LookupByMode( top : StreamPtr; 
    Mode : TStreamMode ) : StreamPtr;
Function Streams_LookupByDevice( top : StreamPtr; 
    Dev : TIODeviceType ) : StreamPtr;
Function Streams_LookupByDescriptor( top : StreamPtr; 
    Desc : TFileDescriptor ) : StreamPtr;
Function Streams_LookupByAlias( top : StreamPtr; Alias : TAlias ) : StreamPtr;

{ stack: top }
Function Streams_CurrentInput( top : StreamPtr ) : StreamPtr;
Function Streams_CurrentOutput( top : StreamPtr ) : StreamPtr;

{ stack: close }
Procedure Streams_CloseAll( top : StreamPtr );

{ stack: edit }
Procedure Streams_Chain( f, g : StreamPtr );
Procedure Streams_Unchain( Var top : StreamPtr; f : StreamPtr );
Procedure Streams_Push( Var top : StreamPtr; f : StreamPtr );
Procedure Streams_MoveToTop( Var top : StreamPtr; f : StreamPtr );

{ echo state }
Function Stream_GetEcho : Boolean;
Procedure Stream_SetEcho( state : Boolean );

Implementation
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------}
{ path                                                                  }
{-----------------------------------------------------------------------}

{ is a filename absolute? }
Function Path_IsAbsolute( Filename : TPath ) : Boolean;
Begin
  Path_IsAbsolute := Str_StartsWith(Filename,[GetDirectorySeparator,'/'])
End;

{ extract the path part of a filename; must mimic ExtractPath }
Function Path_ExtractPath( Filename : TPath ) : TPath;
Var
  Path : TPath;
Begin
  Path := Str_Clone(Filename);
  Str_DeleteLastCharUntil(Path,[GetDirectorySeparator,'/']);
  Path_ExtractPath := Path
End;


{-----------------------------------------------------------------------}
{ global "echo" state                                                   }
{-----------------------------------------------------------------------}

Var
  Echo : Boolean;

Function Stream_GetEcho : Boolean;
Begin
  Stream_GetEcho := Echo
End;

Procedure Stream_SetEcho( state : Boolean );
Begin
  Echo := State
End;

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
{ mow-level helpers (used by constructors)                                   }
{----------------------------------------------------------------------------}

{ reset the read buffers of an input stream; must be done once before
 reading from a file stream, or before reading a line from the keyboard }
Procedure Stream_ResetInput( f : StreamPtr );
Begin
  With f^ Do
  Begin
    FI_CHAR := 0;
    FI_CBUF := '';
    BufInit(FI_IBUF)
  End
End;

{ set the mode of stream f }
Procedure Stream_SetMode( f : StreamPtr; Mode : TStreamMode );
Begin
  With f^ Do
  Begin
    FI_MODE := Mode;
    Case FI_MODE Of 
    MODE_READ:
      Begin
        FI_OPEN := OpenForRead(Stream_GetShortPath(f),FI_IFIL);
        Stream_ResetInput(f)
      End;
    MODE_WRITE:
      Begin
        FI_OPEN := OpenForWrite(Stream_GetShortPath(f),FI_OFIL)
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
Function Stream_New( Alias : TAlias; Path : TPath; Dev : TIODeviceType;
    Mode : TStreamMode; Locked, WithDesc : Boolean ) : StreamPtr;
Var 
  f : StreamPtr;
  ptr : TObjectPtr Absolute f;
Begin
  ptr := NewRegisteredPObject(FI,SizeOf(TObjStream),4,False,0);
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
  Stream_SetMode(f,Mode);
  Stream_New := f
End;

{ create a new buffer, w/o a mode, which thus will not be returned as the 
 current input or output stream; in the stack, several buffer with the same
 name can coexist; for now, we simulate buffers with files, so we have to
 generate a unique buffer file name for each new buffer; WARNING: those fake
 file names can no more be used as user file names }
Function Stream_NewBuffer( Alias : TAlias ) : StreamPtr;
Var
  Path : TPath;
Begin
  BufferCount := BufferCount + 1;
  Path := Str_Clone(Alias);
  Str_Append(Path,'-' + PosIntToShortString(BufferCount) + '.txt');
  Stream_NewBuffer := Stream_New(Alias,Path,DEV_BUFFER,MODE_NONE,
      False,False)
End;

{ create a new console in mode Mode }
Function Stream_NewConsole( Alias : TAlias; Mode : TStreamMode ) : StreamPtr;
Var
  f : StreamPtr;
Begin
  f := Stream_New(Alias,Alias,DEV_TERMINAL,Mode,False,False);
  Stream_SetEncoding(f,GetSystemCEncoding);
  Stream_NewConsole := f
End;


{-----------------------------------------------------------------------}
{                                                                       }
{ individual streams                                                    }
{                                                                       }
{-----------------------------------------------------------------------}

{-----------------------------------------------------------------------}
{ stream: get / set                                                     }
{-----------------------------------------------------------------------}

{ is a stream open? }
Function Stream_IsOpen( f : StreamPtr ) : Boolean;
Begin
  With f^ Do
    Stream_IsOpen := FI_OPEN
End;

{ set open state }
Procedure Stream_SetOpen( f : StreamPtr; IsOpen : Boolean );
Begin
  With f^ Do
    FI_OPEN := IsOpen
End;

{ is an input stream protected? When a stream is protected, the user cannot
 do anything on it (open, close, etc.) Prolog programs read through 'insert' 
 must be protected }
Function Stream_IsLocked( f : StreamPtr ) : Boolean;
Begin
  With f^ Do
    Stream_IsLocked := FI_LOCK
End;

{ get file mode }
Function Stream_GetMode( f : StreamPtr ) : TStreamMode;
Begin
  With f^ Do
    Stream_GetMode := FI_MODE
End;

{ get file descriptor }
Function Stream_GetDescriptor( f : StreamPtr ) : TFileDescriptor;
Begin
  With f^ Do
    Stream_GetDescriptor := FI_DESC
End;

{ get file alias }
Function Stream_GetAlias( f : StreamPtr ) : TAlias;
Begin
  With f^ Do
    Stream_GetAlias := FI_ALIA
End;

{ get file alias as a Pascal string}
Function Stream_GetShortAlias( f : StreamPtr ) : TString;
Begin
  Stream_GetShortAlias := Str_AsShortString(Stream_GetAlias(f))
End;

{ get file path }
Function Stream_GetPath( f : StreamPtr ) : TPath;
Begin
  With f^ Do
    Stream_GetPath := FI_PATH
End;

{ get file path as a Pascal string }
Function Stream_GetShortPath( f : StreamPtr ) : TShortPath;
Begin
  Stream_GetShortPath := Str_AsShortString(Stream_GetPath(f))
End;

{ get device type }
Function Stream_GetDeviceType( f : StreamPtr ) : TIODeviceType;
Begin
  With f^ Do
    Stream_GetDeviceType := FI_TYPE
End;

{ true if f is a terminal }
Function Stream_IsConsole( f : StreamPtr ) : Boolean;
Begin
  Stream_IsConsole := Stream_GetDeviceType(f) = DEV_TERMINAL
End;

{ get encoding }
Function Stream_GetEncoding( f : StreamPtr ) : TEncoding;
Begin
  With f^ Do
    Stream_GetEncoding := FI_ENCO
End;

{ set encoding }
Procedure Stream_SetEncoding( f : StreamPtr; Enc : TEncoding );
Begin
  With f^ Do
    FI_ENCO := Enc
End;

{ return true if there is no more chars available for reading in f's buffer
 system }
Function Stream_IsDry( f : StreamPtr ) : Boolean;
Begin
  With f^ Do
  Begin
    CheckCondition(FI_OPEN,'Stream_IsDry: file is closed');
    Stream_IsDry := (BufNbUnread(FI_IBUF) = 0) And (Length(FI_CBUF) = 0)
  End
End;


{----------------------------------------------------------------------------}
{ close                                                                      }
{----------------------------------------------------------------------------}

{ close an stream; never close a console }
Procedure Stream_Close( f : StreamPtr );
Begin
  If Stream_IsOpen(f) And Not Stream_IsConsole(f) Then
  Begin
    Case Stream_GetMode(f) Of
    MODE_READ:
      CloseIFile(Stream_GetShortPath(f),f^.FI_IFIL);
    MODE_WRITE:
      CloseOFile(Stream_GetShortPath(f),f^.FI_OFIL);
    MODE_NONE:
      Bug('Stream_Close: open buffer with no mode ');
    End;
    Stream_SetOpen(f,False)
  End
End;

{----------------------------------------------------------------------------}
{ error message                                                              }
{----------------------------------------------------------------------------}

{ write an error message on Crt, pointing a cursor to the error }
Procedure Stream_DisplayErrorMessage( f : StreamPtr; msg : TString );
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
      Begin
        CWrite('In file: ''' + Stream_GetShortPath(f) + '''');
        CWriteLn;
        CWrite('Error line ' + IntToShortString(e.Lnb) + 
            ', position ' + IntToShortString(e.Pos) + ': ' + msg)
      End;
      DEV_TERMINAL:
        CWrite('Error at position ' + IntToShortString(e.Pos) + ': ' + msg)
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

{----------------------------------------------------------------------------}
{ get: buffered access                                                       }
{----------------------------------------------------------------------------}

{ replenish the small char buffer FI_CBUF as much as possible }
Procedure Stream_GetChars( f : StreamPtr );
Var
  c : Char;
Begin
  With f^ Do
  Begin
    While Not Error And FI_OPEN And (Length(FI_CBUF) < MaxBytesPerChar) Do
    Begin
      If Eof(FI_IFIL) Then
        Stream_Close(f)
      Else If ReadFromFile(Stream_GetShortPath(f),FI_IFIL,c) Then
      Begin
        FI_CBUF := FI_CBUF + c;
        FI_CHAR := FI_CHAR + 1
      End
      Else
        RuntimeError('i/o error when reading from "' + 
            Stream_GetShortPath(f) + '"')
    End
  End
End;

{----------------------------------------------------------------------------}
{ read using circular buffer                                                 }
{----------------------------------------------------------------------------}

{ clear the input buffer of an input stream }
Procedure Stream_ClearInput( f : StreamPtr );
Begin
  With f^ Do
    BufDiscardUnread(FI_IBUF)
End;

{ input one line from the keyboard and store it into the char buffer; note 
 that the buffer is not *reset* }
Procedure Stream_ReadLineFromKeyboard( f : StreamPtr );
Begin
  Stream_ResetInput(f);
  With f^ Do
    ReadLnKbd(FI_IBUF,FI_ENCO)
End;

{ move the read cursor backward by one }
Procedure Stream_UngetChar( f : StreamPtr );
Begin
  With f^ Do
    BufUnread(FI_IBUF)
End;

{ move the read cursor backward up to (and including) a certain line and 
 column number }
Procedure Stream_UngetChars( f : StreamPtr; line : TLineNum; 
    col : TCharPos );
Var 
  e : TIChar;
  Found : Boolean;
Begin
  With f^ Do
  Begin
    { are we already done? special case: beginning of input stream }
    If (BufNbRead(FI_IBUF) = 0) And (line = 1) And (col = 1) Then
      Exit;
    { are we already done? }
    BufGetRead(e,FI_IBUF,0);
    If (e.Lnb = line) And (E.Pos = col - 1) Then
      Exit;
    { we are not done: some characters must be unread }
    Found := False;
    While Not Error And Not Found Do
    Begin
      If BufNbRead(FI_IBUF) = 0 Then
        RuntimeError('parsing limit reached: lookup too ahead to handle');
      If Error Then Exit;
      BufGetRead(e,FI_IBUF,0);
      CheckCondition((e.Lnb > line) Or (e.Lnb = line) And (E.Pos >= col), 
          'Stream_UngetChars: target char disappeared!');
      Found := (e.Lnb = line) And (E.Pos = col);
      BufUnread(FI_IBUF)
    End
  End
End;

{ read a TChar; if the end of the file has been reached or the TChar 
 cannot be set, return EndOfInput; to test for the later case, the caller must 
 check Error; replace CR, LF, and CRLF with NewLine; return EndOfInput when
 the stream cannot return additional TChars; Note to self: make sure all
 the code paths end with a call to BufRead, otherwise a subsequent call to 
 BufUnread with create a bug, as it will unread a different TChar }
Procedure Stream_ReadTChar( f : StreamPtr; Var e : TIChar );
Var
  cc : TChar;
Begin
  With f^ Do
  Begin
    { no more unread TChars available: try to replenish the buffer }
    If BufNbUnread(FI_IBUF) = 0 Then
    Begin
      { make room for one additional TChar }
      If BufNbFree(FI_IBUF) = 0 Then
        BufDiscard(FI_IBUF,1);
      Case FI_TYPE Of
      DEV_FILE,DEV_BUFFER: { for now, a buffer is a file }
        Begin
          Stream_GetChars(f); { fill up FI_CBUF }
          ASCIIChar(cc,EndOfInput);
          If Length(FI_CBUF) > 0 Then
            If GetOneTCharNL(FI_CBUF,cc,FI_ENCO) Then
              If Stream_GetEcho Then
                CWriteChar(cc);
          BufPushChar(FI_IBUF,cc)
        End;
      DEV_TERMINAL:
        Begin
          ASCIIChar(cc,EndOfInput);
          BufPushChar(FI_IBUF,cc)
        End
      End
    End;

    { read the next TChar }
    BufRead(e,FI_IBUF)
  End
End;

{ read one TChar with position }
Procedure Stream_GetIChar( f : StreamPtr; Var e : TIChar );
Var
  cc : TChar;
Begin
  ASCIIChar(cc,' ');
  SetIChar(e,cc,0,0);
  Stream_ReadTChar(f,e)
End;

{ read one TChar }
Procedure Stream_GetChar( f : StreamPtr; Var c : TChar );
Var
  e : TIChar;
Begin
  Stream_GetIChar(f,e);
  If Error Then Exit;
  c := e.Val
End;

{ read the next non-blank TChar }
Procedure Stream_GetCharNb( f : StreamPtr; Var c : TChar );
Begin
  Repeat 
    Stream_GetChar(f,c);
    If Error Then Exit
  Until Not IsIn(c,[' ',#9,#13,NewLine])
End;

{ return the next TChar with position, without consuming it }
Procedure Stream_NextIChar( f : StreamPtr; Var e : TIChar );
Begin
  Stream_GetIChar(f,e);
  If Error Then Exit;
  Stream_UngetChar(f)
End;

{ return the next TChar without consuming it }
Procedure Stream_NextChar( f : StreamPtr; Var c : TChar );
Var
  e : TIChar;
Begin
  Stream_NextIChar(f,e);
  If Error Then Exit;
  c := e.Val
End;

{ ditto but two TChar in advance }
Procedure Stream_NextNextChar( f : StreamPtr; Var c : TChar );
Begin
  Stream_GetChar(f,c);
  If Error Then Exit;
  Stream_GetChar(f,c);
  If Error Then Exit;
  Stream_UngetChar(f);
  Stream_UngetChar(f)
End;

{ if f is a console, then read a line (from the keyboard) if no more characters 
 are available in the input line; optionally skip spaces beforehand; skipping 
 spaces is useful when reading a terms with in(t); this is the opposite when 
 reading a char (in_char) or line (inl); }
Procedure Stream_CheckConsoleInput( f : StreamPtr; SkipSpaces : Boolean );
Var 
  cc : TChar;
Begin
  If Stream_IsConsole(f) Then
  Begin
    If SkipSpaces Then
    Begin
      Stream_GetCharNb(f,cc);
      If Error Then Exit;
      If cc.Bytes = EndOfInput Then
        Stream_ResetInput(f)
      Else
        Stream_UngetChar(f); { put back the non-blank character }
    End;
    If Stream_IsDry(f) Then
      Stream_ReadLineFromKeyboard(f)
  End
End;

{ new string whose context is steam f, meaning that the string will be either 
 read from or written to f (or a console or a file having the same encoding 
 as f) }
Function Stream_NewStr( f : StreamPtr ) : StrPtr;
Begin
  Stream_NewStr := Str_New(Stream_GetEncoding(f))
End;


{----------------------------------------------------------------------------}
{ write                                                                      }
{----------------------------------------------------------------------------}

{-----------------------------------------------------------------------}
{ methods: write to file                                                }
{-----------------------------------------------------------------------}

{ write a string to a stream }
Procedure Stream_WriteStr( f : StreamPtr; s : StrPtr ) ;
Var
  Iter : StrIter;
  cc : TChar;
Begin
  StrIter_ToStart(Iter,s);
  While StrIter_NextChar(Iter,cc) Do 
    Stream_WriteShortString(f,cc.Bytes)
End;

{ writeln a string to a stream }
Procedure Stream_WriteLnStr( f : StreamPtr; s : StrPtr ) ;
Begin
  Stream_WriteStr(f,s);
  Stream_WritelnShortString(f,'')
End;


{ flush an output stream }
Procedure Stream_Flush( f : StreamPtr );
Begin
  With f^ Do
    If FI_OPEN And (FI_TYPE = DEV_FILE) Then
      FlushFile(Stream_GetShortPath(f),FI_OFIL)
End;

{ write a short string to an output stream }
Procedure Stream_WriteShortString( f : StreamPtr; s : TString );
Begin
  With f^ Do
    Case FI_TYPE Of
      DEV_TERMINAL:
        CWrite(s);
      DEV_FILE,DEV_BUFFER:
        WriteToFile(Stream_GetShortPath(f),FI_OFIL,s)
    End
End;

{ writeln a short string to an output stream }
Procedure Stream_WritelnShortString( f : StreamPtr; s : TString );
Begin
  With f^ Do
    Case FI_TYPE Of
      DEV_TERMINAL:
        Begin
          CWrite(s);
          CWriteLn
        End;
      DEV_FILE,DEV_BUFFER:
        WritelnToFile(Stream_GetShortPath(f),FI_OFIL,s)
    End
End;

{ writeln a new line to an output stream }
Procedure Stream_Writeln( f : StreamPtr );
Begin
  Stream_WritelnShortString(f,'')
End;

{----------------------------------------------------------------------------}
{ Debug                                                                      }
{----------------------------------------------------------------------------}

Procedure Stream_Dump( f : StreamPtr );
Begin
  With f^ Do
  Begin
    WritelnToTraceFile('State of stream #' + PosIntToShortString(FI_DESC));
    WritelnToTraceFile(' FI_ALIA: ' + Stream_GetShortAlias(f));
    WritelnToTraceFile(' Path: ' + Stream_GetShortPath(f));
    WritelnToTraceFile(' FI_IBUF: ');
    BufDump(FI_IBUF);
    WritelnToTraceFile('');
    WritelnToTraceFile(' FI_CBUF: ');
    WritelnToTraceFile(FI_CBUF);
    WritelnToTraceFile('')
  End
End;


{-----------------------------------------------------------------------}
{                                                                       }
{ lists of streams                                                      }
{                                                                       }
{-----------------------------------------------------------------------}

{-----------------------------------------------------------------------}
{ list of streams: stack management                                     }
{-----------------------------------------------------------------------}

{ previous stream, upper in the stack }
Function Streams_GetPrev( f : StreamPtr ) : StreamPtr;
Begin
  With f^ Do
    Streams_GetPrev := FI_PREV
End;

{ set g to be the previous stream of f }
Procedure Streams_SetPrev( f,g : StreamPtr );
Begin
  With f^ Do
    FI_PREV := g
End;

{ next stream, lower in the stack }
Function Streams_GetNext( f : StreamPtr ) : StreamPtr;
Begin
  With f^ Do
    Streams_GetNext := FI_NEXT
End;

{ set g to be the next stream of f }
Procedure Streams_SetNext( f,g : StreamPtr );
Begin
  With f^ Do
    FI_NEXT := g
End;

{ chain stream f to stream g, so we have f <--> g where f is higher in the 
 stack }
Procedure Streams_Chain( f, g : StreamPtr );
Begin
  Streams_SetPrev(g,f);
  Streams_SetNext(f,g)
End;

{ take f off chain }
Procedure Streams_Unlink( f : StreamPtr );
Begin
  Streams_SetPrev(f,Nil);
  Streams_SetNext(f,Nil)
End;

{ unchain stream f, so we go from g <--> f <--> h to g <--> h; take f off 
 chain top; update the top of the list when necessary, that is, when f was 
 on top }
Procedure Streams_Unchain( Var top : StreamPtr; f : StreamPtr );
Begin
  If f = top Then { Case 1: f was on top }
  Begin
    top := Streams_GetNext(f);
    If top <> Nil Then
      Streams_SetPrev(top,Nil)
  End
  Else If Streams_GetNext(f) = Nil Then { Case 2: f was at the bottom }
    Streams_SetNext(Streams_GetPrev(f),Nil)
  Else
  Begin { Case 3: f was between the top and bottom of the chain }
    Streams_SetNext(Streams_GetPrev(f),Streams_GetNext(f));
    Streams_SetPrev(Streams_GetNext(f),Streams_GetPrev(f))
  End;
  Streams_Unlink(f)
End;

{ push a stream to new top; update top }
Procedure Streams_Push( Var top : StreamPtr; f : StreamPtr );
Begin
  If top <> Nil Then
    Streams_Chain(f,top);
  top := f
End;

{ set a stream (already part of the list) to be the new top; update top }
Procedure Streams_MoveToTop( Var top : StreamPtr; f : StreamPtr );
Begin
  If f = top Then { f already on top }
    Exit;
  Streams_Unchain(top,f);
  Streams_Push(top,f)
End;

{-----------------------------------------------------------------------}
{ stack: reset                                                          }
{-----------------------------------------------------------------------}

{ close all opened, non-terminal files; note that buffers will be closed,
 which results in an invalid PII program state; a reset of the stack should
follow any call to this }
Procedure Streams_CloseAll( top : StreamPtr );
Var
  f : StreamPtr;
Begin
  f := top;
  While f <> Nil Do
  Begin
    If Stream_IsOpen(f) And Not Stream_IsConsole(f) Then
      Stream_Close(f);
    f := Streams_GetNext(f)
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
Function Streams_Lookup( top : StreamPtr; Desc : TFileDescriptor; 
    Alias : TAlias; Path : TPath; Mode : TStreamMode; 
    Dev : TIODeviceType ) : StreamPtr;
Var
  f : StreamPtr;
Begin
  f := top;
  While f <> Nil Do
  Begin
    If ((Desc = 0) Or (Stream_GetDescriptor(f) = Desc)) And
      ((Alias = Nil) Or Str_Equal(Stream_GetAlias(f),Alias)) And
      ((Path = Nil) Or Str_Equal(Stream_GetPath(f),Path)) And
      ((Mode = MODE_ANY) Or (Stream_GetMode(f) = Mode)) And
      ((Dev = DEV_ANY) Or (Stream_GetDeviceType(f) = Dev)) Then
    Begin
      Streams_Lookup := f;
      Exit
    End;
    f := Streams_GetNext(f)
  End;
  Streams_Lookup := f
End;

{ return the input console }
Function Streams_InputConsole( top : StreamPtr ) : StreamPtr;
Begin
  Streams_InputConsole := Streams_Lookup(top,0,Nil,Nil,MODE_READ,DEV_TERMINAL)
End;

{ return the output console }
Function Streams_OutputConsole( top : StreamPtr ) : StreamPtr;
Begin
  Streams_OutputConsole := Streams_Lookup(top,0,Nil,Nil,MODE_WRITE,DEV_TERMINAL)
End;

{ return the first stream having a given path }
Function Streams_LookupByPath( top : StreamPtr; Path : TPath ) : StreamPtr;
Begin
  Streams_LookupByPath := Streams_Lookup(top,0,Nil,Path,MODE_ANY,DEV_ANY)
End;

{ return the first stream having a given mode }
Function Streams_LookupByMode( top : StreamPtr; 
    Mode : TStreamMode ) : StreamPtr;
Begin
  Streams_LookupByMode := Streams_Lookup(top,0,Nil,Nil,Mode,DEV_ANY)
End;

{ return the first stream having a device type }
Function Streams_LookupByDevice( top : StreamPtr; 
    Dev : TIODeviceType ) : StreamPtr;
Begin
  Streams_LookupByDevice := Streams_Lookup(top,0,Nil,Nil,MODE_ANY,Dev)
End;

{ return the stream having descriptor Desc in the list top, or Nil }
Function Streams_LookupByDescriptor( top : StreamPtr; 
    Desc : TFileDescriptor ) : StreamPtr;
Begin
  Streams_LookupByDescriptor := Streams_Lookup(top,Desc,Nil,Nil,MODE_ANY,DEV_ANY)
End;

{ return the stream having descriptor Desc in the list top, or Nil }
Function Streams_LookupByAlias( top : StreamPtr; Alias : TAlias ) : StreamPtr;
Begin
  Streams_LookupByAlias := Streams_Lookup(top,0,Alias,Nil,MODE_ANY,DEV_ANY)
End;

{----------------------------------------------------------------------------}
{ current i/o                                                                }
{----------------------------------------------------------------------------}

{ return the current input stream }
Function Streams_CurrentInput( top : StreamPtr ) : StreamPtr;
Var
  f : StreamPtr;
Begin
  f := Streams_LookupByMode(top,MODE_READ);
  CheckCondition(f <> Nil,'Streams_CurrentInput: no input stream');
  Streams_CurrentInput := f
End;

{ return the current output stream }
Function Streams_CurrentOutput( top : StreamPtr ) : StreamPtr;
Var
  f : StreamPtr;
Begin
  f := Streams_LookupByMode(top,MODE_WRITE);
  CheckCondition(f <> Nil,'Streams_CurrentOutput: no output stream');
  Streams_CurrentOutput := f
End;


Begin
  Stream_SetEcho(False);
  FreeDesc := 1;
  BufferCount := 0
End.