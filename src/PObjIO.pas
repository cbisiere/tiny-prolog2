{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObjIO.pas                                                 }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022-2026                                                  }
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
  Echo,
  Dump,
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
  TLineWidth = PosInt; { maximum line width for output streams }
  { device type }
  TIODeviceType = (
    DEV_FILE,
    DEV_BUFFER,
    DEV_TERMINAL,
    DEV_ANY { to search for any type of device }
  );
  { opening mode }
  TStreamMode = (
    MODE_READ,
    MODE_WRITE,
    MODE_NONE,
    MODE_ANY { to search for any mode }
  );
  { type of line continuation }
  TLineContinuation = (
    CONT_NONE,      { Prolog II }
    CONT_BACKSLASH  { \<RET> (or \<EOF>): PrologII+ and Prolog Edinburgh }
  );

{ stack of input /output streams; 
 Note: FI_LWID (maximum line width, set by set_line_width/1) only applies to 
  output stream; however, since we support changing the stream's mode (from 
  write to read, and then back to write) we choose to maintain this value 
  across changes of mode; a maximum of zero means no maximum }
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
    FI_EOLS : TEolStyle;             { type of EOL (CR,LF...)                  }
    FI_CONT : TLineContinuation;     { type of line continuation, if any       }
    FI_LWID : TLineWidth;            { maximum line width: set_line_width/1    }
    FI_CCUR : TLineWidth;            { character cursor in the current line    } 
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
    Mode : TStreamMode; Locked, WithDesc : Boolean; LCont : TLineContinuation;
    Width : TLineWidth ) : StreamPtr;
Function Stream_NewBuffer( Alias : TAlias; LCont : TLineContinuation;
    Width : TLineWidth ) : StreamPtr;
Function Stream_NewConsole( Alias : TAlias; Mode : TStreamMode; 
    Width : TLineWidth ) : StreamPtr;

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
Function Stream_GetEolStyle( f : StreamPtr ) : TEolStyle;
Procedure Stream_SetEolStyle( f : StreamPtr; Style : TEolStyle );
Function Stream_GetLineContinuation( f : StreamPtr ) : TLineContinuation;
Procedure Stream_SetLineContinuation( f : StreamPtr; LCont : TLineContinuation );
Function Stream_GetLineWidth( f : StreamPtr ) : TLineWidth;
Procedure Stream_SetLineWidth( f : StreamPtr; Width : TLineWidth );
Function Stream_GetCharacterPosition( f : StreamPtr ) : TLineWidth;
Function Stream_RemainingSpacesInLine( f : StreamPtr ) : TLineWidth;

{ stream: methods }
Procedure Stream_CloseFile( f : StreamPtr );
Procedure Stream_Close( f : StreamPtr );

{ stream: formatted output (that use the line width system) }
Procedure Stream_OutNewLine( f : StreamPtr );
Procedure Stream_OutChar( f : StreamPtr; cc : TChar );
Procedure Stream_OutNChar( f : StreamPtr; cc : TChar; n : PosInt );
Procedure Stream_Page( f : StreamPtr );

{ stream: write that ignore the line width system }
Procedure Stream_LineBreak( f : StreamPtr );
Procedure Stream_WriteChar( f : StreamPtr; cc : TChar );
Procedure Stream_WriteLongString( f : StreamPtr; s : StrPtr );
Procedure Stream_WriteShortString( f : StreamPtr; s : TString );
Procedure Stream_WritelnShortString( f : StreamPtr; s : TString );
Procedure Stream_Flush( f : StreamPtr );

{ stream: error messages }
Procedure Stream_DisplayErrorMessage( f : StreamPtr; msg : TString );

{ stream: read }
Procedure Stream_ClearInput( f : StreamPtr );
Procedure Stream_ReadLineFromKeyboard( f : StreamPtr );
Procedure Stream_CheckConsoleInput( f : StreamPtr; SkipSpaces : Boolean );
Procedure Stream_GetCharNb( f : StreamPtr; Var e : TIChar );
Procedure Stream_UngetChars( f : StreamPtr; e : TIChar );
Procedure Stream_GetChars( f : StreamPtr; e : TIChar );
Procedure Stream_GetChar( f : StreamPtr; Var e : TIChar );
Procedure Stream_NextChar( f : StreamPtr; Var e : TIChar );
Procedure Stream_NextNextChar( f : StreamPtr; Var e : TIChar );
Function Stream_NewStr( f : StreamPtr ) : StrPtr;

{ stack: loop }
Function Streams_GetNext( f : StreamPtr ) : StreamPtr;

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
Function Streams_LookupByDescriptorAndMode( top : StreamPtr; 
    Desc : TFileDescriptor; Mode : TStreamMode ) : StreamPtr;
Function Streams_LookupByAlias( top : StreamPtr; Alias : TAlias ) : StreamPtr;
Function Streams_LookupByAliasAndMode( top : StreamPtr; Alias : TAlias; 
    Mode : TStreamMode ) : StreamPtr;

{ stack: top }
Function Streams_CurrentInput( top : StreamPtr ) : StreamPtr;
Function Streams_CurrentOutput( top : StreamPtr ) : StreamPtr;

{ stack: edit }
Procedure Streams_Chain( f, g : StreamPtr );
Procedure Streams_Unchain( Var top : StreamPtr; f : StreamPtr );
Procedure Streams_Push( Var top : StreamPtr; f : StreamPtr );
Procedure Streams_MoveToTop( Var top : StreamPtr; f : StreamPtr );

{ stack: debug }
Procedure Streams_Dump( top : StreamPtr );

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
Procedure Stream_ResetInputBuffer( f : StreamPtr );
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
        Stream_ResetInputBuffer(f)
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
    Mode : TStreamMode; Locked, WithDesc : Boolean; LCont : TLineContinuation;
    Width : TLineWidth ) : StreamPtr;
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
    FI_ENCO := ENC_UNDECIDED;
    FI_EOLS := EOL_UNDECIDED;
    FI_CONT := LCont;
    FI_LWID := Width;
    FI_CCUR := 1
  End;
  Stream_SetMode(f,Mode);
  Stream_New := f
End;

{ create a new buffer, w/o a mode, which thus will not be returned as the 
 current input or output stream; in the stack, several buffer with the same
 name can coexist; for now, we simulate buffers with files, so we have to
 generate a unique buffer file name for each new buffer; WARNING: those fake
 file names can no more be used as user file names; when in write mode, the 
 buffer maximum line width will be Width }
Function Stream_NewBuffer( Alias : TAlias; LCont : TLineContinuation;
    Width : TLineWidth ) : StreamPtr;
Var
  Path : TPath;
  f : StreamPtr;
Begin
  BufferCount := BufferCount + 1;
  Path := Str_Clone(Alias);
  Str_Append(Path,'-' + PosIntToShortString(BufferCount) + '.txt');
  f := Stream_New(Alias,Path,DEV_BUFFER,MODE_NONE,
      False,False,LCont,Width);
  Stream_SetEncoding(f,GetSystemEncoding);
  Stream_SetEolStyle(f,GetSystemEolStyle);
  Stream_NewBuffer := f
End;

{ create a new console in mode Mode, with a line width of Width; 
 TBC: we assume console i/o does not use line continuation }
Function Stream_NewConsole( Alias : TAlias; Mode : TStreamMode; 
    Width : TLineWidth ) : StreamPtr;
Var
  f : StreamPtr;
Begin
  f := Stream_New(Alias,Alias,DEV_TERMINAL,Mode,False,False,CONT_NONE,Width);
  Stream_SetEncoding(f,GetSystemEncoding);
  Stream_SetEolStyle(f,EOL_UNDECIDED);
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

{ get EOL style }
Function Stream_GetEolStyle( f : StreamPtr ) : TEolStyle;
Begin
  With f^ Do
    Stream_GetEolStyle := FI_EOLS
End;

{ set EOL style }
Procedure Stream_SetEolStyle( f : StreamPtr; Style : TEolStyle );
Begin
  With f^ Do
    FI_EOLS := Style
End;

{ get EOL style }
Function Stream_GetLineContinuation( f : StreamPtr ) : TLineContinuation;
Begin
  With f^ Do
    Stream_GetLineContinuation := FI_CONT
End;

{ set EOL style }
Procedure Stream_SetLineContinuation( f : StreamPtr; LCont : TLineContinuation );
Begin
  With f^ Do
    FI_CONT := LCont
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

{ get line width }
Function Stream_GetLineWidth( f : StreamPtr ) : TLineWidth;
Begin
  With f^ Do
    Stream_GetLineWidth := FI_LWID
End;

{ set line width }
Procedure Stream_SetLineWidth( f : StreamPtr; Width : TLineWidth );
Begin
  With f^ Do
    FI_LWID := Width
End;

{ get character position }
Function Stream_GetCharacterPosition( f : StreamPtr ) : TLineWidth;
Begin
  With f^ Do
    Stream_GetCharacterPosition := FI_CCUR
End;

{ reset character position }
Procedure Stream_ResetCharacterPosition( f : StreamPtr );
Begin
  With f^ Do
    FI_CCUR := 1
End;

{ inc character position; we do not check for overflows as set_line_width/1 
 might have shortened the line width very aggressively (see below) }
Procedure Stream_IncCharacterPosition( f : StreamPtr; Delta : TLineWidth );
Begin
  With f^ Do
    FI_CCUR := FI_CCUR + Delta
End;

{ remaining spaces (in number of chars) in the current line of an output 
 stream; if set_line_width/1 has set the width to a value shorter than the 
 length of the current line, we return zero }
Function Stream_RemainingSpacesInLine( f : StreamPtr ) : TLineWidth;
Begin
  Stream_RemainingSpacesInLine := Max(0,Stream_GetLineWidth(f) - 
      (Stream_GetCharacterPosition(f) - 1))
End;


{----------------------------------------------------------------------------}
{ close / reset                                                              }
{----------------------------------------------------------------------------}

{ close the file attached to a stream stream; never close a console; do not 
 reset the input buffer }
Procedure Stream_CloseFile( f : StreamPtr );
Begin
  If Stream_IsOpen(f) And Not Stream_IsConsole(f) Then
  Begin
    Case Stream_GetMode(f) Of
    MODE_READ:
      CloseIFile(Stream_GetShortPath(f),f^.FI_IFIL);
    MODE_WRITE:
      CloseOFile(Stream_GetShortPath(f),f^.FI_OFIL);
    MODE_NONE:
      Bug('Stream_CloseFile: open buffer with no mode ');
    End;
    Stream_SetOpen(f,False)
  End
End;

{ close a stream }
Procedure Stream_Close( f : StreamPtr );
Begin
  If Stream_IsOpen(f) Then
    Stream_Close(f);
  If Stream_GetMode(f) = MODE_READ Then
    Stream_ResetInputBuffer(f)
End;


{----------------------------------------------------------------------------}
{ error message                                                              }
{----------------------------------------------------------------------------}

{ write on console an error message about an error that happened on file f, 
 pointing a cursor to the error }
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
      n := BufDisplayLine(FI_IBUF,(CrtGetScreenWidth div 3) * 2);
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

{ replenish the small byte buffer FI_CBUF as much as possible }
Procedure Stream_ReplenishBuffer( f : StreamPtr );
Var
  c : Char;
Begin
  With f^ Do
  Begin
    While Not Error And FI_OPEN And (Length(FI_CBUF) < MaxBytesPerChar) Do
    Begin
      If Eof(FI_IFIL) Then
        Stream_CloseFile(f)
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

{ to consume characters from the input stream, use the two following procedures:
 - read a char with Stream_GetChar(e); later on, when needed, roll everything 
   back to this character using Stream_UngetChars(e); it works even if you have 
   read other characters past e using Stream_GetChar
 - lookahead a char with Stream_NextChar(e), and then, when appropriate, accept 
   it with Stream_GetChars(e); it works even if there have been other lookahead 
   operations past e
}

{-----------------------------------------------------------------------}
{ interface with the stream's buffer                                    }
{-----------------------------------------------------------------------}

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
  Stream_ResetInputBuffer(f);
  With f^ Do
    ReadLnKbd(FI_IBUF,FI_ENCO,FI_EOLS);
  { by design, ReadLnKbd stops on enter key (or user interrupt); as the goals
   to clear may use set_line_cursor/1 or out/1 and friends, we must reset
   the character position to 1 }
  Stream_ResetCharacterPosition(f)
End;

{ move the read cursor backward up to (and including) the line and 
 column number of character e }
Procedure Stream_UngetChars( f : StreamPtr; e : TIChar );
Var 
  e1 : TIChar;
  Found : Boolean;
Begin
  With f^ Do
  Begin
    CheckCondition(BufNbRead(FI_IBUF) > 0, 'Stream_UngetChars: no read');
    Found := False;
    While Not Error And Not Found Do
    Begin
      If BufNbRead(FI_IBUF) = 0 Then
        RuntimeError('parsing limit reached: lookup too ahead to undo');
      If Error Then Exit;
      BufGetRead(e1,FI_IBUF,0); 
      CheckCondition((e1.Lnb > e.Lnb) Or (e1.Lnb = e.Lnb) And (e1.Pos >= e.Pos), 
          'Stream_UngetChars: target char disappeared!');
      Found := (e1.Lnb = e.Lnb) And (e1.Pos = e.Pos);
      BufUnread(FI_IBUF)
    End
  End
End;

{ move the read cursor forward up to (and including) the line and column number 
 of character e; it is assumed that character e has been returned by a previous 
 call to a forward-looking primitive (Stream_NextChar or friends) and thus is 
 already in the buffer; in other words, this primitive is meant to accept
 one or more characters that as been already forward-looked at }
Procedure Stream_GetChars( f : StreamPtr; e : TIChar );
Var 
  e1 : TIChar;
  Found : Boolean;
Begin
  With f^ Do
  Begin
    CheckCondition(BufNbUnRead(FI_IBUF) > 0, 'Stream_GetChars: no lookahead');
    Found := False;
    While Not Error And Not Found Do
    Begin
      If BufNbUnRead(FI_IBUF) = 0 Then
        RuntimeError('parsing limit reached: lookup too ahead to accept');
      If Error Then Exit;
      BufRead(e1,FI_IBUF);
      CheckCondition((e1.Lnb < e.Lnb) Or (e1.Lnb = e.Lnb) And (e1.Pos <= e.Pos), 
          'Stream_GetChars: ahead of the target char!');
      Found := (e1.Lnb = e.Lnb) And (e1.Pos = e.Pos)
    End
  End
End;

{ read a character; if the end of the file has been reached or the char 
 cannot be set, return end-of-input; to test for the later case, the caller must 
 check Error; replace CR, LF, and CRLF with EOL; return end-of-input when
 the stream cannot return additional char; Note to self: make sure all
 the code paths end with a call to BufRead, otherwise a subsequent call to 
 BufUnread with create a bug, as it will unread a different TChar }
Procedure Stream_ReadChar( f : StreamPtr; Var e : TIChar );
Var
  cc : TChar;
Begin
  With f^ Do
  Begin
    { 1: if the last char read was EOF, leave the buffer as-is and return EOF }
    If (BufNbRead(FI_IBUF) > 0) Then
    Begin
      BufGetLastRead(e,FI_IBUF);
      If TICharIsEndOfInput(e) Then
      Begin
        RuntimeError('Stream_ReadChar: read past EOF');
        Exit
      End
    End;
    { 2: no more unread chars available? try to replenish the 4-byte buffer }
    If BufNbUnread(FI_IBUF) = 0 Then
    Begin
      { make room for one additional TChar }
      If BufNbFree(FI_IBUF) = 0 Then
        BufDiscard(FI_IBUF,1);
      Case FI_TYPE Of
      DEV_FILE,DEV_BUFFER: { for now, a buffer is a file }
        Begin
          Stream_ReplenishBuffer(f); { tiny 4-byte buffer }
          cc := CC_END_OF_INPUT;
          If Length(FI_CBUF) > 0 Then
            If TCharGetOne(FI_CBUF,cc,FI_ENCO,FI_EOLS) Then { get a char }
              If GetEchoState Then
              Begin
                { spec. of echo/0 is: : write on console what is read or 
                 written to disk files or buffers }
                If Not Stream_IsConsole(f) Then
                  CWriteChar(cc) { note: this will take care of paper/0 }
              End;
          BufPushChar(FI_IBUF,cc)
        End;
      DEV_TERMINAL:
        Begin
          cc := CC_END_OF_INPUT;
          BufPushChar(FI_IBUF,cc)
        End
      End
    End;
    { 3: read the next char }
    BufRead(e,FI_IBUF)
  End
End;

{-----------------------------------------------------------------------}
{ higher-level primitives.                                              }
{-----------------------------------------------------------------------}

{ get a char, skipping one or several line continuation characters (depending 
 on the stream parameter); note that a subsequent Stream_UngetChars(f,e) would 
 put back that character only, and not the skipped line continuation 
 characters }
Procedure Stream_GetChar( f : StreamPtr; Var e : TIChar );
Var
  Stop : Boolean;
  e1 : TIChar;
Begin
  Repeat
    Stream_ReadChar(f,e);
    If Error Then Exit;
    { stop when this char is enough to tell that it is not a line continuation }
    Stop := Not ((Stream_GetLineContinuation(f) = CONT_BACKSLASH) And 
        TICharIs(e,'\'));
    If Not Stop Then { we have a '\' that *could* be a line continuation }
    Begin
      Stream_ReadChar(f,e1); { read the char just after the '\'}
      If Error Then Exit;
      { is this '\' not an actual line continuation? (\<EOL> or \<EOF>) }
      Stop := Not (TICharIsEol(e1) Or TICharIsEndOfInput(e1));
      { if not \<EOL>, put this last char back; note that if the char is EOF, 
       since Stop is false, the next loop will catch it back and return it }
      If Not TICharIsEol(e1) Then
      Begin
        Stream_UngetChars(f,e1);  { put back the char after '\' } 
        If Error Then Exit
      End
    End
  Until Stop
End;

{ read the next non-blank character; may return EOL or EOF }
Procedure Stream_GetCharNb( f : StreamPtr; Var e : TIChar );
Begin
  Repeat 
    Stream_GetChar(f,e);
    If Error Then Exit
  Until Not (TICharIsEol(e) Or TICharIsSpace(e))
End;

{ return the next character, without consuming it }
Procedure Stream_NextChar( f : StreamPtr; Var e : TIChar );
Begin
  Stream_GetChar(f,e);
  If Error Then Exit;
  Stream_UngetChars(f,e)
End;

{ return the char after the next character without consuming it }
Procedure Stream_NextNextChar( f : StreamPtr; Var e : TIChar );
Var
  e1 : TIChar;
Begin
  Stream_GetChar(f,e1);
  If Error Then Exit;
  Stream_GetChar(f,e);
  If Error Then Exit;
  Stream_UngetChars(f,e1)
End;

{ if f is a console, then read a line (from the keyboard) if no more characters 
 are available in the input line; optionally skip spaces beforehand; skipping 
 spaces is useful when reading a terms with in(t); this is the opposite when 
 reading a char (in_char) or line (inl); }
Procedure Stream_CheckConsoleInput( f : StreamPtr; SkipSpaces : Boolean );
Var 
  e : TIChar;
Begin
  If Stream_IsConsole(f) Then
  Begin
    If SkipSpaces Then
    Begin
      Stream_GetCharNb(f,e);
      If Error Then Exit;
      If TICharIsEndOfInput(e) Then
        Stream_ResetInputBuffer(f)
      Else
        Stream_UngetChars(f,e); { put back the non-blank character }
    End;
    If Stream_IsDry(f) Then
    Begin
      CLISetPrompt(''); { no prompt }
      Stream_ReadLineFromKeyboard(f)
    End
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

{ About line width: 

In Prolog II, line_width/1 and set_line_width/1 get and set the maximum line 
width of the current output unit (see PII+ doc p.34 and 131). 

We handle this as follows:
 - for each stream, we store and maintain the current character position CCUR;
 - when using out/1 or one of its derivatives (outm/1, outl/1, outml/1, line/0, 
   page/0), line continuation characters (LCC, '\') are inserted whenever 
   necessary and CCUR is updated;
 - in/1 and its derivatives can be used to read the stream back;
 - because honoring LCC is a low-level feature (at GetChar level), even 
   non-in/1-based inputs containing LCC (e.g. a Prolog program) work.
 
So, any output that is not meant to be read back (e.g. solutions, warnings,
error or trace message) does not use LCC and does not update CCUR.

For a console, it implies that:
 - output of solutions and messages ignore the maximum line width and do not 
   use LCC;
 - as a side effect, such outputs may trigger automatic line breaks on the 
   screen;
 - out/1 does use LCC (TODO: to be tested on PII+), even if it is useless as 
   such output cannot be read back.
 }

{-----------------------------------------------------------------------}
{ private methods: write bytes to a stream                              }
{-----------------------------------------------------------------------}

{ handles writing to screen or files, honoring to echo and paper features, 
 assuming none of the characters to print is a soft mark (or an equivalent,
 actual character, as \n) }

{ write a short string of bytes to an output stream }
Procedure Stream_WriteBytes( f : StreamPtr; s : TString );
Begin
  With f^ Do
    Case FI_TYPE Of
      DEV_TERMINAL:
        CWrite(s);
      DEV_FILE,DEV_BUFFER:
        Begin
          If GetEchoState Then
            CWrite(s);
          WriteToFile(Stream_GetShortPath(f),FI_OFIL,s)
        End
    End
End;

{ write a new line to an output stream }
Procedure Stream_WriteNewLine( f : StreamPtr );
Begin
  With f^ Do
    Case FI_TYPE Of
      DEV_TERMINAL:
        CWriteLn;
      DEV_FILE,DEV_BUFFER:
        Begin
          If GetEchoState Then
            CWriteLn;
          WritelnToFile(Stream_GetShortPath(f),FI_OFIL,'')
        End
    End
End;

{-----------------------------------------------------------------------}
{ public methods: write to a stream, updating current char position     }
{-----------------------------------------------------------------------}

{ handles writing to screen or files, mirroring to echo and trace files, 
 updating the character position of the output stream, assuming none of the
 characters to print is an actual line breaks }

{ write a new line to an output stream }
Procedure Stream_OutNewLine( f : StreamPtr );
Begin
  Stream_WriteNewLine(f);
  Stream_ResetCharacterPosition(f)
End;

{ write a single TChar to an output stream; the char is supposed to fit into
 the current lne }
Procedure Stream_OutChar( f : StreamPtr; cc : TChar );
Begin
  Stream_WriteBytes(f,TCharGetBytes(cc));
  Stream_IncCharacterPosition(f,1)
End;

{ write n times a single TChar to an output stream }
Procedure Stream_OutNChar( f : StreamPtr; cc : TChar; n : PosInt );
Var
  i : PosInt;
Begin
  For i := 1 to n Do
    Stream_OutChar(f,cc)
End;

{ go to the next page; has no effect if the steam is not a console  }
Procedure Stream_Page( f : StreamPtr );
Begin
  If Stream_IsConsole(f) Then
  Begin
    CrtClrSrc;
    Stream_ResetCharacterPosition(f)
  End
End;

{-----------------------------------------------------------------------}
{ public methods: write ignoring the line width system.                 }
{-----------------------------------------------------------------------}

{ line break }
Procedure Stream_LineBreak( f : StreamPtr );
Begin
  Stream_WriteNewLine(f)
End;

{ write a single TChar to an output stream, honoring line break soft marks, 
 ignoring other soft marks }
Procedure Stream_WriteChar( f : StreamPtr; cc : TChar );
Begin
  If TCharIsEol(cc) Then
    Stream_LineBreak(f)
  Else If Not TCharIsSoftMark(cc) Then
    Stream_WriteBytes(f,TCharGetBytes(cc))
End;

{ write a long stream while honoring line break soft marks, ignoring other 
 soft marks }
Procedure Stream_WriteLongString( f : StreamPtr; s : StrPtr ) ;
Var 
  Iter : StrIter;
  cc : TChar;
Begin
  StrIter_ToStart(Iter,s);
  While StrIter_NextChar(Iter,cc) Do
    Stream_WriteChar(f,cc)
End;

{ write a short string of ascii chars to an output stream  }
Procedure Stream_WriteShortString( f : StreamPtr; s : TString );
Begin
  Stream_WriteLongString(f,Str_NewFromShortString(s))
End;

{ writeln a short string of ascii chars to an output stream }
Procedure Stream_WritelnShortString( f : StreamPtr; s : TString );
Begin
  Stream_WriteShortString(f,s);
  Stream_WriteNewLine(f)
End;

{ flush an output stream }
Procedure Stream_Flush( f : StreamPtr );
Begin
  With f^ Do
    If FI_OPEN And (FI_TYPE = DEV_FILE) Then
      FlushFile(Stream_GetShortPath(f),FI_OFIL)
End;

{----------------------------------------------------------------------------}
{ dump                                                                       }
{----------------------------------------------------------------------------}

Procedure Stream_Dump( f : StreamPtr );
Begin
  With f^ Do
  Begin
    WritelnToDumpFile('State of stream #' + PosIntToShortString(FI_DESC));
    WritelnToDumpFile(' FI_ALIA: ' + Stream_GetShortAlias(f));
    WritelnToDumpFile(' Path: ' + Stream_GetShortPath(f));
    WritelnToDumpFile(' FI_MODE: ' + IntToShortString(Ord(FI_MODE)));
    { WritelnToDumpFile(' FI_IBUF: ');
    BufDump(FI_IBUF);
    WriteLineBreakToDumpFile;
    WritelnToDumpFile(' FI_CBUF: ');
    WritelnToDumpFile(FI_CBUF);
    WriteLineBreakToDumpFile }
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

{ return the stream having descriptor Desc and mode Mode in the list top, or 
 Nil }
Function Streams_LookupByDescriptorAndMode( top : StreamPtr; 
    Desc : TFileDescriptor; Mode : TStreamMode ) : StreamPtr;
Begin
  Streams_LookupByDescriptorAndMode := 
      Streams_Lookup(top,Desc,Nil,Nil,Mode,DEV_ANY)
End;

{ return the stream having alias Alias in the list top, or Nil }
Function Streams_LookupByAlias( top : StreamPtr; Alias : TAlias ) : StreamPtr;
Begin
  Streams_LookupByAlias := Streams_Lookup(top,0,Alias,Nil,MODE_ANY,DEV_ANY)
End;

{ return the stream having alias Alias and mode Mode in the list top, or Nil }
Function Streams_LookupByAliasAndMode( top : StreamPtr; Alias : TAlias; 
    Mode : TStreamMode ) : StreamPtr;
Begin
  Streams_LookupByAliasAndMode := Streams_Lookup(top,0,Alias,Nil,Mode,DEV_ANY)
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

{----------------------------------------------------------------------------}
{ dump                                                                       }
{----------------------------------------------------------------------------}

Procedure Streams_Dump( top : StreamPtr );
Var
  f : StreamPtr;
Begin
  WritelnToDumpFile('STREAM STACK:');
  f := top;
  While f <> Nil Do
  Begin
    Stream_Dump(f);
    f := Streams_GetNext(f)
  End
End;

{----------------------------------------------------------------------------}
{ Init                                                                       }
{----------------------------------------------------------------------------}

Begin
  FreeDesc := 1;
  BufferCount := 0
End.