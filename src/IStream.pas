{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : IStream.pas                                                }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                           I N P U T   S T E A M                            }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ read characters from single-byte or UTF-8 input stream }

{
  ASSUMPTIONS AND LIMITS:
  -----------------------
  1) input streams (files or console) are single-byte or UTF8 encoded
  2) for correct display of strings contained in a Prolog program, terminal  
    encoding is the same as the file
  3) single-byte files do not start with a sequence of chars equal to the 
    UTF-8 BOM sequence EF BB BF
  4) single-byte files do not contain any sequence of bytes that are
    valid UTF-8 code point 
}


{ input stream }
Type
  TIStream = Record
    FName : TString;
    IFile : TIFile;                 { file handler (if not a console)         }
    IsOpen : Boolean;               { is the stream ready for input?          }
    DeviceType : TIODeviceType;     { where do we get input from?             }
    IBuf : TBuf;                    { input buffer                            }
    Encoding : TEncoding;           { character encoding of the input stream  }
    CBuf : String[MaxBytesPerChar]; { char buffer to handle multi-byte chars  }
    ByteCount : LongInt;            { total number of bytes read so far       }
  End;

{----------------------------------------------------------------------------}
{ init                                                                       }
{----------------------------------------------------------------------------}

{ reset the read buffers of an input stream; must be done once before
 reading from a file stream, or before reading a line from the keyboard }
Procedure ResetIStream( Var f : TIStream );
Begin
  With f Do
  Begin
    ByteCount := 0;
    CBuf := '';
    BufInit(IBuf)
  End
End;

{----------------------------------------------------------------------------}
{ getters / setters                                                          }
{----------------------------------------------------------------------------}

{ get encoding }
Function GetIStreamEncoding( Var f : TIStream ) : TEncoding;
Begin
  GetIStreamEncoding := f.Encoding
End;

{ set encoding }
Procedure SetIStreamEncoding( Var f : TIStream; Enc : TEncoding );
Begin
  f.Encoding := Enc
End;

{ return true if there is no more chars available for reading in f's buffer
 system }
Function IStreamIsDry( Var f : TIStream ) : Boolean;
Begin
  CheckCondition(f.IsOpen,'IStreamIsDry: file is closed');
  With f Do
    IStreamIsDry := (BufNbUnread(IBuf) = 0) And (Length(CBuf) = 0)
End;

{----------------------------------------------------------------------------}
{ open / close                                                               }
{----------------------------------------------------------------------------}

{ open an input stream }
Procedure OpenIStream( FileName : TString; Var f : TIStream );
Begin
  With f Do
  Begin
    FName := FileName;
    Encoding := UNDECIDED; { default; detection is dynamic }
    If FName = CONSOLE_NAME Then
    Begin
      DeviceType := TTerminal;
      IsOpen := True
    End
    Else
    Begin
      DeviceType := TFile;
      IsOpen := OpenForRead(FName,IFile)
    End;
  End;
  ResetIStream(f)
End;

{ close an input stream }
Procedure CloseIStream( Var f : TIStream );
Begin
  With f Do
  Begin
    If IsOpen And (DeviceType = TFile) Then
      CloseIFile(FName,IFile);
    IsOpen := False
  End
End;

{----------------------------------------------------------------------------}
{ error message                                                              }
{----------------------------------------------------------------------------}

{ write an error message, pointing a cursor to the error }
Procedure DisplayIStreamErrorMessage( Var f : TIStream; msg : TString );
Var 
  HasRead : Boolean; { buffer contains read chars }
  e : TIChar; { last char read }
  i : TBufIndex;
  n : TBufIndex;
Begin
  With f Do
  Begin
    HasRead := BufNbRead(IBuf) > 0;
    If Not HasRead Then
    Begin
      CWrite('Error: ' + msg);
      CWriteLn
    End
    Else
    Begin
      BufGetLastRead(e,IBuf);
      Case DeviceType Of
      TFile:
        CWrite('Error line ' + IntToStr(e.Lnb) + ', position ' + IntToStr(e.Pos) + ': ' + msg);
      TTerminal:
        CWrite('Error at position ' + IntToStr(e.Pos) + ': ' + msg)
      End;
      CWriteLn;
      n := BufDisplayLine(IBuf,(CrtScreenWidth div 3) * 2);
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

{ replenish the small char buffer CBuf as much as possible }
Procedure GetCharsFromStream( Var f : TIStream );
Var
  c : Char;
Begin
  With f Do
  Begin
    While Not Error And IsOpen And (Length(CBuf) < MaxBytesPerChar) Do
    Begin
      If Eof(IFile) Then
        CloseIStream(f)
      Else If ReadFromFile(FName,IFile,c) Then
      Begin
        CBuf := CBuf + c;
        ByteCount := ByteCount + 1
      End
      Else
        RuntimeError('i/o error when reading from "' + FName + '"')
    End
  End
End;

{----------------------------------------------------------------------------}
{ read using circular buffer                                                 }
{----------------------------------------------------------------------------}

{ input one line from the keyboard and store it into the char buffer; note 
 that the buffer is not *reset* }
Procedure ReadLineFromKeyboard( Var f : TIStream );
Begin
  ResetIStream(f);
  With f Do
    ReadLnKbd(IBuf,Encoding)
End;

{ move the read cursor backward by one }
Procedure UngetCharFromStream( Var f : TIStream );
Begin
  With f Do
    BufUnread(IBuf)
End;

{ move the read cursor backward up to (and including) a certain line and 
 column number }
Procedure UngetCharsFromStream( Var f : TIStream; line : TLineNum; 
    col : TCharPos );
Var 
  e : TIChar;
  Found : Boolean;
Begin
  With f Do
  Begin
    Found := False;
    While Not Found And Not Error Do
    Begin
      If BufNbUnread(IBuf) = 0 Then
        RuntimeError('parsing limit reached: lookup too ahead to handle');
      If Error Then Exit;
      BufGetRead(e,IBuf,0);
      CheckCondition((e.Lnb > line) Or (e.Lnb = line) And (E.Pos >= col), 
          'UngetCharsFromStream: target char disappeared!');
      Found := (e.Lnb = line) And (E.Pos = col);
      BufUnread(IBuf)
    End
  End
End;

{ read a codepoint; if the end of the file has been reached or the codepoint 
 cannot be set, return EndOfInput; to test for the later case, the caller must 
 check Error; replace CR, LF, and CRLF with NewLine; return EndOfInput when
 the stream cannot return additional codepoints; Note to self: make sure all
 the code paths end with a call to BufRead, otherwise a subsequent call to 
 BufUnread with create a bug, as it will unread a different codepoint }
Procedure ReadCodepointFromStream( Var f : TIStream; Var e : TIChar );
Var
  cc : TChar;
Begin
  With f Do
  Begin
    { no more unread codepoints available: try to replenish the buffer }
    If BufNbUnread(IBuf) = 0 Then
    Begin
      { make room for one additional codepoint }
      If BufNbFree(IBuf) = 0 Then
        BufDiscard(IBuf,1);
      Case DeviceType Of
      TFile :
        Begin
          GetCharsFromStream(f); { fill up CBuf }
          cc := EndOfInput;
          If Length(CBuf) > 0 Then
            If CodePointWithNewLine(CBuf,cc,Encoding) Then
              Pass;
          BufAppendTChar(IBuf,cc)
        End;
      TTerminal:
        BufAppendTChar(IBuf,EndOfInput);
      End
    End;

    { read the next codepoint }
    BufRead(e,IBuf)
  End
End;

{ read one codepoint with position }
Function GetICharFromStream( Var f : TIStream; Var e : TIChar ) : TIChar;
Begin
  SetIChar(e,'',0,0);
  GetICharFromStream := e;
  ReadCodepointFromStream(f,e);
  If Error Then Exit;
  GetICharFromStream := e
End;

{ read one codepoint }
Function GetCharFromStream( Var f : TIStream; Var c : TChar ) : TChar;
Var
  e : TIChar;
Begin
  GetCharFromStream := '';
  c := '';
  e := GetICharFromStream(f,e);
  If Error Then Exit;
  c := e.Val;
  GetCharFromStream := c
End;

{ read the next non-blank codepoint }
Function GetCharNbFromStream( Var f : TIStream; Var c : TChar ) : TChar;
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
Function NextICharFromStream( Var f : TIStream; Var e : TIChar ) : TIChar;
Begin
  NextICharFromStream := GetICharFromStream(f,e);
  If Error Then Exit;
  UngetCharFromStream(f)
End;

{ return the next codepoint without consuming it }
Function NextCharFromStream( Var f : TIStream; Var c : TChar ) : TChar;
Var
  e : TIChar;
Begin
  NextCharFromStream := '';
  c := '';
  e := NextICharFromStream(f,e);
  If Error Then Exit;
  c := e.Val;
  NextCharFromStream := c
End;

{ ditto but two codepoints in advance }
Function NextNextCharFromStream( Var f : TIStream; Var c : TChar ) : TChar;
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
Procedure CheckConsoleInputStream( Var f : TIStream; SkipSpaces : Boolean );
Var 
  c : TChar;
Begin
  If f.DeviceType = TTerminal Then
  Begin
    If SkipSpaces Then
    Begin
      c := GetCharNbFromStream(f,c);
      If Error Then Exit;
      If c = EndOfInput Then
        ResetIStream(f)
    End;
    If IStreamIsDry(f) Then
      ReadLineFromKeyboard(f)
  End
End;


{----------------------------------------------------------------------------}
{ DEBUG                                                                      }
{----------------------------------------------------------------------------}

Procedure IStreamDump( f : TIStream );
Begin
  With f Do
  Begin
    WriteToEchoFile('State of stream "' + FName + '"');
    WriteToEchoFile(CRLF);
    WriteToEchoFile(' IBuf: ');
    BufDump(IBuf);
    WriteToEchoFile(CRLF);
    WriteToEchoFile(' CBuf: ');
    CharDump(CBuf);
    WriteToEchoFile(CRLF)
  End
End;
