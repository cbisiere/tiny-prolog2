{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Keyboard.pas                                               }
{   Author      : Christophe Bisiere                                         }
{   Date        : 2022-09-17                                                 }
{   Updated     : 2022-09-17                                                 }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{          K E Y B O A R D   I N P U T   W I T H   H I S T O R Y             }
{                                                                            }
{----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ Read a character from the keyboard.                                        }
{ It is assumed that a character made of several bytes will be send quickly, }
{ at least faster than a human can type.                                     }
{----------------------------------------------------------------------------}

Function ReadChar : AnyStr;
Var cc : AnyStr;
Begin
  cc := ReadKey;
  While KeyPressed Do
    cc := cc + ReadKey;
  ReadChar := cc
End;

{----------------------------------------------------------------------------}
{                                                                            }
{ Type: String of possibly multi-byte chars                                  }
{                                                                            }
{                                                                            }
{----------------------------------------------------------------------------}

Type TString = Record
  Chars : AnyStr; { raw chars }
  Len : Byte; { actual length }
  RunSize : Array[0..254] Of Byte { 0..Len: size of each run }
End;

{----------------------------------------------------------------------------}
{ Empty a TString.                                                           }
{----------------------------------------------------------------------------}

Procedure ResetString( Var S : TString );
Begin
  S.Len := 0;
  S.Chars := ''
End;

{----------------------------------------------------------------------------}
{ Append a possibly multi-byte char to a TString.                            }
{----------------------------------------------------------------------------}

Procedure PushCharToString( Var S : TString; cc : AnyStr );
Begin
  With S Do
  Begin
    Len := Len + 1;
    RunSize[Len] := Length(cc);
    Chars := Chars + cc
  End
End;

{----------------------------------------------------------------------------}
{ Append string of one-byte chars to a TString.                              }
{----------------------------------------------------------------------------}

Procedure PushStringToString( Var S : TString; str : AnyStr );
Var i : Byte;
Begin
  For i := 1 To Length(str) Do
    PushCharToString(S, str[i])
End;

{----------------------------------------------------------------------------}
{ Delete the last possibly multi-byte char from a TString                    }
{----------------------------------------------------------------------------}

Procedure PopCharFromString( Var S : TString );
Begin
  With S Do
  Begin
    Delete(Chars,Length(Chars)-RunSize[Len]+1,RunSize[Len]);
    Len := Len - 1
  End
End;

{----------------------------------------------------------------------------}
{ Dump a TString for debugging purpose                                       }
{----------------------------------------------------------------------------}

Procedure DumpString( S : TString );
Var
  p,k,j : Byte;
  cc : AnyStr;
Begin
  p := 1;
  For k := 1 To S.Len Do
  Begin
    cc := Copy(S.Chars,p,S.RunSize[k]);
    CWrite(' ' + cc + ' ');
    For j := 1 to Length(cc) Do
      CWrite('[' + IntToStr(Ord(cc[j])) + ']');
    p := p + S.RunSize[k]
  End
End;


{----------------------------------------------------------------------------}
{                                                                            }
{ Type: Command line history                                                 }
{                                                                            }
{                                                                            }
{----------------------------------------------------------------------------}

Const
  MaxHist = 10;
  MaxHistPlusOne = 11; { TP3 compatibility }

Type
  THIndex = 1..MaxHist;
  THCount = 0..MaxHist;
  THCursor = 0..MaxHistPlusOne;
  THistory = Record
    Cur : THCursor; { cursor when the user hit up/down arrow keys }
    Len : THCount; { nb of entries in use }
    Str : Array[THIndex] of TString { most recent on top }
  End;

{----------------------------------------------------------------------------}
{ Initialize command-line history H                                          }
{----------------------------------------------------------------------------}

Procedure ResetHistory( Var H : THistory );
Begin
  H.Len := 0;
  H.Cur := 0
End;

{----------------------------------------------------------------------------}
{ Register a new TString S in the history H                                  }
{----------------------------------------------------------------------------}

Procedure PushToHistory( Var H : THistory; S : TString );
Var j : THIndex;
Begin
  If H.Len < MaxHist Then
    H.Len := H.Len + 1;
  For j := H.Len DownTo 2 Do
    H.Str[j] := H.Str[j-1];
  H.Str[1] := S;
  H.Cur := 0
End;

{----------------------------------------------------------------------------}
{ Dump a history H for debugging purpose                                     }
{----------------------------------------------------------------------------}

Procedure DumpHistory( H : THistory );
Var j : THIndex;
Begin
  CWrite('Len=' + IntToStr(H.len) + ' Cur=' + IntToStr(H.Cur));
  CWriteLn;
  For j := 1 to H.Len Do
  Begin
    CWrite(IntToStr(j) + ': ');
    DumpString(H.Str[j]);
    CWriteLn;
  End
End;


{----------------------------------------------------------------------------}
{                                                                            }
{ Command line Management                                                    }
{                                                                            }
{                                                                            }
{----------------------------------------------------------------------------}

Var
  Hist : THistory;

{----------------------------------------------------------------------------}
{ Initialize the command-line history                                        }
{----------------------------------------------------------------------------}

Procedure InitHistory;
Begin
  ResetHistory(Hist)
End;

{----------------------------------------------------------------------------}
{ Read one line from the keyboard                                            }
{----------------------------------------------------------------------------}

Procedure ReadLnKbd( Var str : AnyStr );
Var
  Inp : TString;
  cc : AnyStr;
  Stop : Boolean;

  Procedure Backspace;
  Begin
    If Inp.Len > 0 Then
    Begin
      Write(#08);
      ClrEol;
      PopCharFromString(Inp)
    End
    Else
      Write(#07) { bell }
  End;

  Procedure BackspaceAll;
  Begin
    While Inp.Len > 0 Do
      Backspace
  End;

  Procedure UpArrow;
  Begin
    If Hist.Cur < Hist.Len Then
    Begin
      BackspaceAll;
      Hist.Cur := Hist.Cur + 1;
      Inp := Hist.Str[Hist.Cur];
      CWrite(Inp.Chars)
    End
  End;

  Procedure DownArrow;
  Begin
    If Hist.Cur > 1 Then
    Begin
      BackspaceAll;
      Hist.Cur := Hist.Cur - 1;
      Inp := Hist.Str[Hist.Cur];
      CWrite(Inp.Chars)
    End
  End;

  { echo a char and append it to the input line }
  Procedure AcceptChar( cc : AnyStr );
  Begin
    CWrite(cc);
    PushCharToString(Inp, cc)
  End;

  Procedure Pass;
  Begin
  End;

Begin
  ResetString(Inp);
  Stop := False;
  While Not Stop Do
  Begin
    If KeyPressed Then
    Begin
      cc := ReadChar;
      CheckCondition(cc[1]<>#10,
        'fixme: terminal use LF as first char of line termination');
      Case cc[1] Of
      #08: { Backspace }
        Backspace;
      #13: { Carriage Return }
        Begin
          CWriteLn;
          If Inp.Len > 0 Then
            PushToHistory(Hist, Inp);
          { EndOfLine must be part of the input, so that:
            "> in_char(c);<CR>" sets c to EndOfLine; 
            see PII+ R 5-4 }
          PushCharToString(Inp, EndOfLine);
          Stop := True
        End;
      #3: { Ctrl-C }
        Begin
          CWriteLn;
          ResetString(Inp);
          PushStringToString(Inp, 'quit;');
          Stop := True;
        End;
      #00: { extended or function key }
        If Length(cc) > 1 Then
        Begin
          Case cc[2] Of
          #72:
            UpArrow;
          #80:
            DownArrow;
          Else
            Pass { ignore others }
          End
        End;
      Else 
        { any other character all other extended or function keys }
        AcceptChar(cc)
      End
    End
  End;
  Str := Inp.Chars
End;
