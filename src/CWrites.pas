{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : CWrites.pas                                                }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                       W R I T E   T O   C O N S O L E                      }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ Write to the console, duplicating output to an echo file }

Unit CWrites;

Interface

Uses
  Chars,
  ShortStr,
  Num,
  Errs,
  Mirror,
  Crt2;


Procedure CWriteChar( cc : TChar );
Procedure CWrite( s : TString );
Procedure CWriteLn;
Procedure CWriteInt( v : Integer );
Procedure CWritePosInt( v : PosInt );
Procedure CWriteLongInt( v : LongInt );
Procedure CWriteBool( b : Boolean );
Procedure CWriteWarning( s : TString );
Procedure CWriteLnWarning( s : TString );

Implementation
{-----------------------------------------------------------------------------}

{ write a char to the terminal }
Procedure CWriteChar( cc : TChar );
Begin
  WriteToMirrorFiles(cc.Bytes);
  CrtWriteChar(cc)
End;

{ write a string of 1-byte chars to the terminal }
Procedure CWrite( s : TString );
Begin
  WriteToMirrorFiles(s);
  CrtWriteShortString(s)
End;

{ write a new line to the terminal }
Procedure CWriteLn;
Begin
  WritelnToMirrorFiles('');
  CrtWriteLn
End;

{ write a byte or an integer }
Procedure CWriteInt( v : Integer );
Begin
  CWrite(IntToShortString(v))
End;

{ write a byte or a positive integer }
Procedure CWritePosInt( v : PosInt );
Begin
  CWrite(PosIntToShortString(v))
End;

{ write a long integer }
Procedure CWriteLongInt( v : LongInt );
Begin
  CWrite(LongIntToShortString(v))
End;

{ write a byte or an integer }
Procedure CWriteBool( b : Boolean );
Begin
  CWrite(BoolToShortString(b))
End;

{ display a string content as an array of char codes }
Procedure CWriteStrCharCodes( s : TString );
Var 
  i : 1..StringMaxSize;
  First : Boolean;
Begin
  First := True;
  CWrite('[');
  For i := 1 to Length(s) Do
  Begin
    If Not First Then
      CWrite(',');
    CWriteInt(Ord(s[i]));
    First := False
  End;
  CWrite(']')
End;

{ write a warning }
Procedure CWriteWarning( s : TString );
Begin
  CWrite('***WARNING: ');
  CWrite(s);
End;

{ writeln a warning }
Procedure CWriteLnWarning( s : TString );
Begin
  CWriteWarning(s);
  CWriteLn
End;

End.