{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Num.pas                                                    }
{   Author      : Christophe Bisiere                                         }
{   Date        : 2022-09-17                                                 }
{   Updated     : 2022-2026                                                  }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                        N U M E R I C A L   V A L U E S                     }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }

Unit Num;

Interface

Uses
  TPointer,
  ShortStr;

{$IFDEF TPC}
Const
  MaxWord = 65535; { 2^16 - 1 }
  MaxPosInt = 2147483647; { 2^31 - 1 }
  LongRealPrecision = 20; { max number of significant digits; TP4 p.40 }
Type 
  { Word is too small on MSDOS, e.g. to store UTF-8 codepoints; so we use 4-byte
   signed integer instead }
  PosInt = LongInt; 
  LongLongInt = Extended; { simulate a very long integer }
  LongReal = Extended; { high precision real }
{$ELSE}
Const
  MaxWord = 65535; { 2^16 - 1 }
  MaxPosInt = 4294967295; { 2^32 - 1 }
  MaxLongInt = 1e+24;
  LongRealPrecision = 20; { https://www.freepascal.org/docs-html/ref/refsu5.html }
 Type
  PosInt = UInt32; { 4-byte unsigned integer }
  LongLongInt = Extended; { simulate a very long integer }
  LongReal = Extended; { highest precision real }
{$ENDIF}

Function Max( a,b : Integer ) : Integer;
Function Min( a,b : Integer ) : Integer;
Function PointerToShortString( p : Pointer ) : TString;
Function PosIntToShortString( v : PosInt ) : TString;
Function LongRealToLongInt( v : LongReal ) : LongInt;
Function LongIntToShortString( v : LongInt ) : TString;
Function LongLongIntToShortString( v : LongLongInt ) : TString;
Function LongRealToShortString( v : LongReal ) : TString;
Function FormatRealInShortString( s : TString; Round : Boolean ) : TString;
Function ShortStringToLongInt( s : TString; Var code : Integer ) : LongInt;
Function ShortStringToLongLongInt( s : TString; 
    Var code : Integer ) : LongLongInt;
Function ShortStringToPosInt( s : TString; Var code : Integer ) : PosInt;
Function ShortStringToLongReal( s : TString; Var code : Integer ) : LongReal;
Function LongIntDiv( x,y : LongInt ) : LongInt;

Implementation
{-----------------------------------------------------------------------------}

{ maximum of two integers }
Function Max( a,b : Integer ) : Integer;
Begin
  If a >= b Then
    Max := a
  Else
    Max := b
End;

{ minimum of two integers }
Function Min( a,b : Integer ) : Integer;
Begin
  If a <= b Then
    Min := a
  Else
    Min := b
End;

{ format a pointer for display }
Function PointerToShortString( p : Pointer ) : TString;
Var 
  s1,s2 : TString;
Begin
  s1 := '';
  If Seg(p) <> 0 Then { Seg(p) always 0 on Free Pascal }
  Begin
    Str(Seg(p),s1);
    s1 := s1 + ':'
  End;
  Str(Ofs(p),s2);
  PointerToShortString := s1 + s2
End;

{ format a positive integer for display }
Function PosIntToShortString( v : PosInt ) : TString;
Var 
  s : TString;
Begin
  Str(v,s);
  PosIntToShortString := s
End;

{ round a long real to a long integer value; may crash }
Function LongRealToLongInt( v : LongReal ) : LongInt;
Begin
  LongRealToLongInt := Round(v)
End;

{ format a LongInt for display }
Function LongIntToShortString( v : LongInt ) : TString;
Var 
  s : TString;
Begin
  Str(v,s);
  LongIntToShortString := s
End;

{ format a LongLongInt for display }
Function LongLongIntToShortString( v : LongLongInt ) : TString;
Var 
  s : TString;
Begin
  Str(v:StringMaxSize:0,s);
  LongLongIntToShortString := TrimLeftSpaces(s)
End;

{ convert a LongReal to a short string (Pascal format) }
Function LongRealToShortString( v : LongReal ) : TString;
Var 
  s : TString;
Begin
  Str(v,s);
  LongRealToShortString := s
 End;


{ remove useless leading zeros from a number in s, leaving one zero if needed; 
 s may start with a sign, and my contain a fractional part, which are preserved; 
 if s is malformed, just return the original string }
Function CleanLeadingZeros( s : TString ) : TString;
Var
  sign,frac : TString;
  dot : TStringSize;
Begin
  CleanLeadingZeros := s;
  If Length(s) = 0 Then
    Exit;
  sign := '';
  If s[1] In ['+','-'] Then
  Begin
    sign := s[1];
    Delete(s,1,1)
  End;
  If Length(s) = 0 Then
    Exit;
  frac := '';
  dot := Pos('.',s);
  If dot > 0 Then
  Begin
    frac := Copy(s,dot,Length(s));
    Delete(s,dot,Length(s))
  End;
  If Length(s) = 0 Then
    Exit;
  While (Length(s) > 1) And (s[1] = '0') Do
    Delete(s,1,1);
  CleanLeadingZeros := sign + s + frac
End;

 { format a string containing a real value for display; makes it look nice, 
  and like a Prolog real constant; '1.20000000000000000004E+0002' => 
  '1.2e+2', optionally rounding for errors (e.g., when the real value results 
  from an operation on real values) }
Function FormatRealInShortString( s : TString; Round : Boolean ) : TString;
Var 
  man, exp : TString; { mantissa, exponent }
  e,dot,i : TStringSize;
  Done : Boolean;
Begin
  s := TrimLeftSpaces(s);
  e := Pos('E',s);
  If e > 0 Then
  Begin
    { extract mantissa and exponent, including signs if any }
    man := Copy(s,1,e-1);
    exp := Copy(s,e+1,Length(s));
    { remove leading zeros from the exponent }
    exp := CleanLeadingZeros(exp);
    { remove useless leading zeros from the mantissa }
    man := CleanLeadingZeros(man);
    { remove useless trailing digit from the mantissa, keeping one zero after 
     the dot }
    dot := Pos('.',man);
    If dot > 0 Then
    Begin
      { remove the last two digits, which are often a rounding error }
      If Round And (Length(man) > dot+2) Then
        Delete(man,Length(man)-1,2);
      { removes all trailing zeros but one }
      While (Length(man) > dot+1) And (man[Length(man)] = '0') Do
        Delete(man,Length(man),1);
      If Round Then
      Begin
        { round "long" trailing sequence of 9: "22.001999999" => "22.002" }
        i := Length(man);
        While (i >= dot) And (man[i] = '9') Do
          i := i - 1;
        { at this point, i is the index of the character before the trailing 
        sequence of 9s if any, so len-i is the number of trailing 9s; if fixing
        floating point rounding errors is requested, consider such sequence as
        long when its length is at least half of the LongReal precision, that is
        about 10 digits }
        If (Length(man)-i) >= (LongRealPrecision Div 2) Then
        Begin
          Case man[i] Of 
          '0'..'8': { stop on a non-9 after the dot, e.g. "1" in "22.001999999" }
            Begin
              Delete(man,i+1,Length(man)); { drop the trailing 9s }
              man[i] := Chr(Ord(man[i])+1) { replace "1" with "2" }
            End;
          '.': { the frac part is only made of 9s, e.g. "19.9999999999" }
            Begin
              { drop the frac part, including the dot: "19.9999999999" => "19" }
              Delete(man,i,Length(man));
              { add 1: "19" => "20" }
              i := Length(man);
              Done := False;
              While (i >= 1) And (Not Done) Do
              Begin
                Case man[i] Of
                '0'..'8':
                  Begin
                    man[i] := Chr(Ord(man[i])+1);
                    Done := True;
                  End;
                '9':
                  Begin
                    man[i] := '0' { not done yat, as wa have a carry of 1 }
                  End;
                End;
                i := i - 1
              End;
              If Not Done Then
                man := '1' + man;
              man := man + '.0'
            End
          End
        End
      End
    End;
    { reconstruct the string representation }
    s := man + 'e' + exp
  End;
  FormatRealInShortString := s
End;

{ convert a Pascal string to a LongInt; code is 0 if the operation succeeds,
  or the index of the character preventing the conversion }
Function ShortStringToLongInt( s : TString; Var code : Integer ) : LongInt;
Var 
  v : LongInt;
Begin
  Val(s,v,code);
  ShortStringToLongInt := v
End;

{ convert a Pascal string to a LongLongInt; code is 0 if the operation succeeds,
  or the index of the character preventing the conversion }
Function ShortStringToLongLongInt( s : TString; 
    Var code : Integer ) : LongLongInt;
Var 
  v : LongLongInt;
Begin
  Val(s,v,code);
  ShortStringToLongLongInt := v
End;

{ convert a Pascal string to a PosInt; code is 0 if the operation succeeds,
  or the index of the character preventing the conversion }
Function ShortStringToPosInt( s : TString; Var code : Integer ) : PosInt;
Var 
  v : PosInt;
Begin
  Val(s,v,code);
  ShortStringToPosInt := v
End;

{ convert a Pascal string to a high precision Real; code is 0 if the operation 
 succeeds, or the index of the character preventing the conversion }
Function ShortStringToLongReal( s : TString; Var code : Integer ) : LongReal;
Var 
  v : LongReal;
Begin
  Val(s,v,code);
  ShortStringToLongReal := v
End;

{ integer division of two LongInt values }
Function LongIntDiv( x,y : LongInt ) : LongInt;
Begin
  LongIntDiv := x Div y
End;

End.