{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Num.pas                                                    }
{   Author      : Christophe Bisiere                                         }
{   Date        : 2022-09-17                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                        N U M E R I C A L   V A L U E S                     }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Unit Num;

Interface

Uses
  ShortStr;

{$IFDEF MSDOS}
Const
  MaxPosInt = 2147483647; { 2^31 - 1 }
Type 
  { Word is too small on MDSOS, eg to store UTF-8 codepoints; so we use 4-byte
   signed integer instead }
  PosInt = LongInt; 
  LongLongInt = Real; { simulate a very LongInt }
  LongReal = Extended; { high precision real }
  Pointer = ^Integer; { generic pointer }
{$ELSE}
Const
  MaxPosInt = 4294967295; { 2^32 - 1 }
  MaxLongInt = 1e+24;
 Type
  PosInt = UInt32; { 4-byte unsigned integer }
  LongLongInt = Real; { simulate a very long integer }
  LongReal = Extended; { highest precision real }
{$ENDIF}

Function Max( a,b : Integer ) : Integer;
Function Min( a,b : Integer ) : Integer;
Function PosIntToShortString( v : PosInt ) : TString;
Function LongRealToLongInt( v : LongReal ) : LongInt;
Function LongIntToShortString( v : LongInt ) : TString;
Function LongLongIntToShortString( v : LongLongInt ) : TString;
Function LongRealToShortString( v : LongReal ) : TString;
Function ShortStringToLongInt( s : TString; Var code : Integer ) : LongInt;
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

{ format a LongReal for display; makes it look nice, and like a Prolog real 
 constant; '1.20000000000000000000E+0002' => '1.2e+2' }
Function LongRealToShortString( v : LongReal ) : TString;
Var 
  s : TString;
  man, exp : TString; { mantissa, exponent }
  e,dot : Byte;
Begin
  Str(v,s);
  s := TrimLeftSpaces(s);
  e := Pos('E',s);
  If e > 0 Then
  Begin
    man := Copy(s,1,e-1);
    exp := Copy(s,e+1,Length(s));
    { remove useless trailing zeros from the mantissa, keeping one zero after 
     the dot }
    dot := Pos('.',man);
    If dot > 0 Then
      While (Length(man) > dot+1) And (man[Length(man)] = '0') Do
        Delete(man,Length(man),1);
    { remove leading zeros from the exponent }
    If (Length(exp) > 0) And (exp[1] In ['+','-']) Then
      While (Length(exp) > 2) And (exp[2] = '0') Do
        Delete(exp,2,1);
    { reconstruct the string representation }
    s := man + 'e' + exp
  End;
  LongRealToShortString := s
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