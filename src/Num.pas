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
  Strings;

Const
  MaxLongInt = 1e+24;

{$IFDEF MSDOS}
Type 
  LongInt = Real; { simulate a LongInt }
  LongLongInt = Real; { simulate a very LongInt }
  LongReal = Real; { high precision real }
  Pointer = ^Integer; { generic pointer }
{$ELSE}
 Type
  LongLongInt = Real; { simulate a very long integer }
  LongReal = Extended; { highest precision real }
{$ENDIF}

Function Max( a,b : Integer ) : Integer;
Function Min( a,b : Integer ) : Integer;
Function LongRealToLongInt( v : LongReal ) : LongInt;
Function LongIntToStr( v : LongInt ) : TString;
Function LongLongIntToStr( v : LongLongInt ) : TString;
Function LongRealToStr( v : LongReal ) : TString;
Function StrToLongInt( s : TString; Var code : Integer ) : LongInt;
Function StrToLongReal( s : TString; Var code : Integer ) : LongReal;
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

{ round a long real to a long integer value; may crash }
Function LongRealToLongInt( v : LongReal ) : LongInt;
Begin
{$IFDEF MSDOS}
  LongRealToLongInt := v
{$ELSE}
  LongRealToLongInt := Round(v)
{$ENDIF}
End;

{ format a LongInt for display }
Function LongIntToStr( v : LongInt ) : TString;
Var 
  s : TString;
Begin
{$IFDEF MSDOS}
  Str(v:StringMaxSize:0,s);
  LongIntToStr := TrimLeftSpaces(s);
{$ELSE}
  Str(v,s);
  LongIntToStr := s
{$ENDIF}
End;

{ format a LongLongInt for display }
Function LongLongIntToStr( v : LongLongInt ) : TString;
{$IFDEF MSDOS}
Begin
  LongLongIntToStr := LongIntToStr(v)
{$ELSE}
Var 
  s : TString;
Begin
  Str(v:StringMaxSize:0,s);
  LongLongIntToStr := TrimLeftSpaces(s)
{$ENDIF}
End;

{ format a LongReal for display }
Function LongRealToStr( v : LongReal ) : TString;
Var 
  s : TString;
Begin
  Str(v,s);
  LongRealToStr := TrimLeftSpaces(s)
End;

{ convert a Pascal string to a LongInt; code is 0 if the operation succeeds,
  or the index of the character preventing the conversion }
Function StrToLongInt( s : TString; Var code : Integer ) : LongInt;
Var v : LongInt;
Begin
  Val(s,v,code);
  StrToLongInt := v
End;

{ convert a Pascal string to a high precision Real; code is 0 if the operation 
 succeeds, or the index of the character preventing the conversion }
Function StrToLongReal( s : TString; Var code : Integer ) : LongReal;
Var 
  v : LongReal;
Begin
  Val(s,v,code);
  StrToLongReal := v
End;

{ integer division of two LongInt values }
Function LongIntDiv( x,y : LongInt ) : LongInt;
Begin
{$IFDEF MSDOS}
  LongIntDiv := Int(x/y)
{$ELSE}
  LongIntDiv := x Div y
{$ENDIF}
End;

End.