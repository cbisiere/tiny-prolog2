{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : DateTime.pas                                               }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2024                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                          D A T E   &   T I M E                             }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }

Unit DateTime;

Interface

Uses 
{$IFDEF FPC}
  SysUtils,
  DateUtils,
{$ENDIF}
  Dos;

Function UnixTime : LongInt;
Function SecondsSinceMidnight : LongInt;

implementation

{ is Year a leap year? }
Function IsLeapYear( Year : Word ) : Boolean;
Begin
  IsLeapYear := (((Year mod 4) = 0) And ((Year mod 100) <> 0)) Or 
      ((Year mod 400) = 0)
End;

{ number of full days between January 1st and a particular day (not counting 
 that day itself) }
Function FullDaysSinceJan1( Year,Month,Day : Word ) : Word;
Const
  CumDays : Array[0..11] Of Integer = 
      (0,31,59,90,120,151,181,212,243,273,304,334);
Var
  n : Word;
Begin
  n := CumDays[Month-1] + Day - 1;
  If (Month > 2) And IsLeapYear(Year) Then
    n := n + 1;
  FullDaysSinceJan1 := n
End;

{ number of seconds since Jan 1, 1970; YearDays is the number of full days 
 elapsed since the beginning of the year; today's unix time is around 1.7
 billion; a LongInt on TP4 (signed 4-byte integer) is large enough to store 
 a Unix time stamp until Jan 19, 2038; it is assumed, and not checked, that
 this function is called with a date between Jan 1, 1970 and that upper bound }
Function SecondsSinceEpoch( Year,YearDays,Hour,Min,Sec : Word ) : LongInt;
Begin
  Year := Year - 1900;
  SecondsSinceEpoch := Sec + Min*60 + Hour*3600 
    + (YearDays
      + (Year-70)*365 
      + ((Year-69) div 4) { + nb. of years divisible by 4, exc. current year }
      - ((Year-1) div 100)  { - those div. by 100, ... }
      + ((Year+299) div 400))*86400; { + those divisible by 400, ... }
End;

{ Unix timestamp }
{$IFNDEF FPC}
Function UnixTime : LongInt;
Var
  Hour,Min,Sec,HSec : Word;
  Year, Month, Day, DayofWeek : Word;
  YearDays : Word;
Begin
  GetTime(Hour,Min,Sec,HSec);
  GetDate(Year,Month,Day,DayofWeek);
  YearDays := FullDaysSinceJan1(Year,Month,Day);
  UnixTime := SecondsSinceEpoch(Year,YearDays,Hour,Min,Sec)
End;
{$ELSE}
Function UnixTime : LongInt;
Begin
  UnixTime := DateTimeToUnix(Now)
End;
{$ENDIF}

{ number of seconds since midnight this morning}
Function SecondsSinceMidnight : LongInt;
Var
  Hour,Min,Sec,HSec : Word;
Begin
  GetTime(Hour,Min,Sec,HSec);
  SecondsSinceMidnight := Sec + Min*60 + Hour*3600;
End;

End.
