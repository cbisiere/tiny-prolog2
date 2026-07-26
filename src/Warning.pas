{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Warning.pas                                                }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022-2026                                                  }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                               W A R N I N G S                              }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }

Unit Warning;

Interface

Uses
  ShortStr,
  CWrites,
  PObjStr,
  PObjDef,
  PObjBTer;

Procedure LongWarning( B : BTermPtr; s : StrPtr );
Procedure ShortWarning( B : BTermPtr; s : TString );
Procedure WarnAbout( B : BTermPtr; prompt : TString; str : StrPtr );

Implementation
{-----------------------------------------------------------------------------}

{ display the prefix of a warning message s (w/ a copy to paper file, when 
 requested) about the clearing of a goal using a rule whose head is the Bterm B; 
 B is Nil if the goal is part of a query, not a rule's queue, as in 
 "-> syscall(sysfail);" }
Procedure WarningPrefix( B : BTermPtr );
Begin
  CWrite('***WARNING: ');
  If B <> Nil Then
  Begin
    Str_CWrite(BTerm_GetSignature(B));
    CWrite(': ')
  End
End;

{ display a warning message s (long string) }
Procedure LongWarning( B : BTermPtr; s : StrPtr );
Begin
  WarningPrefix(B);
  Str_CWrite(s);
  CWriteLn
End;

{ display a warning message s (short string) }
Procedure ShortWarning( B : BTermPtr; s : TString );
Begin
  WarningPrefix(B);
  CWrite(s);
  CWriteLn
End;

{ warning about a piece of data contained into a long string }
Procedure WarnAbout( B : BTermPtr; prompt : TString; str : StrPtr );
Var
  msg : StrPtr;
Begin
  msg := Str_NewFromShortString(prompt + ': ''');
  Str_Concat(msg,str);
  Str_Append(msg,'''');
  LongWarning(B,msg)
End;

End.