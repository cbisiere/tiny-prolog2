{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Main.pas                                                   }
{   Author      : Christophe Bisière                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                           M A I N  P R O G R A M                           }
{                                                                            }
{----------------------------------------------------------------------------}

{$C-} { TP3: Ctrl-C during I/O does not interrupt program execution }
{$U-} { TP3: Ctrl-C does not interrupt program execution }

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{$I TP3.pas     }  { TP3.pas: Turbo Pascal 3; FPC.pas: Free Pascal Compiler }

Procedure CheckCondition( Cond : Boolean; Message : AnyStr ); Forward;
Procedure DumpBacktrace; Forward;

{$I Memory.pas    }  { memory management: GC, cloning...           }
{$I Restore.pas   }  { restore stack                               }
{$I PObj.pas      }  { Prolog objects: common definitions          }
{$I PObjStr.pas   }  { Prolog objects: long string                 }
{$I PObjEq.pas    }  { Prolog objects: (in)equations, system       }
{$I PObjTerm.pas  }  { Prolog objects: terms                       }
{$I PObjProg.pas  }  { Prolog objects: program, rules, queries     }
{$I Keyboard.pas  }  { read from keyboard w/ history               }
{$I Input.pas     }  { read the input flow                         }
{$I Unparse.pas   }  { decode objects                              }
{$I Parse.pas     }  { encode objects                              }
{$I Reduc.pas     }  { system reduction                            }
{$I Clock.pas     }  { Prolog clock                                }
{$I Run.pas       }  { load rules and execute queries              }
{$I Sys.pas       }  { system calls                                }

{$I Init.pas      }  { Module : Initializations                    }
{$I Debug.pas     }

{ reset the Prolog engine }
Function ResetMachine : ProgPtr;
Var P : ProgPtr;
Begin
  Initialisation;
  P := NewProgram;
  CurrentProgram := P; { debug }
  RegisterPredefinedConstants(P);
  ResetMachine := P
End;

{ compile the user program and solve each query }
Procedure Main;
var
  P : ProgPtr;
  PP : TPObjPtr Absolute P;
  Q : QueryPtr;
  FileName : StrPtr;
  FileNameObj : TPObjPtr Absolute FileName;
Begin
  MMInit;
  P := ResetMachine;
  AddGCRoot(PP);
  LoadProgram(P,NewStringFrom('start.pro'),RTYPE_AUTO);
  If ParamCount = 1 Then
  Begin
    FileName := NewStringFrom(ParamStr(1)); { warning: this string might be GC'ed }
    LoadProgram(P,FileName,RTYPE_USER);
  End;
  InitHistory;
  Repeat
    Error := False;
    Write('> ');
    ReadCommand;
    Q := CompileCommandLineQueries(P);
    If Not Error Then
      AnswerQueries(P,Q);
    RemoveCommandLineQueries(P);
  Until False
End;

{ main }
Begin
  Main
End.
