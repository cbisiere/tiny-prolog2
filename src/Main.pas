{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Main.pas                                                   }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                           M A I N  P R O G R A M                           }
{                                                                            }
{----------------------------------------------------------------------------}

{ $C-} { TP3: Ctrl-C during I/O does not interrupt program execution }
{ $U-} { TP3: Ctrl-C does not interrupt program execution }

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{$I TP4.pas     }  { TP3|4.pas: Turbo Pascal; FPC.pas: Free Pascal }

Procedure CoreDump( Message : AnyStr; Trace : Boolean ); Forward;
Procedure CheckCondition( Cond : Boolean; Message : AnyStr ); Forward;
Procedure WriteToCurrentOutput( s : AnyStr ); Forward;

{$I Files.pas     }  { i/o helpers                                 }
{$I Strings.pas   }  { string and chars                            }
{$I Trace.pas     }  { trace file                                  }
{$I Memory.pas    }  { memory management: GC, cloning...           }
{$I PObjRest.pas  }  { restore stack                               }
{$I PObj.pas      }  { Prolog objects: common definitions          }
{$I PObjStr.pas   }  { Prolog objects: long string                 }
{$I PObjDict.pas  }  { Prolog objects: dictionary entry            }
{$I PObjEq.pas    }  { Prolog objects: (in)equations, system       }
{$I PObjTerm.pas  }  { Prolog objects: terms                       }
{$I PObjProg.pas  }  { Prolog objects: program, rules, queries     }
{$I PObjNew.pas   }  { Prolog objects: new / dispose               }
{$I Error.pas     }  { error handling, termination                 }
{$I Keyboard.pas  }  { read from keyboard w/ history               }
{$I Input.pas     }  { terminal and input file stack               }
{$I Output.pas    }  { terminal and output file stack              }
{$I Unparse.pas   }  { decode objects                              }
{$I Reduc.pas     }  { system reduction                            }
{$I Parse.pas     }  { encode objects                              }
{$I Clock.pas     }  { Prolog clock                                }
{$I Run.pas       }  { load rules and execute queries              }
{$I Sys.pas       }  { system calls                                }
{$I Debug.pas     }  { core dump                                   }
{$I Init.pas      }  { initialization                              }


{ compile the user program and solve each query }
Procedure Main;
var
  P : ProgPtr;
Begin
  Initialize;
  P := CreateProgram;
  ProcessParameters(P);
  InitHistory;
  Repeat
    Error := False;
    CWrite('> ');
    ReadFromConsole;
    CompileCommandLineQueries(P);
    If Not Error Then
      AnswerQueries(P,False)
  Until False
End;

{ main }
Begin
  Main
End.
