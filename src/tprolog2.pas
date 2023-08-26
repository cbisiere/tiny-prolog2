{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : tprolog2.pas                                               }
{   Author      : Christophe Bisiere                                         }
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

{$I FPC.pas     }  { TP3|4.pas: Turbo Pascal; FPC.pas: Free Pascal }

Procedure WriteToCurrentOutput( s : TString ); Forward;
Procedure WriteToEchoFile( s : TString ); Forward;

{$I Common.pas    }  { useful constants, types, etc.               }
{$I Files.pas     }  { i/o helpers                                 }
{$I Error.pas     }  { error handling, termination                 }
{$I Strings.pas   }  { string and chars                            }
{$I Char.pas      }  { multi-byte char                             }
{$I Crt2.pas      }  { Crt with multi-byte support                 }
{$I Trace.pas     }  { trace file                                  }
{$I IChar.pas     }  { input char with position                    }
{$I Buffer.pas    }  { circular input buffer                       }
{$I Readline.pas  }  { read from keyboard w/ history               }
{$I Memory.pas    }  { memory management: GC, cloning...           }
{$I PObjRest.pas  }  { restore stack                               }
{$I PObj.pas      }  { Prolog objects: common definitions          }
{$I PObjStr.pas   }  { Prolog objects: long string                 }
{$I PObjTok.pas   }  { Prolog objects: token                       }
{$I PObjDict.pas  }  { Prolog objects: dictionary entry            }
{$I PObjEq.pas    }  { Prolog objects: (in)equations, system       }
{$I PObjTerm.pas  }  { Prolog objects: terms                       }
{$I PObjProg.pas  }  { Prolog objects: program, rules, queries     }
{$I PObjNew.pas   }  { Prolog objects: new / dispose               }
{$I IStream.pas   }  { read from file                              }
{$I IStack.pas    }  { terminal and input file stack               }
{$I OStack.pas    }  { terminal and output file stack              }
{$I Encoding.pas  }  { encode and decode terms                     }
{$I Unparse.pas   }  { print objects                               }
{$I Reduc.pas     }  { system reduction                            }
{$I Tokenize.pas  }  { Tokenize an input stream                    }
{$I Expr.pas      }  { expressions                                 }
{$I Parse.pas     }  { encode objects                              }
{$I Clock.pas     }  { Prolog clock                                }
{$I Run.pas       }  { load rules and execute queries              }
{$I Sys.pas       }  { system calls                                }
{$I Debug.pas     }  { core dump                                   }
{$I Init.pas      }  { initialization                              }


{ compile the user program and solve each query }
Procedure Main;
Var
  P : ProgPtr;
  Prompt : TString;
Begin
  Initialize;
  P := CreateProgram;
  ProcessParameters(P);
  Repeat
    Error := False;
    Case GetSyntax(P) Of
    PrologII:
      Prompt := '> ';
    PrologIIc:
      Prompt := 'c> ';
    PrologIIp:
      Prompt := '+> ';
    Edinburgh:
      Prompt := '?- ';
    End;
    CWrite(Prompt);
    ReadFromConsole;
    While ParseCommandLineQuery(P) And Not Error Do
      AnswerQueries(P,False)
  Until False
End;

{ main }
Begin
  Main
End.
