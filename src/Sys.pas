{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Dict.pas                                                   }
{   Author      : Christophe Bisière                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                         S Y S T E M   C A L L S                            }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ predefined predicates }

Const
  NBPred = 11;
  MaxPredLength = 9; { max string length of predefined predicate }
Type 
  TPP = (PP_QUIT,PP_INSERT,PP_LIST,PP_OUT,PP_OUTM,PP_LINE,PP_BACKTRACE,PP_CLRSRC,PP_EVAL,PP_ASSIGN,PP_DUMP);
  TPPred = Record
    I : TPP; { identifier }
    S : String[MaxPredLength]; { identifier as string }
    N : Byte { number of arguments }
  End;
  TAPPred = Array[1..NBPred] Of TPPred;

Const 
  APPred : TAPPred = (
    (I:PP_QUIT;S:'QUIT';N:0),
    (I:PP_INSERT;S:'INSERT';N:1),
    (I:PP_LIST;S:'LIST';N:0),
    (I:PP_OUT;S:'OUT';N:1),
    (I:PP_OUTM;S:'OUTM';N:1),
    (I:PP_LINE;S:'LINE';N:0),
    (I:PP_BACKTRACE;S:'BACKTRACE';N:0),
    (I:PP_CLRSRC;S:'CLRSRC';N:0),
    (I:PP_EVAL;S:'EVAL';N:2),
    (I:PP_ASSIGN;S:'ASSIGN';N:2),
    (I:PP_DUMP;S:'DUMP';N:0)
  );

{ lookup for a predicate; set the found predicate record; return True if found  }
Function LookupPred( str : AnyStr; Var rec : TPPred) : Boolean;
Var 
  i : 0..NBPred;
  Found : Boolean;
Begin
  i := 0;
  Found := False;
  While (Not Found) And (i<NBPred) Do
  Begin
    i := i + 1;
    If APPred[i].S = str Then
    Begin
      Found := True;
      rec := APPred[i]
    End
  End;
  LookupPred := Found
End;



{ install all predefined constants }
Procedure RegisterPredefined( P : ProgPtr );
Var I : IdPtr;
Begin
  I := InstallIdentifier(P^.PP_DCON,NewStringFrom('SYSCALL'))
End;

{ execute a system call <SYSCALL,Code,Arg1,...ArgN>, meaning Code(Arg1,...,ArgN) }
Function ExecutionSysCallOk; (* ( T : TermPtr; P : ProgPtr; Q : QueryPtr ) : Boolean; *)
Var
  FT : FuncPtr Absolute T;
  Ok : Boolean;
  Ident : StrPtr;
  NbArgs : Integer;
  SysCallCode : StrPtr;
  NbPar : Integer; { number of parameters of the system call }
  T1 : TermPtr;
  IT1 : IdPtr Absolute T1;
  VT1 : VarPtr Absolute T1;
  T2 : TermPtr;
  IT2 : IdPtr Absolute T2;
  C : ConstPtr;
  TC : TPObjPtr Absolute C;
  rec : TPPred;
  str : AnyStr;
  I : IdPtr;
  TI : TermPtr Absolute I; 

  { get n-th argument of the predicate represented by tuple F }
  Function GetPArg( n : Byte; F : FuncPtr ) : TermPtr;
  Begin
    GetPArg := Argument(2+n,F)
  End;

Begin
  { coded as a functional symbol }
  CheckCondition(TypeOfTerm(T) = FuncSymbol,'SYSCALL: functional symbol expected');
  
  { first parameter is SYSCALL }
  T1 := Argument(1,FT);
  CheckCondition(TypeOfTerm(T1) = Identifier,'SYSCALL: constant expected');
  SysCallCode := IdentifierGetStr(IT1);
  CheckCondition(StrEqualTo(SysCallCode,'SYSCALL'),'Not a SYSCALL');

  { there are at least two arguments: <SYSCALL,identifier,..> }
  NbArgs := NbArguments(FT);
  Ok := NbArgs >= 2;  
  If Ok Then
  Begin
    T2 := Argument(2,FT);
    Ident := IdentifierGetStr(IT2);
    Ok := StrLength(Ident) <= MaxPredLength
  End;

  { predicate is known and has the correct number of parameters }
  If Ok Then
  Begin
    str := StrGetString(Ident);
    Ok := LookupPred(str,rec);
    If Ok Then
    Begin
      NbPar := NbArgs - 2;
      Ok := NbPar = rec.N
    End;
  End;

  { clear the predicate }
  If Ok Then
  Begin
    Case rec.I Of
    PP_ASSIGN: { assign(file_name, "myfile.txt") }
      Begin
        { note: re-assignments are tricky to handle, as the reduced system, after
          e.g. "assign(test,1)", contains i=1 (w/o any remaining reference to the 
          identifier) }
        { get the identifier }
        T1 := GetPArg(1,FT);
        Case TypeOfTerm(T1) Of
        Identifier: { an identifier, thus unbound (first assignment) }
          I := IT1;
        Variable:
          Begin
            I := VT1^.TV_IRED;
            If I = Nil Then { variable has never been bound to an identifier }
              I := EvaluateToIdentifier(GetPArg(1,FT));
            Ok := I <> Nil;
            If Ok Then
            Begin
              { unbound the variable (which was bounded to the term) }
              VT1^.TV_TRED := Nil;
              VT1^.TV_FWAT := Nil
            End
          End;
        Else
          Ok := False
        End;
        If Ok Then
        Begin
          { neutralize its role (if any) in the reduced system as variable-like, 
            assigned identifier }
          I^.TV_TRED := Nil;
          I^.TV_FWAT := Nil;
          { assign }
          I^.TV_ASSI := True;
          { copy the term, including variables and constraints;
          thus, our assign is similar to "cassign(i,t)";
          see p113 of the PrologII+ documentation }
          T2 := DeepCopy(GetPArg(2,FT));
          Ok := ReduceOneEq(TI,T2) { "ident = term" }
        End
      End;
    PP_EVAL: { val(100,x) }
      Begin
        C := EvaluateToConstant(GetPArg(1,FT));
        Ok := C <> Nil;
        If Ok Then
          Ok := ReduceOneEq(GetPArg(2,FT),TC);
      End;
    PP_QUIT:
      Halt;
    PP_INSERT:
      Begin
        C := EvaluateToConstant(GetPArg(1,FT)); { TODO: check is a QString }
        Ok := C <> Nil;
        If Ok Then
        Begin
          LoadProgram(P,GetConstAsString(C,False),RTYPE_USER);
          Ok := Not Error
        End
      End;
    PP_LIST:
      OutQuestionRules(Q,RTYPE_USER);
    PP_OUT:
      OutTerm(GetPArg(1,FT));
    PP_OUTM:
      OutTermBis(GetPArg(1,FT),False,False);
    PP_LINE:
      WriteLn;
    PP_BACKTRACE:
      DumpBacktrace;
    PP_CLRSRC:
      ClrScr;
    PP_DUMP:
      DumpRegisteredObject
    End
  End;
    
  ExecutionSysCallOk := Ok
End;
