{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Sys.pas                                                    }
{   Author      : Christophe Bisiere                                         }
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

Procedure DumpBacktrace; Forward;

{ predefined predicates and functions }

Const
  NBPred = 28;
  MaxPredLength = 20; { max string length of a predefined predicate 
   or evaluable function }
Type
  TPPType = (PPredicate,PFunction); { type: predicate or function }
  TPP = (
    PP_INPUT_IS,PP_INPUT,PP_CLOSE_CURRENT_INPUT,PP_CLOSE_INPUT,PP_CLEAR_INPUT,
    PP_IN_TERM,PP_IN_CHAR,
    PP_OUTPUT_IS,PP_OUTPUT,PP_CLOSE_CURRENT_OUTPUT,PP_CLOSE_OUTPUT,PP_FLUSH,
    PP_QUIT,PP_INSERT,PP_LIST,
    PP_OUT,PP_OUTM,PP_LINE,
    PP_BACKTRACE,PP_CLRSRC,PP_EVAL,PP_ASSIGN,PP_DUMP,
    PF_ADD,PF_SUB,PF_MUL,PF_DIV,PF_INF);
  TPPred = Record
    T : TPPType;
    I : TPP; { identifier }
    S : String[MaxPredLength]; { identifier as string }
    N : Byte { number of arguments }
  End;
  TAPPred = Array[1..NBPred] Of TPPred;

Const 
  APPred : TAPPred = (
    (T:PPredicate;I:PP_INPUT_IS;S:'INPUT_IS';N:1),
    (T:PPredicate;I:PP_INPUT;S:'INPUT';N:1),
    (T:PPredicate;I:PP_CLOSE_CURRENT_INPUT;S:'CLOSE_CURRENT_INPUT';N:0),
    (T:PPredicate;I:PP_CLOSE_INPUT;S:'CLOSE_INPUT';N:1),
    (T:PPredicate;I:PP_CLEAR_INPUT;S:'CLEAR_INPUT';N:0),
    (T:PPredicate;I:PP_IN_TERM;S:'IN_TERM';N:1),
    (T:PPredicate;I:PP_IN_CHAR;S:'IN_CHAR';N:1),
    (T:PPredicate;I:PP_OUTPUT_IS;S:'OUTPUT_IS';N:1),
    (T:PPredicate;I:PP_OUTPUT;S:'OUTPUT';N:1),
    (T:PPredicate;I:PP_CLOSE_CURRENT_OUTPUT;S:'CLOSE_CURRENT_OUTPUT';N:0),
    (T:PPredicate;I:PP_CLOSE_OUTPUT;S:'CLOSE_OUTPUT';N:1),
    (T:PPredicate;I:PP_FLUSH;S:'FLUSH';N:0),
    (T:PPredicate;I:PP_QUIT;S:'QUIT';N:0),
    (T:PPredicate;I:PP_INSERT;S:'INSERT';N:1),
    (T:PPredicate;I:PP_LIST;S:'LIST';N:0),
    (T:PPredicate;I:PP_OUT;S:'OUT';N:1),
    (T:PPredicate;I:PP_OUTM;S:'OUTM';N:1),
    (T:PPredicate;I:PP_LINE;S:'LINE';N:0),
    (T:PPredicate;I:PP_BACKTRACE;S:'BACKTRACE';N:0),
    (T:PPredicate;I:PP_CLRSRC;S:'CLRSRC';N:0),
    (T:PPredicate;I:PP_EVAL;S:'EVAL';N:2),
    (T:PPredicate;I:PP_ASSIGN;S:'ASSIGN';N:2),
    (T:PPredicate;I:PP_DUMP;S:'DUMP';N:0),
    (T:PFunction;I:PF_ADD;S:'add';N:2),
    (T:PFunction;I:PF_SUB;S:'sub';N:2),
    (T:PFunction;I:PF_MUL;S:'mul';N:2),
    (T:PFunction;I:PF_DIV;S:'div';N:2),
    (T:PFunction;I:PF_INF;S:'inf';N:2)
  );

Const
  MaxFuncNbParams = 2; { maximum number of parameter for a predefined function }
Type
  TParArray = Array[1..MaxFuncNbParams] Of LongInt; { parameter value }

{ lookup for a predefined predicate or function; set the found record; 
  return True if found  }
Function LookupPred( typ : TPPType; str : AnyStr; Var rec : TPPred) : Boolean;
Var 
  i : 0..NBPred;
  Found : Boolean;
Begin
  i := 0;
  Found := False;
  While (Not Found) And (i<NBPred) Do
  Begin
    i := i + 1;
    If (APPred[i].T = typ) And (APPred[i].S = str) Then
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

{ evaluate a term T; The expression to be evaluated is constructed 
  recursively from constants, identifiers and evaluable functions;
  return Nil if the expression cannot be evaluated }
Function EvaluateExpression( T : TermPtr; P : ProgPtr ) : TermPtr;
Var
  FT : FuncPtr Absolute T;
  IT : IdPtr Absolute T;
  e : TermPtr;
  Ce : ConstPtr Absolute e;
  Ident : TermPtr;
  IIdent : IdPtr Absolute Ident;
  T1,T2 : TermPtr;
  CT1 : ConstPtr Absolute T1;
  CT2 : ConstPtr Absolute T2;
  r : LongInt;
  code : Integer;
  rs : AnyStr;
  s : StrPtr;
  Ok : Boolean;
  rec : TPPred;
  str : AnyStr;
  ParVal : TParArray;
  i : Byte;
Begin
  e := Nil;
  T := RepresentativeOf(T);
  If T <> Nil Then
  Begin
    Case TypeOfTerm(T) Of
    Identifier, Constant:
      e := T;
    Variable:
      e := T; { unbounded variable evaluates to itself (different from standard behavior) ) }
    FuncSymbol:
      Begin
        { try to get an evaluate an evaluable function }
        Ident := EvaluateExpression(Argument(1,FT),P);
        If Ident <> Nil Then
        Begin
          If TypeOfTerm(Ident) = Identifier Then
          Begin
            { function is known and has the correct number of parameters }
            str := IdentifierGetPStr(IIdent);
            Ok := LookupPred(PFunction,str,rec);
            If Ok Then
              Ok := NbArguments(FT) = rec.N + 1
            Else
              e := T; { not a known function: return the term }
            { evaluate the function's parameters }
            If Ok Then
              For i := 1 to rec.N Do
              Begin
                If Ok Then
                Begin
                  T1 := EvaluateExpression(Argument(1+i,FT),P);
                  Ok := T1 <> Nil;
                End;
                If Ok Then
                  Ok := TypeOfTerm(T1) = Constant;
                If Ok Then
                  Ok := ConstType(CT1) = Number;
                If Ok Then
                Begin
                  ParVal[i] := StrToLongInt(ConstGetPStr(CT1),code);
                  Ok := code = 0
                End
              End;
            If Ok Then
            Begin
              Case rec.I Of { TODO: absolute precision }
                PF_ADD:
                  r := ParVal[1] + ParVal[2];
                PF_SUB:
                  r := ParVal[1] - ParVal[2];
                PF_MUL:
                  r := ParVal[1] * ParVal[2];
                PF_DIV:
                  Begin
                    Ok := ParVal[2] <> 0;
                    If Ok Then
                      r := LongIntDiv(ParVal[1],ParVal[2])
                  End;
                PF_INF:
                  r := Ord(ParVal[1] < ParVal[2])
              End;
              If Ok Then
              Begin
                rs := LongIntToStr(r);
                s := NewStringFrom(rs);
                Ce := InstallConst(P^.PP_DCON,s,CN)
              End
            End
          End
          Else
            e := T { not an identifier (e.g. <100>), return the term }
        End
      End
    End
  End;
  EvaluateExpression := e
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
  VT2 : VarPtr Absolute T2;
  C : ConstPtr;
  TC : TPObjPtr Absolute C;
  rec : TPPred;
  str : AnyStr;
  ch : Char;
  I : IdPtr;
  TI : TermPtr Absolute I;
  Qi, QLast : QueryPtr;
  Stop : Boolean;
  FileName : AnyStr;

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
    Ok := LookupPred(PPredicate,str,rec);
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
              UnbindVar(VT1)
            End
          End;
        Else
          Ok := False
        End;
        If Ok Then
        Begin
          { neutralize its role (if any) in the reduced system as variable-like, 
            assigned identifier }
          UnbindVar(I);
          I^.TV_ASSI := True;
          { second parameter }
          T2 := GetPArg(2,FT);
          { assign }
          Ok := ReduceOneEq(TI,T2) { "ident = term" }
        End
      End;
    PP_EVAL: { val(100,x) }
      Begin
        { evaluate the term; it may includes variables and constraints;
          thus, our assign is similar to "cassign(i,t)";
          see p113 of the PrologII+ documentation }
        T1 := EvaluateExpression(GetPArg(1,FT),P); { FIXME: do a copy and unbound variables? }
        Ok := T1 <> Nil;
        If Ok Then
        Begin
          T2 := GetPArg(2,FT);
          Ok := ReduceOneEq(T2,T1) { FIXME: shouldn't it be backtrackable? }
        End
      End;
    PP_QUIT:
      Terminate(0);
    PP_INSERT: { insert("file.pro") }
      Begin
        C := EvaluateToString(GetPArg(1,FT));
        Ok := C <> Nil;
        If Ok Then
        Begin
          QLast := LastProgramQuery(P);
          LoadProgram(P,GetConstAsString(C,False),RTYPE_USER);
          Ok := Not Error;
          If Ok Then 
          Begin
            { newly loaded rules are also in the scope of the current 
              query and queries that follow, up to the last query before
              the program was loaded }
            Qi := Q;
            Stop := False;
            While Not Stop Do
            Begin
              UpdateQueryScope(P,Qi);
              Qi := NextQuery(Qi);
              Stop := (Qi=Nil) Or (Qi=QLast)
            End
          End
        End
      End;
    PP_INPUT: { input("buffer") }
      Begin
        C := EvaluateToString(GetPArg(1,FT));
        Ok := C <> Nil;
        If Ok Then
        Begin
          FileName := ConstGetPStr(C);
          CloseOutput(FileName); { close the file if it was already open for output }
          Ok := SetFileForInput(FileName) { TODO: warn when length > 255 }
        End
      End;
    PP_INPUT_IS: { input_is(s) }
      Begin
        C := InstallConst(P^.PP_DCON,NewStringFrom(InputIs),CS);
        Ok := ReduceOneEq(GetPArg(1,FT),TC)
      End;
    PP_CLOSE_CURRENT_INPUT: { close_input }
      Begin
        Ok := True;
        CloseCurrentInput
      End;
    PP_CLOSE_INPUT: { close_input("buffer") }
      Begin
        C := EvaluateToString(GetPArg(1,FT));
        Ok := C <> Nil;
        If Ok Then
          CloseInput(ConstGetPStr(C)) { TODO: warn when length > 255 }
      End;
    PP_CLEAR_INPUT: { clear_input }
      Begin
        Ok := True;
        ClearInput
      End;
    PP_OUTPUT: { output("buffer") }
      Begin
        C := EvaluateToString(GetPArg(1,FT));
        Ok := C <> Nil;
        If Ok Then
        Begin
          FileName := ConstGetPStr(C);
          CloseInput(FileName); { close the file if it was already open for input }
          Ok := SetFileForOutput(FileName) { TODO: warn when length > 255 }
        End;
      End;
    PP_OUTPUT_IS: { output_is(s) }
      Begin
        C := InstallConst(P^.PP_DCON,NewStringFrom(OutputIs),CS);
        Ok := ReduceOneEq(GetPArg(1,FT),TC)
      End;
    PP_CLOSE_CURRENT_OUTPUT: { close_output }
      Begin
        Ok := True;
        CloseCurrentOutput
      End;
    PP_CLOSE_OUTPUT: { close_output("buffer") }
      Begin
        C := EvaluateToString(GetPArg(1,FT));
        Ok := C <> Nil;
        If Ok Then
          CloseOutput(ConstGetPStr(C)) { TODO: warn when length > 255 }
      End;
    PP_FLUSH: { flush }
      Begin
        Ok := True;
        FlushCurrentOutput
      End;
    PP_LIST:
      Begin
        Ok := True;
        OutQuestionRules(Q,RTYPE_USER,False)
      End;
    PP_OUT:
      Begin
        Ok := True;
        OutTerm(GetPArg(1,FT),True)
      End;
    PP_OUTM:
      Begin
        Ok := True;
        OutTermBis(GetPArg(1,FT),False,False,True)
      End;
    PP_LINE:
      Begin
        Ok := True;
        OutCR(True)
      End;
    PP_CLRSRC:
      Begin
        Ok := True;
        If OutputIsTerminal Then
          ClrScr
      End;
    PP_IN_TERM:
      Begin
        CheckConsoleInput(True);
        T1 := ReadOneTerm(P,False);
        If Not Error Then
          Ok := ReduceOneEq(GetPArg(1,FT),T1)
      End;
    PP_IN_CHAR:
      Begin
        CheckConsoleInput(False);
        str := GetChar(ch); { FIXME: UFT8 }
        Ok := Not Error;
        If Ok Then
        Begin
          C := InstallConst(P^.PP_DCON,NewStringFrom(str),CS);
          Ok := ReduceOneEq(GetPArg(1,FT),TC)
        End
      End;
    PP_BACKTRACE:
      Begin
        Ok := True;
        DumpBacktrace
      End;
    PP_DUMP:
      Begin
        Ok := True;
        DumpRegisteredObject
      End
    End
  End;
    
  ExecutionSysCallOk := Ok
End;
