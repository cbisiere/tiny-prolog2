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

{ predefined predicates; use identifiers that are portable 
 across all the supported syntaxes: lowercase letters with at least two
 letters }

{----------------------------------------------------------------------------}
{ predefined predicates                                                      }
{----------------------------------------------------------------------------}

Const
  NB_PP = 25;
  MAX_PP_LENGHT = 21; { max string length }
  SYSCALL_IDENT_AS_STRING = 'syscall'; 
Type
  TPP = (
    PP_INPUT_IS,PP_INPUT,PP_CLOSE_CURRENT_INPUT,PP_CLOSE_INPUT,PP_CLEAR_INPUT,
    PP_IN_TERM,PP_IN_CHAR,
    PP_OUTPUT_IS,PP_OUTPUT,PP_CLOSE_CURRENT_OUTPUT,PP_CLOSE_OUTPUT,PP_FLUSH,
    PP_QUIT,PP_INSERT,PP_LIST,
    PP_OUT,PP_OUTM,PP_LINE,
    PP_BACKTRACE,PP_CLRSRC,PP_EVAL,PP_OP,PP_ASSIGN,PP_DUMP,
    PP_DIF);
  TPPRec = Record
    I : TPP; { identifier }
    S : String[MAX_PP_LENGHT]; { identifier as string }
    N : Byte; { number of arguments }
  End;
  TPPArray = Array[1..NB_PP] Of TPPRec;

Const 
  PPArray : TPPArray = (
    (I:PP_INPUT_IS;S:'sysinputis';N:1),
    (I:PP_INPUT;S:'sysinput';N:1),
    (I:PP_CLOSE_CURRENT_INPUT;S:'sysclosecurrentinput';N:0),
    (I:PP_CLOSE_INPUT;S:'syscloseinput';N:1),
    (I:PP_CLEAR_INPUT;S:'sysclearinput';N:0),
    (I:PP_IN_TERM;S:'sysinterm';N:1),
    (I:PP_IN_CHAR;S:'sysinchar';N:1),
    (I:PP_OUTPUT_IS;S:'sysoutputis';N:1),
    (I:PP_OUTPUT;S:'sysoutput';N:1),
    (I:PP_CLOSE_CURRENT_OUTPUT;S:'sysclosecurrentoutput';N:0),
    (I:PP_CLOSE_OUTPUT;S:'syscloseoutput';N:1),
    (I:PP_FLUSH;S:'sysflush';N:0),
    (I:PP_QUIT;S:'sysquit';N:0),
    (I:PP_INSERT;S:'sysinsert';N:1),
    (I:PP_LIST;S:'syslist';N:0),
    (I:PP_OUT;S:'sysout';N:1),
    (I:PP_OUTM;S:'sysoutm';N:1),
    (I:PP_LINE;S:'sysline';N:0),
    (I:PP_BACKTRACE;S:'sysbacktrace';N:0),
    (I:PP_CLRSRC;S:'sysclrsrc';N:0),
    (I:PP_EVAL;S:'syseval';N:2),
    (I:PP_OP;S:'sysop';N:4), { TODO: 3-arg version }
    (I:PP_ASSIGN;S:'sysassign';N:2),
    (I:PP_DUMP;S:'sysdump';N:0),
    (I:PP_DIF;S:'sysdif';N:2)
  );

{ lookup for a predefined predicates; set the found record; 
  return True if found  }
Function LookupPP( str : TString; Var rec : TPPRec) : Boolean;
Var 
  i : 0..NB_PP;
  Found : Boolean;
Begin
  i := 0;
  Found := False;
  While (Not Found) And (i < NB_PP) Do
  Begin
    i := i + 1;
    If PPArray[i].S = str Then
    Begin
      Found := True;
      rec := PPArray[i]
    End
  End;
  LookupPP := Found
End;


{ is an identifier a syscall? }
Function IdentifierIsSyscall; (*( I : IdPtr ) : Boolean; *)
Begin
  IdentifierIsSyscall := IdentifierEqualTo(I,SYSCALL_IDENT_AS_STRING)
End;

{ install all predefined, persistent constants }
Procedure RegisterPredefined( P : ProgPtr );
Var I : IdPtr;
Begin
  I := InstallIdentifier(P^.PP_DCON,NewStringFrom(SYSCALL_IDENT_AS_STRING),True)
End;


{----------------------------------------------------------------------------}
{ syscall                                                                    }
{----------------------------------------------------------------------------}

{ execute a system call syscall(Code,Arg1,...ArgN), meaning Code(Arg1,...,ArgN) }
Function ExecutionSysCallOk; (* ( T : TermPtr; P : ProgPtr; Q : QueryPtr ) : Boolean; *)
Var
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
  C,C1,C3 : ConstPtr;
  rec : TPPRec;
  str : TString;
  ch : TChar;
  I,I2,I3,I4 : IdPtr;
  TI : TermPtr Absolute I;
  v,code : Integer;
  Id2,Id3,Id4 : TString;
  Qi, QLast : QueryPtr;
  Stop : Boolean;
  FileName : TString;
  o : OpPtr;
  ot : TOpType;

  { get n-th argument of the predicate represented by tuple U }
  Function GetPArg( n : Byte; U : TermPtr ) : TermPtr;
  Begin
    GetPArg := TupleArgN(2+n,U)
  End;

Begin
  ExecutionSysCallOk := False; { default is to fail }

  { coded as a functional symbol }
  CheckCondition(TypeOfTerm(T) = FuncSymbol,'syscall: functional symbol expected');
  
  { first parameter is syscall }
  T1 := TupleArgN(1,T);
  CheckCondition(TypeOfTerm(T1) = Identifier,'syscall: constant expected');
  SysCallCode := IdentifierGetStr(IT1);
  CheckCondition(StrEqualTo(SysCallCode,SYSCALL_IDENT_AS_STRING),'Not a syscall');

  { there are at least two arguments: 'syscall' and the identifier }
  NbArgs := TupleArgCount(T);
  Ok := NbArgs >= 2;  
  If Ok Then
  Begin
    T2 := TupleArgN(2,T);
    Ident := IdentifierGetStr(IT2);
    Ok := StrLength(Ident) <= MAX_PP_LENGHT
  End;

  { predicate is known and has the correct number of parameters }
  If Ok Then
  Begin
    str := StrGetString(Ident);
    Ok := LookupPP(str,rec);
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
    PP_DIF:
      Begin
        T1 := GetPArg(1,T);
        T2 := GetPArg(2,T);
        Ok := ReduceOneIneq(T1,T2)
      End;
    PP_ASSIGN: { assign(file_name, "myfile.txt") }
      Begin
        { note: re-assignments are tricky to handle, as the reduced system, after
          e.g. "assign(test,1)", contains i=1 (w/o any remaining reference to the 
          identifier) }
        { get the identifier }
        T1 := GetPArg(1,T);
        Case TypeOfTerm(T1) Of
        Identifier: { an identifier, thus unbound (first assignment) }
          Begin
            DictSetGlobal(IT1^.TV_DVAR,True); { dict entry is now persistent }
            I := IT1
          End;
        Variable:
          Begin
            I := VT1^.TV_IRED;
            If I = Nil Then { variable has never been bound to an identifier }
              I := EvaluateToIdentifier(GetPArg(1,T));
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
          T2 := GetPArg(2,T);
          { assign }
          Ok := ReduceOneEq(TI,T2) { "ident = term" }
        End
      End;
    PP_EVAL: { val(100,x) }
      Begin
        { evaluate the term; it may includes variables and constraints;
          thus, our assign is similar to "cassign(i,t)";
          see p113 of the PrologII+ documentation }
        T1 := EvaluateExpression(GetPArg(1,T),P); { FIXME: do a copy and unbound variables? }
        Ok := T1 <> Nil;
        If Ok Then
        Begin
          T2 := GetPArg(2,T);
          Ok := ReduceOneEq(T2,T1) { FIXME: shouldn't it be backtrackable? }
        End
      End;
    PP_OP: { op(700,xfx,"<",inf) } { TODO: implement full specs PII+ p137}
      Begin
        { 1: precedence (integer value between 1 and 1200) }
        C1 := EvaluateToInteger(GetPArg(1,T));
        If C1 = Nil Then
          Exit;
        str := ConstGetPStr(C1);
        If Length(str) > 4 Then
          Exit;
        Val(str,v,code);
        If code <> 0 Then
          Exit;
        If (v < 0) Or (v > 1200) Then
          Exit;
        { 2: type of operator (identifier in a list) }
        I2 := EvaluateToIdentifier(GetPArg(2,T));
        If I2 = Nil Then
          Exit;
        Id2 := IdentifierGetPStr(I2);
        If Not IsOpTypeString(Id2) Then
          Exit;
        ot := PStrToOpType(Id2);
        { 3: operator (string or identifier) }
        I3 := EvaluateToIdentifier(GetPArg(3,T));
        If I3 <> Nil Then
          Id3 := IdentifierGetPStr(I3)
        Else
        Begin
          C3 := EvaluateToString(GetPArg(3,T));
          If C3 = Nil Then
            Exit;
          Id3 := ConstGetPStr(C3) { limits to StringMaxSize chars }
        End;
        { 4: functional symbol (identifier) }
        I4 := EvaluateToIdentifier(GetPArg(4,T));
        If I4 = Nil Then
          Exit;
        Id4 := IdentifierGetPStr(I4);
        { not allowed: existing function with same number of parameters }
        o := OpLookup(P^.PP_OPER,'',Id4,[],TOpTypeToArity(ot),1200);
        If o <> Nil Then { TODO: do not fail when both declarations match }
          Exit;
        { register the new operator }
        o := OpAppend(P^.PP_OPER,Id3,Id4,ot,v);
        Ok := True
      End;
    PP_QUIT:
      Terminate(0);
    PP_INSERT: { insert("file") }
      Begin
        C := EvaluateToString(GetPArg(1,T));
        Ok := C <> Nil;
        If Ok Then
        Begin
          QLast := LastProgramQuery(P);
          LoadProgram(P,GetConstAsString(C,False));
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
        C := EvaluateToString(GetPArg(1,T));
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
        T1 := EmitConst(P,NewStringFrom(InputIs),CS,False);
        Ok := ReduceOneEq(GetPArg(1,T),T1)
      End;
    PP_CLOSE_CURRENT_INPUT: { close_input }
      Begin
        Ok := True;
        CloseCurrentInput
      End;
    PP_CLOSE_INPUT: { close_input("buffer") }
      Begin
        C := EvaluateToString(GetPArg(1,T));
        Ok := C <> Nil;
        If Ok Then
          CloseInputByName(ConstGetPStr(C)) { TODO: warn when length > 255 }
      End;
    PP_CLEAR_INPUT: { clear_input }
      Begin
        Ok := True;
        ClearInput
      End;
    PP_OUTPUT: { output("buffer") }
      Begin
        C := EvaluateToString(GetPArg(1,T));
        Ok := C <> Nil;
        If Ok Then
        Begin
          FileName := ConstGetPStr(C);
          CloseInputByName(FileName); { close the file if it was already open for input }
          Ok := SetFileForOutput(FileName) { TODO: warn when length > 255 }
        End;
      End;
    PP_OUTPUT_IS: { output_is(s) }
      Begin
        T1 := EmitConst(P,NewStringFrom(OutputIs),CS,False);
        Ok := ReduceOneEq(GetPArg(1,T),T1)
      End;
    PP_CLOSE_CURRENT_OUTPUT: { close_output }
      Begin
        Ok := True;
        CloseCurrentOutput
      End;
    PP_CLOSE_OUTPUT: { close_output("buffer") }
      Begin
        C := EvaluateToString(GetPArg(1,T));
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
        OutTerm(GetPArg(1,T),True)
      End;
    PP_OUTM:
      Begin
        Ok := True;
        OutTermBis(GetPArg(1,T),False,False,True)
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
          CrtClrSrc
      End;
    PP_IN_TERM:
      Begin
        CheckConsoleInput(True);
        T1 := ParseOneTerm(P);
        Ok := Not Error;
        If Ok Then
          Ok := ReduceOneEq(GetPArg(1,T),T1)
      End;
    PP_IN_CHAR:
      Begin
        CheckConsoleInput(False);
        str := GetChar(ch);
        Ok := Not Error;
        If Ok Then
        Begin
          T1 := EmitConst(P,NewStringFrom(str),CS,False);
          Ok := ReduceOneEq(GetPArg(1,T),T1)
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
