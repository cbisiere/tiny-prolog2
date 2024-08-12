{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Expr.pas                                                   }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                          E X P R E S S I O N S                             }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ handle expressions with evaluable functions }

Unit Expr;

Interface

Uses
  ShortStr,
  Num,
  Errs,
  Common,
  Memory,
  PObj,
  PObjFCVI,
  PObjOp,
  PObjStr,
  PObjTerm,
  PObjDef,
  PObjProg,
  Tuple,
  Encoding;

Function EvaluateExpression( T : TermPtr; P : ProgPtr ) : TermPtr;

Implementation
{-----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ standard order of terms                                                    }
{----------------------------------------------------------------------------}

{ see section 4.6.1 in: https://www.swi-prolog.org/pldoc/man?section=compare }

{ compare two high-precision numerical values }
Function CompVal( Val1,Val2 : LongReal ) : TComp;
Var
  Cmp : TComp;
Begin
  If Val1 < Val2 Then
    Cmp := CompLower
  Else If Val1 > Val2 Then
    Cmp := CompGreater
  Else
    Cmp := CompEqual;
  CompVal := Cmp
End;

{ compare two numerical constants }
Function CompNum( C1,C2 : ConstPtr ) : TComp;
Var
  CT1,CT2 : TConst;
  Val1,Val2 : LongReal;
  Cmp : TComp;
  code : Integer;
Begin
  CompNum := CompUndefined;
  CT1 := ConstType(C1);
  CT2 := ConstType(C2);
  CheckCondition(CT1 In [IntegerNumber,RealNumber],'CompNum: C1 not a number');
  CheckCondition(CT2 In [IntegerNumber,RealNumber],'CompNum: C2 not a number');
  If CT1 = IntegerNumber Then
    Val1 := ShortStringToLongInt(ConstGetShortString(C1),code)
  Else
    Val1 := ShortStringToLongReal(ConstGetShortString(C1),code);
  If code <> 0 Then Exit;
  If CT2 = IntegerNumber Then
    Val2 := ShortStringToLongInt(ConstGetShortString(C2),code)
  Else
    Val2 := ShortStringToLongReal(ConstGetShortString(C2),code);
  If code <> 0 Then Exit;
  Cmp := CompVal(Val1,Val2);
  If Cmp = CompEqual Then
    If (CT1 = RealNumber) And (CT2 = IntegerNumber) Then
      Cmp := CompLower
    Else If (CT1 = IntegerNumber) And (CT2 = RealNumber) Then
      Cmp := CompGreater;
  CompNum := Cmp
End;

{ compare T1 and T2 }
Function StandardOrderOfTerms( T1,T2 : TermPtr ) : TComp;
Var
  Cmp : TComp;
  TT1,TT2 : TTerm;
  V1 : VarPtr Absolute T1;
  V2 : VarPtr Absolute T2;
  C1 : ConstPtr Absolute T1;
  C2 : ConstPtr Absolute T2;
  I1 : IdPtr Absolute T1;
  I2 : IdPtr Absolute T2;
  H1,H2 : TermPtr;
Begin
  StandardOrderOfTerms := CompUndefined;
  TT1 := TypeOfTerm(T1);
  TT2 := TypeOfTerm(T2);
  { Variables < Numbers < Strings < Atoms < Compound Terms }
  Case TT1 Of
  Variable:
    Begin
      If TT2 = Variable Then
        Cmp := Str_Comp(VariableGetName(V1),VariableGetName(V2))
      Else
        Cmp := CompLower
    End;
  Constant:
    Begin
      Case TT2 Of
      Identifier,FuncSymbol:
        Cmp := CompLower;
      Constant:
        Begin
          If ConstType(C1) = QString Then
            If ConstType(C2) = QString Then
              Cmp := Str_Comp(ConstGetStr(C1),ConstGetStr(C2))
            Else
              Cmp := CompGreater
          Else
            If ConstType(C2) = QString Then
              Cmp := CompLower
            Else
              Cmp := CompNum(C1,C2)
        End;
      Variable:
        Cmp := CompGreater
      End
    End;
  Identifier:
    Begin
      Case TT2 Of
      FuncSymbol:
        Cmp := CompLower;
      Identifier:
        Cmp := Str_Comp(IdentifierGetStr(I1),IdentifierGetStr(I2));
      Variable,Constant:
        Cmp := CompGreater;
      End
    End;
  FuncSymbol:
    Begin
      Cmp := CompVal(TupleArgCount(T1),TupleArgCount(T2));
      If Cmp = CompEqual Then
      Begin
        H1 := ProtectedGetTupleArg(T1,True);
        H2 := ProtectedGetTupleArg(T2,True);
        Cmp := StandardOrderOfTerms(H1,H2);
        If Cmp = CompEqual Then
          Cmp := StandardOrderOfTerms(T1,T2)
      End
    End
  End;
  StandardOrderOfTerms := Cmp
End;

{----------------------------------------------------------------------------}
{ val                                                                        }
{----------------------------------------------------------------------------}

{ data structure for high precision arithmetic }
Const
  MaxFuncNbParams = 2; { maximum number of parameter for a predefined function }
Type
  TParVal = Record
    IsReal : Boolean;
    Val : LongReal
  End;
  TParArray = Array[1..MaxFuncNbParams] Of TParVal; { parameter value }

{ TODO: ugliest code ever below this point, please cleanup }

{ evaluate a term T; The expression to be evaluated is constructed 
  recursively from constants, identifiers and evaluable functions;
  return Nil if the expression cannot be evaluated }
Function EvaluateExpression( T : TermPtr; P : ProgPtr ) : TermPtr;
Var
  IT : IdPtr Absolute T;
  y : TSyntax;
  e : TermPtr; { the result, as a term }
  Ident : TermPtr;
  IIdent : IdPtr Absolute Ident;
  T1,T2 : TermPtr;
  CT1 : ConstPtr Absolute T1;
  CT2 : ConstPtr Absolute T2;
  code : Integer;
  rs : TString;
  s : StrPtr;
  Ok : Boolean;
  ParVal : TParArray;
  i : Byte;
  r : LongReal; { result of numerical evaluation }
  isInt : Boolean; { should this result be converted to an integer value? }
  cot : TypePrologObj;
  o : OpPtr;
  n : Byte;
  func : TString;
  U : TermPtr; { tuple to store the current evaluated ident and arguments }
  U1 : TermPtr; { last element in this tuple }
  Cmp : TComp;
  ArrIndex : PosInt;
Begin
  y := GetSyntax(P); 
  T := ProtectedRepOf(T);
  e := T; { by default, the term evaluates to itself }
  If T <> Nil Then
  Begin
    Case TypeOfTerm(T) Of
    Identifier:
      If IsAssigned(IdPtr(T)) Then
        e := GetValue(IdPtr(T));
    Constant:
      Pass;
    Variable:
      Pass; { unbounded variable evaluates to itself (different from standard behavior) ) }
    FuncSymbol:
      Begin
        { try to get and evaluate an evaluable function or an array element }
        Ident := ProtectedGetTupleHead(T,True);
        If Ident <> Nil Then
        Begin
          If IsIdentifier(Ident) Then
          Begin
            If IsArray(IIdent) And (TupleArgCount(T) = 1+1) Then { array, e.g; stack(2) }
            Begin
              T1 := EvaluateExpression(TupleArgN(1+1,T),P); { array index }
              If (TypeOfTerm(T1) = Constant) And 
                  (ConstType(CT1) = IntegerNumber) Then { is an integer value }
              Begin
                ArrIndex := ShortStringToPosInt(ConstGetShortString(CT1),code);
                If (code = 0) And (ArrIndex >= 1) And 
                    (ArrIndex <= GetArraySize(IIdent)) Then
                  e := Array_GetElement(GetArray(IIdent),ArrIndex)
              End
            End
            Else
            Begin
              { function is known and has the correct number of parameters }
              func := IdentifierGetShortString(IIdent);
              n := TupleArgCount(T) - 1;
              o := Op_Lookup(P^.PP_OPER,[OP_FUNCTION,OP_ARITY,OP_PRECEDENCE],
                  '',func,[],n,1200);
              Ok := o <> Nil;
              If Ok Then
              Begin
                If ((func = '@<') Or (func = '@>') Or (func = '@=<') 
                    Or (func = '@>=')) And (y = Edinburgh) Then
                Begin
                  T1 := EvaluateExpression(TupleArgN(1+1,T),P);
                  T2 := EvaluateExpression(TupleArgN(1+2,T),P);
                  Cmp := StandardOrderOfTerms(T1,T2);
                  Ok := Cmp <> CompUndefined;
                  If Ok Then
                  Begin
                    IsInt := True;
                    r := Ord((Cmp = CompEqual) 
                          And ((func = '@=<') Or (func = '@>='))
                        Or (Cmp = CompLower) 
                          And ((func = '@=<') Or (func = '@<'))
                        Or (Cmp = CompGreater) 
                          And ((func = '@>=') Or (func = '@>')));
                  End
                End
                Else
                Begin
                  { evaluate the function's parameters, building a new term }
                  U := NewTuple(EmitShortIdent(P,func,False)); { FIXME: UTF-8 }
                  e := U; { we may return the term with its args evaluated }
                  U1 := U;
                  For i := 1 to n Do
                  Begin
                    T1 := EvaluateExpression(TupleArgN(1+i,T),P);
                    SetTupleQueueTerm(U1,T1);
                    U1 := TupleQueue(U1);
                    { does that arg prevents the function to be evaluated? }
                    If Ok Then
                      Ok := T1 <> Nil;
                    If Ok Then
                      Ok := IsConstant(T1);
                    If Ok Then
                      Ok := ConstType(CT1) In [IntegerNumber,RealNumber]
                  End;
                  If Ok Then
                    Ok := n <= MaxFuncNbParams;
                  If Ok Then
                  Begin
                    { extract numerical values from the arguments }
                    For i := 1 to n Do
                    Begin
                      T1 := TupleArgN(1+i,U);
                      With ParVal[i] Do
                      Begin
                        If ConstType(CT1) = IntegerNumber Then
                        Begin
                          IsReal := False;
                          Val := ShortStringToLongInt(ConstGetShortString(CT1),code)
                        End
                        Else
                        Begin
                          IsReal := True;
                          Val := ShortStringToLongReal(ConstGetShortString(CT1),code)
                        End
                      End;
                      If Ok Then
                        Ok := code = 0
                    End
                  End;
                  If Ok Then
                  Begin
                    { by default, ops on integers give integer results }
                    IsInt := True;
                    For i := 1 to n Do
                      IsInt := IsInt And Not ParVal[i].IsReal;
                    { TODO: absolute precision }
                    If ((func = 'add') And (y <> Edinburgh) Or 
                        (func = '+') And (y = Edinburgh)) And (n = 1) Then
                    Begin
                      r := ParVal[1].Val
                    End
                    Else If ((func = 'sub') And (y <> Edinburgh) Or 
                        (func = '-') And (y = Edinburgh)) And (n = 1) Then
                    Begin
                      r := -1*ParVal[1].Val
                    End
                    Else If ((func = 'add') And (y <> Edinburgh) Or 
                        (func = '+') And (y = Edinburgh)) And (n = 2) Then
                    Begin
                      r := ParVal[1].Val + ParVal[2].Val
                    End
                    Else If ((func = 'sub') And (y <> Edinburgh) Or 
                        (func = '-') And (y = Edinburgh)) And (n = 2) Then
                    Begin
                      r := ParVal[1].Val - ParVal[2].Val
                    End
                    Else If ((func = 'mul') And (y <> Edinburgh) Or 
                        (func = '*') And (y = Edinburgh)) And (n = 2) Then
                    Begin
                      r := ParVal[1].Val * ParVal[2].Val
                    End
                    Else If ((func = 'div') And (y <> Edinburgh) Or 
                        ((func = '/') Or (func = '//')) And (y = Edinburgh)) 
                        And (n = 2) Then
                    Begin
                      Ok := ParVal[2].Val <> 0;
                      If Ok Then
                        Ok := Not ((func = '//') And Not IsInt);
                      If Ok Then
                      Begin
                        If IsInt Then { integer division }
                          r := LongIntDiv(LongRealToLongInt(ParVal[1].Val),
                              LongRealToLongInt(ParVal[2].Val))
                        Else
                          r := ParVal[1].Val / ParVal[2].Val
                      End
                      Else
                        e := Nil { val(div(3,0),v) fails on PII+; TODO: warning  }
                    End
                    Else If (func = '^') And (n = 2) Then
                    Begin
                      If ParVal[2].Val = 0 Then { x^0 = 1 }
                        r := 1
                      Else If ParVal[1].Val = 1 Then { 1^y = 1 }
                        r := 1
                      Else If ParVal[1].Val = 0 Then { 0^y = 0 }
                        r := 0
                      Else If ParVal[2].Val = 1 Then { x^1 = x }
                        r := ParVal[1].Val
                      Else If ParVal[1].Val < 0 Then { (-x)^y }
                      Begin
                        Ok := Not ParVal[2].IsReal; { (-x)^real not defined }
                        If Ok Then
                        Begin
                          r := Exp(ParVal[2].Val*Ln(Abs(ParVal[1].Val)));
                          If Frac(ParVal[2].Val/2)<>0 Then
                            r := -1*r
                        End
                      End
                      Else
                      Begin
                        r := Exp(ParVal[2].Val*Ln(ParVal[1].Val))
                      End
                    End
                    Else If ((func = 'inf') And (y <> Edinburgh) 
                        Or (func = '<') And (y = Edinburgh)) And (n = 2) Then
                    Begin
                      r := Ord(ParVal[1].Val < ParVal[2].Val); { TODO: order on strings, etc.}
                      IsInt := True
                    End
                    Else If ((func = 'infe') And (y <> Edinburgh) 
                        Or (func = '=<') And (y = Edinburgh)) And (n = 2) Then
                    Begin
                      r := Ord(ParVal[1].Val <= ParVal[2].Val); { TODO: order on strings, etc.}
                      IsInt := True
                    End
                    Else If ((func = 'sup') And (y <> Edinburgh) 
                        Or (func = '>') And (y = Edinburgh)) And (n = 2) Then
                    Begin
                      r := Ord(ParVal[1].Val > ParVal[2].Val); { TODO: order on strings, etc.}
                      IsInt := True
                    End
                    Else If ((func = 'supe') And (y <> Edinburgh) 
                        Or (func = '>=') And (y = Edinburgh)) And (n = 2) Then
                    Begin
                      r := Ord(ParVal[1].Val >= ParVal[2].Val); { TODO: order on strings, etc.}
                      IsInt := True
                    End
                    Else If ((func = 'eq') And (y = PrologIIc) 
                        Or (func = 'eql') And (y <> PrologIIc)) 
                        And (n = 2) Then { PII+ p.110: automatic type conversion }
                    Begin
                      r := Ord(ParVal[1].Val = ParVal[2].Val);
                      IsInt := True
                    End
                    Else If (func = 'is') And (y = Edinburgh) And (n = 2) Then
                    Begin
                      If ParVal[1].IsReal Xor ParVal[2].IsReal Then
                        r := 0
                      Else
                        r := Ord(ParVal[1].Val = ParVal[2].Val);
                      IsInt := True
                    End
                    Else
                      Ok := False { unknown function; TODO: may be cleared }
                  End
                End;
                { emit result r }
                If Ok Then
                Begin
                  { generate canonical numeric constants }
                  If IsInt Then
                  Begin
                    Ok := (r > -1.0*MaxLongInt) And (r < 1.0*MaxLongInt);
                    If Ok Then
                    Begin
                      rs := LongIntToShortString(LongRealToLongInt(r));
                      cot := CI
                    End
                    { overflow: keep the term; TODO: emit a warning? }
                  End
                  Else
                  Begin
                    rs := LongRealToShortString(r);
                    cot := CR
                  End;
                  If Ok Then
                  Begin
                    s := Str_NewFromShortString(rs); { TODO: UTF-8 user ops }
                    e := EmitConst(P,s,cot,False) { FIXME: not glob? }
                  End
                End
              End
            End
          End
        End
      End
    End
  End;
  EvaluateExpression := e
End;

End.