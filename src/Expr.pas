{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Expr.pas                                                   }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                          E X P R E S S I O N S                             }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ handle expressions with unary and binary operators }

{----------------------------------------------------------------------------}
{ predefined functions                                                       }
{----------------------------------------------------------------------------}

Const
  NB_PF = 8;
  MAX_PF_LENGHT = 5; { max string length }
Type
  TPF = (
    PF_MINUS,PF_PLUS,PF_ADD,PF_SUB,PF_MUL,PF_DIV,PF_INF,PF_IS);
  TPrecedence = Integer; { the higher the lower the priority }
  TOpType = (xfx,yfx,fx); { see PIII+ doc p45 }
  TPFRec = Record
    I : TPF; { identifier }
    S : String[MAX_PF_LENGHT]; { identifier as string }
    N : Byte; { number of arguments }
    T : TOpType;
    P : TPrecedence { precedence, see PII+ manual p44 }
  End;
  TPFArray = Array[1..NB_PF] Of TPFRec;

Const
  PFArray : TPFArray = (
    (I:PF_IS;S:'is';N:2;T:xfx;P:700), { FIXME: Edinburgh-specific }
    (I:PF_INF;S:'inf';N:2;T:xfx;P:700),
    (I:PF_ADD;S:'add';N:2;T:yfx;P:500),
    (I:PF_SUB;S:'sub';N:2;T:yfx;P:500),
    (I:PF_MUL;S:'mul';N:2;T:yfx;P:400),
    (I:PF_DIV;S:'div';N:2;T:yfx;P:400),
    (I:PF_PLUS;S:'plus';N:1;T:fx;P:200),
    (I:PF_MINUS;S:'minus';N:1;T:fx;P:200)
  );

{ lookup for a predefined function; set the found record; 
  return True if found  }
Function LookupPF( str : TString; Var rec : TPFRec) : Boolean;
Var 
  i : 0..NB_PF;
  Found : Boolean;
Begin
  i := 0;
  Found := False;
  While (Not Found) And (i < NB_PF) Do
  Begin
    i := i + 1;
    If PFArray[i].S = str Then
    Begin
      Found := True;
      rec := PFArray[i]
    End
  End;
  LookupPF := Found
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

{ evaluate a term T; The expression to be evaluated is constructed 
  recursively from constants, identifiers and evaluable functions;
  return Nil if the expression cannot be evaluated }
Function EvaluateExpression( T : TermPtr; P : ProgPtr ) : TermPtr;
Var
  IT : IdPtr Absolute T;
  e : TermPtr;
  Ident : TermPtr;
  IIdent : IdPtr Absolute Ident;
  T1,T2 : TermPtr;
  CT1 : ConstPtr Absolute T1;
  CT2 : ConstPtr Absolute T2;
  code : Integer;
  rs : TString;
  s : StrPtr;
  Ok : Boolean;
  rec : TPFRec;
  str : TString;
  ParVal : TParArray;
  i : Byte;
  r : LongReal; { result of numerical evaluation }
  isInt : Boolean; { should this result be converted to an integer value? }
  cot : TypePrologObj;
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
        Ident := EvaluateExpression(TupleArgN(1,T),P);
        If Ident <> Nil Then
        Begin
          If TypeOfTerm(Ident) = Identifier Then
          Begin
            { function is known and has the correct number of parameters }
            str := IdentifierGetPStr(IIdent);
            Ok := LookupPF(str,rec);
            If Ok Then
              Ok := TupleArgCount(T) = rec.N + 1
            Else
              e := T; { not a known function: return the term }
            { evaluate the function's parameters }
            If Ok Then
              For i := 1 to rec.N Do
              Begin
                If Ok Then
                Begin
                  T1 := EvaluateExpression(TupleArgN(1+i,T),P);
                  Ok := T1 <> Nil;
                End;
                If Ok Then
                  Ok := TypeOfTerm(T1) = Constant;
                If Ok Then
                  Ok := ConstType(CT1) In [IntegerNumber,RealNumber];
                If Ok Then
                Begin
                  With ParVal[i] Do
                  Begin
                    If ConstType(CT1) = IntegerNumber Then
                    Begin
                      IsReal := False;
                      Val := StrToLongInt(ConstGetPStr(CT1),code)
                    End
                    Else
                    Begin
                      IsReal := True;
                      Val := StrToLongReal(ConstGetPStr(CT1),code)
                    End
                  End;
                  Ok := code = 0
                End
              End;
            If Ok Then
            Begin
              { by default, ops on integers give integer results }
              IsInt := True;
              For i := 1 to rec.N Do
                IsInt := IsInt And Not ParVal[i].IsReal;
              Case rec.I Of { TODO: absolute precision }
                PF_MINUS:
                  r := -1*ParVal[1].Val;
                PF_PLUS:
                  r := ParVal[1].Val;
                PF_ADD:
                  r := ParVal[1].Val + ParVal[2].Val;
                PF_SUB:
                  r := ParVal[1].Val - ParVal[2].Val;
                PF_MUL:
                  r := ParVal[1].Val * ParVal[2].Val;
                PF_DIV:
                  Begin
                    Ok := ParVal[2].Val <> 0;
                    If Ok Then
                      If IsInt Then { integer division }
                        r := LongIntDiv(LongRealToLongInt(ParVal[1].Val),
                            LongRealToLongInt(ParVal[2].Val))
                      Else
                        r := ParVal[1].Val / ParVal[2].Val
                  End;
                PF_INF:
                  Begin
                    r := Ord(ParVal[1].Val < ParVal[2].Val); { TODO: order on strings, etc.}
                    IsInt := True
                  End;
                PF_IS:
                  Begin
                    If ParVal[1].IsReal Xor ParVal[2].IsReal Then
                      r := 0
                    Else
                      r := Ord(ParVal[1].Val = ParVal[2].Val);
                    IsInt := True
                  End
              End;
              If Ok Then
              Begin
                { generate canonical numeric constants }
                If IsInt Then
                Begin
                  rs := LongIntToStr(LongRealToLongInt(r));
                  cot := CI
                End
                Else
                Begin
                  rs := LongRealToStr(r);
                  cot := CR
                End;
                s := NewStringFrom(rs);
                e := EmitConst(P,s,cot,False) { FIXME: not glob? }
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
