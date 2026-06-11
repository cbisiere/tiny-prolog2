{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Expr.pas                                                   }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022-2026                                                  }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                          E X P R E S S I O N S                             }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }

{ handle expressions with evaluable functions }

Unit Expr;

Interface

Uses
  ShortStr,
  Num,
  Serial,
  Errs,
  CWrites,
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

Procedure RegisterEvaluableFunctions( P : ProgPtr );
Procedure RegisterOperators( P : ProgPtr );
Function ProtectedEvaluateTerm( T : TermPtr; P : ProgPtr ) : TermPtr;

Implementation
{-----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ install predefined evaluable functions                                     }
{----------------------------------------------------------------------------}

{ declare an evaluable function (that has *no* corresponding operator) }
Procedure DeclareEvaluableFunction( code : TOpCode; func : TString; 
    arity : TOpArity; P : ProgPtr );
Var
  o : OpPtr;
  ot : TOpType;
Begin
  ot := fx; { fake, unused }
  o := Op_Append(P^.PP_OPER,code,'',func,arity,ot,0)
End;

Const
  NbFunctions = 37;
Type
  TFunctionIndex = 1..NbFunctions;
  TFunctions = Array[TFunctionIndex] Of 
    Record
      C : TOpCode;
      QF : Boolean; { is name quoted in PII doc p.110+? }
      F : String[8]; { name }
      A : TOpArity; { arity }
      S : Set Of TSyntax { Prolog syntaxes in which the function is available }
    End;

{ predefined functions; we note whether the functor appears quoted in PII+ doc,
 but how this piece of information must be used is not clear; playing with
 PII+ shows that quotes can be omitted (e.g. in "aaa @< bbb" or "@<(aaa,bbb)") }
Const
  EvaluableFunctions : TFunctions = (
    (C:OPER_ADD_2;   QF:False; F:'add';      A:2; S:[PrologIIv1, PrologIIv2]),
    (C:OPER_ADD_1;   QF:False; F:'add';      A:1; S:[            PrologIIv2]),
    (C:OPER_SUB_2;   QF:False; F:'sub';      A:2; S:[PrologIIv1, PrologIIv2]),
    (C:OPER_SUB_1;   QF:False; F:'sub';      A:1; S:[            PrologIIv2]),
    (C:OPER_MUL;     QF:False; F:'mul';      A:2; S:[PrologIIv1, PrologIIv2]),
    (C:OPER_DIV_INT; QF:False; F:'div';      A:2; S:[PrologIIv1]),
    (C:OPER_DIV;     QF:False; F:'div';      A:2; S:[            PrologIIv2]),
    (C:OPER_MOD;     QF:False; F:'mod';      A:2; S:[PrologIIv1, PrologIIv2]),
    (C:OPER_COMP_L;  QF:False; F:'inf';      A:2; S:[PrologIIv1, PrologIIv2]),
    (C:OPER_COMP_EQ; QF:False; F:'eq';       A:2; S:[PrologIIv1            ]),
    (C:OPER_COMP_EQ; QF:False; F:'eql';      A:2; S:[            PrologIIv2]),
    (C:OPER_IF;      QF:False; F:'si';       A:3; S:[PrologIIv1            ]),
    (C:OPER_IF;      QF:False; F:'if';       A:3; S:[            PrologIIv2, PrologIIp]),
    (C:NA;           QF:False; F:'trunc';    A:1; S:[            PrologIIv2, PrologIIp]),
    (C:NA;           QF:False; F:'float';    A:1; S:[            PrologIIv2, PrologIIp]),
    (C:NA;           QF:False; F:'abs';      A:1; S:[            PrologIIv2, PrologIIp]),
    (C:NA;           QF:False; F:'atan';     A:1; S:[            PrologIIv2, PrologIIp]),
    (C:NA;           QF:False; F:'cos';      A:1; S:[            PrologIIv2, PrologIIp]),
    (C:NA;           QF:False; F:'exp';      A:1; S:[            PrologIIv2, PrologIIp]),
    (C:NA;           QF:False; F:'ln';       A:1; S:[            PrologIIv2, PrologIIp]),
    (C:NA;           QF:False; F:'rad';      A:1; S:[            PrologIIv2, PrologIIp]),
    (C:NA;           QF:False; F:'sin';      A:1; S:[            PrologIIv2, PrologIIp]),
    (C:NA;           QF:False; F:'sqrt';     A:1; S:[            PrologIIv2, PrologIIp]),
    (C:NA;           QF:False; F:'tan';      A:1; S:[                        PrologIIp]),
    (C:NA;           QF:False; F:'sign';     A:1; S:[                        PrologIIp]),
    (C:OPER_CEIL;    QF:False; F:'ceiling';  A:1; S:[                        PrologIIp]),
    (C:OPER_FLOOR;   QF:False; F:'floor';    A:1; S:[                        PrologIIp, Edinburgh]),
    (C:NA;           QF:False; F:'round';    A:1; S:[                        PrologIIp]),
    (C:NA;           QF:False; F:'double';   A:1; S:[                        PrologIIp]),
    (C:NA;           QF:True;  F:'/\';       A:2; S:[                        PrologIIp]),
    (C:NA;           QF:True;  F:'\/';       A:2; S:[                        PrologIIp]),
    (C:NA;           QF:True;  F:'>>';       A:2; S:[                        PrologIIp]),
    (C:NA;           QF:True;  F:'<<';       A:2; S:[                        PrologIIp]),
    (C:NA;           QF:True;  F:'~';        A:1; S:[                        PrologIIp]),
    (C:NA;           QF:False; F:'log';      A:1; S:[                                   Edinburgh]),
    (C:NA;           QF:False; F:'truncate'; A:1; S:[                                   Edinburgh]),
    { only in SWI-Prolog, but needed to run Colmerauer's Sudoku.pl }
    (C:OPER_RANDOM;  QF:False; F:'random';   A:1; S:[                                   Edinburgh])
);

Procedure RegisterEvaluableFunctions( P : ProgPtr );
Var
  y : TSyntax;
  i : TFunctionIndex;
Begin
  y := GetSyntax(P);
  For i := 1 To NbFunctions Do
    With EvaluableFunctions[i] Do
      If y In S Then
        DeclareEvaluableFunction(C,F,A,P)
End;

{----------------------------------------------------------------------------}
{ install predefined operators.                                              }
{----------------------------------------------------------------------------}

Const
  NbOperators = 56;
Type
  TOperatorIndex = 1..NbOperators;
  TOperators = Array[TOperatorIndex] Of 
    Record
      C : TOpCode;
      QN : Boolean; { is name quoted in PII doc p. 216? }
      N : String[5]; { operator }
      QF : Boolean; { is functor quoted in PII doc p. 216? }
      F : String[5]; { functor }
      T : TOpType;
      R : TPrecedence;
      S : Set Of TSyntax { Prolog syntaxes in which the operator is available }
    End;

Const
  Operators : TOperators = (
    (C:OPER_COMP_L;   QN:True;  N:'<';   QF:False; F:'inf';  T:xfx; R:700;  S:[PrologIIp]),
    (C:OPER_COMP_LE;  QN:True;  N:'=<';  QF:False; F:'infe'; T:xfx; R:700;  S:[PrologIIp]),
    (C:OPER_COMP_G;   QN:True;  N:'>';   QF:False; F:'sup';  T:xfx; R:700;  S:[PrologIIp]),
    (C:OPER_COMP_GE;  QN:True;  N:'>=';  QF:False; F:'supe'; T:xfx; R:700;  S:[PrologIIp]),
    (C:NA;            QN:False; N:'=/='; QF:True;  F:'=/=';  T:xfx; R:700;  S:[PrologIIp]),
    (C:OPER_COMP_EQ;  QN:False; N:'=:='; QF:False; F:'eql';  T:xfx; R:700;  S:[PrologIIp]),
    (C:OPER_ADD_2;    QN:False; N:'+';   QF:False; F:'add';  T:yfx; R:500;  S:[PrologIIp]),
    (C:OPER_SUB_2;    QN:False; N:'-';   QF:False; F:'sub';  T:yfx; R:500;  S:[PrologIIp]),
    (C:NA;            QN:True;  N:'/\';  QF:True;  F:'/\';   T:yfx; R:500;  S:[PrologIIp]),
    (C:NA;            QN:True;  N:'\/';  QF:True;  F:'\/';   T:yfx; R:500;  S:[PrologIIp]),
    (C:OPER_MUL;      QN:False; N:'*';   QF:False; F:'mul';  T:yfx; R:400;  S:[PrologIIp]),
    (C:OPER_DIV;      QN:False; N:'/';   QF:False; F:'div';  T:yfx; R:400;  S:[PrologIIp]),
    (C:OPER_MOD;      QN:False; N:'mod'; QF:False; F:'mod';  T:yfx; R:400;  S:[PrologIIp, Edinburgh]),
    (C:OPER_REM;      QN:False; N:'rem'; QF:False; F:'rem';  T:yfx; R:400;  S:[PrologIIp, Edinburgh]),
    (C:NA;            QN:True;  N:'<<';  QF:True;  F:'<<';   T:yfx; R:400;  S:[PrologIIp]),
    (C:NA;            QN:True;  N:'>>';  QF:True;  F:'>>';   T:yfx; R:400;  S:[PrologIIp]),
    (C:OPER_POWER;    QN:False; N:'^';   QF:True;  F:'^';    T:xfy; R:200;  S:[PrologIIp, Edinburgh]),
    (C:OPER_ADD_1;    QN:False; N:'+';   QF:False; F:'add';  T:fx;  R:200;  S:[PrologIIp]),
    (C:OPER_SUB_1;    QN:False; N:'-';   QF:False; F:'sub';  T:fx;  R:200;  S:[PrologIIp]),
    (C:NA;            QN:False; N:':-';  QF:True;  F:':-';   T:xfx; R:1200; S:[           Edinburgh]),
    (C:NA;            QN:False; N:':-';  QF:True;  F:':-';   T:fx;  R:1200; S:[           Edinburgh]),
    (C:NA;            QN:False; N:'?-';  QF:True;  F:'?-';   T:fx;  R:1200; S:[           Edinburgh]),
    (C:NA;            QN:False; N:'-->'; QF:True;  F:'-->';  T:xfx; R:1200; S:[           Edinburgh]),
    (C:NA;            QN:False; N:';';   QF:True;  F:';';    T:xfy; R:1100; S:[           Edinburgh]),
    (C:NA;            QN:False; N:'->';  QF:True;  F:'->';   T:xfy; R:1050; S:[           Edinburgh]),
    (C:NA;            QN:False; N:',';   QF:True;  F:',';    T:xfy; R:1001; S:[           Edinburgh]),
    (C:NA;            QN:False; N:'\+';  QF:False; F:'not';  T:fy;  R:900;  S:[           Edinburgh]),
    (C:OPER_COMP_IS;  QN:False; N:'is';  QF:False; F:'is';   T:xfx; R:700;  S:[           Edinburgh]),
    (C:OPER_COMP_EQ;  QN:False; N:'=';   QF:True;  F:'=';    T:xfx; R:700;  S:[           Edinburgh]),
    (C:OPER_COMP_L;   QN:False; N:'<';   QF:True;  F:'<';    T:xfx; R:700;  S:[           Edinburgh]),
    (C:OPER_COMP_G;   QN:False; N:'>';   QF:True;  F:'>';    T:xfx; R:700;  S:[           Edinburgh]),
    (C:OPER_COMP_GE;  QN:False; N:'>=';  QF:True;  F:'>=';   T:xfx; R:700;  S:[           Edinburgh]),
    (C:OPER_COMP_LE;  QN:False; N:'=<';  QF:True;  F:'=<';   T:xfx; R:700;  S:[           Edinburgh]),
    (C:OPER_ORDER_L;  QN:False; N:'@<';  QF:True;  F:'@<';   T:xfx; R:700;  S:[           Edinburgh]),
    (C:OPER_ORDER_G;  QN:False; N:'@>';  QF:True;  F:'@>';   T:xfx; R:700;  S:[           Edinburgh]),
    (C:OPER_ORDER_GE; QN:False; N:'@>='; QF:True;  F:'@>=';  T:xfx; R:700;  S:[           Edinburgh]),
    (C:OPER_ORDER_LE; QN:False; N:'@=<'; QF:True;  F:'@=<';  T:xfx; R:700;  S:[           Edinburgh]),
    (C:NA;            QN:False; N:'\=';  QF:True;  F:'\=';   T:xfx; R:700;  S:[           Edinburgh]),
    (C:NA;            QN:False; N:'==';  QF:True;  F:'==';   T:xfx; R:700;  S:[           Edinburgh]),
    (C:NA;            QN:False; N:'=\='; QF:True;  F:'=\=';  T:xfx; R:700;  S:[           Edinburgh]),
    (C:NA;            QN:False; N:'\=='; QF:True;  F:'\==';  T:xfx; R:700;  S:[           Edinburgh]),
    (C:NA;            QN:False; N:'=..'; QF:True;  F:'=..';  T:xfx; R:700;  S:[           Edinburgh]),
    (C:NA;            QN:False; N:'=:='; QF:True;  F:'=:=';  T:xfx; R:700;  S:[           Edinburgh]),
    (C:OPER_ADD_2;    QN:False; N:'+';   QF:True;  F:'+';    T:yfx; R:500;  S:[           Edinburgh]),
    (C:OPER_SUB_2;    QN:False; N:'-';   QF:True;  F:'-';    T:yfx; R:500;  S:[           Edinburgh]),
    (C:NA;            QN:False; N:'/\';  QF:True;  F:'/\';   T:yfx; R:500;  S:[           Edinburgh]),
    (C:NA;            QN:False; N:'\/';  QF:True;  F:'\/';   T:yfx; R:500;  S:[           Edinburgh]),
    (C:OPER_MUL;      QN:False; N:'*';   QF:True;  F:'*';    T:yfx; R:400;  S:[           Edinburgh]),
    (C:OPER_DIV;      QN:False; N:'/';   QF:True;  F:'/';    T:yfx; R:400;  S:[           Edinburgh]),
    (C:NA;            QN:False; N:'<<';  QF:True;  F:'<<';   T:yfx; R:400;  S:[           Edinburgh]),
    (C:NA;            QN:False; N:'>>';  QF:True;  F:'>>';   T:yfx; R:400;  S:[           Edinburgh]),
    (C:OPER_DIV_INT;  QN:False; N:'//';  QF:True;  F:'//';   T:yfx; R:400;  S:[           Edinburgh]),
    (C:NA;            QN:False; N:'\';   QF:True;  F:'\';    T:fy;  R:200;  S:[           Edinburgh]),
    (C:OPER_POWER;    QN:False; N:'**';  QF:True;  F:'**';   T:xfx; R:200;  S:[           Edinburgh]),
    (C:OPER_ADD_1;    QN:False; N:'+';   QF:True;  F:'+';    T:fy;  R:200;  S:[           Edinburgh]),
    (C:OPER_SUB_1;    QN:False; N:'-';   QF:True;  F:'-';    T:fy;  R:200;  S:[           Edinburgh])
);


Procedure RegisterOperators( P : ProgPtr );
Var
  y : TSyntax;
  i : TOperatorIndex;
  o : OpPtr;
Begin
  y := GetSyntax(P);
  For i := 1 To NbOperators Do
    With Operators[i] Do
      If y In S Then
        o := Op_Append(P^.PP_OPER,C,N,F,TOpTypeToArity(T),T,R)
End;

{----------------------------------------------------------------------------}
{ standard order of terms                                                    }
{----------------------------------------------------------------------------}

{ use PII+ doc p.121 , this is different from SWI Prolog: section 4.6.1 in: 
 https://www.swi-prolog.org/pldoc/man?section=compare }

{ compare the creation time of two terms}
Function CompCreationTime( T1,T2 : TermPtr ) : TComp;
Var
  Cmp : TComp;
  G1, G2 : TObjectID;
Begin
  G1 := ObjectGuid(TObjectPtr(T1));
  G2 := ObjectGuid(TObjectPtr(T2));
  If G1 < G2 Then
    Cmp := CompLower
  Else If G1 > G2 Then
    Cmp := CompGreater
  Else
    Cmp := CompEqual;
  CompCreationTime := Cmp
End;

{ compare two real values }
Function CompReal( Val1,Val2 : LongReal ) : TComp;
Var
  Cmp : TComp;
Begin
  If Val1 < Val2 Then
    Cmp := CompLower
  Else If Val1 > Val2 Then
    Cmp := CompGreater
  Else
    Cmp := CompEqual;
  CompReal := Cmp
End;

{ compare two integer values }
Function CompInt( Val1,Val2 : LongInt ) : TComp;
Var
  Cmp : TComp;
Begin
  If Val1 < Val2 Then
    Cmp := CompLower
  Else If Val1 > Val2 Then
    Cmp := CompGreater
  Else
    Cmp := CompEqual;
  CompInt := Cmp
End;

{ compare two integer constants }
Function CompIntConst( C1,C2 : ConstPtr ) : TComp;
Var
  Val1,Val2 : LongInt;
  code : Integer;
Begin
  CompIntConst := CompUndefined;
  Val1 := ShortStringToLongInt(ConstGetShortString(C1),code);
  If code <> 0 Then 
    Exit;
  Val2 := ShortStringToLongInt(ConstGetShortString(C2),code);
  If code <> 0 Then 
    Exit;
  CompIntConst := CompInt(Val1,Val2)
End;

{ compare two real constants }
Function CompRealConst( C1,C2 : ConstPtr ) : TComp;
Var
  Val1,Val2 : LongReal;
  code : Integer;
Begin
  CompRealConst := CompUndefined;
  Val1 := ShortStringToLongReal(ConstGetShortString(C1),code);
  If code <> 0 Then 
    Exit;
  Val2 := ShortStringToLongReal(ConstGetShortString(C2),code);
  If code <> 0 Then 
    Exit;
  CompRealConst := CompReal(Val1,Val2)
End;

{ compare T1 and T2, cf. PII+ doc p.121: 
 variable < real < integer < identifier < string < tuple of less than 2 < 
 list < other tuples }
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
  Len1,Len2 : TTupleArgNumber;
  IsList1,IsList2 : Boolean;
  Cat1,Cat2 : 1..3;
Begin
  StandardOrderOfTerms := CompUndefined;
  TT1 := TypeOfTerm(T1);
  TT2 := TypeOfTerm(T2);
  { cases by increasing priority }
  Case TT1 Of
  Variable:
    Begin
      Case TT2 Of 
      Variable:
        Cmp := CompCreationTime(T1,T2);
      Else
        Cmp := CompLower
      End
    End;
  Constant:
    Begin
      Case TT2 Of
      Variable:
        Cmp := CompGreater;
      Constant: { real < integer < string }
        Begin
          Case ConstType(C1) Of
          RealNumber:
            Begin
              Case ConstType(C2) Of
              RealNumber:
                Cmp := CompRealConst(C1,C2);
              Else
                Cmp := CompLower
              End
            End;
          IntegerNumber:
            Begin
              Case ConstType(C2) Of
              RealNumber:
                Cmp := CompGreater;
              IntegerNumber:
                Cmp := CompIntConst(C1,C2);
              Else
                Cmp := CompLower
              End
            End;
          QString:
            Begin
              Case ConstType(C2) Of
              RealNumber,
              IntegerNumber:
                Cmp := CompGreater;
              QString:
                Cmp := Str_Comp(ConstGetStr(C1),ConstGetStr(C2))
              End
            End
          End
        End;
      Identifier:
        Begin
          Case ConstType(C1) Of 
            RealNumber,
            IntegerNumber:
              Cmp := CompLower;
            Else
              Cmp := CompGreater
          End
        End;
      Else
        Cmp := CompLower
      End
    End;
  Identifier:
    Begin
      Case TT2 Of
      Variable:
        Cmp := CompGreater;
      Constant:
        Begin
          Case ConstType(C2) Of
          RealNumber,
          IntegerNumber:
            Cmp := CompGreater;
          Else
            Cmp := CompLower
          End
        End;
      Identifier:
        Cmp := Str_Comp(IdentifierGetStr(I1),IdentifierGetStr(I2));
      Else
        Cmp := CompLower
      End
    End;
  FuncSymbol:
    Begin
      { type and length }
      IsList1 := ProtectedIsListOfKnownSize(T1,True,Len1);
      IsList2 := ProtectedIsListOfKnownSize(T2,True,Len2);
      If Not IsList1 Then
        Len1 := TupleArgCount(T1); { FIXME: loop? }
      If Not IsList2 Then
        Len2 := TupleArgCount(T2);
      { rank in increasing order of priority: 1..3}
      Cat1 := Ord((Not IsList1) And (Len1 < 2)) + 2*Ord(IsList1) + 
          3*Ord((Not IsList1) And (Len1 >= 2));
      Cat2 := Ord((Not IsList2) And (Len2 < 2)) + 2*Ord(IsList2) + 
          3*Ord((Not IsList2) And (Len2 >= 2));
      { compare }
      If Cat1 < Cat2 Then
        Cmp := CompLower
      Else If Cat1 > Cat2 Then
        Cmp := CompGreater
      Else 
      Begin
        Cmp := CompInt(Len1,Len2);
        { not two tuples of different sizes? }
        If IsList1 Or (Cmp = CompEqual) Then
        Begin
          H1 := ProtectedGetTupleArg(T1,True);
          H2 := ProtectedGetTupleArg(T2,True);
          Cmp := StandardOrderOfTerms(H1,H2);
          If Cmp = CompEqual Then
            Cmp := StandardOrderOfTerms(T1,T2)
        End
      End
    End
  End;
  StandardOrderOfTerms := Cmp
End;


{----------------------------------------------------------------------------}
{ warnings                                                                   }
{----------------------------------------------------------------------------}

{ check a condition is met }
Function Fail( Condition : Boolean; msg : TString ) : Boolean;
Begin
  If Not Condition Then
    EvaluationError(msg);
  Fail := Not Condition
End;


{----------------------------------------------------------------------------}
{ eval: tuple                                                                }
{----------------------------------------------------------------------------}

Function EvaluateTerm( T : TermPtr; P : ProgPtr; g : TSerial ) : TermPtr;
Forward;

{ evaluate a whole tuple, returning a new tuple }
Function EvaluateTuple( T : TermPtr; P : ProgPtr; g : TSerial ) : TermPtr;
Var
  H,Q : TermPtr;
  Seen : Boolean;
Begin
  EvaluateTuple := Nil;

  { loop detection: get and set 'seen' status }
  Seen := Term_GetSeen(T,1,g);
  Term_SetSeen(T,1,g);
  If Seen Then
    Exit;

  If IsEmptyTuple(T) Then
    EvaluateTuple := NewEmptyTuple
  Else
  Begin
    { get head and queue }
    H := ProtectedGetTupleHead(T,True);
    Q := ProtectedGetTupleQueue(T,True);
    { evaluate both }
    H := EvaluateTerm(H,P,g);
    If Error Then Exit;
    Q := EvaluateTuple(Q,P,g);
    If Error Then Exit;
    { form and return the resulting tuple }
    EvaluateTuple := NewTuple(H,Q)
  End
End;


{----------------------------------------------------------------------------}
{ eval: array                                                                }
{----------------------------------------------------------------------------}

{ evaluate an array T ident(args) }
Function EvaluateArray( Ident,Args : TermPtr; NbArgs : TTupleArgNumber;
    P : ProgPtr; g : TSerial ) : TermPtr;
Var
  IIdent : IdPtr Absolute Ident;
  ArrIndex : PosInt;
  T1 : TermPtr;
  CT1 : ConstPtr Absolute T1;
  code : Integer;
Begin
  { default }
  EvaluateArray := Nil;

  { check: dimension }
  If Fail(NbArgs = 1, 'malformed array') Then
    Exit;
  { index }
  T1 := EvaluateTerm(TupleArg(Args),P,g);
  { check: index is an integer }
  If Fail((TypeOfTerm(T1) = Constant) And (ConstType(CT1) = IntegerNumber),
      'array index must be an integer value') Then
    Exit;
  { check: index is positive }
  ArrIndex := ShortStringToPosInt(ConstGetShortString(CT1),code);
  If Fail(code = 0,'array index must be a positive integer') Then
    Exit;
  { check: index is within bounds }
  If Fail((ArrIndex >= 1) And (ArrIndex <= GetArraySize(IIdent)),
      'array index is out of bounds') Then
    Exit;

  { result }
  EvaluateArray := Array_GetElement(GetArray(IIdent),ArrIndex)
End;


{----------------------------------------------------------------------------}
{ eval: @-comparisons                                                        }
{----------------------------------------------------------------------------}

{ evaluate a @-comparison (OpCade), with ident(args) }
Function EvaluateOrderExp( o : OpPtr; Ident,Args : TermPtr; 
    NbArgs : TTupleArgNumber; P : ProgPtr; g : TSerial ) : TermPtr;
Var
  OpCode : TOpCode; 
  func : TString;
  U,T1,T2 : TermPtr;
  Cmp : TComp;
  Ok : Boolean;
Begin
  EvaluateOrderExp := Nil;

  OpCode := Op_GetCode(o);
  func := Op_GetFunction(o);

  If Fail(NbArgs = 2, func + ' requires two parameters') Then
    Exit;
  { get arguments }
  { FIXME: this passes through user variables }
  U := Args;
  T1 := ProtectedGetTupleArg(U,True);
  T2 := ProtectedGetTupleArg(U,True);
  Cmp := StandardOrderOfTerms(T1,T2);

  If Fail(Cmp <> CompUndefined,'operands of ' + func + ' not comparable') Then
    Exit;

  Ok := (Cmp = CompEqual) 
        And (OpCode In [OPER_ORDER_LE,OPER_ORDER_GE])
      Or (Cmp = CompLower) 
        And (OpCode In [OPER_ORDER_L,OPER_ORDER_LE])
      Or (Cmp = CompGreater) 
        And (OpCode In [OPER_ORDER_G,OPER_ORDER_GE]);

  EvaluateOrderExp := EmitPositiveInteger(P,Ord(Ok))
End;


{----------------------------------------------------------------------------}
{ eval: numerical expressions                                                }
{----------------------------------------------------------------------------}

{ evaluate a numerical expression: ident(args) }
Function EvaluateNumExp( o : OpPtr; Ident,Args : TermPtr; 
    NbArgs : TTupleArgNumber; P : ProgPtr; g : TSerial ) : TermPtr;
{ data structure for high precision arithmetic }
Const
  MaxFuncNbParams = 3; { maximum number of parameter for a predefined function }
Type
  TParVal = Record
    IsReal : Boolean;
    Val : LongReal
  End;
  TParArray = Array[1..MaxFuncNbParams] Of TParVal; { parameter value }
Var
  IIdent : IdPtr Absolute Ident;
  U,T1 : TermPtr;
  CT1 : ConstPtr Absolute T1;
  code : Integer;
  OpCode : TOpCode; 
  func : TString;
  rs : TString;
  s : StrPtr;
  ParVal : TParArray;
  i : TTupleArgNumber;
  r : LongReal; { result of numerical evaluation }
  isInt : Boolean; { should this result be converted to an integer value? }
  cot : TypePrologObj;
Begin
  EvaluateNumExp := Nil;

  OpCode := Op_GetCode(o);
  func := Op_GetFunction(o);

  U := EvaluateTuple(Args,P,g);
  { extract numerical values from the arguments }
  For i := 1 to NbArgs Do
  Begin
    T1 := TupleArg(U);
    If Fail(IsConstant(T1) And 
        (ConstType(CT1) In [IntegerNumber,RealNumber]),
        'operand ' + PosIntToShortString(i) + ' of ' + func + 
        ' does not evaluate to a numerical value') Then
      Exit;
    With ParVal[i] Do
    Begin
      If ConstType(CT1) = IntegerNumber Then
      Begin
        IsReal := False;
        Val := ShortStringToLongInt(ConstGetShortString(CT1),code);
        If Fail(code = 0,'error in integer value') Then
          Exit
      End
      Else
      Begin
        IsReal := True;
        Val := ShortStringToLongReal(ConstGetShortString(CT1),code);
        If Fail(code = 0,'error in real value') Then
          Exit
      End
    End;
  End;

  { by default, ops on integers give integer results }
  IsInt := True;
  For i := 1 to NbArgs Do
    IsInt := IsInt And Not ParVal[i].IsReal;
  { TODO: absolute precision }
  Case OpCode Of
  OPER_ADD_1:
    Begin
      r := ParVal[1].Val
    End;
  OPER_SUB_1: 
    Begin
      r := -1*ParVal[1].Val
    End;
  OPER_ADD_2:  
    Begin
      r := ParVal[1].Val + ParVal[2].Val
    End;
  OPER_SUB_2:  
    Begin
      r := ParVal[1].Val - ParVal[2].Val
    End;
  OPER_MUL:
    Begin
      r := ParVal[1].Val * ParVal[2].Val
    End;
  OPER_DIV:
    Begin
      If Fail(ParVal[2].Val <> 0,'division by zero') Then
        Exit;
      r := ParVal[1].Val / ParVal[2].Val
    End;
  OPER_DIV_INT:
    Begin
      If Fail(ParVal[2].Val <> 0,'division by zero') Then
        Exit;
      If Fail(IsInt,
          'arguments of an integer division must be integer values') Then
        Exit;
      r := LongIntDiv(LongRealToLongInt(ParVal[1].Val),
          LongRealToLongInt(ParVal[2].Val))
    End;
  OPER_MOD,
  OPER_REM:
    Begin
      If Fail(IsInt,'arguments must be integer values') Then
        Exit;
      If Fail(ParVal[2].Val <> 0,'divisor is zero') Then
        Exit;
      { mod(x,y) = rem(x,y) when x,y>0
        sign(rem(x,y) = sign(x)
        sign(mod(x,y) = sign(y)
        }
      If ParVal[1].Val = 0 Then
        r := 0
      Else
      Begin
        { note that TP4's 'mod' is actually the remainder operator }
        r := LongRealToLongInt(ParVal[1].Val) mod 
            LongRealToLongInt(ParVal[2].Val);
        { fix mod when operands are of opposing signs }
        If (OpCode = OPER_MOD) And 
          ((ParVal[1].Val < 0) Xor (ParVal[2].Val < 0)) Then
          r := r + ParVal[2].Val
      End
    End;
  OPER_POWER:
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
        { (-x)^real is not defined }
        If Fail(ParVal[2].IsReal,
            'exponent of a negative value cannot be a real number') Then
          Exit;
        r := Exp(ParVal[2].Val*Ln(Abs(ParVal[1].Val)));
        If Frac(ParVal[2].Val/2) <> 0 Then
          r := -1*r
      End
      Else
      Begin
        r := Exp(ParVal[2].Val*Ln(ParVal[1].Val))
      End
    End;
  OPER_COMP_L:
    Begin
      r := Ord(ParVal[1].Val < ParVal[2].Val); { TODO: order on strings, etc.}
      IsInt := True
    End;
  OPER_COMP_LE:
    Begin
      r := Ord(ParVal[1].Val <= ParVal[2].Val); { TODO: order on strings, etc.}
      IsInt := True
    End;
  OPER_COMP_G:
    Begin
      r := Ord(ParVal[1].Val > ParVal[2].Val); { TODO: order on strings, etc.}
      IsInt := True
    End;
  OPER_COMP_GE:
    Begin
      r := Ord(ParVal[1].Val >= ParVal[2].Val); { TODO: order on strings, etc.}
      IsInt := True
    End;
  OPER_COMP_EQ: { PII+ p.110: automatic type conversion }
    Begin
      r := Ord(ParVal[1].Val = ParVal[2].Val);
      IsInt := True
    End;
  OPER_COMP_IS:
    Begin
      If ParVal[1].IsReal Xor ParVal[2].IsReal Then
        r := 0
      Else
        r := Ord(ParVal[1].Val = ParVal[2].Val);
      IsInt := True
    End;
  OPER_FLOOR:
    Begin
      If (Not ParVal[1].IsReal) Or (Frac(ParVal[1].Val) = 0) Then
        r := ParVal[1].Val
      Else 
      Begin
        r := Int(ParVal[1].Val);
        If ParVal[1].Val < 0 Then
          r := r - 1
      End;
      IsInt := True
    End;
  OPER_CEIL:  
    Begin
      If (Not ParVal[1].IsReal) Or (Frac(ParVal[1].Val) = 0) Then
        r := ParVal[1].Val
      Else 
      Begin
        r := Int(ParVal[1].Val);
        If ParVal[1].Val > 0 Then
          r := r + 1
      End;
      IsInt := True
    End;
  OPER_RANDOM:
    Begin
      With ParVal[1] Do
      Begin
        If Fail((Not IsReal) And (Val > 0),
            'argument of random/1 must be a positive integer') Then
          Exit;
        r := Random * Val
      End;
      IsInt := True
    End;
  OPER_IF:
    Begin
      If ParVal[1].Val <> 0 Then
        i := 2
      Else
        i := 3;
      r := ParVal[i].Val;
      IsInt := Not ParVal[i].IsReal
    End;
  NA:
    Begin
      If Fail(False,
        'operator or function not implemented yet: ''' + func + 
            '''') Then
        Exit
    End;
  Else
    Exit
  End;

  { emit result r }
  { generate canonical numeric constants }
  If IsInt Then
  Begin
    If Fail((r > -1.0*MaxLongInt) And (r < 1.0*MaxLongInt),
        'integer value overflow') Then
      Exit;
    rs := LongIntToShortString(LongRealToLongInt(r));
    cot := CI
  End
  Else
  Begin
    rs := LongRealToShortString(r);
    cot := CR
  End;
  s := Str_NewFromShortString(rs); { TODO: UTF-8 user ops }
  EvaluateNumExp := EmitConst(P,s,cot,False); { FIXME: not glob? }
End;


{----------------------------------------------------------------------------}
{ eval: function                                                             }
{----------------------------------------------------------------------------}

{ evaluate a function T = ident(...) }
Function EvaluateFunction( Ident,T : TermPtr; P : ProgPtr; 
    g : TSerial ) : TermPtr;
Var
  IIdent : IdPtr Absolute Ident;
  Args : TermPtr;
  NbArgs : TTupleArgNumber;
  func : TString;
  o : OpPtr;
  OpCode : TOpCode;
Begin
  EvaluateFunction := Nil;

  Args := TupleQueue(T);
  NbArgs := ProtectedGetTupleArgCount(Args,True);

  { array? e.g; stack(2) }
  If IsArray(IIdent) Then 
  Begin
    EvaluateFunction := EvaluateArray(Ident,Args,NbArgs,P,g);
    Exit
  End;

  { evaluable function? }
  func := IdentifierGetShortString(IIdent);
  o := Op_Lookup(P^.PP_OPER,[OP_FUNCTION,OP_ARITY],'',func,[],NbArgs,0);
  
  If Fail(o <> Nil,'unknown evaluable function or operator: ' + func) Then
    Exit;

  OpCode := Op_GetCode(o);

  { @-comparison }
  If OpCode In [OPER_ORDER_L,OPER_ORDER_LE,
      OPER_ORDER_G,OPER_ORDER_GE] Then
  Begin
    EvaluateFunction := EvaluateOrderExp(o,Ident,Args,NbArgs,P,g);
    Exit
  End;

  { num op }
  EvaluateFunction := EvaluateNumExp(o,Ident,Args,NbArgs,P,g)
End;


{----------------------------------------------------------------------------}
{ eval: term                                                                 }
{----------------------------------------------------------------------------}

{ evaluate a term T; the expression to be evaluated is constructed 
  recursively from constants, identifiers and evaluable functions }
Function EvaluateTerm( T : TermPtr; P : ProgPtr; g : TSerial ) : TermPtr;
Var
  Ident : TermPtr;
  Seen : Boolean;
Begin
  T := ProtectedRepOf(T);
  EvaluateTerm := T; { default is to return T's rep }

  { loop detection: get and set 'seen' status }
  Seen := Term_GetSeen(T,1,g);
  Term_SetSeen(T,1,g);

  Case TypeOfTerm(T) Of
  Variable:
    Exit; { a free variable evaluates to itself }
  Identifier:
    Begin
      { assigned? unassigned identifier evaluates to itself }
      If IsAssigned(IdPtr(T)) Then
        EvaluateTerm := GetValue(IdPtr(T));
      Exit
    End;
  Constant:
    Exit;
  FuncSymbol:
    Begin
      If IsEmptyTuple(T) Then
        Exit;
      If Seen Then { break loop }
        Exit;
      { look for something evaluable }
      Ident := ProtectedGetTupleHead(T,True); { head, reduced }
      If IsIdentifier(Ident) Then
        EvaluateTerm := EvaluateFunction(Ident,T,P,g)
      Else
        EvaluateTerm := EvaluateTuple(T,P,g)
    End
  End
End;
 
 { evaluate a term, detecting loops }
Function ProtectedEvaluateTerm( T : TermPtr; P : ProgPtr ) : TermPtr;
Begin
  ProtectedEvaluateTerm := EvaluateTerm(T,P,NewSerial)
End;

End.