{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObjOp.pas                                                 }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022-2026                                                  }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{             P R O L O G   O B J E C T S :   O P E R A T O R                }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }

Unit PObjOp;

Interface

Uses
  ShortStr,
  Errs,
  Memory,
  PObj;

Type
  TOpCode = (
    OPER_USER, { user defined }
    OPER_ADD_1, { +1 }
    OPER_ADD_2, { 1 + 1 }
    OPER_SUB_1, { -1 }
    OPER_SUB_2, { 1 - 1 }
    OPER_MUL, { 1 * 1}
    OPER_DIV, { 3 / 2 }
    OPER_POWER, { 2^3  => 8 }
    OPER_DIV_INT, { 3 // 2 }
    OPER_MOD, { -21 mod 4  => 3 }
    OPER_REM, { -21 rem 4  => -1 }
    OPER_FLOOR, { floor(1.5e0)  => 1 }
    OPER_CEIL, { ceiling(1.5e0)  => 2 }
    OPER_RANDOM, { random(10)  => 2 }
    OPER_IF, { if(1<2,10,20)  => 10 }
    OPER_COMP_EQ, { 2 = 2  => 1}
    OPER_COMP_IS, { 2 is 2e0  => 0 }
    OPER_COMP_L, { 1 < 2 }
    OPER_COMP_G, { 2 > 1 }
    OPER_COMP_LE, { 1 =< 1 }
    OPER_COMP_GE, { 2 >= 2 }
    OPER_ORDER_L, { aa @< bb }
    OPER_ORDER_G, { bb @> aa }
    OPER_ORDER_LE, { aa @=< aa }
    OPER_ORDER_GE, { aa @>= aa }
    NA
  );

Type 
  TOpType = (fx,fy,xf,yf,xfx,xfy,yfx);
  TOpTypes = Set Of TOpType;
  TPrecedence = 0..1200; { the higher the lower the priority }
  TOpArity = 1..3;

{ operator }
Type
  OpPtr = ^TObjOp;
  TObjOp = Record
    PO_META : TObjMeta;
    { not deep copied: }
    OP_PREV : OpPtr;
    OP_NEXT : OpPtr;
    { extra data: }
    OP_CODE : TOpCode;  { code, OPER_USER for user defined (can be deleted) }
    OP_OPER : TString;  { when used as an operator }
    OP_FUNC : TString;  { when used as a function }
    OP_TYPE : TOpType;
    OP_NPAR : TOpArity; { Number of parameters: 1, 2 }
    OP_PRED : TPrecedence;
  End;


Function IsOpTypeString( s : TString ) : Boolean;
Function PStrToOpType( s : TString ) : TOpType;
Function TOpTypeToArity( ot : TOpType ) : TOpArity;

Function Op_GetCode( o : OpPtr ) : TOpCode;
Function Op_GetType( o : OpPtr ) : TOpType;
Function Op_GetArity( o : OpPtr ) : TOpArity;
Function Op_GetOperator( o : OpPtr ) : TString;
Function Op_GetFunction( o : OpPtr ) : TString;
Function Op_GetPrecedence( o : OpPtr ) : TPrecedence;
Function Op_IsUser( o : OpPtr ) : Boolean;

{ operator lookup fields }
Type
  TOpField = (
    OP_OPERATOR,
    OP_FUNCTION,
    OP_TYPES,
    OP_ARITY,
    OP_PRECEDENCE,
    OP_MAX_PRECEDENCE
  );
  TSetOfOpField = Set Of TOpField;

Function Op_Lookup( start : OpPtr; Fields : TSetOfOpField; ope,func : TString; 
    OpTypes : TOpTypes; Arity: Byte; Pred : TPrecedence ) : OpPtr;

Function Op_Append( Var list : OpPtr; code : TOpCode; ope,func : TString; 
    arity : TOpArity; ot : TOpType; pred : TPrecedence ) : OpPtr;

Procedure Op_Remove( Var list : OpPtr; o : OpPtr );


Implementation
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------}
{ helpers                                                               }
{-----------------------------------------------------------------------}

{ a string contain a known operator type }
Function IsOpTypeString( s : TString ) : Boolean;
Begin
  IsOpTypeString := (s = 'fx') Or (s = 'fy') Or (s = 'xf') Or (s = 'yf') 
      Or (s = 'xfx') Or (s = 'xfy') Or (s = 'yfx')
End;

{ Pascal string to operator type }
Function PStrToOpType( s : TString ) : TOpType;
Var 
  ot : TOpType;
Begin
  If s = 'fx' Then 
    ot := fx
  Else If s = 'fy' Then 
    ot := fy
  Else If s = 'xf' Then 
    ot := xf
  Else If s = 'yf' Then 
    ot := yf
  Else If s = 'xfx' Then 
    ot := xfx
  Else If s = 'xfy' Then 
    ot := xfy
  Else If s = 'yfx' Then 
    ot := yfx
  Else
    Bug('unknown operator type: ' + s);
  PStrToOpType := ot
End;

{ number of parameters of an operator of a certain type }
Function TOpTypeToArity( ot : TOpType ) : TOpArity;
Begin
  If ot In [fx,fy,xf,yf] Then
    TOpTypeToArity := 1
  Else
    TOpTypeToArity := 2
End;

{-----------------------------------------------------------------------}
{ constructors                                                          }
{-----------------------------------------------------------------------}

{ create a new operator object }
Function Op_New( code : TOpCode; ope,func : TString; arity : TOpArity; 
    ot : TOpType; pred : TPrecedence ) : OpPtr;
Var 
  o : OpPtr;
  ptr : TObjectPtr Absolute o;
Begin
  ptr := NewRegisteredPObject(OP,SizeOf(TObjOp),2,False,0);
  With o^ Do
  Begin
    OP_PREV := Nil;
    OP_NEXT := Nil;
    OP_CODE := code;
    OP_OPER := ope;
    OP_FUNC := func;
    OP_TYPE := ot;
    OP_NPAR := arity;
    OP_PRED := pred
  End;
  Op_New := o
End;

{-----------------------------------------------------------------------}
{ get / set                                                             }
{-----------------------------------------------------------------------}

{ code }
Function Op_GetCode( o : OpPtr ) : TOpCode;
Begin
  Op_GetCode := o^.OP_CODE
End;

{ type }
Function Op_GetType( o : OpPtr ) : TOpType;
Begin
  Op_GetType := o^.OP_TYPE
End;

{ arity }
Function Op_GetArity( o : OpPtr ) : TOpArity;
Begin
  Op_GetArity := o^.OP_NPAR
End;

{ name when used as an operator }
Function Op_GetOperator( o : OpPtr ) : TString;
Begin
  Op_GetOperator := o^.OP_OPER
End;

{ name when used as a function }
Function Op_GetFunction( o : OpPtr ) : TString;
Begin
  Op_GetFunction := o^.OP_FUNC
End;

{ precedence }
Function Op_GetPrecedence( o : OpPtr ) : TPrecedence;
Begin
  Op_GetPrecedence := o^.OP_PRED
End;

{ created using op predicate? }
Function Op_IsUser( o : OpPtr ) : Boolean;
Begin
  Op_IsUser := Op_GetCode(o) = OPER_USER
End;

{-----------------------------------------------------------------------}
{ methods                                                               }
{-----------------------------------------------------------------------}

{ look up an operator object by operator, function, type, arity, precedence or
 max precedence; return a pointer to the operator object found, or Nil if not 
 found }
Function Op_Lookup( start : OpPtr; Fields : TSetOfOpField; ope,func : TString; 
    OpTypes : TOpTypes; Arity: Byte; Pred : TPrecedence ) : OpPtr;
Var
  o : OpPtr;
  Found : Boolean;
Begin
  o := start;
  Found := False;
  While (o<>Nil) And Not Found Do
  Begin
    If ((Not (OP_OPERATOR In Fields)) Or (Op_GetOperator(o) = ope))
        And ((Not (OP_FUNCTION In Fields)) Or (Op_GetFunction(o) = func)) 
        And ((Not (OP_TYPES In Fields)) Or (Op_GetType(o) In OpTypes)) 
        And ((Not (OP_ARITY In Fields)) Or (Op_GetArity(o) = Arity))
        And ((Not (OP_PRECEDENCE In Fields)) 
          Or (Op_GetPrecedence(o) = Pred))
        And ((Not (OP_MAX_PRECEDENCE In Fields)) 
          Or (Op_GetPrecedence(o) <= Pred)) Then
      Found := True
    Else
      o := o^.OP_NEXT
  End;
  If Not Found Then
    o := Nil;
  Op_Lookup := o
End;

{ append an operator to a list of operators }
Function Op_Append( Var list : OpPtr; code : TOpCode; ope,func : TString; 
    arity : TOpArity; ot : TOpType; pred : TPrecedence ) : OpPtr;
Var 
  o : OpPtr;
Begin
  o := Op_New(code,ope,func,arity,ot,pred);
  { insert }
  If list <> Nil Then
    list^.OP_PREV := o;
  o^.OP_NEXT := list;
  list := o;
  { done }
  Op_Append := o
End;

{ remove an operator to a list of operators }
Procedure Op_Remove( Var list : OpPtr; o : OpPtr );
Begin
  With o^ Do
  Begin
    { update previous item }
    If OP_PREV = Nil Then { was head? }
      list := OP_NEXT
    Else
      OP_PREV^.OP_NEXT := OP_NEXT;
    { update next item }
    If OP_NEXT <> Nil Then { had a next? }
      OP_NEXT^.OP_PREV := OP_PREV
  End
End;

End.