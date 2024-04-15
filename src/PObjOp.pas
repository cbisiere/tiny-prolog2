{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObjOp.pas                                                 }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2024                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{             P R O L O G   O B J E C T S :   O P E R A T O R                }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Unit PObjOp;

Interface

Uses
  ShortStr,
  Errs,
  Memory,
  PObj;

{-----------------------------------------------------------------------}
{ types                                                                 }
{-----------------------------------------------------------------------}

Type 
  TOpType = (fx,fy,xf,yf,xfx,xfy,yfx);
  TOpTypes = Set Of TOpType;
  TPrecedence = 0..1200; { the higher the lower the priority }
  TOpArity = 1..2;

{ operator }
Type
  OpPtr = ^TObjOp;
  TObjOp = Record
    PO_META : TObjMeta;
    { not deep copied: }
    OP_NEXT : OpPtr;
    { extra data: }
    OP_OPER : TString;  { when used as an operator }
    OP_FUNC : TString;  { when used as a function }
    OP_TYPE : TOpType;
    OP_NPAR : TOpArity; { Number of parameters: 1, 2 }
    OP_PRED : TPrecedence
  End;


Function IsOpTypeString( s : TString ) : Boolean;
Function PStrToOpType( s : TString ) : TOpType;
Function TOpTypeToArity( ot : TOpType ) : TOpArity;

Function Op_GetType( o : OpPtr ) : TOpType;
Function Op_GetArity( o : OpPtr ) : TOpArity;
Function Op_GetOperator( o : OpPtr ) : TString;
Function Op_GetFunction( o : OpPtr ) : TString;
Function Op_GetPrecedence( o : OpPtr ) : TPrecedence;

Function Op_Lookup( start : OpPtr; ope,func : TString; 
    OpTypes : TOpTypes; Arity: Byte; MaxPred : TPrecedence ) : OpPtr;
Function Op_Append( Var list : OpPtr; ope,func : TString; ot : TOpType; 
    pred : TPrecedence ) : OpPtr;


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

{ return True if a short string is single-quoted }
Function IsSingleQuoted( s : TString ) : Boolean;
Begin
  IsSingleQuoted := False;
  If Length(s) >= 2 Then
    If (s[1] = '''') And (s[Length(s)] = '''') Then
      IsSingleQuoted := True
End;

{ return a short string w/o its single quotes if any }
Function Unquoted( ope : TString ) : TString;
Var
  s : TString;
Begin
  s := ope;
  If IsSingleQuoted(s) Then
  Begin
    Delete(s,Length(s),1);
    Delete(s,1,1)
  End;
  Unquoted := s
End;

{ are two operators identical, regardless of simple quotes? }
Function SameAs( ope1,ope2 : TString ) : Boolean;
Begin
  SameAs := Unquoted(ope1) = Unquoted(ope2)
End;

{-----------------------------------------------------------------------}
{ constructors                                                          }
{-----------------------------------------------------------------------}

{ create a new operator object }
Function Op_New( ope,func : TString; ot : TOpType; pred : TPrecedence ) : OpPtr;
Var 
  o : OpPtr;
  ptr : TObjectPtr Absolute o;
Begin
  ptr := NewRegisteredPObject(OP,SizeOf(TObjOp),1,False,0);
  With o^ Do
  Begin
    OP_NEXT := Nil;
    OP_OPER := ope;
    OP_FUNC := func;
    OP_TYPE := ot;
    OP_NPAR := TOpTypeToArity(ot);
    OP_PRED := pred
  End;
  Op_New := o
End;

{-----------------------------------------------------------------------}
{ get / set                                                             }
{-----------------------------------------------------------------------}

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

{-----------------------------------------------------------------------}
{ methods                                                               }
{-----------------------------------------------------------------------}

{ look up an operator object by operator, function, type, arity, max precedence; 
 search criteria are active when:
 ope: different from ''
 func: different from '' 
 OpTypes: different from []
 Arity: different from 0
 MaxPred: 1200 as no precedence is larger
 return a pointer to the operator object found, or Nil if not found }
Function Op_Lookup( start : OpPtr; ope,func : TString; OpTypes : TOpTypes; 
    Arity: Byte; MaxPred : TPrecedence ) : OpPtr;
Var
  o : OpPtr;
  Found, OpMatch : Boolean;
Begin
  o := start;
  Found := False;
  While (o<>Nil) And Not Found Do
  Begin
    { identifiers match? An op declared without quotes only match unquoted 
     operators, while an op declared with quotes match both quoted and
     unquoted operators; we do this to solve an issue spotted while 
     reading the Orbis program, which contains op(400,fy,'#') used as e.g. #N, 
     and also '-' used as a character; declaring op(200,fx,-,sub) fix it. }
    If ope = '' Then
      OpMatch := True
    Else If IsSingleQuoted(Op_GetOperator(o)) Then { FIXME: inefficient }
      OpMatch := SameAs(Op_GetOperator(o),ope)
    Else
      OpMatch := Op_GetOperator(o) = ope;
    { search criteria all ok }
    If (OpMatch)
        And ((func = '') Or (Op_GetFunction(o) = func)) 
        And ((OpTypes = []) Or (Op_GetType(o) In OpTypes)) 
        And ((Arity = 0) Or (Op_GetArity(o) = Arity))
        And (Op_GetPrecedence(o) <= MaxPred) Then
      Found := True
    Else
      o := o^.OP_NEXT
  End;
  If Not Found Then
    o := Nil;
  Op_Lookup := o
End;

{ append an operator to a list of operators }
Function Op_Append( Var list : OpPtr; ope,func : TString; ot : TOpType; 
    pred : TPrecedence ) : OpPtr;
Var 
  o : OpPtr;
Begin
  o := Op_New(ope,func,ot,pred);
  With o^ Do
  Begin
    OP_NEXT := list
  End;
  list := o;
  Op_Append := o
End;

End.