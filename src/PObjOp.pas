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
  Strings,
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

Function OpLookup( start : OpPtr; ope,func : TString; 
    OpTypes : TOpTypes; Arity: Byte; MaxPred : TPrecedence ) : OpPtr;
Function OpAppend( Var list : OpPtr; ope,func : TString; ot : TOpType; 
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

{ return True if an operator is quoted }
Function IsQuotedOp( s : TString ) : Boolean;
Begin
  IsQuotedOp := False;
  If Length(s) >= 2 Then
    If (s[1] = '''') And (s[Length(s)] = '''') Then
      IsQuotedOp := True
End;

{ return an operator w/o its quotes if any }
Function UnquotedOp( ope : TString ) : TString;
Var
  s : TString;
Begin
  s := ope;
  If IsQuotedOp(s) Then
  Begin
    Delete(s,Length(s),1);
    Delete(s,1,1)
  End;
  UnquotedOp := s
End;

{ are two operators identical, regardless of simple quotes? }
Function SameOp( ope1,ope2 : TString ) : Boolean;
Begin
  SameOp := UnquotedOp(ope1) = UnquotedOp(ope2)
End;

{-----------------------------------------------------------------------}
{ constructors                                                          }
{-----------------------------------------------------------------------}

{ create a new operator object }
Function NewOp( ope,func : TString; ot : TOpType; pred : TPrecedence ) : OpPtr;
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
  NewOp := o
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
Function OpLookup( start : OpPtr; ope,func : TString; OpTypes : TOpTypes; 
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
    Else If IsQuotedOp(o^.OP_OPER) Then { FIXME: inefficient }
      OpMatch := SameOp(o^.OP_OPER,ope)
    Else
      OpMatch := o^.OP_OPER = ope;
    { search criteria all ok }
    If (OpMatch)
        And ((func = '') Or (o^.OP_FUNC = func)) 
        And ((OpTypes = []) Or (o^.OP_TYPE In OpTypes)) 
        And ((Arity = 0) Or (o^.OP_NPAR = Arity))
        And (o^.OP_PRED <= MaxPred) Then
      Found := True
    Else
      o := o^.OP_NEXT
  End;
  If Not Found Then
    o := Nil;
  OpLookup := o
End;

{ append an operator to a list of operators }
Function OpAppend( Var list : OpPtr; ope,func : TString; ot : TOpType; 
    pred : TPrecedence ) : OpPtr;
Var 
  o : OpPtr;
Begin
  o := NewOp(ope,func,ot,pred);
  With o^ Do
  Begin
    OP_NEXT := list
  End;
  list := o;
  OpAppend := o
End;

End.