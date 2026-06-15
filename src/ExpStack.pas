{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : ExpStack.pas                                               }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022-2026                                                  }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                   S T A C K   O F   E X P R E S S I O N S                  }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }

{ stack to parse expressions }

Unit ExpStack;

Interface

Uses
  Errs,
  PObjStr,
  PObjTerm,
  PObjOp;

Const
  MAX_EXPR_DEEP = 50;
Type
  TOperandStackLen = 0..MAX_EXPR_DEEP;
  TOpStackLen = 0..MAX_EXPR_DEEP;

Procedure OpStack_Prepare;
Procedure OpStack_Terminate;

{ operands ("Operand") }
Function OpStack_OperandCount( TBottom : TOperandStackLen ) : TOperandStackLen;
Function OpStack_TopOperandIndex : TOperandStackLen;
Function OpStack_TopOperand( TBottom : TOperandStackLen ) : TermPtr;
Function OpStack_HasOperand( TBottom : TOperandStackLen ) : Boolean;
Procedure OpStack_PushOperand( T : TermPtr );
Procedure OpStack_PopOperand( Var T : TermPtr; TBottom : TOperandStackLen  );

{ operators ("Op") }
Function OpStack_OpCount( OBottom : TOpStackLen ) : TOpStackLen;
Function OpStack_TopOpIndex : TOpStackLen;
Function OpStack_TopOp( OBottom : TOpStackLen ) : OpPtr;
Function OpStack_BelowTopOp( OBottom : TOpStackLen ) : OpPtr;
Procedure OpStack_PushOp( o : OpPtr );
Procedure OpStack_PopOp( Var o : OpPtr; OBottom : TOpStackLen );

Implementation

Var 
  { operands }
  OperandStack : Array[1..MAX_EXPR_DEEP] Of TermPtr;
  OperandStackTop : TOperandStackLen;
  { operators }
  OpStack : Array[1..MAX_EXPR_DEEP] Of OpPtr;
  OpStackTop : TOpStackLen;

{----------------------------------------------------------------------------}
{ start / end parsing                                                        }
{----------------------------------------------------------------------------}

{ reset the expression stack; must be called before any parsing phase (command 
 line, file, in/1) ); if the previous parsing phase ended with an error, some 
 garbage might have been left in this stack }
Procedure OpStack_Prepare;
Begin
  OperandStackTop := 0;
  OpStackTop := 0
End;

{ check the stack is empty }
Procedure OpStack_Terminate;
Begin
  CheckCondition(OpStackTop = 0,
      'OpStack_Terminate: op stack not empty')
End;

{----------------------------------------------------------------------------}
{ operands                                                                   }
{----------------------------------------------------------------------------}

{ return the number of operands above TBottom }
Function OpStack_OperandCount( TBottom : TOperandStackLen ) : TOperandStackLen;
Begin
  CheckCondition(OperandStackTop >= TBottom,
      'OpStack_OperandCount: negative count');
  OpStack_OperandCount := OperandStackTop - TBottom
End;

{ return the index of the top of the operand stack }
Function OpStack_TopOperandIndex : TOperandStackLen;
Begin
  OpStack_TopOperandIndex := OperandStackTop
End;

{ return the (possibly Nil) operand at the top of the stack, above TBottom, 
 or Nil }
Function OpStack_TopOperand( TBottom : TOperandStackLen ) : TermPtr;
Begin
  If OperandStackTop > TBottom Then
    OpStack_TopOperand := OperandStack[OperandStackTop]
  Else
    OpStack_TopOperand := Nil
End;

{ is a non-null term available in the term stack, above TBottom? }
Function OpStack_HasOperand( TBottom : TOperandStackLen ) : Boolean;
Begin
  OpStack_HasOperand := OpStack_TopOperand(TBottom) <> Nil
End;

{ push an operand }
Procedure OpStack_PushOperand( T : TermPtr );
Begin
  If OperandStackTop > MAX_EXPR_DEEP - 1 Then
    SyntaxError('expression too complex');
  If Error Then Exit;
  OperandStackTop := OperandStackTop + 1;
  OperandStack[OperandStackTop] := T
End;

{ pop an operand  }
Procedure OpStack_PopOperand( Var T : TermPtr; TBottom : TOperandStackLen  );
Begin
  CheckCondition(OperandStackTop > TBottom,
      'OpStack_PopOperand: term stack is empty');
  T := OpStack_TopOperand(TBottom);
  OperandStackTop := OperandStackTop - 1
End;

{----------------------------------------------------------------------------}
{ operators                                                                  }
{----------------------------------------------------------------------------}

{ return the number of operators above OBottom }
Function OpStack_OpCount( OBottom : TOpStackLen ) : TOpStackLen;
Begin
  CheckCondition(OpStackTop >= OBottom,
      'OpStack_OpCount: negative count');
  OpStack_OpCount := OpStackTop - OBottom
End;

{ return the index of the top of the operator stack }
Function OpStack_TopOpIndex : TOpStackLen;
Begin
  OpStack_TopOpIndex := OpStackTop
End;

{ return the operator at the top of the stack, above OBottom, or Nil }
Function OpStack_TopOp( OBottom : TOpStackLen ) : OpPtr;
Begin
  If OpStackTop > OBottom Then
    OpStack_TopOp := OpStack[OpStackTop]
  Else
    OpStack_TopOp := Nil
End;

{ return the operator just below the top of the stack, above OBottom, or Nil }
Function OpStack_BelowTopOp( OBottom : TOpStackLen ) : OpPtr;
Begin
  If OpStackTop - 1 > OBottom Then
    OpStack_BelowTopOp := OpStack[OpStackTop-1]
  Else
    OpStack_BelowTopOp := Nil
End;

{ push an op }
Procedure OpStack_PushOp( o : OpPtr );
Begin
  If OpStackTop > MAX_EXPR_DEEP - 1 Then
      SyntaxError('expression too complex');
  If Error Then Exit;
  OpStackTop := OpStackTop + 1;
  OpStack[OpStackTop] := o
End;

{ pop an op }
Procedure OpStack_PopOp( Var o : OpPtr; OBottom : TOpStackLen );
Begin
  CheckCondition(OpStackTop > OBottom,
      'OpStack_PopOp: op stack is empty');
  o := OpStack_TopOp(OBottom);
  OpStackTop := OpStackTop - 1
End;


End.