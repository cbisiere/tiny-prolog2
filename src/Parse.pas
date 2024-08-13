{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Parse.pas                                                  }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                              P A R S I N G                                 }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Unit Parse;

Interface

Uses
  ShortStr,
  Errs,
  IChar,
  Memory,
  PObj,
  PObjTerm,
  PObjFCVI,
  PObjIO,
  PObjOp,
  PObjStr,
  PObjTok,
  PObjEq,
  PObjSys,
  PObjDef,
  PObjBter,
  PObjRule,
  PObjQury,
  PObjProg,
  Tuple,
  Encoding,
  Tokenize;

Procedure VerifyToken( f : StreamPtr; P : ProgPtr; Var K : TokenPtr; 
    typ : TTokenType );

Function ParseOneRule( f : StreamPtr; P : ProgPtr; Var K : TokenPtr ) : RulePtr;
Function ParseOneTerm( f : StreamPtr; P : ProgPtr ) : TermPtr;
Function ParseOneQuery( f : StreamPtr; P : ProgPtr;
    Var K : TokenPtr ) : QueryPtr;

Implementation
{-----------------------------------------------------------------------------}

{ per-syntax elements }
Type
  TSyntaxElement = Array[TSyntax] Of Record
    RuleEnd: TTokenType;
    PromptEnd: TTokenType;
    AcceptSys: Boolean
  End;
Const
  Syntax : TSyntaxElement = (
    (RuleEnd:TOKEN_SEMICOLON;PromptEnd:TOKEN_SEMICOLON;AcceptSys:True),
    (RuleEnd:TOKEN_SEMICOLON;PromptEnd:TOKEN_SEMICOLON;AcceptSys:True),
    (RuleEnd:TOKEN_SEMICOLON;PromptEnd:TOKEN_SEMICOLON;AcceptSys:False),
    (RuleEnd:TOKEN_DOT;PromptEnd:TOKEN_DOT;AcceptSys:False)
  );


{ Note: in the following procedures, the K parameter is the *current* token 
 to analyze; the procedures themselves must make sure that, upon return, K 
 is the next token to analyze (since it will be passed to another parsing 
 procedure) }

{ read a term, possibly in a global context (that is, when parsing a
  rule), possibly accepting a cut as a valid term }
Function ReadTerm( f : StreamPtr; P : ProgPtr; Var K : TokenPtr; 
    glob : Boolean; Cut : Boolean) : TermPtr; Forward;

Function ReadPTerm( f : StreamPtr; P : ProgPtr; Var K : TokenPtr; 
    glob : Boolean; Cut : Boolean) : TermPtr; Forward;

{----------------------------------------------------------------------------}
{ verify                                                                     }
{----------------------------------------------------------------------------}

{ raise an error if a token is not of a certain type; read the token 
 following the token to verify }
Procedure VerifyToken( f : StreamPtr; P : ProgPtr; Var K : TokenPtr; 
    typ : TTokenType );
Begin
  If Token_GetType(K) = typ Then
    K := ReadProgramToken(P,f)
  Else
    SyntaxError(TokenStr[typ] + ' expected')
End;

{----------------------------------------------------------------------------}
{ expression parser                                                          }
{----------------------------------------------------------------------------}

Const
  MAX_EXPR_DEEP = 50;
Type
  TStackLen = 0..MAX_EXPR_DEEP;
Var 
  TStack : Array[1..MAX_EXPR_DEEP] Of TermPtr;
  OpStack : Array[1..MAX_EXPR_DEEP] Of OpPtr;
  TStackTop : TStackLen;
  OpStackTop : TStackLen;

{ reset the expression stack; must be called before any parsing phase (command
 line, file, in/1) ); if the previous parsing phase ended with an error, some
 garbage might have been left in this stack }
Procedure PrepareExprParsing;
Begin
  TStackTop := 0;
  OpStackTop := 0
End;

{ check the stack is empty }
Procedure TerminateExprParsing;
Begin
  CheckCondition(OpStackTop = 0,'op stack not empty')
End;

{ return the (possibly Nil) term at the top of the stack, above TBottom, 
 or Nil }
Function TopTerm( TBottom : TStackLen ) : TermPtr;
Begin
  If TStackTop > TBottom Then
    TopTerm := TStack[TStackTop]
  Else
    TopTerm := Nil
End;

{ is a non-null term available in the term stack, above TBottom? }
Function HasTerm( TBottom : TStackLen ) : Boolean;
Begin
  HasTerm := TopTerm(TBottom) <> Nil
End;

{ return the operator at the top of the stack, above OBottom, or Nil }
Function TopOp( OBottom : TStackLen ) : OpPtr;
Begin
  If OpStackTop > OBottom Then
    TopOp := OpStack[OpStackTop]
  Else
    TopOp := Nil
End;

{ push an op }
Procedure PushExprOp( o : OpPtr );
Begin
  If OpStackTop > MAX_EXPR_DEEP - 1 Then
    SyntaxError('expression too complex');
  If Error Then Exit;
  OpStackTop := OpStackTop + 1;
  OpStack[OpStackTop] := o
End;

{ pop an op }
Procedure PopExprOp( Var o : OpPtr; OBottom : TStackLen );
Begin
  CheckCondition(OpStackTop > OBottom,'PopExprOp: op stack is empty');
  o := OpStack[OpStackTop];
  OpStackTop := OpStackTop - 1
End;

{ push a term }
Procedure PushExprTerm( T : TermPtr );
Begin
  If TStackTop > MAX_EXPR_DEEP - 1 Then
    SyntaxError('expression too complex');
  If Error Then Exit;
  TStackTop := TStackTop + 1;
  TStack[TStackTop] := T
End;

{ pop a term }
Procedure PopExprTerm( Var T : TermPtr; TBottom : TStackLen  );
Begin
  CheckCondition(TStackTop > TBottom,'PopExprTerm: term stack is empty');
  T := TStack[TStackTop];
  TStackTop := TStackTop - 1
End;

{ replace the top operator and associated operand(s) with a term }
Procedure ReduceTopExpr( P : ProgPtr; TBottom, OBottom : TStackLen );
Var
  o : OpPtr;
  T,T1,T2 : TermPtr;
Begin
  PopExprOp(o,OBottom);
  CheckCondition(Op_GetArity(o) In [1,2],'ReduceTopExpr: unexpected arity');
  Case Op_GetArity(o) Of
  1:
    Begin
      PopExprTerm(T1,TBottom);
      CheckCondition(T1 <> Nil,'ReduceTopExpr: unexpected Nil term');
      T2 := Nil
    End;
  2:
    Begin
      PopExprTerm(T2,TBottom);
      PopExprTerm(T1,TBottom);
      CheckCondition((T1<>Nil) And (T2<>Nil),
          'ReduceTopExpr: unexpected Nil terms');
      
    End
  End;
  T := NewFunc2(P,Op_GetFunction(o),T1,T2,True);
  PushExprTerm(T)
End;

{ reduce as much as possible the expression stack, e.g
 1*2+3 => mul(1,2)+3 }
Procedure ReduceExprStack( P : ProgPtr; TBottom, OBottom : TStackLen );
Var
  T : TermPtr;
  o : OpPtr;
  Stop : Boolean; { no more reduction }
  pre1,pre2 : TPrecedence;
  typ1 : TOpType;
Begin
  Repeat
    Stop := True;
    If (OpStackTop - OBottom) >= 2 Then { at least two operators }
    Begin
      If (OpStack[OpStackTop] <> Nil) Then { no pending term }
      Begin
        CheckCondition(OpStack[OpStackTop-1] <> Nil,
            'ReduceExprStack: unexpected Nil term');
        pre1 := Op_GetPrecedence(OpStack[OpStackTop]);
        typ1 := Op_GetType(OpStack[OpStackTop]);
        pre2 := Op_GetPrecedence(OpStack[OpStackTop-1]);
        { priorities allow reduction? }
        If (pre1 > pre2) Or (pre1 = pre2) And (typ1 In [fy,yf,yfx]) Then
        Begin
          PopExprOp(o,OBottom);
          Case Op_GetArity(o) Of
          1:
            Begin
              ReduceTopExpr(P,TBottom,OBottom)
            End;
          2:
            Begin
              PopExprTerm(T,TBottom);
              ReduceTopExpr(P,TBottom,OBottom);
              PushExprTerm(T)
            End
          End;
          PushExprOp(o);
          Stop := False
        End
      End
    End
  Until Stop
End;


{ reduce all the expressions remaining in the stack and return the remaining 
 term }
Function ReduceAllExpr( P : ProgPtr; TBottom, OBottom : TStackLen ) : TermPtr;
Var
  T : TermPtr;
Begin
  ReduceAllExpr := Nil;
  While OpStackTop > OBottom Do
    ReduceTopExpr(P,TBottom,OBottom);
  PopExprTerm(T,TBottom); { final term }
  CheckCondition(TStackTop=TBottom,'ReduceAllExpr: terms left in the stack');
  ReduceAllExpr := T
End;

{ if the next token is an operator whose type is in a given set and has a 
 max precedence of MaxPred, return it; otherwise return Nil;
 do not consume the token }
Function NextOp( P : ProgPtr; Var K : TokenPtr; OpTypes : TOpTypes;
    MaxPred : TPrecedence ) : OpPtr;
Var
  o : OpPtr;
  oper : TString;
Begin
  o := Nil;
  If (Token_GetType(K) = TOKEN_IDENT) Or 
      (GetSyntax(P) = Edinburgh) And 
      (Token_GetType(K) In [TOKEN_ARROW,TOKEN_COMMA]) Then
  Begin
    oper := Str_GetShortStringTruncate(Token_GetStr(K));
    o := Op_Lookup(P^.PP_OPER,[OP_OPERATOR,OP_TYPES,OP_PRECEDENCE],
        oper,'',OpTypes,0,MaxPred);
      
    { special case: prefixed unary operator used as ident, e.g. ['-'|aa]. or 
     aa('-'); it is assumed that a quoted unary operator is actually a quoted 
     identifier, not an operator, so one cannot write 1 '-' 2 }
    If (o <> Nil) And (Op_GetType(o) In [fx,fy]) And 
        Token_IsQuoted(K) Then
      o := Nil
  End;
  NextOp := o
End;

{ read an expression of max precedence MaxPred, reducing the expression using 
 the term and operator stacks above Bottom only.
 (see pII+ p.44, "1.9.1 The syntactic level", rules 6.* "expr") 
 TODO: in PII+ eq(x,1 '<' 2 '<' 3) raises a syntax error
 https://www.swi-prolog.org/pldoc/doc_for?object=op/3 }
Function ReadExpr( f : StreamPtr; P : ProgPtr; Var K : TokenPtr; 
    TBottom, OBottom : TStackLen; MaxPred : TPrecedence; 
    glob : Boolean; Cut : Boolean) : TermPtr;
Var
  T : TermPtr;
  o : OpPtr;
  Found : Boolean; { case identified and treated }
  Done : Boolean; { are we done parsing the expression? }
  Pred : TPrecedence; { precedence of the current expression }
  ArgMaxPred : TPrecedence;
  s : StrPtr;
Begin
  ReadExpr := Nil;
  Done := False;
  Repeat
    Found := False;
    
    If Not Found Then 
    Begin
      { rule 6.1: prefixed (unary) operator }
      { FIXME: is it legal in PII+ to use op's identifiers without arguments? 
      as, e.g, op_add("+",add) ->; As such, this code triggers a syntax error 
      on the right parenthesis }
      o := NextOp(P,K,[fx,fy],MaxPred);
      If (o <> Nil) And (Not HasTerm(TBottom)) Then 
      Begin
        Pred := Op_GetPrecedence(o);
        K := ReadProgramToken(P,f);
        If Error Then Exit;
        { simplify +/- integer:
        PII+ p.48: "The trees corresponding to the unary operators + and - are  
        evaluated when analyzed if their argument is an integer constant." }
        If (Op_GetType(o) = fx) And ((Op_GetOperator(o) = '+') 
            Or (Op_GetOperator(o) = '-')) 
            And (Token_GetType(K) = TOKEN_INTEGER) Then
        Begin
          T := ReadPTerm(f,P,K,glob,Cut); { read the integer constant }
          If Error Then Exit;
          If Op_GetOperator(o) = '-' Then
          Begin
            s := Str_NewFromShortString('-');
            Str_Concat(s,ConstGetStr(ConstPtr(T)));
            T := EmitConst(P,s,CI,True)
          End;
          PushExprTerm(T)
        End
        Else
        Begin  
          PushExprOp(o);
          Case Op_GetType(o) Of
          fx : ArgMaxPred := Pred - 1;
          fy : ArgMaxPred := Pred
          End;
          T := ReadExpr(f,P,K,TStackTop,OpStackTop,ArgMaxPred,glob,Cut);
          If Error Then Exit;
          PushExprTerm(T)
        End;
        Found := True
      End
    End;

    { rule 6.3: infixed (binary) operator }
    If Not Found Then 
    Begin
      o := NextOp(P,K,[xfx,xfy,yfx],MaxPred);
      If (o <> Nil) And HasTerm(TBottom) And 
          ((Op_GetType(o) = yfx) Or (Pred < Op_GetPrecedence(o))) Then
      Begin
        Pred := Op_GetPrecedence(o);
        K := ReadProgramToken(P,f);
        If Error Then Exit;
        PushExprOp(o);
        Case Op_GetType(o) Of
        yfx,xfx : ArgMaxPred := Pred - 1;
        xfy : ArgMaxPred := Pred
        End;
        T := ReadExpr(f,P,K,TStackTop,OpStackTop,ArgMaxPred,glob,Cut);
        If Error Then Exit;
        PushExprTerm(T);
        Found := True
      End
    End;

    { rule 6.2: postfixed (unary) operator (rare case, user defined) }
    If Not Found Then 
    Begin
      o := NextOp(P,K,[xf,yf],MaxPred);
      If (o <> Nil) And HasTerm(TBottom) And 
          ((Op_GetType(o) = yf) Or (Pred < Op_GetPrecedence(o))) Then
      Begin
        Pred := Op_GetPrecedence(o);
        K := ReadProgramToken(P,f);
        If Error Then Exit;
        PushExprOp(o);
        Found := True
      End
    End;

    { rules 6.5: pterm }
    If Not Found Then 
    Begin
      If Not HasTerm(TBottom) Then
      Begin
        Pred := 0;
        T := ReadPTerm(f,P,K,glob,Cut);
        If Error Then Exit;
        PushExprTerm(T);
        Found := True
      End
    End;
    
    Done := Not Found And HasTerm(TBottom);

    { reduce the stack as much as possible }
    ReduceExprStack(P,TBottom,OBottom)

  Until Done;

  { finally reduce the stack to a single term: 
   1+2*3^4 => add(1,mul(2,^(3,4))) }
  ReadExpr := ReduceAllExpr(P,TBottom,OBottom) 
End;

{ read an expression of max precedence MaxPred, returning the tree of
 functions and arguments as a result }
Function ReadOneExpr( f : StreamPtr; P : ProgPtr; Var K : TokenPtr; 
    MaxPred : TPrecedence; glob : Boolean; Cut : Boolean) : TermPtr;
Var
  y : TSyntax;
Begin
  y := GetSyntax(P);
  If y In [PrologIIc,PrologII] Then { no expressions in old PrologII syntax }
    ReadOneExpr := ReadPTerm(f,P,K,glob,Cut)
  Else
    ReadOneExpr := ReadExpr(f,P,K,TStackTop,OpStackTop,MaxPred,glob,Cut)
End;

{----------------------------------------------------------------------------}
{ parser                                                                     }
{----------------------------------------------------------------------------}

{ read a term: expr [. term]*; is right-associative
 (see pII+ p.44, "1.9.1 The syntactic level", rules 4.1 and 4.2 "term") }
Function ReadTerm( f : StreamPtr; P : ProgPtr; Var K : TokenPtr; 
    glob : Boolean; Cut : Boolean) : TermPtr;
Var
  y : TSyntax;
  T : TermPtr;
  T2 : TermPtr;
  MaxPred : TPrecedence;
Begin
  ReadTerm := Nil;
  y := GetSyntax(P);
  If y = Edinburgh Then
    MaxPred := 1200
  Else
    MaxPred := 1000;
  T := ReadOneExpr(f,P,K,MaxPred,glob,Cut); { rules 4.1 and 4.2 }
  If Error Then Exit;
  If (y <> Edinburgh) And (Token_GetType(K) = TOKEN_DOT) Then { rule 4.1, cont. }
  Begin
    K := ReadProgramToken(P,f);
    T2 := ReadTerm(f,P,K,glob,Cut);
    If Error Then Exit;
    T := NewList2(P,T,T2)
  End;
  If Error Then Exit;
  ReadTerm := T
End;

{ read the comma-separated list of expressions "a,b,c..." 
 (see pII+ p.44, "1.9.1 The syntactic level", rule 5 "termlist")
 - return Nil if an error occurs; 
 - in Edinburgh mode, the precedence level 999, 
 lower than the comma-as-an-operator's precedence, ensures that commas in the 
 termlist are interpreted as separators
 - in non-Edinburgh mode, I believe there is an issue with rule 5, which does
  not allows for dotted list; we fix this by defining a termlist as a list of 
  terms }
Function ReadTermList( f : StreamPtr; P : ProgPtr; Var K : TokenPtr; 
    glob : Boolean; Cut : Boolean ) : TermPtr;
Var
  y : TSyntax;
  T,T2 : TermPtr;
Begin
  ReadTermList := Nil;
  y := GetSyntax(P);
  If y = Edinburgh Then
    T := ReadOneExpr(f,P,K,999,glob,Cut) { rule 5 }
  Else
    T := ReadTerm(f,P,K,glob,Cut); { rule 5, modified }
  If Error Then Exit;
  T2 := Nil;
  If Token_GetType(K) = TOKEN_COMMA Then { rule 5, cont. }
  Begin
    K := ReadProgramToken(P,f);
    If Error Then Exit;
    T2 := ReadTermList(f,P,K,glob,Cut);
    If Error Then Exit
  End;
  ReadTermList := Func_NewAsTerm(T,T2)
End;

{ read a []-style list expression
 (see pII+ p.44, "1.9.1 The syntactic level", rule 8.1 and 8.1 "listexpr")
 listexpr = "expr" or "expr, list_expr" or "expr | expr" 
 if comma is true, the listexpr follows a comma, that is, is the remaining part 
 of a comma-separated list as in [a,b,c] }
Function ReadListExpr( f : StreamPtr; P : ProgPtr; Var K : TokenPtr; 
    comma, glob, Cut : Boolean ) : TermPtr;
Var
  y : TSyntax;
  T,T2 : TermPtr;
Begin
  ReadListExpr := Nil;
  y := GetSyntax(P);
  If y = Edinburgh Then
    T := ReadOneExpr(f,P,K,999,glob,Cut) { rule 8 }
  Else
    T := ReadTerm(f,P,K,glob,Cut); { rule 8, modified to allow infixed list }
  If Error Then Exit;
  If Token_GetType(K) = TOKEN_COMMA Then  { rule 8.1: "a,b" <=> a.b.nil }
  Begin
    K := ReadProgramToken(P,f);
    If Error Then Exit;
    T2 := ReadListExpr(f,P,K,True,glob,Cut); { remaining list }
    If Error Then Exit;
    T := NewList2(P,T,T2)
  End
  Else If Token_GetType(K) = TOKEN_PIPE Then  { rule 8.2: "a|b" <=> a.b }
  Begin
    K := ReadProgramToken(P,f);
    If Error Then Exit;
    If y = Edinburgh Then
      T2 := ReadOneExpr(f,P,K,999,glob,Cut) { rule 8 }
    Else
      T2 := ReadTerm(f,P,K,glob,Cut); { rule 8, modified to allow infixed list }
    If Error Then Exit;
    T := NewList2(P,T,T2)
  End
  Else { "a" <=> a.nil, see 6 p.45 }
  Begin
    T := NewList2(P,T,Nil)
  End;
  ReadListExpr := T
End;

{ read a []-style list 
 (see pII+ p.44, "1.9.1 The syntactic level", rule 7.6 "pterm")
}
Function ReadList( f : StreamPtr; P : ProgPtr; Var K : TokenPtr; 
    glob : Boolean; Cut : Boolean ) : TermPtr;
Var
  T : TermPtr;
Begin
  ReadList := Nil;
  VerifyToken(f,P,K,TOKEN_LEFT_BRA);
  If Token_GetType(K) = TOKEN_RIGHT_BRA Then { "[]": empty list }
  Begin
    T := NewEmptyList(P);
    K := ReadProgramToken(P,f)
  End
  Else
  Begin
    T := ReadListExpr(f,P,K,False,glob,Cut);
    If Error Then Exit;
    VerifyToken(f,P,K,TOKEN_RIGHT_BRA)
  End;
  ReadList := T
End;

{ read a pterm, possibly accepting a cut as a valid pterm; 
 (see pII+ p.44, "1.9.1 The syntactic level", rules 8.* "pterm")
 pterms are terms authorized at the first level of terms in the rule body }
Function ReadPTerm( f : StreamPtr; P : ProgPtr; Var K : TokenPtr; 
    glob : Boolean; Cut : Boolean ) : TermPtr;
Var
  y : TSyntax;
  T : TermPtr;
  L : TermPtr;
Begin
  ReadPTerm := Nil;
  y := GetSyntax(P);
  T := Nil;
  Case Token_GetType(K) Of
  TOKEN_INTEGER: { rule 7.7 }
    Begin
      If Not NormalizeConstant(K^.TK_STRI,ObjectTypeToConstType(CI)) Then
        SyntaxError('invalid integer constant');
      If Error Then Exit;
      T := EmitConst(P,Token_GetStr(K),CI,glob);
      K := ReadProgramToken(P,f)
    End;
  TOKEN_REAL: { rule 7.7 }
    Begin
      If Not NormalizeConstant(K^.TK_STRI,ObjectTypeToConstType(CR)) Then
        SyntaxError('invalid real constant');
      If Error Then Exit;
      T := EmitConst(P,Token_GetStr(K),CR,glob);
      K := ReadProgramToken(P,f)
    End;
  TOKEN_STRING: { rule 7.7 }
    Begin
      T := EmitConst(P,Token_GetStr(K),CS,glob);
      K := ReadProgramToken(P,f)
    End;
  TOKEN_CUT: { rule 7.7 }
    Begin
      If Cut Then 
      Begin
        T := EmitShortIdent(P,SPECIAL_IDENT_CUT,True);
        K := ReadProgramToken(P,f)
      End
      Else
        SyntaxError(TokenStr[TOKEN_CUT] + ' not allowed here')
    End;
  TOKEN_VARIABLE, TOKEN_IDENT: { rule 7.1: "a" or "a(b,c)" }
    Begin
      If Token_GetType(K) = TOKEN_VARIABLE Then
      Begin
        T := EmitVariable(P,Token_GetStr(K),Token_IsAnonymous(K),glob)
      End
      Else
      Begin
        T := EmitIdent(P,Token_GetStr(K),glob)
      End;
      K := ReadProgramToken(P,f);
      If Error Then Exit;
      If Token_GetType(K) = TOKEN_LEFT_PAR Then { predicate's arguments }
      Begin
        K := ReadProgramToken(P,f);
        If Error Then Exit;
        L := ReadTermList(f,P,K,glob,Cut);
        If Error Then Exit;
        VerifyToken(f,P,K,TOKEN_RIGHT_PAR);
        If Error Then Exit;
        T := Func_NewAsTerm(T,L)
      End
    End;
  TOKEN_LEFT_PAR: { rule 7.8: parenthesized term }
    Begin
      K := ReadProgramToken(P,f);
      If Error Then Exit;
      T := ReadTerm(f,P,K,glob,Cut);
      If Error Then Exit;
      VerifyToken(f,P,K,TOKEN_RIGHT_PAR);
      If Error Then Exit
    End;
  TOKEN_LEFT_CHE: { rules 7.3 and 7.4: tuple }
    Begin
      K := ReadProgramToken(P,f);
      If Error Then Exit;
      If Token_GetType(K) = TOKEN_RIGHT_CHE Then
      Begin
        { "<>" }
        K := ReadProgramToken(P,f);
        If Error Then Exit;
        If (y In [PrologIIp,Edinburgh]) And { "<>(t1,...tn)" }
            (Token_GetType(K) = TOKEN_LEFT_PAR) Then
        Begin
          K := ReadProgramToken(P,f);
          If Error Then Exit;
          L := ReadTermList(f,P,K,glob,Cut);
          If Error Then Exit;
          VerifyToken(f,P,K,TOKEN_RIGHT_PAR)
        End
        Else { "<>" only }
          L := NewEmptyTuple
      End
      Else 
        If y <> Edinburgh Then 
        Begin
          L := ReadTermList(f,P,K,glob,Cut);
          If Error Then Exit;
          VerifyToken(f,P,K,TOKEN_RIGHT_CHE)
        End
        Else
          SyntaxError('such tuples syntax is not allowed in Edinburgh mode');
      If Error Then Exit;
      T := L
    End;
  TOKEN_LEFT_BRA: { rule 7.6: PII+ []-style list }
    Begin
      If (y In [PrologIIp,Edinburgh]) Then
        T := ReadList(f,P,K,glob,Cut)
      Else
        SyntaxError('[]-style list not allowed in the current syntax');
    End;
  Else
    SyntaxError(Token_GetTypeAsShortString(K) + ' not allowed here')
  End;
  If Error Then Exit;
  ReadPTerm := T
End;

{ read an equations or inequation (PrologIIv1 only) }
Function ReadEquation( f : StreamPtr; P : ProgPtr; Var K : TokenPtr; 
    glob : Boolean ) : EqPtr;
Var
  E : EqPtr;
  T1, T2 : TermPtr;
  Code : EqType;
Begin
  ReadEquation := Nil;
  E := Nil;
  T1 := ReadTerm(f,P,K,glob,False);
  If Error Then Exit;
  Case Token_GetType(K) Of
  TOKEN_EQUAL:
    Begin
      Code := REL_EQUA;
      K := ReadProgramToken(P,f)
    End;
  TOKEN_DIFF:
    Begin
      Code := REL_INEQ;
      K := ReadProgramToken(P,f)
    End;
  Else
    SyntaxError('comparison symbol expected')
  End;
  If Error Then Exit;
  T2 := ReadTerm(f,P,K,glob,False);  { right term }
  If Error Then Exit;
  E := Eq_New(Code,T1,T2);
  ReadEquation := E
End;

{ read a system of equations or inequations (PrologIIv1 only) }
Function ReadSystem( f : StreamPtr; P : ProgPtr; Var K : TokenPtr; 
    glob : Boolean ) : EqPtr;
Var
  E, FirstE, PrevE : EqPtr;
  First : Boolean;
Begin
  ReadSystem := Nil;
  FirstE := Nil;
  PrevE := Nil;
  VerifyToken(f,P,K,TOKEN_LEFT_CUR);
  If Error Then Exit;
  First := True;
  Repeat
    If Not First Then
      VerifyToken(f,P,K,TOKEN_COMMA);
    If Error Then Exit;
    E := ReadEquation(f,P,K,glob);
    If Error Then Exit;
    If First Then
    Begin
      FirstE := E;
      First := False
    End
    Else
      Eqs_SetNext(PrevE,E);
    PrevE := E
  Until (Token_GetType(K) <> TOKEN_COMMA) Or Error;
  If Error Then Exit;
  VerifyToken(f,P,K,TOKEN_RIGHT_CUR);
  If Error Then Exit;
  ReadSystem := FirstE
End;

{----------------------------------------------------------------------------}
{ highest-level (rules and query) procedures and functions                   }
{----------------------------------------------------------------------------}

{ compile a system of equations and inequations (PrologIIc only) }
Function CompileSystem( f : StreamPtr; P : ProgPtr; Var K : TokenPtr; 
    glob : Boolean ) : EqPtr;
Begin
  PrepareExprParsing;
  CompileSystem := ReadSystem(f,P,K,glob);
  TerminateExprParsing
End;

{ compile a goal at rule or query level }
Function CompileRuleHead( f : StreamPtr; P : ProgPtr; Var K : TokenPtr; 
    glob : Boolean; Cut : Boolean ) : BTermPtr;
Var 
  T : TermPtr;
Begin
  CompileRuleHead := Nil;
  PrepareExprParsing;
  T := ReadPTerm(f,P,K,glob,Cut);
  If Error Then Exit;
  TerminateExprParsing;
  CompileRuleHead := BTerm_New(T)
End;

{ compile a goal at rule or query level; non-Edinburgh only }
Function CompileOneGoal( f : StreamPtr; P : ProgPtr; Var K : TokenPtr; 
  glob : Boolean; Cut : Boolean ) : BTermPtr;
Var 
  T : TermPtr;
Begin
  CompileOneGoal := Nil;
  PrepareExprParsing;
  T := ReadPTerm(f,P,K,glob,Cut);
  If Error Then Exit;
  TerminateExprParsing;
  CompileOneGoal := BTerm_New(T)
End;

{ compile a (possibly empty) sequence of goals, stopping at a token in 
 StopTokens; non-Edinburgh only }
Function CompileGoals( f : StreamPtr; P : ProgPtr; Var K : TokenPtr; 
    glob : Boolean; StopTokens : TTokenSet ) : BTermPtr;

  Function DoCompileGoals : BTermPtr;
  Var
    B : BTermPtr;
  Begin
    DoCompileGoals := Nil;
    B := Nil;
    If (Not (Token_GetType(K) In StopTokens)) And (Not Error) Then
    Begin
      B := CompileOneGoal(f,P,K,glob,True);
      If Error Then Exit;
      B^.BT_NEXT := DoCompileGoals
    End;
    DoCompileGoals := B
  End;

Begin
  CompileGoals := Nil;
  CompileGoals := DoCompileGoals
End;

{ set up local variable context to prepare compiling a new rule }
Procedure OpenLocalContextForRule( P : ProgPtr; R : RulePtr );
Begin
  P^.PP_DVAR := Nil
End;

{ parse a rule; 
 Note: the system cannot be reduced right away as the reduction may depends 
 on global assignments;
 Note: P-rule 3.1 on PII+ doc p44 implies that the rule head can be a term 
 (instead of a pterm), thus also a dotted list; this contradicts a statement 
 on p40: 
   "The term which is the first member of a rule (rule head) must be: - either 
   an identifier or - a tuple whose first argument is an identifier"
 actual tests on PII+ confirm that a rule head cannot be a list  
  }
Function ParseOneRule( f : StreamPtr; P : ProgPtr; Var K : TokenPtr ) : RulePtr;
Var 
  y : TSyntax;
  R : RulePtr;
  B : BTermPtr;
  T : TermPtr;
  StopTokens : TTokenSet;
Begin
  ParseOneRule := Nil;
  y := GetSyntax(P);
  R := Rule_New(y);
  OpenLocalContextForRule(P,R);
  PrepareExprParsing;

  If y = Edinburgh Then { rule 3.2 }
  Begin
    T := ReadTerm(f,P,K,True,True);
    If Error Then Exit;
    B := RuleExpToBTerms(P,T);
    Rule_SetHeadAndQueue(R,B);
    If Not Rule_HeadIsValid(R) Then
      SyntaxError('invalid rule head');
    If Error Then Exit
  End
  Else { rule 3.1 }
  Begin
    StopTokens := [Syntax[y].RuleEnd];
    If Syntax[y].AcceptSys Then
      StopTokens := StopTokens + [TOKEN_COMMA];
    B := CompileRuleHead(f,P,K,True,False);
    If Error Then Exit;
    Rule_SetHeadAndQueue(R,B);
    VerifyToken(f,P,K,TOKEN_ARROW);
    If Error Then Exit;
    BTerms_SetNext(B,CompileGoals(f,P,K,True,StopTokens));
    If Error Then Exit;
    If (Syntax[y].AcceptSys) And (Token_GetType(K) = TOKEN_COMMA) Then
    Begin
      K := ReadProgramToken(P,f);
      Rule_SetEqs(R,CompileSystem(f,P,K,True))
    End;
    If Error Then Exit
  End;

  TerminateExprParsing;
  VerifyToken(f,P,K,Syntax[y].RuleEnd);
  If Error Then Exit;
  ParseOneRule := R
End;

{ set up local variable context to prepare compiling a new query }
Procedure OpenLocalContextForQuery( P : ProgPtr; Q : QueryPtr );
Begin
  P^.PP_DVAR := Nil
End;

{ close this local variable context }
Procedure CloseLocalContextForQuery( P : ProgPtr; Q : QueryPtr );
Begin
  Query_SetDict(Q,P^.PP_DVAR)
End;

{----------------------------------------------------------------------------}
{ public procedures and functions                                            }
{----------------------------------------------------------------------------}

{ parse a query, including a system of equations and equations if any;
 - note that this system cannot be reduced right away, as its 
   solution (or lack thereof) may depend on global variables;
 - do *not* read the next token after the end-of-query mark; Indeed:
   1) when reading a goal from a file while echo mode is on, the answer must be
   displayed *before* reading the next token, otherwise the output will be 
   mangled; in that situation the caller must take care of reading the next
   token after clearing the goal
   2) when reading a goal typed at the prompt, the char following the 
   end-of-query mark is supposed to be returned when the goal is in_char(c) }
Function ParseOneQuery( f : StreamPtr; P : ProgPtr;  
    Var K : TokenPtr ) : QueryPtr;
Var
  Q : QueryPtr;
  y : TSyntax;
  B : BTermPtr;
  T : TermPtr;
  StopTokens : TTokenSet;
Begin
  ParseOneQuery := Nil;
  y := GetSyntax(P);
  Q := NewProgramQuery(P);
  OpenLocalContextForQuery(P,Q);
  PrepareExprParsing;

  If y = Edinburgh Then { rule 2.2 }
  Begin
    T := ReadOneExpr(f,P,K,1199,True,True);
    If Error Then Exit;
    B := CommaExpToBTerms(P,T);
    If B = Nil Then
      SyntaxError('query expected');
    If Error Then Exit;
    Query_SetTerms(Q,B)
  End
  Else { rule 2.1 }
  Begin
    StopTokens := [Syntax[y].PromptEnd,TOKEN_END_OF_INPUT];
    If Syntax[y].AcceptSys Then
      StopTokens := StopTokens + [TOKEN_LEFT_CUR,TOKEN_COMMA];
    Query_SetTerms(Q,CompileGoals(f,P,K,False,StopTokens));
    With Q^ Do
    Begin
      If Error Then Exit;
      If (Syntax[y].AcceptSys) And 
          (Token_GetType(K) In [TOKEN_LEFT_CUR,TOKEN_COMMA]) Then
      Begin
        If Query_GetTerms(Q) <> Nil Then
          VerifyToken(f,P,K,TOKEN_COMMA);
        Query_SetSys(Q,CompileSystem(f,P,K,False))
      End;
      If Error Then Exit
    End
  End;

  { verify end-of-query mark }
  If Token_GetType(K) <> Syntax[y].PromptEnd Then
    SyntaxError(TokenStr[Syntax[y].PromptEnd] + ' expected');

  TerminateExprParsing;
  CloseLocalContextForQuery(P,Q);
  ParseOneQuery := Q
End;

{ this high-level entry point starts with reading one token }

{ parse a term from an input stream; stop token chars stay in the 
 input buffer }
Function ParseOneTerm( f : StreamPtr; P : ProgPtr ) : TermPtr;
Var
  K : TokenPtr;
  line : TLineNum;
  col : TCharPos;
Begin
  ParseOneTerm := Nil;
  K := ReadProgramToken(P,f);
  If Error Then Exit;
  PrepareExprParsing;
  ParseOneTerm := ReadTerm(f,P,K,False,True);
  If Error Then Exit;
  TerminateExprParsing;
  { since K is now the token *following* the compiled term, we must unread it 
   (and all the spaces before) so that in_char will read the first char after 
   the term }
  Token_GetLocation(K,line,col);
  Stream_UngetChars(f,line,col)
End;


End.