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
  IStream,
  IStack,
  Memory,
  PObj,
  PObjOp,
  PObjStr,
  PObjTok,
  PObjEq,
  PObjTerm,
  PObjProg,
  Encoding,
  Tokenize;

Function ParseOneTerm( f : TIStreamPtr; P : ProgPtr ) : TermPtr;
Function ParseCommandLineQuery( P : ProgPtr ) : Boolean;
Procedure ParseRulesAndQueries( f : TIStreamPtr; P : ProgPtr; Q : QueryPtr; 
    RuleType : RuType );

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
    (RuleEnd:TOKEN_SEMICOLON;PromptEnd:TOKEN_SEMICOLON;AcceptSys:False),
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
Function ReadTerm( f : TIStreamPtr; P : ProgPtr; Var K : TokenPtr; 
    glob : Boolean; Cut : Boolean) : TermPtr; Forward;

Function ReadPTerm( f : TIStreamPtr; P : ProgPtr; Var K : TokenPtr; 
    glob : Boolean; Cut : Boolean) : TermPtr; Forward;

{----------------------------------------------------------------------------}
{ verify                                                                     }
{----------------------------------------------------------------------------}

{ raise an error if a token is not of a certain type; read the token 
 following the token to verify }
Procedure VerifyToken( f : TIStreamPtr; P : ProgPtr; Var K : TokenPtr; 
    typ : TTokenType );
Var
  y : TSyntax;
Begin
  y := GetSyntax(P);
  If TokenType(K) = typ Then
    K := ReadToken(f,y)
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

{ is there a placeholder at the top of the term stack, indicating that a term  
 is required to complete an expression (e.g. the second operand of a binary 
 operator) }
Function HasPlaceholder( TBottom : TStackLen ) : Boolean;
Begin
  HasPlaceholder := False;
  If TStackTop > TBottom Then
    If TStack[TStackTop] = Nil Then
      HasPlaceholder := True
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

{ push a placeholder in the term stack, indicating that a term is required  
 to complete an expression (e.g. the second operand of a binary operator) }
Procedure PushPlaceholder;
Begin
  PushExprTerm(Nil)
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
  CheckCondition(o^.OP_NPAR In [1,2],'ReduceTopExpr: unexpected arity');
  Case o^.OP_NPAR Of
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
  T := NewFunc2(P,o^.OP_FUNC,T1,T2,True);
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
        pre1 := OpStack[OpStackTop]^.OP_PRED;
        typ1 := OpStack[OpStackTop]^.OP_TYPE;
        pre2 := OpStack[OpStackTop-1]^.OP_PRED;
        { priorities allow reduction? }
        If (pre1 > pre2) Or (pre1 = pre2) And (typ1 In [fy,yf,yfx]) Then
        Begin
          PopExprOp(o,OBottom);
          Case o^.OP_NPAR Of
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
  y : TSyntax;
  o : OpPtr;
  oper : TString;
Begin
  y := GetSyntax(P);
  o := Nil;
  If TokenType(K) = TOKEN_IDENT Then
  Begin
    oper := StrGetFirstData(K^.TK_STRI);
    o := OpLookup(P^.PP_OPER,oper,'',OpTypes,0,MaxPred)
  End;
  NextOp := o
End;

{ read an expression of max precedence MaxPred, reducing the expression using 
 the term and operator stacks above Bottom only.
 (see pII+ p.44, "1.9.1 The syntactic level", rules 6.* "expr") 
 TODO: in PII+ eq(x,1 '<' 2 '<' 3) raises a syntax error
 https://www.swi-prolog.org/pldoc/doc_for?object=op/3 }
Function ReadExpr( f : TIStreamPtr; P : ProgPtr; Var K : TokenPtr; 
    TBottom, OBottom : TStackLen; MaxPred : TPrecedence; 
    glob : Boolean; Cut : Boolean) : TermPtr;
Var
  y : TSyntax;
  T : TermPtr;
  o : OpPtr;
  Found : Boolean; { case identified and treated }
  Done : Boolean; { are we done parsing the expression? }
  CT : ConstPtr Absolute T;
  s : StrPtr;
Begin
  y := GetSyntax(P);

  Done := False;

  Repeat
    Found := False;

    { rule 6.1: prefixed (unary) operator }
    o := NextOp(P,K,[fx,fy],MaxPred);
    If (o <> Nil) And (Not HasTerm(TBottom)) Then 
    Begin
      K := ReadToken(f,y);
      { simplify +/- integer:
       PII+ p.48: "The trees corresponding to the unary operators + and - are  
       evaluated when analyzed if their argument is an integer constant." }
      If (o^.OP_TYPE = fx) And ((o^.OP_OPER = '+') Or (o^.OP_OPER = '-')) 
          And (TokenType(K) = TOKEN_INTEGER) Then
      Begin
        T := ReadPTerm(f,P,K,glob,Cut); { read the integer constant }
        If o^.OP_OPER = '-' Then
        Begin
          s := NewStringFrom('-');
          StrConcat(s,ConstGetStr(CT));
          T := EmitConst(P,s,CI,True)
        End;
        PushExprTerm(T)
      End
      Else
      Begin  
        PushExprOp(o);
        PushPlaceholder
      End;
      Found := True
    End;

    { rule 6.3: infixed (binary) operator }
    If Not Found Then 
    Begin
      o := NextOp(P,K,[xfx,xfy,yfx],MaxPred);
      If (o <> Nil) And (HasTerm(TBottom)) Then
      Begin
        K := ReadToken(f,y);
        PushExprOp(o);
        PushPlaceholder;
        Found := True
      End
    End;

    { rule 6.2: postfixed (unary) operator (rare case, user defined) }
    If Not Found Then 
    Begin
      o := NextOp(P,K,[xf,yf],MaxPred);
      If (o <> Nil) And (HasTerm(TBottom)) Then
      Begin
        K := ReadToken(f,y);
        PushExprOp(o);
        Found := True
      End
    End;

    Done := Not Found And HasTerm(TBottom);

    { rules 6.5: pterm }
    If Not HasTerm(TBottom) Or HasPlaceholder(TBottom) Then
    Begin
      If HasPlaceholder(TBottom) Then
        PopExprTerm(T,TBottom);
      T := ReadTerm(f,P,K,glob,Cut);
      If Error Then Exit;
      PushExprTerm(T);
      Found := True
    End;

    { reduce the stack as much as possible }
    ReduceExprStack(P,TBottom,OBottom)

  Until Done;

  { finally reduce the stack to a single term: 
   1+2*3^4 => add(1,mul(2,^(3,4))) }
  ReadExpr := ReduceAllExpr(P,TBottom,OBottom) 
End;

{ read an expression of max precedence MaxPred, returning the tree of
 functions and arguments as a result }
Function ReadOneExpr( f : TIStreamPtr; P : ProgPtr; Var K : TokenPtr; 
    MaxPred : TPrecedence; glob : Boolean; Cut : Boolean) : TermPtr;
Var
  y : TSyntax;
Begin
  y := GetSyntax(P);
  If y = PrologII Then { expressions are part of the old PrologII syntax }
    ReadOneExpr := ReadTerm(f,P,K,glob,Cut)
  Else
    ReadOneExpr := ReadExpr(f,P,K,TStackTop,OpStackTop,MaxPred,glob,Cut)
End;

{----------------------------------------------------------------------------}
{ parser                                                                     }
{----------------------------------------------------------------------------}

{ read a term: pterm [. term]*; is right-associative
 (see pII+ p.44, "1.9.1 The syntactic level", rules 4.1 and 4.2 "term") }
Function ReadTerm( f : TIStreamPtr; P : ProgPtr; Var K : TokenPtr; 
    glob : Boolean; Cut : Boolean) : TermPtr;
Var
  y : TSyntax;
  T : TermPtr;
  T2 : TermPtr;
  WasCut : Boolean;
Begin
  ReadTerm := Nil;
  y := GetSyntax(P);
  WasCut := TokenType(K) = TOKEN_CUT;
  T := ReadPTerm(f,P,K,glob,Cut);
  If Error Then Exit;
  If (y <> Edinburgh) And (TokenType(K) = TOKEN_DOT) 
      And (Not WasCut) Then { rule 4.1 }
  Begin
    K := ReadToken(f,y);
    T2 := ReadTerm(f,P,K,glob,False);
    If Error Then Exit;
    T := NewList2(P,T,T2)
  End;
  If Error Then Exit;
  ReadTerm := T
End;

{ read the comma-separated list of expressions "a,b,c..." 
 (see pII+ p.44, "1.9.1 The syntactic level", rule 5 "termlist")
 return Nil if an error occurs }
Function ReadTermList( f : TIStreamPtr; P : ProgPtr; Var K : TokenPtr; 
    glob : Boolean ) : TermPtr;
Var
  y : TSyntax;
  T,T2 : TermPtr;
Begin
  ReadTermList := Nil;
  y := GetSyntax(P);
  T := ReadOneExpr(f,P,K,999,glob,False);
  If Error Then Exit;
  T2 := Nil;
  If TokenType(K) = TOKEN_COMMA Then
  Begin
    K := ReadToken(f,y);
    T2 := ReadTermList(f,P,K,glob);
    If Error Then Exit
  End;
  ReadTermList := NewF(T,T2)
End;

{ read a []-style list expression
 (see pII+ p.44, "1.9.1 The syntactic level", rule 8.1 and 8.1 "listexpr")
 listexpr = "expr" or "expr, list_expr" or "expr | expr" 
 if comma is true, the listexpr follows a comma, that is, is the remaining part 
 of a comma-separated list as in [a,b,c] }
Function ReadListExpr( f : TIStreamPtr; P : ProgPtr; Var K : TokenPtr; 
    comma, glob, Cut : Boolean ) : TermPtr;
Var
  y : TSyntax;
  T,T2 : TermPtr;
Begin
  ReadListExpr := Nil;
  y := GetSyntax(P);
  T := ReadOneExpr(f,P,K,999,glob,Cut); { TODO: cut allowed here?? }
  If Error Then Exit;
  If TokenType(K) = TOKEN_COMMA Then  { rule 8.1: "a,b" <=> a.b.nil }
  Begin
    K := ReadToken(f,y);
    If Error Then Exit;
    T2 := ReadListExpr(f,P,K,True,glob,Cut); { remaining list }
    If Error Then Exit;
    T := NewList2(P,T,T2)
  End
  Else If TokenType(K) = TOKEN_PIPE Then  { rule 8.2: "a|b" <=> a.b }
  Begin
    K := ReadToken(f,y);
    If Error Then Exit;
    T2 := ReadOneExpr(f,P,K,999,glob,Cut);
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
Function ReadList( f : TIStreamPtr; P : ProgPtr; Var K : TokenPtr; 
    glob,Cut : Boolean ) : TermPtr;
Var
  y : TSyntax;
  T : TermPtr;
Begin
  ReadList := Nil;
  y := GetSyntax(P);
  VerifyToken(f,P,K,TOKEN_LEFT_BRA);
  If TokenType(K) = TOKEN_RIGHT_BRA Then { "[]": empty list }
  Begin
    T := NewEmptyList(P);
    K := ReadToken(f,y)
  End
  Else
  Begin
    T := ReadListExpr(f,P,K,False,glob,Cut); { TODO: cut allowed here?? }
    If Error Then Exit;
    VerifyToken(f,P,K,TOKEN_RIGHT_BRA)
  End;
  ReadList := T
End;

{ read a pterm, possibly accepting a cut as a valid pterm; 
 (see pII+ p.44, "1.9.1 The syntactic level", rules 8.* "pterm")
 pterms are terms authorized at the first level of terms in the rule body }
Function ReadPTerm( f : TIStreamPtr; P : ProgPtr; Var K : TokenPtr; 
    glob : Boolean; Cut : Boolean ) : TermPtr;
Var
  y : TSyntax;
  T : TermPtr;
  L : TermPtr;
  V : VarPtr;
  TV : TermPtr Absolute V;
  I : IdPtr;
  TI : TermPtr Absolute I;
  MaxPred : TPrecedence;
Begin
  ReadPTerm := Nil;
  y := GetSyntax(P);
  T := Nil;
  Case TokenType(K) Of
  TOKEN_INTEGER: { rule 7.7 }
    Begin
      If Not NormalizeConstant(K^.TK_STRI,ObjectTypeToConstType(CI)) Then
        SyntaxError('invalid integer constant');
      If Error Then Exit;
      T := EmitConst(P,K^.TK_STRI,CI,glob);
      K := ReadToken(f,y)
    End;
  TOKEN_REAL: { rule 7.7 }
    Begin
      If Not NormalizeConstant(K^.TK_STRI,ObjectTypeToConstType(CR)) Then
        SyntaxError('invalid real constant');
      If Error Then Exit;
      T := EmitConst(P,K^.TK_STRI,CR,glob);
      K := ReadToken(f,y)
    End;
  TOKEN_STRING: { rule 7.7 }
    Begin
      T := EmitConst(P,K^.TK_STRI,CS,glob);
      K := ReadToken(f,y)
    End;
  TOKEN_CUT: { rule 7.7 }
    Begin
      If Cut Then 
      Begin
        T := EmitShortIdent(P,'!',True);
        K := ReadToken(f,y)
      End
      Else
        SyntaxError(TokenStr[TOKEN_CUT] + ' not allowed here')
    End;
  TOKEN_VARIABLE, TOKEN_IDENT: { rule 7.1: "a" or "a(b,c)" }
    Begin
      If TokenType(K) = TOKEN_VARIABLE Then
      Begin
        V := InstallVariable(P^.PP_DVAR,P^.PP_LVAR,K^.TK_STRI,glob);
        T := TV
      End
      Else
      Begin
        I := InstallIdentifier(P^.PP_DIDE,K^.TK_STRI,glob);
        T := TI
      End;
      K := ReadToken(f,y);
      If TokenType(K) = TOKEN_LEFT_PAR Then { predicate's arguments }
      Begin
        K := ReadToken(f,y);
        L := ReadTermList(f,P,K,glob);
        If Error Then Exit;
        VerifyToken(f,P,K,TOKEN_RIGHT_PAR);
        If Error Then Exit;
        T := NewF(T,L)
      End
    End;
  TOKEN_LEFT_PAR: { (modified) rule 7.8: parenthesized expression }
    Begin
      K := ReadToken(f,y);
      If y = Edinburgh Then
        MaxPred := 1200
      Else
        MaxPred := 1000;
      T := ReadOneExpr(f,P,K,MaxPred,glob,False);
      If Error Then Exit;
      VerifyToken(f,P,K,TOKEN_RIGHT_PAR);
      If Error Then Exit
    End;
  TOKEN_LEFT_CHE: { rules 7.3 and 7.4: tuple }
    Begin
      K := ReadToken(f,y);
      If TokenType(K) = TOKEN_RIGHT_CHE Then
      Begin
        { "<>" }
        K := ReadToken(f,y);
        If (y In [PrologIIp,Edinburgh]) And { "<>(t1,...tn)" }
            (TokenType(K) = TOKEN_LEFT_PAR) Then
        Begin
          K := ReadToken(f,y);
          L := ReadTermList(f,P,K,glob);
          If Error Then Exit;
          VerifyToken(f,P,K,TOKEN_RIGHT_PAR)
        End
        Else { "<>" only }
          L := NewEmptyTuple
      End
      Else 
        If y <> Edinburgh Then 
        Begin
          L := ReadTermList(f,P,K,glob);
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
    SyntaxError(TokenTypeAsString(K) + ' not allowed here')
  End;
  If Error Then Exit;
  ReadPTerm := T
End;

{ read an equations or inequation (PrologIIc only) }
Function ReadEquation( f : TIStreamPtr; P : ProgPtr; Var K : TokenPtr; 
    glob : Boolean ) : EqPtr;
Var
  y : TSyntax;
  E : EqPtr;
  T1, T2 : TermPtr;
  Code : EqType;
Begin
  ReadEquation := Nil;
  y := GetSyntax(P);
  E := Nil;
  T1 := ReadTerm(f,P,K,glob,False);
  If Error Then Exit;
  Case TokenType(K) Of
  TOKEN_EQUAL:
    Begin
      Code := REL_EQUA;
      K := ReadToken(f,y)
    End;
  TOKEN_LEFT_CHE:
    Begin
      K := ReadToken(f,y);
      VerifyToken(f,P,K,TOKEN_RIGHT_CHE);
      If Error Then Exit;
      Code := REL_INEQ
    End;
  Else
    SyntaxError('comparison symbol expected')
  End;
  If Error Then Exit;
  T2 := ReadTerm(f,P,K,glob,False);  { right term }
  If Error Then Exit;
  E := NewEquation(Code,T1,T2);
  ReadEquation := E
End;

{ read a system of equations or inequations (PrologIIc only) }
Function ReadSystem( f : TIStreamPtr; P : ProgPtr; Var K : TokenPtr; 
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
      PrevE^.EQ_NEXT := E;
    PrevE := E
  Until (TokenType(K) <> TOKEN_COMMA) Or Error;
  If Error Then Exit;
  VerifyToken(f,P,K,TOKEN_RIGHT_CUR);
  If Error Then Exit;
  ReadSystem := FirstE
End;

{----------------------------------------------------------------------------}
{ highest-level (rules and query) procedures and functions                   }
{----------------------------------------------------------------------------}

{ compile a system of equations and inequations (PrologIIc only) }
Function CompileSystem( f : TIStreamPtr; P : ProgPtr; Var K : TokenPtr; 
    glob : Boolean ) : EqPtr;
Begin
  PrepareExprParsing;
  CompileSystem := ReadSystem(f,P,K,glob);
  TerminateExprParsing
End;

{ compile a goal at rule or query level }
Function CompileRuleHead( f : TIStreamPtr; P : ProgPtr; Var K : TokenPtr; 
    glob : Boolean; Cut : Boolean ) : BTermPtr;
Var 
  T : TermPtr;
Begin
  CompileRuleHead := Nil;
  PrepareExprParsing;
  T := ReadPTerm(f,P,K,glob,Cut);
  If Error Then Exit;
  TerminateExprParsing;
  CompileRuleHead := NewBTerm(T)
End;

{ compile a goal at rule or query level }
Function CompileOneGoal( f : TIStreamPtr; P : ProgPtr; Var K : TokenPtr; 
  glob : Boolean; Cut : Boolean ) : BTermPtr;
Var 
  y : TSyntax;
  T : TermPtr;
Begin
  CompileOneGoal := Nil;
  y := GetSyntax(P);
  PrepareExprParsing;
  If y = Edinburgh Then { see rule 2.2: expr are allowed at top level }
    T := ReadOneExpr(f,P,K,1199,glob,Cut)
  Else
    T := ReadPTerm(f,P,K,glob,Cut);
  If Error Then Exit;
  TerminateExprParsing;
  CompileOneGoal := NewBTerm(T)
End;

{ compile a (possibly empty) sequence of goals, stopping at a token in 
 StopTokens; set HasCut to true if the queue contains a cut, false otherwise }
Function CompileGoals( f : TIStreamPtr; P : ProgPtr; Var K : TokenPtr; 
    glob : Boolean; StopTokens : TTokenSet; Var HasCut : Boolean ) : BTermPtr;
Var
  y : TSyntax;

  Function DoCompileGoals : BTermPtr;
  Var
    B : BTermPtr;
    Must : Boolean;
  Begin
    DoCompileGoals := Nil;
    B := Nil;
    If (Not (TokenType(K) In StopTokens)) And (Not Error) Then
    Begin
      B := CompileOneGoal(f,P,K,glob,True);
      If Error Then Exit;
      With B^ Do
      Begin
        HasCut := HasCut Or TermIsCut(BT_TERM);
        Must := (GetSyntax(P) = Edinburgh) And (TokenType(K) = TOKEN_COMMA);
        If Must Then
          K := ReadToken(f,y);
        BT_NEXT := DoCompileGoals;
        If Must And (BT_NEXT = Nil) Then
          SyntaxError('term expected after ' + TokenStr[TOKEN_COMMA])
      End
    End;
    DoCompileGoals := B
  End;

Begin
  y := GetSyntax(P);
  CompileGoals := Nil;
  HasCut := False;
  CompileGoals := DoCompileGoals
End;

{ set up local variable context to prepare compiling a new rule }
Procedure OpenLocalContextForRule( P : ProgPtr; R : RulePtr );
Begin
  P^.PP_LVAR := P^.PP_DVAR;
  R^.RU_LVAR := P^.PP_DVAR
End;

{ close this local variable context }
Procedure CloseLocalContextForRule( P : ProgPtr; R : RulePtr );
Begin
  R^.RU_FVAR := P^.PP_DVAR
End;

{ compile a rule; 
 Note: the system cannot be reduced right away as the reduction may depends 
 on global assignments;
 Note: P-rule 3.1 on PII+ doc p44 implies that the rule head can be a term 
 (instead of a pterm), thus also a dotted list; this contradicts a statement 
 on p40: 
   "The term which is the first member of a rule (rule head) must be: - either 
   an identifier or - a tuple whose first argument is an identifier"
 actual tests on PII+ confirm that a rule head cannot be a list  
  }
Function CompileOneRule( f : TIStreamPtr; P : ProgPtr; Var K : TokenPtr; 
    RuleType : RuType ) : RulePtr;
Var 
  y : TSyntax;
  R : RulePtr;
  B : BTermPtr;
  HasCut : Boolean;
  StopTokens : TTokenSet;
Begin
  CompileOneRule := Nil;
  y := GetSyntax(P);
  StopTokens := [Syntax[y].RuleEnd];
  If Syntax[y].AcceptSys Then
    StopTokens := StopTokens + [TOKEN_LEFT_CUR];
  R := NewRule(RuleType,y);
  OpenLocalContextForRule(P,R);
  With R^ Do
  Begin
    RU_SYST := Nil;
    B := CompileRuleHead(f,P,K,True,False);
    If Error Then Exit;
    RU_FBTR := B;
    If (y = Edinburgh) And (TokenType(K) = Syntax[y].RuleEnd) Then
      K := ReadToken(f,y)
    Else
    Begin
      VerifyToken(f,P,K,TOKEN_ARROW);
      If Error Then Exit;
      B^.BT_NEXT := CompileGoals(f,P,K,True,StopTokens,HasCut);
      RU_ACUT := HasCut;
      If y = Edinburgh Then 
        If B^.BT_NEXT = Nil Then
          SyntaxError('term expected after ' + TokenStr[TOKEN_ARROW]);
      If Error Then Exit;
      If (Syntax[y].AcceptSys) And (TokenType(K) = TOKEN_LEFT_CUR) Then
        RU_SYST := CompileSystem(f,P,K,True);
      If Error Then Exit;
      VerifyToken(f,P,K,Syntax[y].RuleEnd)
    End
  End;
  If Error Then Exit;
  CloseLocalContextForRule(P,R);
  CompileOneRule := R
End;

{ compile a sequence of rules, stopping at a token in StopTokens  }
Function CompileRules( f : TIStreamPtr; P : ProgPtr; Var K : TokenPtr; 
    StopTokens : TTokenSet; RuleType : RuType ) : RulePtr;
Var
  R1,R2 : RulePtr;
Begin
  CompileRules := Nil;
  If (TokenType(K) In StopTokens) Or (Error) Then
    Exit;
  R1 := CompileOneRule(f,P,K,RuleType);
  If Error Then Exit;
  R2 := CompileRules(f,P,K,StopTokens,RuleType);
  If Error Then Exit;
  If R2 = Nil Then
    R1^.RU_NEXT := Nil
  Else
    ChainRules(R1,R2);
  CompileRules := R1
End;

{ set up local variable context to prepare compiling a new query }
Procedure OpenLocalContextForQuery( P : ProgPtr; Q : QueryPtr );
Begin
  P^.PP_LVAR := P^.PP_DVAR;
  Q^.QU_LVAR := P^.PP_DVAR
End;

{ close this local variable context }
Procedure CloseLocalContextForQuery( P : ProgPtr; Q : QueryPtr );
Begin
  Q^.QU_FVAR := P^.PP_DVAR
End;

{ compile a query, including a system of equations and equations if any;
 - Ra is the last inserted rule
 - note that this system cannot be reduced right away, as its 
   solution (or lack thereof) may depend on global variables;
 - read the token after the end-of-query mark only if ReadNextToken is true;
   setting this parameter to false is useful when reading a goal typed at
   the prompt, as the char following the end-of-query mark is supposed to be 
   returned when the goal is in_char(c) }
Function CompileOneQuery( f : TIStreamPtr; P : ProgPtr; Ra : RulePtr; 
    Var K : TokenPtr; ReadNextToken : Boolean ) : QueryPtr;
Var
  Q : QueryPtr;
  HasCut : Boolean;
  y : TSyntax;
  StopTokens : TTokenSet;
Begin
  CompileOneQuery := Nil;
  y := GetSyntax(P);
  Q := NewQuery(P^.PP_LEVL,y);
  OpenLocalContextForQuery(P,Q);
  StopTokens := [Syntax[y].PromptEnd,TOKEN_END_OF_INPUT];
  If Syntax[y].AcceptSys Then
    StopTokens := StopTokens + [TOKEN_LEFT_CUR];
  With Q^ Do
  Begin
    QU_FBTR := CompileGoals(f,P,K,False,StopTokens,HasCut);
    If Error Then Exit;
    QU_ACUT := HasCut;
    If (Syntax[y].AcceptSys) And (TokenType(K) = TOKEN_LEFT_CUR) Then
      QU_SYST := CompileSystem(f,P,K,False);
    If Error Then Exit;
    { verify end-of-query mark }
    If TokenType(K) <> Syntax[y].PromptEnd Then
      SyntaxError(TokenStr[Syntax[y].PromptEnd] + ' expected');
    { read the next token only when requested; beware of infinite loops :) }
    If ReadNextToken Then
      K := ReadToken(f,y);
    If Error Then Exit
  End;
  CloseLocalContextForQuery(P,Q);
  { scope: range of rules that can be used to clear that query }
  CheckCondition((P^.PP_FRUL = Nil) And (Ra = Nil) 
      Or (P^.PP_FRUL <> Nil) And (Ra <> Nil), 
      'CompileOneQuery: broken candidate scope');
  SetFirstRuleInQueryScope(Q,FirstProgramRule(P));
  SetLastRuleInQueryScope(Q,Ra);
  CompileOneQuery := Q
End;

{ compile a sequence of queries; if ContTokens is not empty, each query 
  must start with a token in this set; the sequence ends with a token 
  in StopTokens; Ra is the last compiled rule }
Function CompileQueries( f : TIStreamPtr; P : ProgPtr; Ra : RulePtr; 
    Var K : TokenPtr; WithArrow : Boolean; 
    ContTokens, StopTokens : TTokenSet ) : QueryPtr;
Var
  Q : QueryPtr;
  More : Boolean;
Begin
  CompileQueries := Nil;
  More := ((ContTokens=[]) Or (TokenType(K) In ContTokens))
    And (Not (TokenType(K) In StopTokens)) And (Not Error);
  If Not More Then
    Exit;
  If WithArrow Then
    VerifyToken(f,P,K,TOKEN_ARROW);
  If Error Then Exit;
  Q := CompileOneQuery(f,P,Ra,K,True);
  If Error Then Exit;
  Q^.QU_NEXT := CompileQueries(f,P,Ra,K,WithArrow,ContTokens,StopTokens);
  If Error Then Exit;
  If Q^.QU_NEXT <> Nil Then
    Q^.QU_NEXT^.QU_PREV := Q;
  CompileQueries := Q
End;

{----------------------------------------------------------------------------}
{ public procedures and functions                                            }
{----------------------------------------------------------------------------}

{ these high-level entry points must start with reading one token }

{ parse a term from an input stream; stop token chars stay in the 
 input buffer }
Function ParseOneTerm( f : TIStreamPtr; P : ProgPtr ) : TermPtr;
Var
  y : TSyntax;
  K : TokenPtr;
  line : TLineNum;
  col : TCharPos;
Begin
  ParseOneTerm := Nil;
  y := GetSyntax(P);
  K := ReadToken(f,y);
  PrepareExprParsing;
  ParseOneTerm := ReadTerm(f,P,K,False,False);
  If Error Then Exit;
  TerminateExprParsing;
  { since K is now the token *following* the compiled term, we must unread it 
   (and all the spaces before) so that in_char will read the first char after 
   the term }
  GetTokenLocation(K,line,col);
  UngetCharsFromStream(f,line,col)
End;

{ compile a query typed by the user; return false if there was no query to
 compile, that is, the user just hit the return key; chars after the 
 end-of-query mark stay in the input buffer }  
Function ParseCommandLineQuery( P : ProgPtr ) : Boolean;
Var
  y : TSyntax;
  K : TokenPtr;
  Q : QueryPtr;
  f : TIStreamPtr;
Begin
  ParseCommandLineQuery := False;
  y := GetSyntax(P);
  f := GetInputConsole;
  K := ReadToken(f,y);
  If TokenType(K) <> TOKEN_END_OF_INPUT Then
  Begin
    Q := CompileOneQuery(f,P,LastProgramRule(P),K,False);
    If Error Then Exit;
    If Q <> Nil Then
      AppendQueries(P,Q);
    ParseCommandLineQuery := True
  End
End;

{ parse and append rules and queries to a program; 
 Q is the query (if any) that triggered the loading;
 top-level strings (that is, when a rule is expected) are taken to have the 
 value of a comment in all the supported Prolog syntaxes }
Procedure ParseRulesAndQueries( f : TIStreamPtr; P : ProgPtr; Q : QueryPtr; 
    RuleType : RuType );
Var
  Stop : Boolean;
  y : TSyntax;
  K : TokenPtr;
  StopTokens : TTokenSet;
  Ra : RulePtr; { where new rules must be inserted }
  Qn : QueryPtr; { new series of queries }
  Rn : RulePtr; { new series of rules }
Begin
  y := GetSyntax(P);
  K := ReadToken(f,y);
  If Q <> Nil Then
    Ra := LastRuleInQueryScope(Q) { insert/1 }
  Else
    Ra := LastProgramRule(P); { not an insert/1: autoexec or CLI }
  Stop := False;
  { common tokens ending a series of queries or rules }
  StopTokens := [TOKEN_END_OF_INPUT,TOKEN_STRING];
  If GetSyntax(P) = PrologII Then
    StopTokens := StopTokens + [TOKEN_SEMICOLON]; 
  Repeat
    Case TokenType(K) Of
    TOKEN_END_OF_INPUT:
      Stop := True;
    TOKEN_SEMICOLON:
      If GetSyntax(P) = PrologII Then { old Prolog II termination }
        Stop := True
      Else
        SyntaxError(TokenStr[TOKEN_SEMICOLON] + ' not expected here');
    TOKEN_STRING:
      K := ReadToken(f,y);
    TOKEN_ARROW: { a series of queries }
      Begin
        Qn := CompileQueries(f,P,Ra,K,True,[TOKEN_ARROW],StopTokens);
        If Qn <> Nil Then
          AppendQueries(P,Qn)
      End;
    Else { a series of rules }
      Begin
        Rn := CompileRules(f,P,K,StopTokens + [TOKEN_ARROW],RuleType);
        If Rn <> Nil Then
          Ra := InsertRulesA(P,Ra,Rn) { Ra is now the last compiled rule }
      End
    End
  Until Stop Or Error;
  If Error Then Exit;
  { machine state }
  P^.PP_UVAR := P^.PP_DVAR;
  P^.PP_UCON := P^.PP_DCON
End;

End.