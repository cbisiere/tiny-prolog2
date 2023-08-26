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
Function ReadTerm( P : ProgPtr; Var K : TokenPtr; glob : Boolean; 
    Cut : Boolean) : TermPtr; Forward;

Function ReadPTerm( P : ProgPtr; Var K : TokenPtr; glob : Boolean; 
    Cut : Boolean) : TermPtr; Forward;

{----------------------------------------------------------------------------}
{ verify                                                                     }
{----------------------------------------------------------------------------}

{ raise an error if a token is not of a certain type; read the token 
 following the token to verify }
Procedure VerifyToken( P : ProgPtr; Var K : TokenPtr; typ : TTokenType );
Var
  y : TSyntax;
Begin
  y := GetSyntax(P);
  If TokenType(K) = typ Then
    K := ReadToken(y)
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
  OStack : Array[1..MAX_EXPR_DEEP] Of TPFRec;
  TStackTop : TStackLen;
  OStackTop : TStackLen;

{ reset the expression stack; must be called before any parsing phase (command
 line, file, in/1) ); if the previous parsing phase ended with an error, some
 garbage might have been left in this stack }
Procedure PrepareExprParsing;
Begin
  TStackTop := 0;
  OStackTop := 0
End;

{ check the stack is empty }
Procedure TerminateExprParsing;
Begin
  CheckCondition(OStackTop = 0,'op stack not empty')
End;

{ push an op }
Procedure PushExprOp( Op : TPFRec );
Begin
  If OStackTop > MAX_EXPR_DEEP - 1 Then
    SyntaxError('expression too complex');
  If Error Then Exit;
  OStackTop := OStackTop + 1;
  OStack[OStackTop] := Op
End;

{ pop an op }
Procedure PopExprOp( Var Op : TPFRec );
Begin
  CheckCondition(OStackTop > 0,'PopExprOp: op stack is empty');
  Op := OStack[OStackTop];
  OStackTop := OStackTop - 1
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
Procedure PopExprTerm( Var T : TermPtr );
Begin
  CheckCondition(TStackTop > 0,'PopExprTerm: term stack is empty');
  T := TStack[TStackTop];
  TStackTop := TStackTop - 1
End;

{ replace the top operator and associated operands terms with a term }
Procedure ReduceTopExpr( P : ProgPtr );
Var
  Op : TPFRec;
  T,T1,T2 : TermPtr;
Begin
  PopExprOp(Op);
  CheckCondition(Op.N in [1,2],'ReduceTopExpr: unexpected arity');
  Case Op.N Of
  1:
    Begin
      PopExprTerm(T1);
      T2 := Nil
    End;
  2:
    Begin
      PopExprTerm(T2);
      PopExprTerm(T1)
    End;
  End;
  T := NewFunc(P,Op.S,T1,T2,True);
  PushExprTerm(T)
End;

{ reduce all the expressions remaining in the stack and return the remaining 
 term }
Function ReduceAllExpr( P : ProgPtr; Bottom : TStackLen ) : TermPtr;
Var
  T : TermPtr;
Begin
  ReduceAllExpr := Nil;
  While OStackTop > Bottom Do
    ReduceTopExpr(P);
  PopExprTerm(T); { final term }
  ReduceAllExpr := T
End;

{ push an expr: binary ("op right-term") or unary ("op Nil"); reduce the top
 expression when possible }
Procedure PushExpr( P : ProgPtr; Bottom : TStackLen; Op : TPFRec; T : TermPtr );
Begin
  If OStackTop > Bottom Then
    If Op.P >= OStack[OStackTop].P Then { new op has lower or equal priority }
      ReduceTopExpr(P);
  PushExprOp(Op);
  If T <> Nil Then
    PushExprTerm(T)
End;

{ read an expression: [uop] term [bop term]*
 TODO: "The trees corresponding to the unary operators + and - are evaluated 
 when analyzed if their argument is an integer constant." (PII+ p48) 
 TODO: handle xfx; in PII+ eq(x,1 '<' 2 '<' 3) raises a syntax error
 To better understand this xfx vs yfx, see the comments there:
 https://www.swi-prolog.org/pldoc/doc_for?object=op/3 }
Procedure ReadExpr( P : ProgPtr; Var K : TokenPtr; Bottom : TStackLen; 
    glob : Boolean; Cut : Boolean);
Var
  y : TSyntax;
  T : TermPtr;
  f : TString;
  Op : TPFRec;
  HasUnaryOp : Boolean;
  Ok, Stop : Boolean;
Begin
  y := GetSyntax(P);
  { optional unitary-op }
  HasUnaryOp := TokenType(K) In [TOKEN_MINUS, TOKEN_PLUS];
  If HasUnaryOp Then { fx }
  Begin
    Case TokenType(K) Of
    TOKEN_MINUS:
      f := 'minus';
    TOKEN_PLUS:
      f := 'plus';
    End;
    Ok := LookupPF(f,Op);
    CheckCondition(Ok, 'ReadOneExpr: unary op not found');
    CheckCondition(Op.N=1, 'ReadOneExpr: not unary');
    K := ReadToken(y)
  End;
  { first pterm }
  T := ReadTerm(P,K,glob,Cut);
  If Error Then Exit;
  PushExprTerm(T);
  { push the unary op if any, offering a chance to reduce the op stack; must
   be done after pushing the term }
  If HasUnaryOp Then
    PushExpr(P,Bottom,Op,Nil);
  Stop := False;
  { optional series of "binary-op pterm" }
  While Not Stop Do
  Begin
    Case TokenType(K) Of
    TOKEN_IDENT:
      Begin
        f := StrGetFirstData(K^.TK_STRI);
        Case y Of
        PrologIIp:
          If f = '''<''' Then
            f := 'inf'
          Else
            Stop := True;
        Edinburgh:
          If f <> 'is' Then
            Stop := True;
        Else
          Stop := True
        End
      End;
    TOKEN_LEFT_CHE:
      If y = Edinburgh Then
        f := 'inf'
      Else
        Stop := True;
    TOKEN_MINUS:
      f := 'sub';
    TOKEN_PLUS:
      f := 'add';
    TOKEN_TIMES:
      f := 'mul';
    TOKEN_DIVIDE:
      f := 'div';
    Else
      Stop := True
    End;
    If Not Stop Then
    Begin
      Ok := LookupPF(f,Op);
      CheckCondition(Ok,'ReadOneExpr: binary op not found');
      CheckCondition(Op.N=2, 'ReadOneExpr: not binary');
      K := ReadToken(y);
      T := ReadTerm(P,K,glob,Cut);
      If Error Then Exit;
      PushExpr(P,Bottom,Op,T)
    End
  End
End;

{ read one expression }
Function ReadOneExpr( P : ProgPtr; Var K : TokenPtr; glob : Boolean; 
    Cut : Boolean) : TermPtr;
Var
  Bottom : TStackLen;
Begin
  ReadOneExpr := Nil;
  Bottom := OStackTop;
  ReadExpr(P,K,Bottom,glob,Cut);
  If Error Then Exit;
  ReadOneExpr := ReduceAllExpr(P,Bottom)
End;

{----------------------------------------------------------------------------}
{ parser                                                                     }
{----------------------------------------------------------------------------}

{ read a term: pterm [. term]*; is right-associative
 Note: dotted lists seem to be allowed in PII+ Edinburgh, see 
 PII+ doc, pdf p. 45; we do not allowed it for now, as it is tricky to 
 distinguish this dot from an end-of-rule dot }
{ TODO: exclude CUT? }
Function ReadTerm; (*( P : ProgPtr; Var K : TokenPtr; glob : Boolean; 
    Cut : Boolean) : TermPtr; *)
Var
  y : TSyntax;
  T : TermPtr;
  T2 : TermPtr;
Begin
  ReadTerm := Nil;
  y := GetSyntax(P);
  T := ReadPTerm(P,K,glob,Cut);
  If Error Then Exit;
  If (y <> Edinburgh) And (TokenType(K) = TOKEN_DOT) Then
  Begin
    K := ReadToken(y);
    T2 := ReadTerm(P,K,glob,Cut);
    If Error Then Exit;
    T := EncodeDot(P,T,T2)
  End;
  If Error Then Exit;
  ReadTerm := T
End;

{ read the comma-separated list of expressions "a,b,c..."; return Nil if an 
 error occurs }
Function ReadExprList( P : ProgPtr; Var K : TokenPtr; 
    glob : Boolean ) : TermPtr;
Var
  y : TSyntax;
  T,T2 : TermPtr;
Begin
  ReadExprList := Nil;
  y := GetSyntax(P);
  T := ReadOneExpr(P,K,glob,False);
  If Error Then Exit;
  T2 := Nil;
  If TokenType(K) = TOKEN_COMMA Then
  Begin
    K := ReadToken(y);
    T2 := ReadExprList(P,K,glob);
    If Error Then Exit
  End;
  ReadExprList := NewTuple(T,T2)
End;

{ read a []-style list expression:
 listexpr = "expr" or "expr, list_expr" or "expr | expr" 
 if comma is true, the listexpr follows a comma, that is, is the remaining part 
 of a comma-separated list as in [a,b,c] }
Function ReadListExpr( P : ProgPtr; Var K : TokenPtr; comma, glob, Cut : Boolean ) : TermPtr;
Var
  y : TSyntax;
  T,T2 : TermPtr;
Begin
  ReadListExpr := Nil;
  y := GetSyntax(P);
  T := ReadOneExpr(P,K,glob,Cut); { TODO: cut allowed here?? }
  If Error Then Exit;
  If TokenType(K) = TOKEN_COMMA Then  { "a,b" <=> a.b.nil }
  Begin
    K := ReadToken(y);
    If Error Then Exit;
    T2 := ReadListExpr(P,K,True,glob,Cut); { remaining list }
    If Error Then Exit;
    T := EncodeDot(P,T,T2)
  End
  Else If TokenType(K) = TOKEN_PIPE Then  { "a|b" <=> a.b }
  Begin
    K := ReadToken(y);
    If Error Then Exit;
    T2 := ReadOneExpr(P,K,glob,Cut);
    If Error Then Exit;
    T := EncodeDot(P,T,T2)
  End
  Else If comma Then { "b" as the end of "a,b" generates a.b.nil }
  Begin
    T := EncodeDot(P,T,Nil)
  End;
  ReadListExpr := T
End;

{ read a []-style list }
Function ReadList( P : ProgPtr; Var K : TokenPtr; glob,Cut : Boolean ) : TermPtr;
Var
  y : TSyntax;
  T : TermPtr;
Begin
  ReadList := Nil;
  y := GetSyntax(P);
  VerifyToken(P,K,TOKEN_LEFT_BRA);
  If TokenType(K) = TOKEN_RIGHT_BRA Then { "[]": empty list }
  Begin
    T := EmitIdent(P,'nil',True);
    K := ReadToken(y)
  End
  Else
  Begin
    T := ReadListExpr(P,K,False,glob,Cut); { TODO: cut allowed here?? }
    If Error Then Exit;
    VerifyToken(P,K,TOKEN_RIGHT_BRA)
  End;
  ReadList := T
End;

{ read a pterm, possibly accepting a cut as a valid pterm; pterms are terms 
 authorized at the first level of terms in the rule body }
Function ReadPTerm; (* ( P : ProgPtr; Var K : TokenPtr; glob : Boolean; 
    Cut : Boolean ) : TermPtr *)
Var
  y : TSyntax;
  T : TermPtr;
  F : TermPtr;
  V : VarPtr;
  TV : TermPtr Absolute V;
  I : IdPtr;
  TI : TermPtr Absolute I;
Begin
  ReadPTerm := Nil;
  y := GetSyntax(P);
  T := Nil;
  Case TokenType(K) Of
  TOKEN_INTEGER:
    Begin
      If Not NormalizeConstant(K^.TK_STRI,ObjectTypeToConstType(CI)) Then
        SyntaxError('invalid integer constant');
      If Error Then Exit;
      T := EmitConst(P,K^.TK_STRI,CI,glob);
      K := ReadToken(y)
    End;
  TOKEN_REAL:
    Begin
      If Not NormalizeConstant(K^.TK_STRI,ObjectTypeToConstType(CR)) Then
        SyntaxError('invalid real constant');
      If Error Then Exit;
      T := EmitConst(P,K^.TK_STRI,CR,glob);
      K := ReadToken(y)
    End;
  TOKEN_STRING:
    Begin
      T := EmitConst(P,K^.TK_STRI,CS,glob);
      K := ReadToken(y)
    End;
  TOKEN_CUT:
    Begin
      If Cut Then 
      Begin
        T := EmitIdent(P,'!',True);
        K := ReadToken(y)
      End
      Else
        SyntaxError(TokenStr[TOKEN_CUT] + ' not allowed here')
    End;
  TOKEN_VARIABLE:
    Begin
      V := InstallVariable(P^.PP_DVAR,P^.PP_LVAR,K^.TK_STRI,glob);
      T := TV;
      K := ReadToken(y)
    End;
  TOKEN_IDENT: { "a" or "a(b,c)" }
    Begin
      I := InstallIdentifier(P^.PP_DIDE,K^.TK_STRI,glob);
      T := TI;
      K := ReadToken(y);
      If TokenType(K) = TOKEN_LEFT_PAR Then { predicate's arguments }
      Begin
        K := ReadToken(y);
        F := ReadExprList(P,K,glob);
        If Error Then Exit;
        VerifyToken(P,K,TOKEN_RIGHT_PAR);
        If Error Then Exit;
        T := NewTuple(TI,F)
      End
    End;
  TOKEN_LEFT_PAR: { parenthesized expression }
    Begin
      K := ReadToken(y);
      T := ReadOneExpr(P,K,glob,False);
      If Error Then Exit;
      VerifyToken(P,K,TOKEN_RIGHT_PAR);
      If Error Then Exit
    End;
  TOKEN_LEFT_CHE: { tuple }
    Begin
      K := ReadToken(y);
      If TokenType(K) = TOKEN_RIGHT_CHE Then
      Begin
        { "<>" }
        K := ReadToken(y);
        If (y In [PrologIIp,Edinburgh]) And { "<>(t1,...tn)" }
            (TokenType(K) = TOKEN_LEFT_PAR) Then
        Begin
          K := ReadToken(y);
          F := ReadExprList(P,K,glob);
          If Error Then Exit;
          VerifyToken(P,K,TOKEN_RIGHT_PAR)
        End
        Else { "<>" only }
          F := NewEmptyTuple
      End
      Else 
        If y <> Edinburgh Then 
        Begin
          F := ReadExprList(P,K,glob);
          If Error Then Exit;
          VerifyToken(P,K,TOKEN_RIGHT_CHE)
        End
        Else
          SyntaxError('such tuples syntax is not allowed in Edinburgh mode');
      If Error Then Exit;
      T := F
    End;
  TOKEN_LEFT_BRA: { PII+ []-style list }
    Begin
      If (y In [PrologIIp,Edinburgh]) Then
        T := ReadList(P,K,glob,Cut)
      Else
        SyntaxError('[]-style list not allowed in the current syntax');
    End;
  Else
    SyntaxError(TokenTypeAsString(K) + ' not allowed here')
  End;
  If Error Then Exit;
  ReadPTerm := T
End;

{ read an equations or inequation }
Function ReadEquation( P : ProgPtr; Var K : TokenPtr; glob : Boolean ) : EqPtr;
Var
  y : TSyntax;
  E : EqPtr;
  T1, T2 : TermPtr;
  Code : EqType;
Begin
  ReadEquation := Nil;
  y := GetSyntax(P);
  E := Nil;
  T1 := ReadTerm(P,K,glob,False);
  If Error Then Exit;
  Case TokenType(K) Of
  TOKEN_EQUAL:
    Begin
      Code := REL_EQUA;
      K := ReadToken(y)
    End;
  TOKEN_LEFT_CHE:
    Begin
      K := ReadToken(y);
      VerifyToken(P,K,TOKEN_RIGHT_CHE);
      If Error Then Exit;
      Code := REL_INEQ
    End;
  Else
    SyntaxError('comparison symbol expected')
  End;
  If Error Then Exit;
  T2 := ReadTerm(P,K,glob,False);  { right term }
  If Error Then Exit;
  E := NewEquation(Code,T1,T2);
  ReadEquation := E
End;

{ read a system of equations or inequations }
Function ReadSystem( P : ProgPtr; Var K : TokenPtr; glob : Boolean ) : EqPtr;
Var
  E, FirstE, PrevE : EqPtr;
  First : Boolean;
Begin
  ReadSystem := Nil;
  FirstE := Nil;
  PrevE := Nil;
  VerifyToken(P,K,TOKEN_LEFT_CUR);
  If Error Then Exit;
  First := True;
  Repeat
    If Not First Then
      VerifyToken(P,K,TOKEN_COMMA);
    If Error Then Exit;
    E := ReadEquation(P,K,glob);
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
  VerifyToken(P,K,TOKEN_RIGHT_CUR);
  If Error Then Exit;
  ReadSystem := FirstE
End;

{----------------------------------------------------------------------------}
{ highest-level (rules and query) procedures and functions                   }
{----------------------------------------------------------------------------}

{ compile a system of equations and inequations }
Function CompileSystem( P : ProgPtr; Var K : TokenPtr; glob : Boolean ) : EqPtr;
Begin
  PrepareExprParsing;
  CompileSystem := ReadSystem(P,K,glob);
  TerminateExprParsing
End;

{ compile a goal at rule or query level }
Function CompileRuleHead( P : ProgPtr; Var K : TokenPtr; 
  glob : Boolean; Cut : Boolean ) : BTermPtr;
Var 
  B : BTermPtr;
Begin
  CompileRuleHead := Nil;
  B := NewBTerm;
  With B^ Do
  Begin
    PrepareExprParsing;
    BT_TERM := ReadPTerm(P,K,glob,Cut);
    If Error Then Exit;
    TerminateExprParsing;
    BT_ACCE := AccessIdentifier(BT_TERM)
  End;
  CompileRuleHead := B
End;

{ compile a goal at rule or query level }
Function CompileOneGoal( P : ProgPtr; Var K : TokenPtr; 
  glob : Boolean; Cut : Boolean ) : BTermPtr;
Var 
  y : TSyntax;
  B : BTermPtr;
  T : TermPtr;
Begin
  CompileOneGoal := Nil;
  y := GetSyntax(P);
  B := NewBTerm;
  With B^ Do
  Begin
    PrepareExprParsing;
    If y = Edinburgh Then { expr are allowed at top level }
      T := ReadOneExpr(P,K,glob,Cut)
    Else
      T := ReadPTerm(P,K,glob,Cut);
    If Error Then Exit;
    TerminateExprParsing;
    BT_TERM := T;
    BT_ACCE := AccessIdentifier(BT_TERM)
  End;
  CompileOneGoal := B
End;

{ compile a (possibly empty) sequence of goals, stopping at a token in 
 StopTokens; set HasCut to true if the queue contains a cut, false otherwise }
Function CompileGoals( P : ProgPtr; Var K : TokenPtr; 
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
      B := CompileOneGoal(P,K,glob,True);
      If Error Then Exit;
      With B^ Do
      Begin
        HasCut := HasCut Or TermIsCut(BT_TERM);
        Must := (GetSyntax(P) = Edinburgh) And (TokenType(K) = TOKEN_COMMA);
        If Must Then
          K := ReadToken(y);
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
Procedure CompileOneRule( P : ProgPtr; Var K : TokenPtr; RuleType : RuType );
Var 
  y : TSyntax;
  R : RulePtr;
  B : BTermPtr;
  HasCut : Boolean;
  StopTokens : TTokenSet;
Begin
  y := GetSyntax(P);
  StopTokens := [Syntax[y].RuleEnd];
  If Syntax[y].AcceptSys Then
    StopTokens := StopTokens + [TOKEN_LEFT_CUR];
  R := NewRule(RuleType);
  OpenLocalContextForRule(P,R);
  With R^ Do
  Begin
    RU_SYST := Nil;
    B := CompileRuleHead(P,K,True,False);
    If Error Then Exit;
    RU_FBTR := B;
    If (y = Edinburgh) And (TokenType(K) = Syntax[y].RuleEnd) Then
      K := ReadToken(y)
    Else
    Begin
      VerifyToken(P,K,TOKEN_ARROW);
      If Error Then Exit;
      B^.BT_NEXT := CompileGoals(P,K,True,StopTokens,HasCut);
      RU_ACUT := HasCut;
      If y = Edinburgh Then 
        If B^.BT_NEXT = Nil Then
          SyntaxError('term expected after ' + TokenStr[TOKEN_ARROW]);
      If Error Then Exit;
      If (Syntax[y].AcceptSys) And (TokenType(K) = TOKEN_LEFT_CUR) Then
        RU_SYST := CompileSystem(P,K,True);
      If Error Then Exit;
      VerifyToken(P,K,Syntax[y].RuleEnd)
    End
  End;
  If Error Then Exit;
  CloseLocalContextForRule(P,R);
  AppendOneRule(P,R)
End;

{ compile a sequence of rules, stopping at a token in StopTokens  }
Procedure CompileRules( P : ProgPtr; Var K : TokenPtr; 
    StopTokens : TTokenSet; RuleType : RuType );
Begin
  While (Not (TokenType(K) In StopTokens)) And (Not Error) Do
    CompileOneRule(P,K,RuleType);
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
 - note that this system cannot be reduced right away, as its 
   solution (or lack thereof) may depend on global variables;
 - read the token after the end-of-query mark only if ReadNextToken is true;
   setting this parameter to false is useful when reading a goal typed at
   the prompt, as the char following the end-of-query mark is supposed to be 
   returned when the goal is in_char(c) }
Procedure CompileOneQuery( P : ProgPtr; Var K : TokenPtr; 
    ReadNextToken : Boolean );
Var
  Q : QueryPtr;
  HasCut : Boolean;
  y : TSyntax;
  StopTokens : TTokenSet;
Begin
  Q := NewQuery(P^.PP_LEVL);
  OpenLocalContextForQuery(P,Q);
  y := GetSyntax(P);
  StopTokens := [Syntax[y].PromptEnd,TOKEN_END_OF_INPUT];
  If Syntax[y].AcceptSys Then
    StopTokens := StopTokens + [TOKEN_LEFT_CUR];
  With Q^ Do
  Begin
    QU_FBTR := CompileGoals(P,K,False,StopTokens,HasCut);
    If Error Then Exit;
    QU_ACUT := HasCut;
    If (Syntax[y].AcceptSys) And (TokenType(K) = TOKEN_LEFT_CUR) Then
      QU_SYST := CompileSystem(P,K,False);
    If Error Then Exit;
    { verify end-of-query mark }
    If TokenType(K) <> Syntax[y].PromptEnd Then
      SyntaxError(TokenStr[Syntax[y].PromptEnd] + ' expected');
    { read the next token only when requested; beware of infinite loops :) }
    If ReadNextToken Then
      K := ReadToken(y);
    If Error Then Exit
  End;
  CloseLocalContextForQuery(P,Q);
  UpdateQueryScope(P,Q);
  AppendOneQuery(P,Q)
End;

{ compile a sequence of queries; if ContTokens is not empty, each query 
  must start with a token in this set; the sequence ends with a token 
  in StopTokens;  }
Procedure CompileQueries( P : ProgPtr; Var K : TokenPtr; WithArrow : Boolean;
  ContTokens, StopTokens : TTokenSet );
Var
  More : Boolean;
Begin
  Repeat
    More := ((ContTokens=[]) Or (TokenType(K) In ContTokens))
      And (Not (TokenType(K) In StopTokens)) And (Not Error);
    If More Then
    Begin
      If WithArrow Then
        VerifyToken(P,K,TOKEN_ARROW);
      If Error Then Exit;
      CompileOneQuery(P,K,True);
      If Error Then Exit
    End
  Until Not More Or Error
End;

{----------------------------------------------------------------------------}
{ public procedures and functions                                            }
{----------------------------------------------------------------------------}

{ these high-level entry points must start with reading one token }

{ parse a term from the current input stream; stop token chars stay in the 
 input buffer }
Function ParseOneTerm( P : ProgPtr ) : TermPtr;
Var
  y : TSyntax;
  K : TokenPtr;
  line : TLineNum;
  col : TCharPos;
Begin
  ParseOneTerm := Nil;
  y := GetSyntax(P);
  K := ReadToken(y);
  PrepareExprParsing;
  ParseOneTerm := ReadTerm(P,K,False,False);
  If Error Then Exit;
  TerminateExprParsing;
  { since K is now the token *following* the compiled term, we must unread it 
   (and all the spaces before) so that in_char will read the first char after 
   the term }
  GetTokenLocation(K,line,col);
  UngetChars(line,col)
End;

{ compile a query typed by the user; return false if there was no query to
 compile, that is, the user just hit the return key; chars after the 
 end-of-query mark stay in the input buffer }  
Function ParseCommandLineQuery( P : ProgPtr ) : Boolean;
Var
  y : TSyntax;
  K : TokenPtr;
Begin
  ParseCommandLineQuery := False;
  y := GetSyntax(P);
  K := ReadToken(y);
  If TokenType(K) <> TOKEN_END_OF_INPUT Then
  Begin
    CompileOneQuery(P,K,False);
    If Error Then Exit;
    ParseCommandLineQuery := True
  End
End;

{ parse and append rules and queries to a program; top-level strings (that is, 
 when a rule is expected) are taken to have the value of a comment in all the 
 supported Prolog syntaxes }
Procedure ParseRulesAndQueries( P : ProgPtr; RuleType : RuType );
Var
  Stop : Boolean;
  y : TSyntax;
  K : TokenPtr;
  StopTokens : TTokenSet;
Begin
  y := GetSyntax(P);
  K := ReadToken(y);
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
      K := ReadToken(y);
    TOKEN_ARROW:
      CompileQueries(P,K,True,[TOKEN_ARROW],StopTokens);
    Else { rules }
      CompileRules(P,K,StopTokens + [TOKEN_ARROW],RuleType)
    End
  Until Stop Or Error;
  If Error Then Exit;
  { machine state }
  P^.PP_UVAR := P^.PP_DVAR;
  P^.PP_UCON := P^.PP_DCON
End;
