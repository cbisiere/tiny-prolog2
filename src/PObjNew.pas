{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : POjbNew.pas                                                }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{           P R O L O G   O B J E C T S :   N E W  /  F R E E                }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ allocate memory on the heap for an object of type t }
Function PObjNew; (*( t : TypePrologObj ) : TPObjPtr;*)
Var
  p : TPObjPtr;
  PR_p : ProgPtr Absolute p;
  RU_p : RulePtr Absolute p; 
  QU_p : QueryPtr Absolute p; 
  SY_p : SysPtr Absolute p;
  EQ_p : EqPtr Absolute p; 
  BT_p : BTermPtr Absolute p; 
  CO_p : ConstPtr Absolute p; 
  FU_p : FuncPtr Absolute p; 
  VA_p : VarPtr Absolute p; 
  ID_p : IdPtr Absolute p; 
  DE_p : DictPtr Absolute p; 
  HE_p : HeadPtr Absolute p; 
  ST_p : StrPtr Absolute p; 
  SD_p : StrDataPtr Absolute p; 
  RE_p : RestorePtr Absolute p;
  OP_p : OpPtr Absolute p;
  TK_p : TokenPtr Absolute p;
Begin
  CheckCondition((t <> CS) And (t <> CI) And (t <> CR),
      'type not meant for direct allocation');
  Case t Of
    PR: New(PR_p);
    RU: New(RU_p);
    QU: New(QU_p);
    SY: New(SY_p);
    EQ: New(EQ_p);
    BT: New(BT_p);
    CO: New(CO_p);
    FU: New(FU_p);
    VA: New(VA_p);
    ID: New(ID_p);
    DE: New(DE_p);
    HE: New(HE_p);
    ST: New(ST_p);
    SD: New(SD_p);
    RE: New(RE_p);
    OP: New(OP_p);
    TK: New(TK_p);
  Else
    Bug('PObjNew: missing case',True)
  End;
  PObjNew := p
End;

{ release memory on the hype for p, a Prolog object of type t }
Procedure PObjDispose; (*( t : TypePrologObj; p : TPObjPtr );*)
Var
  PR_p : ProgPtr Absolute p;
  RU_p : RulePtr Absolute p; 
  QU_p : QueryPtr Absolute p; 
  SY_p : SysPtr Absolute p; 
  EQ_p : EqPtr Absolute p; 
  BT_p : BTermPtr Absolute p; 
  CO_p : ConstPtr Absolute p; 
  FU_p : FuncPtr Absolute p; 
  VA_p : VarPtr Absolute p; 
  ID_p : IdPtr Absolute p; 
  DE_p : DictPtr Absolute p; 
  HE_p : HeadPtr Absolute p; 
  ST_p : StrPtr Absolute p; 
  SD_p : StrDataPtr Absolute p; 
  RE_p : RestorePtr Absolute p;
  OP_p : OpPtr Absolute p;
  TK_p : TokenPtr Absolute p;
Begin
  CheckCondition((t <> CS) And (t <> CI) And (t <> CR),
      'type not meant for direct disposal');
  Case t Of
    PR: Dispose(PR_p);
    RU: Dispose(RU_p);
    QU: Dispose(QU_p);
    SY: Dispose(SY_p);
    EQ: Dispose(EQ_p);
    BT: Dispose(BT_p);
    CO: Dispose(CO_p);
    FU: Dispose(FU_p);
    VA: Dispose(VA_p);
    ID: Dispose(ID_p);
    DE: Dispose(DE_p);
    HE: Dispose(HE_p);
    ST: Dispose(ST_p);
    SD: Dispose(SD_p);
    RE: Dispose(RE_p);
    OP: Dispose(OP_p);
    TK: Dispose(TK_p);
  Else
    Bug('PObjDispose: missing case',True)
  End
End;

{ size of a Prolog object of type t }
Function PObjSizeOf; (*( t : TypePrologObj; p : TPObjPtr ) : Integer;*)
Var
  PR_p : ProgPtr Absolute p;
  RU_p : RulePtr Absolute p; 
  QU_p : QueryPtr Absolute p; 
  SY_p : SysPtr Absolute p; 
  EQ_p : EqPtr Absolute p; 
  BT_p : BTermPtr Absolute p; 
  CO_p : ConstPtr Absolute p; 
  FU_p : FuncPtr Absolute p; 
  VA_p : VarPtr Absolute p; 
  ID_p : IdPtr Absolute p; 
  DE_p : DictPtr Absolute p; 
  HE_p : HeadPtr Absolute p; 
  ST_p : StrPtr Absolute p; 
  SD_p : StrDataPtr Absolute p; 
  RE_p : RestorePtr Absolute p;
  OP_p : OpPtr Absolute p;
  TK_p : TokenPtr Absolute p;
Begin
  CheckCondition((t <> CS) And (t <> CI) And (t <> CR),
      'type not meant for size of');
  Case t Of
    PR: PObjSizeOf := SizeOf(PR_p^);
    RU: PObjSizeOf := SizeOf(RU_p^);
    QU: PObjSizeOf := SizeOf(QU_p^);
    SY: PObjSizeOf := SizeOf(SY_p^);
    EQ: PObjSizeOf := SizeOf(EQ_p^);
    BT: PObjSizeOf := SizeOf(BT_p^);
    CO: PObjSizeOf := SizeOf(CO_p^);
    FU: PObjSizeOf := SizeOf(FU_p^);
    VA: PObjSizeOf := SizeOf(VA_p^);
    ID: PObjSizeOf := SizeOf(ID_p^);
    DE: PObjSizeOf := SizeOf(DE_p^);
    HE: PObjSizeOf := SizeOf(HE_p^);
    ST: PObjSizeOf := SizeOf(ST_p^);
    SD: PObjSizeOf := SizeOf(SD_p^);
    RE: PObjSizeOf := SizeOf(RE_p^);
    OP: PObjSizeOf := SizeOf(OP_p^);
    TK: PObjSizeOf := SizeOf(TK_p^);
  Else
    Bug('PObjSizeOf: missing case',True)
  End
End;