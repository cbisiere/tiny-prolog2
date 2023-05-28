{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObj.pas                                                   }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                P R O L O G   O B J E C T S :   C O M M O N                 }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ term: constant, variable, functional symbol }
Type
  TermPtr = TPObjPtr;

{ return true if T1 and T2 are equal, that is: same variable, same
  identifier or same constant value; an invariant (unique constant 
  values and terms) simplify the test greatly, as testing checking 
  pointers are equal is enough }
Function SameTerms( T1,T2 : TermPtr ) : Boolean;
Begin
  SameTerms := T1 = T2
End;

{ arbitrary order on terms: are two terms ordered? }
Function OrderedTerms( T1,T2 : TermPtr ) : Boolean;
Begin
  CheckCondition((T1 <> Nil) And (T2 <> Nil),'Undefined order');
  OrderedTerms := ObjectGuid(T1) <= ObjectGuid(T2)
End;
