(*!m2pim*) (* Copyright (c) 2015 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE Token;

(* Token Subranges *)

TYPE
  ResWords = TokenT [Alias..While];
  Identifiers = TokenT [Address..OtherIdent];
  Numbers = TokenT [WholeNumber..RealNumber];
  CharsAndStrings = TokenT [Character..QuotedString];
  NonOpPunctuation = TokenT [Dot..Minus];
  Operators = TokenT [Equal..TypeConv];
  NonRWOperL1 = TokenT [Equal..LessOrEq];
  NonRWOperL2 = TokenT [Plus..SetDiff];
  NonRWOperL3 = TokenT [Asterisk..RealDiv];
  
  
(* Functions To Determine Token Classification *)

PROCEDURE isResWord ( t : TokenT ) : BOOLEAN;
 (* Returns TRUE if t is a reserved word, otherwise FALSE. *)
BEGIN
  RETURN (t >= MIN(ResWords) AND t <= MAX(ResWords))
END isResWord;


PROCEDURE isIdentifier ( t : TokenT ) : BOOLEAN;
 (* Returns TRUE if t is an identifier, otherwise FALSE. *)
BEGIN
  RETURN (t >= MIN(Identifiers) AND t <= MAX(Identifiers))
END isResWord;


PROCEDURE isNumber ( t : TokenT ) : BOOLEAN;
 (* Returns TRUE if t is a number literal, otherwise FALSE. *)
BEGIN
  RETURN (t >= MIN(Numbers) AND t <= MAX(Numbers))
END isNumber;


PROCEDURE isCharOrString ( t : TokenT ) : BOOLEAN;
 (* Returns TRUE if t is a character or string, otherwise FALSE. *)
BEGIN
  RETURN (t >= MIN(CharsAndStrings) AND t <= MAX(CharsAndStrings))
END isResWord;


PROCEDURE isPunctuation ( t : TokenT ) : BOOLEAN;
 (* Returns TRUE if t is a number literal, otherwise FALSE. *)
BEGIN
  RETURN
    (t >= MIN(NonOpPunctuation) AND t <= MAX(NonOpPunctuation)) OR
    (t = TokenT.Aster) OR (t = TokenT.Plus) OR (t = TokenT.Minus)
END isPunctuation;


PROCEDURE isOperL1 ( t : TokenT ) : BOOLEAN;
 (* Returns TRUE if t is a level-1 operator, otherwise FALSE. *)
BEGIN
  RETURN
    (t = TokenT.In) OR
    (t >= MIN(OperatorsL1) AND t <= MAX(OperatorsL1))
END isOperL1;


PROCEDURE isOperL2 ( t : TokenT ) : BOOLEAN;
 (* Returns TRUE if t is a level-2 operator, otherwise FALSE. *)
BEGIN
  RETURN
    (t = TokenT.Or) OR
    (t >= MIN(NonRWOperL2) AND t <= MAX(NonRWOperL2))
END isOperL2;


PROCEDURE isOperL3 ( t : TokenT ) : BOOLEAN;
 (* Returns TRUE if t is a level-3 operator, otherwise FALSE. *)
BEGIN
  RETURN
    (t = TokenT.And) OR (t = TokenT.Div) OR (t = TokenT.Mod) OR
    (t >= MIN(NonRWOperL2) AND t <= MAX(NonRWOperL2))
END isOperL3;


PROCEDURE isComment ( t : TokenT ) : BOOLEAN;
 (* Returns TRUE if t is a comment, otherwise FALSE. *)
BEGIN
  RETURN (t = TokenT.Comment)
END isComment;


PROCEDURE isPragma ( t : TokenT ) : BOOLEAN;
 (* Returns TRUE if t is a pragma, otherwise FALSE. *)
BEGIN
  RETURN (t = TokenT.Pragma)
END isPragma;


END Token.
