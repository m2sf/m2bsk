(*!m2pim*) (* Copyright (c) 2015 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE Token;

(* Token Subranges *)

TYPE
  Reswords = TokenT [Alias..Write];
  Identifiers = TokenT [StdIdent..Primitive];
  Numbers = TokenT [RealNumber..WholeNumber];
  ConstExprLiterals = TokenT [WholeNumber..QuotedString];
  CharsAndStrings = TokenT [CharCode..QuotedString];
  NonRWOperL1 = TokenT [Equal..Identity];
  NonRWOperL2 = TokenT [Plus..SetDiff];
  NonRWOperL3 = TokenT [Asterisk..RealDiv];
  
  
(* Functions To Determine Token Classification *)

PROCEDURE isResword ( t : TokenT ) : BOOLEAN;
 (* Returns TRUE if t is a reserved word, otherwise FALSE. *)
BEGIN
  RETURN (t >= MIN(Reswords) AND t <= MAX(Reswords))
END isResword;


PROCEDURE isIdentifier ( t : TokenT ) : BOOLEAN;
 (* Returns TRUE if t is an identifier, otherwise FALSE. *)
BEGIN
  RETURN (t >= MIN(Identifiers) AND t <= MAX(Identifiers))
END isIdentifier;


PROCEDURE isNumber ( t : TokenT ) : BOOLEAN;
 (* Returns TRUE if t is a number literal, otherwise FALSE. *)
BEGIN
  RETURN (t >= MIN(Numbers) AND t <= MAX(Numbers))
END isNumber;


PROCEDURE isCharOrString ( t : TokenT ) : BOOLEAN;
 (* Returns TRUE if t is a character or string, otherwise FALSE. *)
BEGIN
  RETURN (t >= MIN(CharsAndStrings) AND t <= MAX(CharsAndStrings))
END isCharOrString;


PROCEDURE isConstExprLiteral ( t : TokenT ) : BOOLEAN;
 (* Returns TRUE if t is a constant expression literal, otherwise FALSE. *)
BEGIN
  RETURN (t >= MIN(ConstExprLiterals) AND t <= MAX(ConstExprLiterals))
END isConstExprLiteral;


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
    (t = Or) OR
    (t >= MIN(NonRWOperL2) AND t <= MAX(NonRWOperL2))
END isOperL2;


PROCEDURE isOperL3 ( t : TokenT ) : BOOLEAN;
 (* Returns TRUE if t is a level-3 operator, otherwise FALSE. *)
BEGIN
  RETURN
    (t = And) OR (t = Div) OR (t = Mod) OR
    (t >= MIN(NonRWOperL2) AND t <= MAX(NonRWOperL2))
END isOperL3;


PROCEDURE isComment ( t : TokenT ) : BOOLEAN;
 (* Returns TRUE if t is a comment, otherwise FALSE. *)
BEGIN
  RETURN (t = BlockComment) OR (t = LineComment)
END isComment;


PROCEDURE isPragma ( t : TokenT ) : BOOLEAN;
 (* Returns TRUE if t is a pragma, otherwise FALSE. *)
BEGIN
  RETURN (t = Pragma)
END isPragma;


END Token.
